# 2-simulate-shares.R — Compute model-implied shares given parameters
#
# Goeree-style random-consideration-set choice with Stigler-optimal K*.
# c_m varies across county-year markets via demographic heterogeneity:
#
#   c_m = exp(gamma0 + g_log_inc * log_inc_m + g_bach * pct_bach_m
#                    + g_65 * pct_65plus_m)
#
# Demographics are constant within a market (same value across plan rows),
# so we read them off the first row.
#
# theta is a named list with 10 fields:
#   alpha       coefficient on mean_cost (per $1000)
#   delta       coefficient on var_cost  (per $1e6)
#   beta        coefficient on Star_Rating (NA -> 0)
#   xi_ffs      FFS scope-of-benefits intercept
#   gamma0      log search cost intercept
#   g_log_inc   search cost: log(median HH income)
#   g_bach      search cost: % bachelors
#   g_65        search cost: % 65+
#   eta1        salience: incumbent dummy
#   eta2        salience: log(insurer_share)
#
# Requires `markets` from 1-load-panel.R.

theta_default <- list(
  alpha     = 0.5,
  delta     = 0.05,
  beta      = 0.5,
  xi_ffs    = 6,
  gamma0    = log(0.1),
  g_log_inc = 0,
  g_bach    = 0,
  g_65      = 0,
  eta1      = 0.5,
  eta2      = 0.6
)

LOG_SHARE_FLOOR <- log(1e-3)

compute_market_shares <- function(mkt, theta) {
  is_ffs <- mkt$plan_kind == "FFS"
  is_ma  <- !is_ffs

  star <- ifelse(is.na(mkt$Star_Rating), 0, mkt$Star_Rating)
  v <- -theta$alpha * mkt$mean_cost / 1000 -
        theta$delta * mkt$var_cost  / 1e6 +
        theta$beta  * star +
        ifelse(is_ffs, theta$xi_ffs, 0)

  v_ffs <- v[is_ffs]
  v_ma  <- v[is_ma]
  K_m   <- length(v_ma)

  if (K_m == 0L) {
    pred <- numeric(length(v))
    pred[is_ffs] <- 1
    return(list(K_star = 0L, pred = pred))
  }

  ish <- mkt$insurer_share[is_ma]
  ish[is.na(ish) | ish < 1e-6] <- 1e-6
  log_ish <- log(ish)
  log_ish[log_ish < LOG_SHARE_FLOOR] <- LOG_SHARE_FLOOR
  log_w <- theta$eta1 * as.numeric(mkt$incumbent[is_ma]) +
           theta$eta2 * log_ish
  w <- exp(log_w - max(log_w))
  W <- sum(w)

  vmax  <- max(v)
  ev_ma  <- exp(v_ma  - vmax)
  ev_ffs <- exp(v_ffs - vmax)

  M  <- sum(w * ev_ma) / W

  log_inc_m  <- mkt$log_inc[1]
  pct_bach_m <- mkt$pct_bachelors_p[1]
  pct_65_m   <- mkt$pct_65plus[1]
  cm <- exp(theta$gamma0 +
            theta$g_log_inc * log_inc_m +
            theta$g_bach    * pct_bach_m +
            theta$g_65      * pct_65_m)

  K_grid <- 0:K_m
  obj <- log(ev_ffs + K_grid * M) - cm * K_grid
  K_star <- K_grid[which.max(obj)]

  if (K_star == 0L) {
    pred <- numeric(length(v))
    pred[is_ffs] <- 1
    return(list(K_star = 0L, pred = pred))
  }

  phi <- K_star * w / W
  phi[phi > 1] <- 1
  denom <- ev_ffs + sum(phi * ev_ma)

  pred <- numeric(length(v))
  pred[is_ffs] <- ev_ffs / denom
  pred[is_ma]  <- phi * ev_ma / denom
  list(K_star = K_star, pred = pred)
}

compute_all_shares <- function(markets, theta) {
  res     <- vector("list",    length(markets))
  K_stars <- integer(          length(markets))
  for (i in seq_along(markets)) {
    out <- compute_market_shares(markets[[i]], theta)
    res[[i]]   <- out$pred
    K_stars[i] <- out$K_star
  }
  list(pred = unlist(res), K_star = K_stars)
}

t0 <- Sys.time()
out <- compute_all_shares(markets, theta_default)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
message(sprintf("\nBaseline evaluation: %.1f sec across %d markets.",
                elapsed, length(markets)))

panel$predicted_share <- out$pred

message("\nK* distribution at default theta:")
print(summary(out$K_star))
