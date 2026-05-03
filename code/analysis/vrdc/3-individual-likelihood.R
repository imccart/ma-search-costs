# 3-individual-likelihood.R — Per-bene Stigler-search choice probability
#
# For each MCBS respondent i in market (c, t):
#   - c_i = exp(gamma_0 + gamma_1' X_i)
#   - K*_i = arg max E[max U(K)] - c_i K  (Stigler's optimal sample size)
#   - phi_j(K) = K * w_j / W   (Goeree linear-inclusion approximation)
#   - P(j_i | theta, X_i, market) = phi_j * exp(v_j) / [exp(v_FFS) + sum_k phi_k exp(v_k)]
#
# Output:
#   compute_individual_loglik(theta) — log-likelihood at parameter vector

LOG_SHARE_FLOOR <- log(1e-6)


# ---- Pack/unpack theta ----------------------------------------------

# Parameter vector layout (for nloptr):
#   1.  alpha            cost coef                     (>= 0)
#   2.  delta            variance coef                 (>= 0)
#   3.  beta             quality coef                  (free)
#   4.  xi_FFS           FFS intercept                 (free)
#   5.  gamma_0          base log search cost          (free)
#   6.  gamma_inc        income gradient               (free)
#   7.  gamma_educ       education gradient            (free)
#   8.  gamma_age        age gradient                  (free)
#   9.  gamma_inet       internet flag gradient        (free)
#   10. gamma_dual       dual flag gradient            (free)
#   11. gamma_adi        ADI gradient                  (free)
#   12. eta_1            incumbent salience weight     (>= 0)
#   13. eta_2            insurer-share salience weight (>= 0)

theta_names <- c(
  "alpha","delta","beta","xi_FFS",
  "gamma_0","gamma_inc","gamma_educ","gamma_age",
  "gamma_inet","gamma_dual","gamma_adi",
  "eta_1","eta_2"
)
theta_lower <- c(0, 0, -Inf, -Inf, rep(-Inf, 7), 0, 0)
theta_upper <- c(rep(Inf, length(theta_names)))

unpack_theta <- function(theta) {
  setNames(as.list(theta), theta_names)
}


# ---- Per-bene utility v_{j,ct} (market-level, X_i not in v) ---------

compute_market_utility <- function(mkt, th) {
  # mkt: tibble for one market with FFS row + MA rows
  v <- numeric(nrow(mkt))
  is_ffs <- mkt$plan_kind == "FFS"
  is_ma  <- !is_ffs

  v[is_ffs] <- - th$alpha * mkt$mean_cost[is_ffs] -
                 th$delta * mkt$var_cost[is_ffs] +
                 th$xi_FFS

  v[is_ma]  <- - th$alpha * mkt$mean_cost[is_ma] -
                 th$delta * mkt$var_cost[is_ma] +
                 th$beta  * ifelse(is.na(mkt$Star_Rating[is_ma]), 0,
                                   mkt$Star_Rating[is_ma] - 3.5)

  v
}


# ---- Salience weights for MA plans within a market ------------------

compute_salience <- function(mkt, th, override_incumbent = NULL) {
  is_ma <- mkt$plan_kind == "MA"
  ish   <- mkt$insurer_share[is_ma]
  ish[is.na(ish) | ish < 1e-6] <- 1e-6
  log_ish <- log(ish)
  log_ish[log_ish < LOG_SHARE_FLOOR] <- LOG_SHARE_FLOOR

  inc <- if (is.null(override_incumbent)) mkt$incumbent[is_ma] else override_incumbent
  inc[is.na(inc)] <- 0

  log_w <- th$eta_1 * as.numeric(inc) + th$eta_2 * log_ish
  w <- exp(log_w - max(log_w))
  list(
    w = w,
    W = sum(w),
    is_ma = is_ma
  )
}


# ---- Per-bene choice probabilities given K* and salience ------------

compute_bene_choice_prob <- function(mkt, v, sal, K_star) {
  is_ffs <- mkt$plan_kind == "FFS"
  is_ma  <- sal$is_ma
  K_m    <- sum(is_ma)

  # phi_j(K*) = K* * w_j / W; cap at 1
  phi <- numeric(nrow(mkt))
  phi[is_ffs] <- 1
  if (K_star > 0 && K_m > 0) {
    raw <- K_star * sal$w / sal$W
    raw[raw > 1] <- 1
    phi[is_ma] <- raw
  } else {
    phi[is_ma] <- 0
  }

  # Goeree-style choice probability
  ev <- exp(v - max(v))
  num <- phi * ev
  denom <- sum(num)
  num / denom
}


# ---- Stigler-optimal K* per bene ------------------------------------
# c_i is bene-specific; v and salience are market-level. The expected
# max-utility over a salience-weighted draw is concave in K, so we just
# evaluate over K = 0..K_m and take the argmax.

compute_K_star <- function(mkt, v, sal, c_i) {
  is_ma <- sal$is_ma
  K_m   <- sum(is_ma)
  if (K_m == 0) return(0L)

  # Salience-weighted moments of v over MA plans
  v_ma <- v[is_ma]
  w    <- sal$w / sal$W
  ev_ma_weighted <- sum(w * exp(v_ma))   # E[exp(v_j)] under salience
  v_ffs <- v[!is_ma]

  # E[log(exp(v_FFS) + K * E[exp(v_j)])] - c_i * K
  # Approximation: agent expects mean exp(v) each draw (Goeree)
  M <- ev_ma_weighted
  ev_ffs <- exp(v_ffs)
  K_grid <- 0:K_m
  obj <- log(ev_ffs + K_grid * M) - c_i * K_grid
  K_grid[which.max(obj)]
}


# ---- Bene-specific log search cost ----------------------------------

compute_log_c <- function(bene_row, th) {
  th$gamma_0 +
  th$gamma_inc  * bene_row$log_inc_dm +
  th$gamma_educ * bene_row$educ_yrs_dm +
  th$gamma_age  * bene_row$age_dm +
  th$gamma_inet * bene_row$has_inet +
  th$gamma_dual * bene_row$is_dual +
  th$gamma_adi  * bene_row$adi_dm
}


# ---- Full individual log-likelihood ---------------------------------

compute_individual_loglik <- function(theta, return_components = FALSE) {
  th <- unpack_theta(theta)

  ll <- numeric(nrow(bene))
  searched_pred <- numeric(nrow(bene))
  K_star_vec    <- integer(nrow(bene))

  # Precompute v and salience per market (depend only on theta + market data)
  market_v    <- vector("list", length(markets))
  market_sal  <- vector("list", length(markets))
  for (m in seq_along(markets)) {
    market_v[[m]]   <- compute_market_utility(markets[[m]], th)
    market_sal[[m]] <- compute_salience(markets[[m]], th)
  }

  for (i in seq_len(nrow(bene))) {
    mid <- bene$market_id[i]
    mkt <- markets[[mid]]
    v   <- market_v[[mid]]

    # If this bene-plan combination had bene-specific incumbent override,
    # we'd recompute salience here with override_incumbent. For v1 we use
    # the market-level incumbent column.
    sal <- market_sal[[mid]]

    log_c <- compute_log_c(bene[i, ], th)
    c_i   <- exp(log_c)
    K_star <- compute_K_star(mkt, v, sal, c_i)
    K_star_vec[i] <- K_star

    p <- compute_bene_choice_prob(mkt, v, sal, K_star)
    p_chosen <- p[bene$choice_idx[i]]
    ll[i] <- log(pmax(p_chosen, 1e-12))

    searched_pred[i] <- as.numeric(K_star > 0)
  }

  if (return_components) {
    list(
      ll            = ll,
      ll_total      = sum(bene$wgt_full_sample * ll),
      searched_pred = searched_pred,
      K_star        = K_star_vec
    )
  } else {
    sum(bene$wgt_full_sample * ll)
  }
}
