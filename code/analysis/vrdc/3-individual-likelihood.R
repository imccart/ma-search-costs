# 3-individual-likelihood.R — Per-bene Stigler-search choice probability
#
# Three-stage Plan-Finder-anchored ordered-search model (see agents/model.md).
# For each MCBS respondent i in market (c, t):
#   - c_i = c_0 * exp(gamma_X' X_i + gamma_net*KVSITWEB + gamma_help*KCHIHELP_help
#                     + gamma_delegate*KCHIHELP_delegate)
#   - eta_ij = lambda_PF_i * s_PF_j + lambda_broker_i * s_broker_j + lambda_inc * s_inc_ij
#       where lambda_PF_i and lambda_broker_i are channel-conditional on
#       KVSITWEB / KCHIHELP type (model.md "Channel-conditional slopes")
#   - K*_i = arg max E[max U(K)] - c_i K  (Stigler's optimal sample size)
#   - phi_j(K) = K * w_j / W  (Goeree-style linear-inclusion approximation)
#   - P(j_i | theta, X_i, market) = phi_j * exp(v_j) / [exp(v_FFS) + sum_k phi_k exp(v_k)]
#
# Output:
#   compute_individual_loglik(theta) — log-likelihood at parameter vector

LOG_SHARE_FLOOR <- log(1e-6)


# ---- Pack/unpack theta ----------------------------------------------

# Parameter vector layout (for nloptr). 21 params total.
#
# Utility (4):
#   1.  alpha                  cost coef                       (>= 0)
#   2.  delta                  variance coef                   (>= 0)
#   3.  beta                   Star Rating coef                (free)
#   4.  xi_FFS                 FFS intercept                   (free)
#
# Search-cost heterogeneity (9):
#   5.  gamma_0                base log search cost            (free)
#   6.  gamma_inc              income gradient                 (free)
#   7.  gamma_educ             education gradient              (free)
#   8.  gamma_age              age gradient                    (free)
#   9.  gamma_dual             dual flag gradient              (free)
#   10. gamma_adi              ADI gradient                    (free)
#   11. gamma_net              KVSITWEB (PF use) gradient      (free; expect <0)
#   12. gamma_help             KCHIHELP=2 gradient             (free; expect <0)
#   13. gamma_delegate         KCHIHELP=3 gradient             (free; expect <0,
#                                                                < gamma_help)
#
# Awareness/prominence (8):
#   14. lambda_PF_0            PF baseline weight              (>= 0)
#   15. lambda_PF_online       KVSITWEB interaction with PF    (>= 0)
#   16. lambda_PF_help         KCHIHELP=2 interaction with PF  (>= 0)
#   17. lambda_PF_delegate     KCHIHELP=3 interaction with PF  (>= 0)
#   18. lambda_broker_0        broker baseline weight          (>= 0)
#   19. lambda_broker_help     KCHIHELP=2 with broker          (>= 0)
#   20. lambda_broker_delegate KCHIHELP=3 with broker          (>= 0)
#   21. lambda_inc             incumbent free-inclusion weight (>= 0)

theta_names <- c(
  "alpha", "delta", "beta", "xi_FFS",
  "gamma_0", "gamma_inc", "gamma_educ", "gamma_age",
  "gamma_dual", "gamma_adi",
  "gamma_net", "gamma_help", "gamma_delegate",
  "lambda_PF_0", "lambda_PF_online", "lambda_PF_help", "lambda_PF_delegate",
  "lambda_broker_0", "lambda_broker_help", "lambda_broker_delegate",
  "lambda_inc"
)

# Bounds: alpha, delta, all lambdas non-negative; everything else free.
theta_lower <- c(
  0, 0, -Inf, -Inf,                       # alpha, delta, beta, xi_FFS
  rep(-Inf, 9),                           # gammas (all free)
  rep(0, 8)                               # all lambdas (>= 0)
)
theta_upper <- rep(Inf, length(theta_names))
stopifnot(length(theta_lower) == length(theta_names))

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


# ---- Market-level prominence components (precomputable per market) --
#
# s_PF and s_broker depend only on plan and county-year attributes, not on
# the bene's channel use. Precompute once per market and cache.
#
# s_PF_j        = pf_rank_score (already in (0, 1], from script 14)
# s_broker_j    = parent_org_loo_national * broker_density_per_k
# (FFS rows: both = 0; FFS prominence is captured via xi_FFS in the utility,
#  not via the prominence ordering — it gets free inclusion in the considered
#  set anyway.)

compute_market_prominence <- function(mkt) {
  is_ma <- mkt$plan_kind == "MA"

  s_PF <- numeric(nrow(mkt))
  s_PF[is_ma] <- ifelse(is.na(mkt$pf_rank_score[is_ma]), 0,
                        mkt$pf_rank_score[is_ma])

  loo <- ifelse(is.na(mkt$parent_org_loo_national[is_ma]), 0,
                mkt$parent_org_loo_national[is_ma])
  bdens <- ifelse(is.na(mkt$broker_density_per_k[is_ma]), 0,
                  mkt$broker_density_per_k[is_ma])
  s_broker <- numeric(nrow(mkt))
  s_broker[is_ma] <- loo * bdens

  list(
    s_PF     = s_PF,
    s_broker = s_broker,
    is_ma    = is_ma
  )
}


# ---- Bene-specific salience (channel-conditional lambdas + incumbent) -

compute_salience <- function(mkt, prom, bene_row, th) {
  is_ma <- prom$is_ma

  # Channel-conditional lambdas
  lam_PF <- th$lambda_PF_0 +
            th$lambda_PF_online   * bene_row$KVSITWEB_use +
            th$lambda_PF_help     * bene_row$KCHIHELP_help +
            th$lambda_PF_delegate * bene_row$KCHIHELP_delegate

  lam_broker <- th$lambda_broker_0 +
                th$lambda_broker_help     * bene_row$KCHIHELP_help +
                th$lambda_broker_delegate * bene_row$KCHIHELP_delegate

  # Bene-specific incumbent indicator: 1 if plan is bene's prior plan.
  # FFS rows always 0 (FFS is the outside option, not a "considered MA plan").
  inc_ma <- as.integer(
    !is.na(bene_row$prior_plan_id) &
    bene_row$prior_plan_id != "" &
    bene_row$prior_plan_id != "FFS" &
    mkt$plan_id[is_ma] == bene_row$prior_plan_id
  )

  # Composite log-prominence over MA plans only (FFS gets free inclusion in
  # the considered set independently of the ordering).
  log_w <- lam_PF     * prom$s_PF[is_ma] +
           lam_broker * prom$s_broker[is_ma] +
           th$lambda_inc * inc_ma

  # Numerically stable softmax-style weights
  w <- exp(log_w - max(log_w))
  list(
    w     = w,
    W     = sum(w),
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
# c_i is bene-specific; v and salience are market-and-bene-specific.
# Expected max-utility over a salience-weighted draw is concave in K, so we
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
  th$gamma_inc      * bene_row$log_inc_dm  +
  th$gamma_educ     * bene_row$educ_yrs_dm +
  th$gamma_age      * bene_row$age_dm      +
  th$gamma_dual     * bene_row$is_dual     +
  th$gamma_adi      * bene_row$adi_dm      +
  th$gamma_net      * bene_row$KVSITWEB_use +
  th$gamma_help     * bene_row$KCHIHELP_help +
  th$gamma_delegate * bene_row$KCHIHELP_delegate
}


# ---- Full individual log-likelihood ---------------------------------

compute_individual_loglik <- function(theta, return_components = FALSE) {
  th <- unpack_theta(theta)

  ll <- numeric(nrow(bene))
  searched_pred <- numeric(nrow(bene))
  K_star_vec    <- integer(nrow(bene))

  # Precompute v and market-level prominence (s_PF, s_broker) once per
  # market — these depend only on theta + market data, not on bene channel.
  market_v    <- vector("list", length(markets))
  market_prom <- vector("list", length(markets))
  for (m in seq_along(markets)) {
    market_v[[m]]    <- compute_market_utility(markets[[m]], th)
    market_prom[[m]] <- compute_market_prominence(markets[[m]])
  }

  for (i in seq_len(nrow(bene))) {
    mid <- bene$market_id[i]
    mkt <- markets[[mid]]
    v   <- market_v[[mid]]
    prom <- market_prom[[mid]]

    # Channel-conditional lambdas + bene-specific incumbent enter here.
    sal <- compute_salience(mkt, prom, bene[i, ], th)

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
