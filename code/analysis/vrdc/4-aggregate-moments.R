# 4-aggregate-moments.R — Survey-weighted aggregate moments
#
# Combines individual likelihood with three aggregate moments:
#   M1. Search rate     E[searched]   = E[1{K* > 0}]
#   M2. FFS share       E[FFS]        = E[chose FFS]
#   M3. MA tenure       E[MADVYRS|MA] = E[tenure-implied incumbent fraction]
#
# Each moment is one scalar comparison (predicted vs survey-weighted observed),
# weighted by 1 / sigma^2 in the GMM objective.

# ---- Observed targets (computed once at startup) --------------------

# M1: weighted mean of `searched` indicator
m1_obs <- weighted.mean(bene$searched_obs, w = bene$wgt_full_sample, na.rm = TRUE)

# M2: weighted mean of FFS choice (using MBSF admin truth)
m2_obs <- weighted.mean(bene$is_ffs_admin, w = bene$wgt_full_sample, na.rm = TRUE)

# M3: among MA enrollees with an observed prior plan, the fraction who CHOSE
# their incumbent (prior-year) plan. This matches the predicted side below,
# which is P(choose incumbent | MA). Note `incumbent_bene_year` on `bene` flags
# whether the prior plan is AVAILABLE in the market, not whether it was chosen,
# so it is the wrong observed analog for this moment.
ma_only <- bene %>% filter(is_ffs_admin == 0, has_prior_year == 1L)
m3_obs <- weighted.mean(as.integer(ma_only$chosen_pid == ma_only$prior_plan_id),
                        w = ma_only$wgt_full_sample, na.rm = TRUE)

message(sprintf("Observed moments:\n  M1 searched rate     : %.3f", m1_obs))
message(sprintf("  M2 FFS share         : %.3f", m2_obs))
message(sprintf("  M3 incumbent | MA    : %.3f", m3_obs))


# ---- Predicted targets at theta -------------------------------------

compute_aggregate_moments <- function(theta) {
  components <- compute_individual_loglik(theta, return_components = TRUE)
  searched_pred <- components$searched_pred

  # Build predicted choice (P_FFS, P_MA aggregate)
  th <- unpack_theta(theta)
  is_ffs_pred <- numeric(nrow(bene))
  incumbent_choice_pred <- numeric(nrow(bene))

  for (i in seq_len(nrow(bene))) {
    mid <- bene$market_id[i]
    mkt <- markets[[mid]]
    v   <- compute_market_utility(mkt, th)
    sal <- compute_salience(mkt, th)
    K_star <- components$K_star[i]
    p <- compute_bene_choice_prob(mkt, v, sal, K_star)

    ffs_idx <- which(mkt$plan_kind == "FFS")
    is_ffs_pred[i] <- p[ffs_idx]

    # Incumbent-pick probability: weight prob mass on incumbent plans.
    # Incumbency is bene-specific (it depends on THIS bene's prior plan), so
    # build the flag from bene$prior_plan_id[i] against this market's plans —
    # not from a market-level column.
    inc <- as.integer(mkt$plan_id == bene$prior_plan_id[i])
    inc[is.na(inc)] <- 0
    inc[ffs_idx]  <- 0  # FFS is not an MA-incumbent
    p_inc_among_ma <- if (sum(p[mkt$plan_kind == "MA"]) > 0) {
      sum(p[mkt$plan_kind == "MA"] * inc[mkt$plan_kind == "MA"]) /
        sum(p[mkt$plan_kind == "MA"])
    } else 0
    incumbent_choice_pred[i] <- p_inc_among_ma
  }

  m1_pred <- weighted.mean(searched_pred, w = bene$wgt_full_sample)
  m2_pred <- weighted.mean(is_ffs_pred,   w = bene$wgt_full_sample)
  ma_prior <- bene$is_ffs_admin == 0 & bene$has_prior_year == 1L
  m3_pred <- weighted.mean(
    incumbent_choice_pred[ma_prior],
    w = bene$wgt_full_sample[ma_prior]
  )

  c(M1 = m1_pred - m1_obs,
    M2 = m2_pred - m2_obs,
    M3 = m3_pred - m3_obs)
}


# ---- Combined GMM-style objective -----------------------------------
# Combines individual likelihood with the 3 aggregate moments. The
# weighting between the two is governed by LAMBDA — set to 0 for pure
# MLE, set to a large number to put most weight on aggregate moments.

LAMBDA <- 1e3   # default: aggregate moments matter at survey-weighted scale

compute_combined_objective <- function(theta) {
  ll <- compute_individual_loglik(theta)
  m  <- compute_aggregate_moments(theta)

  # Negative log-likelihood + lambda * sum-of-squared-moments
  -ll + LAMBDA * sum(m^2)
}
