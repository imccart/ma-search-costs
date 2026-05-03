# 3b-rf-moments.R — Reduced-form orthogonality moments (precomputed)
#
# Implements 5 RF moments via Frisch-Waugh-Lovell residualization and
# inner products. All FE-residualizations are computed once at startup
# from observed data; per-iteration cost is just aggregate+inner-product.
#
# Y definitions at the county-year level (always defined):
#   y_dom_pop    = (sum dominated MA share) / total_eligibles
#   y_takeup_pop = (sum all     MA share) / total_eligibles
#
# Note: paper-published slopes (+26.6 / +13.8) use within-MA dominated
# share × 100; we use dominated-of-population share to keep the moment
# defined when predicted MA take-up = 0. The slope target is recomputed
# on the same definition, so the structural model matches the analogous
# observed slope (typically smaller than 26.6, but on a well-defined
# object).
#
# Five moments:
#   1. Bartik 2SLS slope of y_dom_pop on log_n_plans
#   2. Bartik 2SLS slope of y_takeup_pop on log_n_plans
#   3-5. OLS slope of y_dom_pop on each of (log_inc, pct_65plus,
#        pct_bachelors_p), holding fixed the others + log_pop + FE.
#
# Each moment expressed as fractional deviation: (slope_pred - slope_obs) /
# |slope_obs|. Comparable across moments and to the BLP residual moments
# (after MOMENT_SCALE rebalancing in 4-estimate-gmm.R).

# County-year observed Y (one row per market, in market_id order)
cy_obs <- panel %>%
  mutate(
    is_ma     = plan_kind == "MA",
    is_ma_dom = is_ma & !is.na(dominated) & dominated
  ) %>%
  group_by(market_id) %>%
  summarize(
    county_fips     = first(county_fips),
    year            = first(year),
    obs_y_dom_pop   = sum(observed_share[is_ma_dom]),
    obs_y_takeup    = sum(observed_share[is_ma]),
    total_eligibles = first(total_eligibles),
    .groups = "drop"
  ) %>%
  inner_join(cy_panel, by = c("county_fips", "year")) %>%
  arrange(market_id)

message("County-year sample for RF moments: ", nrow(cy_obs))

W_CY <- cy_obs$total_enrollment

winner <- function(a, b, w = W_CY) sum(a * b * w) / sum(w)

# ---- Residualize instruments on (demographics + log_pop + FE) ----
fit_z_bartik <- feols(
  bartik_pffs ~ log_inc + pct_65plus + pct_bachelors_p + log_pop | state + year,
  data = cy_obs, weights = ~ total_enrollment, notes = FALSE
)
Z_RESID_BARTIK <- as.numeric(resid(fit_z_bartik))

fit_x_log_n <- feols(
  log_n_plans ~ log_inc + pct_65plus + pct_bachelors_p + log_pop | state + year,
  data = cy_obs, weights = ~ total_enrollment, notes = FALSE
)
X_RESID_LOG_N <- as.numeric(resid(fit_x_log_n))

DENOM_BARTIK <- winner(X_RESID_LOG_N, Z_RESID_BARTIK)

# ---- Residualize each demographic on (other demographics + log_pop + FE) ----
fit_log_inc <- feols(
  log_inc ~ pct_65plus + pct_bachelors_p + log_pop | state + year,
  data = cy_obs, weights = ~ total_enrollment, notes = FALSE
)
LOG_INC_RESID <- as.numeric(resid(fit_log_inc))

fit_pct_65 <- feols(
  pct_65plus ~ log_inc + pct_bachelors_p + log_pop | state + year,
  data = cy_obs, weights = ~ total_enrollment, notes = FALSE
)
PCT_65_RESID <- as.numeric(resid(fit_pct_65))

fit_pct_bach <- feols(
  pct_bachelors_p ~ log_inc + pct_65plus + log_pop | state + year,
  data = cy_obs, weights = ~ total_enrollment, notes = FALSE
)
PCT_BACH_RESID <- as.numeric(resid(fit_pct_bach))

DENOM_LOG_INC  <- winner(LOG_INC_RESID,  LOG_INC_RESID)
DENOM_PCT_65   <- winner(PCT_65_RESID,   PCT_65_RESID)
DENOM_PCT_BACH <- winner(PCT_BACH_RESID, PCT_BACH_RESID)

# ---- Observed slopes (one-time) ----
SLOPE_BARTIK_DOM_OBS    <- winner(cy_obs$obs_y_dom_pop, Z_RESID_BARTIK) / DENOM_BARTIK
SLOPE_BARTIK_TAKEUP_OBS <- winner(cy_obs$obs_y_takeup,  Z_RESID_BARTIK) / DENOM_BARTIK
SLOPE_LOG_INC_OBS       <- winner(cy_obs$obs_y_dom_pop, LOG_INC_RESID)  / DENOM_LOG_INC
SLOPE_PCT_65_OBS        <- winner(cy_obs$obs_y_dom_pop, PCT_65_RESID)   / DENOM_PCT_65
SLOPE_PCT_BACH_OBS      <- winner(cy_obs$obs_y_dom_pop, PCT_BACH_RESID) / DENOM_PCT_BACH

message(sprintf("\nObserved slope targets:"))
message(sprintf("  Bartik 2SLS dom_pop  : %+.4f", SLOPE_BARTIK_DOM_OBS))
message(sprintf("  Bartik 2SLS takeup   : %+.4f", SLOPE_BARTIK_TAKEUP_OBS))
message(sprintf("  log_inc  -> dom_pop  : %+.4f", SLOPE_LOG_INC_OBS))
message(sprintf("  pct_65+  -> dom_pop  : %+.4f", SLOPE_PCT_65_OBS))
message(sprintf("  pct_bach -> dom_pop  : %+.4f", SLOPE_PCT_BACH_OBS))

# ---- Aggregator + per-iter moment computation ----

# Fast aggregation via base-R rowsum: returns length-N_MARKETS vectors
# aligned with market_id 1..N_MARKETS. Markets with zero MA plans get 0.
aggregate_to_cy <- function(predicted_share) {
  ma_pred <- predicted_share[PANEL_MA_IDX]
  takeup_grouped <- rowsum(ma_pred, MA_MARKET_ID)
  takeup_ids <- as.integer(rownames(takeup_grouped))
  takeup <- numeric(N_MARKETS)
  takeup[takeup_ids] <- takeup_grouped[, 1]

  dom_pred <- predicted_share[PANEL_MA_DOM_IDX]
  if (length(dom_pred) > 0L) {
    dom_grouped <- rowsum(dom_pred, MA_DOM_MARKET_ID)
    dom_ids <- as.integer(rownames(dom_grouped))
    dom <- numeric(N_MARKETS)
    dom[dom_ids] <- dom_grouped[, 1]
  } else {
    dom <- numeric(N_MARKETS)
  }

  list(takeup = takeup, dom = dom)
}

compute_rf_moments <- function(panel_with_pred) {
  cy <- aggregate_to_cy(panel_with_pred$predicted_share)

  s_b_dom    <- winner(cy$dom,    Z_RESID_BARTIK) / DENOM_BARTIK
  s_b_takeup <- winner(cy$takeup, Z_RESID_BARTIK) / DENOM_BARTIK
  s_inc      <- winner(cy$dom,    LOG_INC_RESID)  / DENOM_LOG_INC
  s_65       <- winner(cy$dom,    PCT_65_RESID)   / DENOM_PCT_65
  s_bach     <- winner(cy$dom,    PCT_BACH_RESID) / DENOM_PCT_BACH

  c(
    bartik_dom    = (s_b_dom    - SLOPE_BARTIK_DOM_OBS)    / abs(SLOPE_BARTIK_DOM_OBS),
    bartik_takeup = (s_b_takeup - SLOPE_BARTIK_TAKEUP_OBS) / abs(SLOPE_BARTIK_TAKEUP_OBS),
    log_inc_dom   = (s_inc      - SLOPE_LOG_INC_OBS)       / abs(SLOPE_LOG_INC_OBS),
    pct_65_dom    = (s_65       - SLOPE_PCT_65_OBS)        / abs(SLOPE_PCT_65_OBS),
    pct_bach_dom  = (s_bach     - SLOPE_PCT_BACH_OBS)      / abs(SLOPE_PCT_BACH_OBS)
  )
}
