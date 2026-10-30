# model-utility.R — Utility regressors, one row per (beneficiary-year, option).
# Columns are in utility_names order, so v = X %*% theta[utility_names].
# is_inc marks the beneficiary's prior-year option, which is traditional
# Medicare for a beneficiary who was in traditional Medicare the year before.

utility_design <- function(prem, oop, star, has_pd, category, is_tm, is_inc) {
  ma <- as.numeric(!is_tm)
  star0 <- ifelse(is.na(star), 0, star - 3.5)
  pd <- ifelse(is.na(has_pd), 0, as.numeric(has_pd))
  cat <- as.character(category)
  cbind(
    alpha_prem = -prem / 1000,
    alpha_oop  = -oop / 1000,
    beta       = ma * star0,
    xi_TM      = as.numeric(is_tm),
    psi        = as.numeric(is_inc),
    delta_PD   = ma * pd,
    cat_PPO    = ma * (!is.na(cat) & cat == "PPO"),
    cat_PFFS   = ma * (!is.na(cat) & cat == "PFFS")
  )
}
