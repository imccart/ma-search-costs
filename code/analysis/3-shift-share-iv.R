# 3-shift-share-iv.R — Shift-share (Bartik) IV for log(n_plans)
#
# OLS in 2-reduced-form.R cannot separate the supply-side mechanical effect
# of n_plans on dominated enrollment from a behavioral search-cost effect.
# This script uses a Bartik instrument built from baseline (2008) county-level
# plan-type composition x national plan-type growth rates.
#
#   z_{c,t} = sum_k  s_{c,k,2008}  *  log(national_plans_k,t / national_plans_k,2008)
#
# Headline design uses just the PFFS component, exploiting the MIPPA 2008
# network rule that forced national PFFS contraction starting in 2010-2011.
# Counties with high 2008 PFFS dependence saw larger plan-count drops as PFFS
# exited the market.
#
# Validity (Goldsmith-Pinkham, Sorkin, Swift 2020): if 2008 baseline shares
# are exogenous to 2010+ search-cost dynamics, the Bartik is a valid IV
# regardless of where the shocks come from. The 2008 baseline predates any
# of the federal regulatory action driving the shocks (MIPPA 2008 took
# effect in 2011), so this is plausible.
#
# Specs:
#   - First stage: log(n_plans) on bartik_pffs (and bartik_total for robustness)
#   - Reduced form: pct_enrollment_dominated on bartik_pffs
#   - 2SLS: pct_enrollment_dominated on instrumented log(n_plans)
#
# Input:  data/output/analysis_panel.csv (with bartik columns merged in)
# Output: results/tables/shift-share-iv.tex

options(modelsummary_factory_default = "kableExtra",
        modelsummary_format_numeric_latex = "plain")

# ---------------------------------------------------------------------------
# Read panel
# ---------------------------------------------------------------------------

panel <- read_csv(
  "data/output/analysis_panel.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
) %>%
  mutate(
    log_n_plans = log(n_plans),
    log_pop     = log(total_pop),
    log_inc     = log(median_hh_income)
  )

stopifnot(all(c("bartik_pffs", "bartik_total") %in% names(panel)))

message("Analysis panel: ", nrow(panel), " county-years (",
        min(panel$year), "-", max(panel$year), ")")

ctrl_rhs <- "log_inc + pct_65plus + pct_bachelors_p + log_pop"

# ---------------------------------------------------------------------------
# First stage — log(n_plans) on bartik
# ---------------------------------------------------------------------------

fs_pffs <- feols(
  as.formula(paste("log_n_plans ~ bartik_pffs +", ctrl_rhs, "| state + year")),
  data = panel, weights = ~ total_enrollment, cluster = ~ county_fips
)

fs_total <- feols(
  as.formula(paste("log_n_plans ~ bartik_total +", ctrl_rhs, "| state + year")),
  data = panel, weights = ~ total_enrollment, cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Reduced form — outcome on bartik
# ---------------------------------------------------------------------------

rf_pffs <- feols(
  as.formula(paste("pct_enrollment_dominated ~ bartik_pffs +", ctrl_rhs,
                   "| state + year")),
  data = panel, weights = ~ total_enrollment, cluster = ~ county_fips
)

rf_total <- feols(
  as.formula(paste("pct_enrollment_dominated ~ bartik_total +", ctrl_rhs,
                   "| state + year")),
  data = panel, weights = ~ total_enrollment, cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# 2SLS — instrumented log(n_plans) → outcome
# ---------------------------------------------------------------------------

iv_pffs <- feols(
  as.formula(paste("pct_enrollment_dominated ~", ctrl_rhs,
                   "| state + year | log_n_plans ~ bartik_pffs")),
  data = panel, weights = ~ total_enrollment, cluster = ~ county_fips
)

iv_total <- feols(
  as.formula(paste("pct_enrollment_dominated ~", ctrl_rhs,
                   "| state + year | log_n_plans ~ bartik_total")),
  data = panel, weights = ~ total_enrollment, cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Print to console
# ---------------------------------------------------------------------------

cat("\n========== FIRST STAGE: log(n_plans) on Bartik ==========\n")
walk2(list(fs_pffs, fs_total),
      c("PFFS-only Bartik", "Sum-of-types Bartik"),
      ~ { cat("\n--- ", .y, " ---\n", sep = ""); print(summary(.x)) })

cat("\n========== REDUCED FORM: outcome on Bartik ==========\n")
walk2(list(rf_pffs, rf_total),
      c("PFFS-only Bartik", "Sum-of-types Bartik"),
      ~ { cat("\n--- ", .y, " ---\n", sep = ""); print(summary(.x)) })

cat("\n========== 2SLS ==========\n")
walk2(list(iv_pffs, iv_total),
      c("PFFS-only Bartik", "Sum-of-types Bartik"),
      ~ { cat("\n--- ", .y, " ---\n", sep = ""); print(summary(.x)) })

# ---------------------------------------------------------------------------
# Combined table
# ---------------------------------------------------------------------------

models <- list(
  "FS: PFFS"       = fs_pffs,
  "FS: Total"      = fs_total,
  "RF: PFFS"       = rf_pffs,
  "RF: Total"      = rf_total,
  "2SLS: PFFS"     = iv_pffs,
  "2SLS: Total"    = iv_total
)

coef_labels <- c(
  "fit_log_n_plans" = "log(n plans), instrumented",
  "log_n_plans"     = "log(n plans)",
  "bartik_pffs"     = "Bartik (PFFS-only)",
  "bartik_total"    = "Bartik (sum of types)",
  "log_inc"         = "log(median HH income)",
  "pct_65plus"      = "% age 65+",
  "pct_bachelors_p" = "% bachelor's or higher",
  "log_pop"         = "log(population)"
)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
modelsummary(
  models,
  output    = "results/tables/shift-share-iv.tex",
  coef_map  = coef_labels,
  gof_omit  = "AIC|BIC|RMSE|Within|R2 Pseudo",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes     = paste("Enrollment-weighted. SEs clustered at county. State and",
                    "year FE in all specs. Bartik built from 2008 county-level",
                    "plan-type shares interacted with national plan-type log",
                    "growth.")
)

message("\nWrote results/tables/shift-share-iv.tex")
