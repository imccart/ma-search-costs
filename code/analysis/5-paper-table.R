# 5-paper-table.R — Consolidated reduced-form table for paper §4
#
# One LaTeX table that tells the full reduced-form story across five specs:
#   (1) OLS headline                    — log(n_plans) on dominated enrollment
#   (2) OLS + differentiation moderator — adds agg_val
#   (3) 2SLS using PFFS Bartik          — instruments log(n_plans)
#   (4) 2SLS using insurer-level Bartik — same RHS, different IV
#   (5) Decomposition                   — splits agg_val into stable +
#                                          methodology shift
#
# Common: enrollment-weighted, SEs clustered at county_fips, state and year FE.
#
# Input:  data/output/analysis_panel.csv
# Output: results/tables/reduced-form-paper.tex

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
    log_n_plans       = log(n_plans),
    log_pop           = log(total_pop),
    log_inc           = log(median_hh_income),
    methodology_shift = agg_val - agg_val_stable
  )

# ---------------------------------------------------------------------------
# Spec 1: OLS headline
# ---------------------------------------------------------------------------

s1 <- feols(
  pct_enrollment_dominated ~ log_n_plans + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = panel,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 2: OLS + differentiation
# ---------------------------------------------------------------------------

s2 <- feols(
  pct_enrollment_dominated ~ log_n_plans + agg_val + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = panel %>% filter(!is.na(agg_val)),
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 3: 2SLS — PFFS Bartik
# ---------------------------------------------------------------------------

s3 <- feols(
  pct_enrollment_dominated ~ log_inc + pct_65plus + pct_bachelors_p + log_pop |
    state + year | log_n_plans ~ bartik_pffs,
  data    = panel,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 4: 2SLS — insurer-level Bartik
# ---------------------------------------------------------------------------

s4 <- feols(
  pct_enrollment_dominated ~ log_inc + pct_65plus + pct_bachelors_p + log_pop |
    state + year | log_n_plans ~ bartik_insurer,
  data    = panel,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 5: methodology decomposition
# ---------------------------------------------------------------------------

s5 <- feols(
  pct_enrollment_dominated ~ log_n_plans + agg_val_stable +
    methodology_shift + log_inc + pct_65plus + pct_bachelors_p + log_pop |
    state + year,
  data    = panel %>% filter(!is.na(agg_val_stable)),
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Print to console
# ---------------------------------------------------------------------------

cat("\n--- (1) OLS headline ---\n")
print(summary(s1))
cat("\n--- (2) OLS + differentiation ---\n")
print(summary(s2))
cat("\n--- (3) 2SLS, PFFS Bartik ---\n")
print(summary(s3))
cat("\n--- (4) 2SLS, insurer Bartik ---\n")
print(summary(s4))
cat("\n--- (5) Methodology decomposition ---\n")
print(summary(s5))

# ---------------------------------------------------------------------------
# Build paper table — collapse OLS log_n_plans and IV fit_log_n_plans onto
# one display row, so the table reads as a single coefficient across columns
# ---------------------------------------------------------------------------

models <- list(
  "(1) OLS"                  = s1,
  "(2) OLS + agg_val"        = s2,
  "(3) 2SLS (PFFS Bartik)"   = s3,
  "(4) 2SLS (insurer Bartik)" = s4,
  "(5) Decomposition"        = s5
)

coef_labels <- c(
  "log_n_plans"        = "log(n plans)",
  "fit_log_n_plans"    = "log(n plans)",
  "agg_val"            = "Differentiation (full)",
  "agg_val_stable"     = "Differentiation (stable)",
  "methodology_shift"  = "Methodology shift",
  "log_inc"            = "log(median HH income)",
  "pct_65plus"         = "% age 65+",
  "pct_bachelors_p"    = "% bachelor's or higher",
  "log_pop"            = "log(population)"
)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
modelsummary(
  models,
  output    = "results/tables/reduced-form-paper.tex",
  coef_map  = coef_labels,
  gof_omit  = "AIC|BIC|RMSE|Within|R2 Pseudo",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes     = paste("Enrollment-weighted. Standard errors clustered at the",
                    "county level. State and year fixed effects in all",
                    "specifications. Columns (3) and (4) instrument log(n",
                    "plans) using shift-share Bartik instruments built from",
                    "2008 county-level plan-type and parent-insurer shares",
                    "interacted with national plan-count growth.")
)

message("\nWrote results/tables/reduced-form-paper.tex")
