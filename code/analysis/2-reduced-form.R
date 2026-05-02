# 2-reduced-form.R — Reduced-form regressions of dominated MA enrollment
#
# Establishes that dominated enrollment is associated with proxies for choice
# complexity, conditional on county demographics. This is correlational
# evidence consistent with search costs, not causal identification.
#
# Specs:
#   1. Headline: pct_enrollment_dominated ~ log(n_plans) + controls + state+year FE
#   2. + Differentiation moderator: agg_val and log(n_plans) × agg_val
#      (clustered subsample only, ~8.5K obs)
#   3. Supply-side decomposition: pct_dominated (% of plans, not enrollment)
#      with the same RHS as spec 1 — sanity check that the relationship is
#      behavioral, not purely mechanical
#   4. Non-linearity: log(n_plans) replaced by tercile bins of n_plans
#
# Common: enrollment-weighted, SEs clustered at county_fips.
#
# Input:  data/output/analysis_panel.csv
# Output: results/tables/reduced-form.tex (modelsummary table)
#         results/figures/n_plans_bins.pdf (spec 4 coefficients)

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
    log_n_plans  = log(n_plans),
    log_pop      = log(total_pop),
    log_inc      = log(median_hh_income),
    n_plans_bin  = cut(
      n_plans,
      breaks = quantile(n_plans, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("low", "mid", "high")
    )
  )

message("Analysis panel: ", nrow(panel), " county-years")
message("Years: ", paste(range(panel$year), collapse = "-"))

# ---------------------------------------------------------------------------
# Spec 1: headline (full sample)
# ---------------------------------------------------------------------------

m1 <- feols(
  pct_enrollment_dominated ~ log_n_plans + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = panel,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 2: differentiation moderator (clustered subsample)
# ---------------------------------------------------------------------------

m2 <- feols(
  pct_enrollment_dominated ~ log_n_plans * agg_val + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = panel %>% filter(!is.na(agg_val)),
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 3: supply-side decomposition (pct of plans dominated, not enrollment)
# ---------------------------------------------------------------------------

m3 <- feols(
  pct_dominated ~ log_n_plans + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = panel,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Spec 4: non-linearity in n_plans (tercile bins)
# ---------------------------------------------------------------------------

m4 <- feols(
  pct_enrollment_dominated ~ n_plans_bin + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = panel,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Table
# ---------------------------------------------------------------------------

models <- list(
  "(1) Headline"            = m1,
  "(2) + Differentiation"   = m2,
  "(3) Supply-side (% plans)" = m3,
  "(4) n_plans bins"        = m4
)

coef_labels <- c(
  "log_n_plans"      = "log(n plans)",
  "agg_val"          = "Agg. coefficient (differentiation)",
  "log_n_plans:agg_val" = "log(n plans) x agg_val",
  "n_plans_binmid"   = "n plans: middle tercile",
  "n_plans_binhigh"  = "n plans: high tercile",
  "log_inc"          = "log(median HH income)",
  "pct_65plus"       = "% age 65+",
  "pct_bachelors_p"  = "% bachelor's or higher",
  "log_pop"          = "log(population)"
)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
modelsummary(
  models,
  output    = "results/tables/reduced-form.tex",
  coef_map  = coef_labels,
  gof_omit  = "AIC|BIC|RMSE|Within|R2 Pseudo",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes     = paste("Enrollment-weighted OLS. Standard errors clustered at",
                    "the county level. State and year fixed effects in all",
                    "specifications.")
)

# Print each spec to the console for inspection
cat("\n--- (1) Headline ---\n")
print(summary(m1))
cat("\n--- (2) + Differentiation ---\n")
print(summary(m2))
cat("\n--- (3) Supply-side (% plans) ---\n")
print(summary(m3))
cat("\n--- (4) n_plans bins ---\n")
print(summary(m4))

# ---------------------------------------------------------------------------
# Spec 4 figure: bin coefficients with CIs
# ---------------------------------------------------------------------------

bin_coefs <- tidy(m4, conf.int = TRUE) %>%
  filter(str_detect(term, "n_plans_bin")) %>%
  mutate(bin = str_remove(term, "n_plans_bin"),
         bin = factor(bin, levels = c("mid", "high"))) %>%
  add_row(bin = factor("low", levels = c("low", "mid", "high")),
          estimate = 0, conf.low = 0, conf.high = 0, .before = 1)

p_bins <- ggplot(bin_coefs, aes(x = bin, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "n plans tercile", y = "Coefficient (low tercile = 0)",
       title = "Spec 4: dominated enrollment by choice-set-size tercile") +
  theme_minimal()

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/n_plans_bins.pdf", p_bins, width = 5, height = 4)

message("\nWrote results/tables/reduced-form.tex and results/figures/n_plans_bins.pdf")
