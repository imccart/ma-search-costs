# 4-methodology-shift.R — Decompose differentiation into stable + methodology
#
# agg_val is computed using the year-t CMS Star Rating measure roster.
# agg_val_stable uses the intersection of year-t and year-(t-1) rosters
# (measures present in both years). The difference,
#
#   methodology_shift_{c,t} = agg_val_{c,t} - agg_val_stable_{c,t}
#
# captures the change in measured differentiation driven by CMS adding or
# removing measures from the rating roster between t-1 and t. This is
# methodology variation, not underlying-quality variation.
#
# Test: decompose the agg_val effect into stable + methodology pieces. If
# methodology_shift independently predicts dominated enrollment after
# conditioning on stable differentiation and choice-set size, that's evidence
# consumers respond to *measured* quality differences (even when the variation
# is just CMS bookkeeping). If not, dominated enrollment tracks underlying
# differentiation only.
#
# Input:  data/output/analysis_panel.csv
# Output: results/tables/methodology-shift.tex

options(modelsummary_factory_default = "kableExtra",
        modelsummary_format_numeric_latex = "plain")

# ---------------------------------------------------------------------------
# Read panel and construct shift
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

stopifnot("agg_val_stable" %in% names(panel))

dec_sample <- panel %>% filter(!is.na(agg_val_stable))
message("Sample with both agg_val and agg_val_stable: ",
        nrow(dec_sample), " of ", nrow(panel), " county-years")
message("methodology_shift summary:")
print(summary(dec_sample$methodology_shift))

# ---------------------------------------------------------------------------
# Headline (replicates spec 2 of 2-reduced-form.R) — full agg_val
# ---------------------------------------------------------------------------

m_full <- feols(
  pct_enrollment_dominated ~ log_n_plans + agg_val + log_inc + pct_65plus +
    pct_bachelors_p + log_pop | state + year,
  data    = dec_sample,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Stable-only — replace agg_val with agg_val_stable
# ---------------------------------------------------------------------------

m_stable <- feols(
  pct_enrollment_dominated ~ log_n_plans + agg_val_stable + log_inc +
    pct_65plus + pct_bachelors_p + log_pop | state + year,
  data    = dec_sample,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Decomposition — split agg_val into stable + methodology pieces
# ---------------------------------------------------------------------------

m_decomp <- feols(
  pct_enrollment_dominated ~ log_n_plans + agg_val_stable +
    methodology_shift + log_inc + pct_65plus + pct_bachelors_p + log_pop |
    state + year,
  data    = dec_sample,
  weights = ~ total_enrollment,
  cluster = ~ county_fips
)

# ---------------------------------------------------------------------------
# Print to console
# ---------------------------------------------------------------------------

cat("\n========== Decomposition specs ==========\n")
cat("\n--- Full agg_val ---\n")
print(summary(m_full))
cat("\n--- Stable agg_val (intersection roster) ---\n")
print(summary(m_stable))
cat("\n--- Decomposition (stable + methodology shift) ---\n")
print(summary(m_decomp))

# ---------------------------------------------------------------------------
# Combined table
# ---------------------------------------------------------------------------

models <- list(
  "Full agg_val"      = m_full,
  "Stable agg_val"    = m_stable,
  "Decomposition"     = m_decomp
)

coef_labels <- c(
  "log_n_plans"        = "log(n plans)",
  "agg_val"            = "Differentiation (full roster)",
  "agg_val_stable"     = "Differentiation (stable roster)",
  "methodology_shift"  = "Methodology shift",
  "log_inc"            = "log(median HH income)",
  "pct_65plus"         = "% age 65+",
  "pct_bachelors_p"    = "% bachelor's or higher",
  "log_pop"            = "log(population)"
)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
modelsummary(
  models,
  output    = "results/tables/methodology-shift.tex",
  coef_map  = coef_labels,
  gof_omit  = "AIC|BIC|RMSE|Within|R2 Pseudo",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes     = paste("Enrollment-weighted OLS. SEs clustered at county.",
                    "State and year FE. Sample restricted to county-years",
                    "with non-missing agg_val and agg_val_stable.")
)

message("\nWrote results/tables/methodology-shift.tex")
