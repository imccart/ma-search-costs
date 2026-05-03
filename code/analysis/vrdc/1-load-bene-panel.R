# 1-load-bene-panel.R — Read bene panel, derive X_i, apply final filters
#
# Input:  /workspace/pl027710/export/bene_panel.csv (from data-build)
# Output: `bene` data.frame, one row per MCBS respondent-year, ready for
#         choice-set construction and individual likelihood.

bene_path <- "/workspace/pl027710/export/bene_panel.csv"
if (!file.exists(bene_path)) stop("bene_panel.csv not found at ", bene_path)

bene <- fread(bene_path)
message("Loaded bene panel: ", nrow(bene), " rows, ", ncol(bene), " cols")


# ---- Final analysis-sample filters (in addition to SAS-side filters) ----

bene <- bene %>%
  filter(
    link_status == "ok",                # drop admin-vs-survey mismatches for now
    !is.na(state_cnty_fips),
    !is.na(income_cat),
    !is.na(education_cat),
    !is.na(has_internet)
  )
message("After R-side filters: ", nrow(bene), " rows")


# ---- Geography: convert MBSF FIPS to integer + character forms ----

bene <- bene %>%
  mutate(
    county_fips = sprintf("%05s", as.character(state_cnty_fips)),
    state_fips  = substr(county_fips, 1, 2)
  )


# ---- Recode INCOME band -> midpoint dollar amount (continuous-ish) ----
# INCOME (SURVEY_DEMO) is 14 categories; midpoints are CMS standard.

income_midpoints <- c(
  `1`  = 2500,    # Less than $5000
  `2`  = 7500,    # $5,000-$9,999
  `3`  = 12500,
  `4`  = 17500,
  `5`  = 22500,
  `6`  = 27500,
  `7`  = 35000,
  `8`  = 45000,
  `9`  = 55000,
  `10` = 70000,
  `11` = 90000,
  `12` = 110000,
  `13` = 130000,
  `14` = 175000   # $140,000+ (open-ended; midpoint ~ $175K assumed)
)
bene <- bene %>%
  mutate(
    income_mid = income_midpoints[as.character(income_cat)],
    log_inc    = log(pmax(income_mid, 1000))
  )


# ---- Recode SPDEGRCV (education) -> years of schooling ----
# SPDEGRCV: 1=No schooling, 2=Nursery-8, 3=9-12 no diploma, 4=HS grad,
#           5=Vocational, 6=Some college no degree, 7=Associate's,
#           8=Bachelor's, 9=Graduate.

educ_years <- c(
  `1` = 0,  `2` = 6,  `3` = 11, `4` = 12, `5` = 13,
  `6` = 14, `7` = 14, `8` = 16, `9` = 19
)
bene <- bene %>%
  mutate(
    educ_yrs = educ_years[as.character(education_cat)],
    has_bach = as.integer(education_cat %in% c(8L, 9L))
  )


# ---- Recode H_RTIRCE (RTI race code) into indicators ----
# H_RTIRCE: 0=Unknown, 1=White, 2=Black, 3=Other, 4=Asian, 5=Hispanic, 6=NA Native

bene <- bene %>%
  mutate(
    race_white    = as.integer(race_cd == 1L),
    race_black    = as.integer(race_cd == 2L),
    race_hisp     = as.integer(race_cd == 5L),
    race_other    = as.integer(race_cd %in% c(3L, 4L, 6L, 0L))
  )


# ---- Dual-eligibility, internet flag, search behavior ----

bene <- bene %>%
  mutate(
    is_dual    = as.integer(dual_annual %in% c(1L, 4L)),  # FULL or QMB
    is_partial_dual = as.integer(dual_annual == 3L),
    has_inet   = as.integer(has_internet == 1L),
    searched_obs = as.integer(searched == 1L | searched == TRUE)
  )


# ---- Demographic shifters for c_i = exp(gamma_0 + gamma_1' X_i) ----
# Final X_i vector for the search-cost heterogeneity.

bene <- bene %>%
  mutate(
    age_dm   = age - 75,                  # de-meaned at 75
    log_inc_dm  = log_inc - mean(log_inc, na.rm = TRUE),
    educ_yrs_dm = educ_yrs - mean(educ_yrs, na.rm = TRUE),
    adi_dm   = adi_national_pct / 100     # rescale to 0-1
  )


# ---- Final sample diagnostics ----

message("\nFinal analysis sample:")
message("  N rows                     : ", nrow(bene))
message("  N unique BASE_IDs           : ", n_distinct(bene$BASE_ID))
message("  Years                      : ", paste(sort(unique(bene$year)), collapse = ", "))
message(sprintf("  Pct MA (admin)             : %.1f%%", 100 * mean(bene$is_ma_admin == 1, na.rm = TRUE)))
message(sprintf("  Pct searched               : %.1f%%", 100 * mean(bene$searched_obs == 1, na.rm = TRUE)))
message(sprintf("  Pct internet access        : %.1f%%", 100 * mean(bene$has_inet == 1, na.rm = TRUE)))
message(sprintf("  Pct dual                   : %.1f%%", 100 * mean(bene$is_dual == 1, na.rm = TRUE)))
message(sprintf("  Pct incumbent              : %.1f%%", 100 * mean(bene$incumbent_bene == 1, na.rm = TRUE)))


# ---- Survey design (for survey-weighted moments and SEs) ----

bene_design <- svydesign(
  ids     = ~variance_psu,
  strata  = ~variance_stratum,
  weights = ~wgt_full_sample,
  nest    = TRUE,
  data    = bene
)
