# 1-build-bene-choice-panel.R — Materialize the estimation panel
#
# Produces the canonical long-format bene × plan panel that downstream
# estimation, diagnostics, and counterfactual scripts consume. One row
# per (bene, plan in bene's county-year market). The panel carries:
#   - bene-level covariates (demographics, channel use, survey design)
#   - plan-level attributes (EC, Var, Star Rating, prominence inputs)
#   - bene × plan items (incumbent flag, is_chosen indicator)
#
# Inputs (RStudio project root = ma-search/):
#   data/input/bene_panel.csv          — SAS-exported bene panel (script 3)
#   data/input/structural_panel.csv    — uploaded plan attributes
#   data/output/bene_cost_sharing.csv  — bene-specific EC (script 0)
# Output:
#   data/output/bene_choice_panel.csv  — checkpoint

bene_path   <- "data/input/bene_panel.csv"
panel_path  <- "data/input/structural_panel.csv"
ec_path     <- "data/output/bene_cost_sharing.csv"
out_path    <- "data/output/bene_choice_panel.csv"

if (!file.exists(bene_path))   stop("bene_panel.csv not found at ",       bene_path)
if (!file.exists(panel_path))  stop("structural_panel.csv not found at ", panel_path)
if (!file.exists(ec_path))     stop("bene_cost_sharing.csv not found at ", ec_path,
                                    "\n  Run script 0-project-bene-cost-sharing.R first.")


# ---------------------------------------------------------------------------
# 1. Load bene panel, apply sample-restriction filters
# ---------------------------------------------------------------------------

bene <- fread(bene_path)
n_full <- nrow(bene)
message(sprintf("Loaded bene_panel.csv: %d rows", n_full))

bene <- bene %>%
  filter(
    link_status == "ok",
    full_year_partAB == 1,
    not_esrd == 1,
    active_shopper == 1,
    !is.na(state_cnty_fips),
    !is.na(income_cat),
    !is.na(education_cat)
  )
message(sprintf("After filters (link / partAB / ESRD / active / non-missing X): %d rows", nrow(bene)))


# ---------------------------------------------------------------------------
# 2. Bene-level covariate recodes (so the materialized panel carries the
#    final form of X_i, channel-use indicators, and de-meaned variables)
# ---------------------------------------------------------------------------

income_midpoints <- c(
  `1`  = 2500,    `2`  = 7500,    `3`  = 12500,   `4`  = 17500,
  `5`  = 22500,   `6`  = 27500,   `7`  = 35000,   `8`  = 45000,
  `9`  = 55000,   `10` = 70000,   `11` = 90000,   `12` = 110000,
  `13` = 130000,  `14` = 175000
)

educ_years <- c(
  `1` = 0,  `2` = 6,  `3` = 11, `4` = 12, `5` = 13,
  `6` = 14, `7` = 14, `8` = 16, `9` = 19
)

bene <- bene %>%
  mutate(
    county_fips = sprintf("%05s", as.character(state_cnty_fips)),
    state_fips  = substr(county_fips, 1, 2),

    income_mid = income_midpoints[as.character(income_cat)],
    log_inc    = log(pmax(income_mid, 1000)),

    educ_yrs   = educ_years[as.character(education_cat)],
    has_bach   = as.integer(education_cat %in% c(8L, 9L)),

    race_white = as.integer(race_cd == 1L),
    race_black = as.integer(race_cd == 2L),
    race_hisp  = as.integer(race_cd == 5L),
    race_other = as.integer(race_cd %in% c(0L, 3L, 4L, 6L)),

    is_dual         = as.integer(dual_annual %in% c(1L, 4L)),
    is_partial_dual = as.integer(dual_annual == 3L),

    has_inet = as.integer(uses_internet_for_info == 1),
    has_pc   = as.integer(has_personal_computer == 1),

    KVSITWEB_use      = as.integer(visited_medicare_site == 1),
    KCHIHELP_help     = as.integer(who_decides_insurance == 2L),
    KCHIHELP_delegate = as.integer(who_decides_insurance == 3L),

    searched_obs = as.integer(searched == 1L | searched == TRUE),

    # The three own-search actions, kept separate (not just their OR `searched`).
    act_info  = as.integer(tried_find_info      == 1),
    act_web   = as.integer(visited_medicare_site == 1),
    act_phone = as.integer(called_800_medicare   == 1),
    # Ordered handbook reading (KBOKREAD via book_read_amount): 0 none / 1 parts /
    # 2 thorough. The value->intensity mapping is ASSUMED; verify codes on the seat.
    book_read = case_when(
      book_read_amount == 1 ~ 2L,    # read thoroughly  (ASSUMED)
      book_read_amount == 2 ~ 1L,    # read parts        (ASSUMED)
      TRUE                  ~ 0L     # not at all / missing
    ),
    # Handbook comprehension difficulty (KBOKUNDR) and MA tenure (years enrolled),
    # demeaned in the imputation block below.
    book_understood_dm = as.numeric(book_understood),
    tenure_dm          = as.numeric(madv_years_enrolled),

    age_dm      = age - 75,
    log_inc_dm  = log_inc - mean(log_inc, na.rm = TRUE),
    educ_yrs_dm = educ_yrs - mean(educ_yrs, na.rm = TRUE),
    adi_dm      = adi_raw / 100,

    chosen_plan_id = if_else(
      is_ffs_mbsf == 1, "FFS",
      paste0(ann_contract, "_", ann_pbp)
    ),
    prior_plan_id = if_else(
      prior_was_ffs == 1, "FFS",
      paste0(prior_contract, "_", prior_pbp)
    )
  )

# Impute missing search-cost covariates so c_i = exp(gamma'x) is never NA. A
# single NA covariate makes the entire compute_K_star objective NA (NA * K is NA
# even at K = 0), so which.max returns nothing and K* has length 0. educ_yrs_dm
# is demeaned, so NA -> 0 (the mean); adi_dm is a scaled level, so NA -> its
# non-missing mean. ADI is unmatched for ~24% of benes; educ NA is a handful with
# education_cat codes outside the lookup. See background/sample-construction.md.
bene <- bene %>%
  mutate(
    educ_yrs_dm = if_else(is.na(educ_yrs_dm), 0, educ_yrs_dm),
    adi_dm      = if_else(is.na(adi_dm), mean(adi_dm, na.rm = TRUE), adi_dm),
    # Demean comprehension and tenure; missing (non-readers / FFS) -> 0.
    book_understood_dm = if_else(is.na(book_understood_dm), 0,
                          book_understood_dm - mean(book_understood_dm, na.rm = TRUE)),
    tenure_dm          = if_else(is.na(tenure_dm), 0,
                          tenure_dm - mean(tenure_dm, na.rm = TRUE)),
    book_read          = if_else(is.na(book_read), 0L, book_read)
  )


# ---------------------------------------------------------------------------
# 3. Load plan-attribute panel. Drop population mean_cost / var_cost / sd_cost
# columns — those are stylized-profile averages from the local data-build and
# are replaced below with bene-specific EC[c|i,j] from script 0.
# ---------------------------------------------------------------------------

panel <- fread(panel_path,
  colClasses = c(county_fips = "character"))

# Collapse the two FFS variants into a single FFS outside option so an FFS
# chooser's chosen_plan_id == "FFS" matches the choice set. omega-weighted
# population cost, mirroring analysis/structural/1-load-panel.R (0.25 bare +
# 0.75 supp). The choice model reads only plan_kind/mean_cost/var_cost for FFS
# (utility = -alpha*EC - delta*Var + xi_FFS); MA-only attributes are NA for FFS
# and never used. Match bare/supp by pattern so the exact suffix doesn't matter.
omega_bare <- 0.25
ffs_one <- panel[plan_kind == "FFS", .(
  plan_id   = "FFS",
  plan_kind = "FFS",
  mean_cost = omega_bare * mean_cost[!grepl("supp", plan_id)] +
              (1 - omega_bare) * mean_cost[grepl("supp", plan_id)],
  var_cost  = omega_bare * var_cost[!grepl("supp", plan_id)] +
              (1 - omega_bare) * var_cost[grepl("supp", plan_id)]
), by = .(county_fips, year)]
panel <- rbind(panel[plan_kind != "FFS"], ffs_one, fill = TRUE)

# Keep the population cost as a fallback column. MA plans get bene-specific EC
# from script 0 below; the FFS row keeps this population value (script 0 projects
# MA plans only).
setnames(panel, c("mean_cost", "var_cost"), c("mean_cost_pop", "var_cost_pop"))
if ("sd_cost" %in% names(panel)) panel[, sd_cost := NULL]
message(sprintf("Loaded structural_panel.csv: %d rows, %d unique plan-county-years (dropped population cost columns)",
                nrow(panel), nrow(unique(panel[, .(county_fips, year, plan_id)]))))


# ---------------------------------------------------------------------------
# 4. Inner join: bene × all plans in their (county, year) market
# ---------------------------------------------------------------------------

bene_dt <- as.data.table(bene)
panel_dt <- as.data.table(panel)

# Ensure no name collisions before join
common_cols <- intersect(names(bene_dt), names(panel_dt))
common_cols <- setdiff(common_cols, c("county_fips", "year"))
if (length(common_cols) > 0) {
  message("Renaming bene-side conflicting columns: ", paste(common_cols, collapse = ", "))
  setnames(bene_dt, common_cols, paste0(common_cols, "_bene"))
}

bcp <- bene_dt[panel_dt, on = c("county_fips", "year"), nomatch = NULL,
               allow.cartesian = TRUE]

message(sprintf("After join: %d bene-plan rows", nrow(bcp)))
message(sprintf("Plans per bene-year: median=%d, mean=%.1f, max=%d",
                bcp[, median(.N), by = .(BASE_ID, year)][, median(V1)],
                mean(bcp[, .N, by = .(BASE_ID, year)]$N),
                bcp[, max(.N), by = .(BASE_ID, year)][, max(V1)]))


# ---------------------------------------------------------------------------
# 4b. Inner-join bene-specific cost-sharing (EC and Var_C from script 0)
# ---------------------------------------------------------------------------

ec <- fread(ec_path, select = c("BENE_ID", "year", "plan_id", "EC", "Var_C_j"))
setnames(ec, c("EC", "Var_C_j"), c("mean_cost", "var_cost"))

n_before <- nrow(bcp)
bcp <- merge(bcp, ec, by = c("BENE_ID", "year", "plan_id"), all.x = TRUE)
message(sprintf("After EC merge: %d rows (was %d). Bene-plan pairs without EC: %d",
                nrow(bcp), n_before, sum(is.na(bcp$mean_cost))))

# Bene-specific EC for MA; fall back to population cost for the FFS row (and any
# MA bene-plan pair script 0 could not project).
bcp[, mean_cost := fifelse(is.na(mean_cost), mean_cost_pop, mean_cost)]
bcp[, var_cost  := fifelse(is.na(var_cost),  var_cost_pop,  var_cost)]
bcp[, c("mean_cost_pop", "var_cost_pop") := NULL]


# ---------------------------------------------------------------------------
# 5. Bene × plan attributes
# ---------------------------------------------------------------------------

bcp[, `:=`(
  is_chosen      = as.integer(plan_id == chosen_plan_id),
  # Clean 0/1: a bene with no observed prior plan (first panel year, all of
  # 2015, or a missing/"NA_NA" prior) is a non-incumbent, never NA. Leaving it
  # NA propagates through max(., na.rm=TRUE) to an all-NA bene-year downstream.
  incumbent_bene = as.integer(!prior_plan_id %in% c(NA, "", "NA_NA") & plan_id == prior_plan_id),
  sd_cost        = sqrt(pmax(var_cost, 0)),
  # Broker density per 1k Medicare eligibles in the county-year. ins_brokers_estab
  # (count) and total_eligibles join from structural_panel.csv at the county-year
  # level; every plan-row in the same (county, year) carries the same values.
  broker_density_per_k = ins_brokers_estab / pmax(total_eligibles, 1) * 1000
)]

# Sanity: every bene should have exactly one chosen plan in their market.
n_chosen <- bcp[, .(n_chosen = sum(is_chosen)), by = .(BASE_ID, year)]
n_zero   <- n_chosen[n_chosen == 0L, .N]
n_multi  <- n_chosen[n_chosen >  1L, .N]
n_one    <- n_chosen[n_chosen == 1L, .N]

message(sprintf("Chosen-plan match: %d benes with exactly 1 chosen, %d with 0, %d with >1",
                n_one, n_zero, n_multi))

if (n_zero > 0) {
  message("Dropping bene-years whose chosen plan is not in the public panel ",
          "(typically SNPs / EGHPs / mid-year-only plans excluded upstream).")
  # Drop the offending (BASE_ID, year) pairs, NOT whole benes — a bene can have
  # a valid chosen plan in one year and an unmatched plan in another (MCBS is a
  # rotating panel). A BASE_ID-only filter would leave the unmatched bene-years
  # in bcp with zero chosen plans, surfacing as NA choice_idx in script 2.
  bcp <- bcp[n_chosen[n_chosen == 1L, .(BASE_ID, year)], on = c("BASE_ID", "year"), nomatch = NULL]
}
if (n_multi > 0) {
  stop(sprintf("%d benes have multiple chosen-plan matches. Investigate.", n_multi))
}


# ---------------------------------------------------------------------------
# 6. Diagnostics
# ---------------------------------------------------------------------------

message("\n========== Bene choice panel ==========")
message(sprintf("Rows                : %d", nrow(bcp)))
message(sprintf("Unique benes        : %d", uniqueN(bcp$BASE_ID)))
message(sprintf("Unique bene-years   : %d", uniqueN(bcp[, .(BASE_ID, year)])))
message(sprintf("Unique markets      : %d", uniqueN(bcp[, .(county_fips, year)])))
message(sprintf("Years               : %s", paste(sort(unique(bcp$year)), collapse = ", ")))

message("\nChosen-plan kind distribution:")
print(bcp[is_chosen == 1, .N, by = plan_kind])

message("\nIncumbency by year (bene was MA last year):")
bcp[is_chosen == 1, .(pct_incumbent = mean(incumbent_bene)), by = year][order(year)] %>% print()

message("\nKey covariate distributions among chosen plans:")
print(bcp[is_chosen == 1, .(
  pct_searched     = mean(searched_obs      == 1, na.rm = TRUE),
  pct_kvsitweb     = mean(KVSITWEB_use      == 1, na.rm = TRUE),
  pct_kchihelp_h   = mean(KCHIHELP_help     == 1, na.rm = TRUE),
  pct_kchihelp_d   = mean(KCHIHELP_delegate == 1, na.rm = TRUE),
  pct_dual         = mean(is_dual           == 1, na.rm = TRUE),
  pct_bach         = mean(has_bach          == 1, na.rm = TRUE)
)])

message("\nBroker density per 1k eligibles (county-year level):")
print(bcp[is_chosen == 1, .(
  mean_density   = mean(broker_density_per_k, na.rm = TRUE),
  median_density = median(broker_density_per_k, na.rm = TRUE),
  p10            = quantile(broker_density_per_k, 0.10, na.rm = TRUE),
  p90            = quantile(broker_density_per_k, 0.90, na.rm = TRUE)
)])


# ---------------------------------------------------------------------------
# 7. Write checkpoint
# ---------------------------------------------------------------------------

fwrite(bcp, out_path)
message(sprintf("\nWrote %s (%d rows, %d cols)", out_path, nrow(bcp), ncol(bcp)))
