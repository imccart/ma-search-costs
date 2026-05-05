# 0-build-bene-choice-panel.R — Materialize the estimation panel
#
# Produces the canonical long-format bene × plan panel that downstream
# estimation, diagnostics, and counterfactual scripts consume. One row
# per (bene, plan in bene's county-year market). The panel carries:
#   - bene-level covariates (demographics, channel use, survey design)
#   - plan-level attributes (EC, Var, Star Rating, prominence inputs)
#   - bene × plan items (incumbent flag, is_chosen indicator)
#
# Inputs:
#   /workspace/pl027710/export/bene_panel.csv         — VRDC SAS extraction
#   /workspace/pl027710/upload/structural_panel.csv   — uploaded local panel
# Output:
#   /workspace/pl027710/export/bene_choice_panel.csv  — checkpoint

bene_path  <- "/workspace/pl027710/export/bene_panel.csv"
panel_path <- "/workspace/pl027710/upload/structural_panel.csv"
out_path   <- "/workspace/pl027710/export/bene_choice_panel.csv"

if (!file.exists(bene_path))  stop("bene_panel.csv not found at ",  bene_path)
if (!file.exists(panel_path)) stop("structural_panel.csv not found at ", panel_path)


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

    KVSITWEB_use = as.integer(visited_medicare_site == 1),
    KCHIHELP_use = as.integer(who_decides_insurance %in% c(2L, 3L)),

    searched_obs = as.integer(searched == 1L | searched == TRUE),

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


# ---------------------------------------------------------------------------
# 3. Load plan-attribute panel
# ---------------------------------------------------------------------------

panel <- fread(panel_path,
  colClasses = c(county_fips = "character"))
message(sprintf("Loaded structural_panel.csv: %d rows, %d unique plan-county-years",
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
# 5. Bene × plan attributes
# ---------------------------------------------------------------------------

bcp[, `:=`(
  is_chosen      = as.integer(plan_id == chosen_plan_id),
  incumbent_bene = as.integer(plan_id == prior_plan_id & prior_plan_id != "")
)]

# Sanity: every bene should have exactly one chosen plan in their market.
n_chosen <- bcp[, .(n_chosen = sum(is_chosen)), by = .(BASE_ID, year)]
n_zero   <- n_chosen[n_chosen == 0L, .N]
n_multi  <- n_chosen[n_chosen >  1L, .N]
n_one    <- n_chosen[n_chosen == 1L, .N]

message(sprintf("Chosen-plan match: %d benes with exactly 1 chosen, %d with 0, %d with >1",
                n_one, n_zero, n_multi))

if (n_zero > 0) {
  message("Dropping benes whose chosen plan is not in the public panel ",
          "(typically SNPs / EGHPs / mid-year-only plans excluded upstream).")
  bcp <- bcp[BASE_ID %in% n_chosen[n_chosen == 1L, BASE_ID]]
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
  pct_searched = mean(searched_obs == 1, na.rm = TRUE),
  pct_kvsitweb = mean(KVSITWEB_use == 1, na.rm = TRUE),
  pct_kchihelp = mean(KCHIHELP_use == 1, na.rm = TRUE),
  pct_dual     = mean(is_dual == 1, na.rm = TRUE),
  pct_bach     = mean(has_bach == 1, na.rm = TRUE)
)])


# ---------------------------------------------------------------------------
# 7. Write checkpoint
# ---------------------------------------------------------------------------

fwrite(bcp, out_path)
message(sprintf("\nWrote %s (%d rows, %d cols)", out_path, nrow(bcp), ncol(bcp)))
