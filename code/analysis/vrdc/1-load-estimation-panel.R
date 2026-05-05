# 1-load-estimation-panel.R — Read the bene × plan checkpoint, set up
# survey design and the bene-level summary needed for moment matching.
#
# Input:  /workspace/pl027710/export/bene_choice_panel.csv (script 0)
# Outputs (in R env):
#   bcp          — long-format bene × plan-in-market panel (data.table)
#   bene         — bene-year summary (one row per BASE_ID×year, dedup of bcp)
#   bene_design  — survey::svydesign for SE-aware moment computations
#   markets      — list of per-market plan tibbles (for fast plan-set lookups)
#   bene_to_market — vector mapping bene row index -> market index
#   choice_idx   — vector giving within-market index of the chosen plan

bcp_path <- "/workspace/pl027710/export/bene_choice_panel.csv"
if (!file.exists(bcp_path)) stop("bene_choice_panel.csv not found at ", bcp_path,
                                  "\nRun 0-build-bene-choice-panel.R first.")

bcp <- fread(bcp_path,
  colClasses = c(county_fips = "character"))

message(sprintf("Loaded bene_choice_panel: %d rows, %d cols", nrow(bcp), ncol(bcp)))


# ---------------------------------------------------------------------------
# Bene-year summary (one row per BASE_ID × year)
# ---------------------------------------------------------------------------
# Used for survey-weighted moments that aggregate to bene-year, not bene-plan.

bene_cols <- c(
  "BASE_ID", "year", "county_fips",
  "age", "age_dm", "sex_cd", "race_cd",
  "race_white", "race_black", "race_hisp", "race_other",
  "income_cat", "income_mid", "log_inc", "log_inc_dm",
  "education_cat", "educ_yrs", "educ_yrs_dm", "has_bach",
  "is_dual", "is_partial_dual",
  "has_inet", "has_pc",
  "KVSITWEB_use", "KCHIHELP_help", "KCHIHELP_delegate",
  "searched_obs",
  "adi_dm",
  "is_ma_admin", "is_ffs_admin",
  "prior_plan_id",
  "wgt_full_sample", "variance_stratum", "variance_psu"
)

# `incumbent_bene` is bene×plan in bcp; collapse to bene-year as
# "was bene incumbent in any plan this year (MA-incumbent vs. new enrollee)".
# This is used downstream as a moment input, NOT in the salience function
# (which uses bene-specific prior_plan_id × plan_id matching at runtime).
bene <- bcp[, .(
  incumbent_bene_year = max(incumbent_bene, na.rm = TRUE)
), by = .(BASE_ID, year)] %>%
  merge(
    unique(bcp[, ..bene_cols]),
    by = c("BASE_ID", "year")
  )

message(sprintf("Bene summary: %d bene-year rows, %d unique benes",
                nrow(bene), uniqueN(bene$BASE_ID)))


# ---------------------------------------------------------------------------
# Survey design declaration
# ---------------------------------------------------------------------------

bene_design <- svydesign(
  ids     = ~variance_psu,
  strata  = ~variance_stratum,
  weights = ~wgt_full_sample,
  nest    = TRUE,
  data    = as.data.frame(bene)
)


# ---------------------------------------------------------------------------
# Per-market plan-set list (for fast simulator lookups)
# ---------------------------------------------------------------------------
# `markets[[m]]` is a data.table with columns plan_id, plan_kind, mean_cost,
# var_cost, Star_Rating, pf_rank_score, parent_org_loo_national, etc.

market_keys <- unique(bcp[, .(county_fips, year)])
market_keys[, market_id := .I]

bcp[market_keys, on = c("county_fips", "year"), market_id := i.market_id]

plan_cols <- c(
  "plan_id", "plan_kind", "plan_category", "has_partd", "parent_org",
  "Star_Rating", "mean_cost", "var_cost", "sd_cost",
  "pf_rank_score", "parent_org_loo_national", "parent_org_loo_state",
  "plan_tenure_national", "plan_tenure_county",
  "ins_brokers_estab", "ins_brokers_emp", "total_eligibles",
  "broker_density_per_k"
)

markets <- vector("list", nrow(market_keys))
for (m in seq_len(nrow(market_keys))) {
  markets[[m]] <- bcp[market_id == m, ..plan_cols][!duplicated(plan_id)]
}
message(sprintf("Built per-market plan-set list: %d markets", length(markets)))


# ---------------------------------------------------------------------------
# bene_to_market and choice_idx for fast likelihood lookups
# ---------------------------------------------------------------------------

bene[market_keys, on = c("county_fips", "year"), market_id := i.market_id]

# For each bene, the within-market index of their chosen plan.
# Stored as a column on `bene` (not a separate vector) so downstream
# scripts can reference `bene$choice_idx[i]` directly.
choice_long <- bcp[is_chosen == 1, .(BASE_ID, year, plan_id, market_id)]
setkey(choice_long, BASE_ID, year)

bene[, choice_idx := vapply(seq_len(.N), function(i) {
  m   <- market_id[i]
  pid <- choice_long[.(BASE_ID[i], year[i]), plan_id, mult = "first"]
  which(markets[[m]]$plan_id == pid)[1]
}, integer(1))]

bene_to_market <- bene$market_id

stopifnot(!any(is.na(bene$choice_idx)))
message(sprintf("Chosen-plan index resolved for all %d benes", nrow(bene)))


# ---------------------------------------------------------------------------
# Final diagnostics
# ---------------------------------------------------------------------------

message("\nFinal estimation sample:")
message(sprintf("  N bene-years              : %d", nrow(bene)))
message(sprintf("  N markets                 : %d", length(markets)))
message(sprintf("  N bene-plan rows (long)   : %d", nrow(bcp)))
message(sprintf("  Years                     : %s", paste(sort(unique(bene$year)), collapse = ", ")))
message(sprintf("  Pct MA (admin)            : %.1f%%", 100 * mean(bene$is_ma_admin == 1)))
message(sprintf("  Pct searched              : %.1f%%", 100 * mean(bene$searched_obs == 1, na.rm = TRUE)))
message(sprintf("  Pct incumbent (MA last yr): %.1f%%", 100 * mean(bene$incumbent_bene_year == 1)))
