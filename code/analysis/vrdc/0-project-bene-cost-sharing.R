# 0-project-bene-cost-sharing.R — Bene-specific EC[c|i,j] and Var(C|j)
#
# Replaces the population-level mean_cost / var_cost columns from
# structural_panel.csv (which were built from stylized utilization profiles)
# with bene-specific cost-sharing projections built from each bene's actual
# claims utilization × each plan's PBP cost-sharing schedule.
#
# For MA benes: utilization comes from the MA encounter panel (script 4).
# For FFS benes: utilization comes from the FFS claims panel (script 5).
# Cost-sharing schedule comes from plan_county_benefits.csv (uploaded local).
#
# Inputs (RStudio project root = ma-search/):
#   data/input/plan_county_benefits.zip     uploaded local PBP cost-sharing
#                                           (zipped due to upload size cap;
#                                           streamed via unzip -p, not extracted)
#   data/input/bene_panel.csv               SAS-exported bene-year panel (script 3)
#   data/input/ma_util_panel.csv            SAS-exported MA utilization (script 4)
#   data/input/ffs_util_panel.csv           SAS-exported FFS utilization (script 5)
#
# Output:
#   data/output/bene_cost_sharing.csv       one row per (BENE_ID, plan_id, year)
#                                           with EC[c|i,j] and Var_C_j
#
# Service categories (matched to the existing dominance pipeline at
# code/data-build/_utilization-profiles.R):
#   pcp  -> n_car_pcp_lines
#   spec -> n_car_spec_lines
#   op   -> n_op_visits - n_op_er_visits  (non-ER outpatient)
#   er   -> n_op_er_visits
#   ip   -> n_ip_days  (per-day cost-sharing applied directly)
# SNF and HHA contributions are deferred (PBP files don't expose B2/B6 fields
# in plan_county_benefits.csv; matches the dominance computation's choice).

pacman::p_load(data.table)

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

pbp_path      <- "data/input/plan_county_benefits.zip"
bene_path     <- "data/input/bene_panel.csv"
ma_util_path  <- "data/input/ma_util_panel.csv"
ffs_util_path <- "data/input/ffs_util_panel.csv"
out_path      <- "data/output/bene_cost_sharing.csv"

for (p in c(pbp_path, bene_path, ma_util_path, ffs_util_path)) {
  if (!file.exists(p)) stop("Required input not found: ", p)
}

# ---------------------------------------------------------------------------
# Allowed-charge constants (from code/data-build/_utilization-profiles.R)
# ---------------------------------------------------------------------------

allowed_pcp  <- 200
allowed_spec <- 350
allowed_op   <- 2000
allowed_er   <- 1500
allowed_ip_day <- 3000   # 15000 per stay / 5-day average LOS

# ---------------------------------------------------------------------------
# Per-event enrollee-cost helper. Mirrors per_event_cost() in
# 5-construct-dominance.R: takes max of dollar copay and coinsurance-implied
# dollar amount when both are present.
# ---------------------------------------------------------------------------

per_event <- function(copay, coins_pct, allowed) {
  cp <- ifelse(is.na(copay), 0, copay)
  co <- ifelse(is.na(coins_pct), 0, coins_pct / 100 * allowed)
  pmax(cp, co)
}

# ---------------------------------------------------------------------------
# Load plan cost-sharing schedule. Use min copay / min coinsurance fields
# (the in-network base case). plan_county_benefits.csv carries one row per
# (plan_id, county_fips, year), so this is the choice-set cost-sharing
# already keyed correctly for the bene-plan join.
# ---------------------------------------------------------------------------

pbp <- fread(cmd = paste("unzip -p", pbp_path), colClasses = c(county_fips = "character"))
pbp[, plan_id := paste0(contractid, "_", planid)]

pbp[, `:=`(
  pcp_per_event  = per_event(pcp_copay_min,        pcp_coins_min,        allowed_pcp),
  spec_per_event = per_event(specialist_copay_min, specialist_coins_min, allowed_spec),
  op_per_event   = per_event(outpatient_copay,     outpatient_coins,     allowed_op),
  er_per_event   = per_event(er_copay_min,         er_coins_min,         allowed_er),
  ip_per_day     = per_event(inpatient_copay,      inpatient_coins_pct,  allowed_ip_day),
  ded_total      = fifelse(is.na(deductible),      0, deductible) +
                   fifelse(is.na(drug_deductible), 0, drug_deductible),
  premium_annual = 12 * fifelse(is.na(premium), 0, premium),
  moop_eff       = fifelse(is.na(moop) | moop == 0, 6700, moop)
)]

pbp_sched <- pbp[, .(
  county_fips, year, plan_id,
  pcp_per_event, spec_per_event, op_per_event, er_per_event, ip_per_day,
  ded_total, premium_annual, moop_eff
)]

# ---------------------------------------------------------------------------
# Load bene panel + utilization panels
# ---------------------------------------------------------------------------

bene <- fread(bene_path, select = c(
  "BASEID", "BENE_ID", "year", "state_cnty_fips", "is_ffs_mbsf",
  "link_status", "full_year_partAB", "not_esrd", "active_shopper"
))
bene <- bene[link_status == "ok" & full_year_partAB == 1 & not_esrd == 1
             & active_shopper == 1 & !is.na(state_cnty_fips)]
bene[, county_fips := sprintf("%05s", as.character(state_cnty_fips))]

ma_util  <- fread(ma_util_path)
ffs_util <- fread(ffs_util_path)

# Stack into one util_panel keyed on (BENE_ID, year). MA/FFS are mutually
# exclusive in any bene-year (a bene is either MA or FFS for the year).
util <- rbindlist(list(
  ma_util [, .(BENE_ID, year, n_car_pcp_lines, n_car_spec_lines,
               n_op_visits, n_op_er_visits, n_ip_days)],
  ffs_util[, .(BENE_ID, year, n_car_pcp_lines, n_car_spec_lines,
               n_op_visits, n_op_er_visits, n_ip_days)]
), use.names = TRUE)

util[, `:=`(
  n_op_other = pmax(n_op_visits - n_op_er_visits, 0)
)]

# ---------------------------------------------------------------------------
# Bene-side utilization, attached to (BENE_ID, year)
# ---------------------------------------------------------------------------

bene_util <- merge(
  bene, util, by = c("BENE_ID", "year"), all.x = TRUE
)

# Benes with no claims rows in either panel get zero utilization (very low
# users — keep them rather than dropping).
util_cols <- c("n_car_pcp_lines", "n_car_spec_lines",
               "n_op_visits", "n_op_er_visits", "n_op_other", "n_ip_days")
for (c in util_cols) {
  bene_util[is.na(get(c)), (c) := 0]
}

cat(sprintf("Bene-util rows: %d\n", nrow(bene_util)))
cat(sprintf("Benes with zero utilization: %d (%.1f%%)\n",
            bene_util[, sum(n_car_pcp_lines + n_car_spec_lines + n_op_visits + n_ip_days == 0)],
            100 * bene_util[, mean(n_car_pcp_lines + n_car_spec_lines + n_op_visits + n_ip_days == 0)]))

# ---------------------------------------------------------------------------
# Bene x plan cartesian inside (county_fips, year). One row per
# (BENE_ID, plan_id, year). EC[c|i,j] = annual premium + min(MOOP, deductible
# + service OOP).
# ---------------------------------------------------------------------------

setkey(bene_util,  county_fips, year)
setkey(pbp_sched,  county_fips, year)

bp <- bene_util[pbp_sched, on = .(county_fips, year),
                nomatch = NULL, allow.cartesian = TRUE]

bp[, `:=`(
  oop_services = n_car_pcp_lines  * pcp_per_event
               + n_car_spec_lines * spec_per_event
               + n_op_other       * op_per_event
               + n_op_er_visits   * er_per_event
               + n_ip_days        * ip_per_day,
  oop_total    = NA_real_,
  EC           = NA_real_
)]
bp[, oop_total  := ded_total + oop_services]
bp[, EC         := premium_annual + pmin(oop_total, moop_eff)]

cat(sprintf("\nBene x plan rows: %d (uniqueN benes %d, plans %d)\n",
            nrow(bp), uniqueN(bp$BENE_ID), uniqueN(bp$plan_id)))

# ---------------------------------------------------------------------------
# Var(C|j) — variance of EC across benes in plan j's market pool. Computed
# at the plan_id x year level (ignoring county heterogeneity within plan,
# which is small for non-regional plans).
# ---------------------------------------------------------------------------

var_j <- bp[, .(Var_C_j = var(EC), n_pool = .N), by = .(plan_id, year)]
bp <- merge(bp, var_j, by = c("plan_id", "year"), all.x = TRUE)

cat(sprintf("\nDistribution of EC across (bene, plan) pairs:\n"))
print(summary(bp$EC))
cat(sprintf("\nDistribution of sqrt(Var_C_j) across plans:\n"))
print(summary(sqrt(var_j$Var_C_j)))

# ---------------------------------------------------------------------------
# Write checkpoint — long format keyed on (BENE_ID, plan_id, year)
# ---------------------------------------------------------------------------

out <- bp[, .(BENE_ID, BASEID, year, county_fips, plan_id, EC, Var_C_j, n_pool)]
fwrite(out, out_path)
cat(sprintf("\nWrote %s (%d rows)\n", out_path, nrow(out)))
