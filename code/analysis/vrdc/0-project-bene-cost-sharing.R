# 0-project-bene-cost-sharing.R — Bene-specific EC[c|i,j] and Var(C|j)
#
# Replaces the population-level mean_cost / var_cost columns from
# structural_panel.csv (which were built from stylized utilization profiles)
# with bene-specific cost-sharing projections. Utilization is each bene's
# EXPECTED use at the time of choice — the mean of their own realized use over
# the prior up-to-three years, or, for benes with no prior claims, a Poisson
# prediction from pre-choice traits — priced against each plan's PBP cost-
# sharing schedule. Expected (not realized same-year) use avoids the look-ahead
# and plan-endogeneity of pricing on the choice year's own claims.
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
#                                           with EC[c|i,j] and Var_C_ij
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
bene[, county_fips := str_pad(state_cnty_fips, 5, side = "left", pad = "0")]

ma_util  <- fread(ma_util_path)
ffs_util <- fread(ffs_util_path)

count_cols <- c("n_car_pcp_lines", "n_car_spec_lines",
                "n_op_visits", "n_op_er_visits", "n_ip_days")

# Realized utilization history, one row per (BENE_ID, year). FFS spans
# 2012-2018 and MA 2015-2018; a bene is FFS xor MA in a year, so the two
# panels never collide on (BENE_ID, year).
util_hist <- rbindlist(list(
  ma_util [, c("BENE_ID", "year", count_cols), with = FALSE],
  ffs_util[, c("BENE_ID", "year", count_cols), with = FALSE]
), use.names = TRUE)

# Expected use = each bene's own realized use over the prior up-to-three years;
# each history year feeds choice years t = year+1..+3. One draw per prior year.
hist_long <- util_hist[rep(seq_len(.N), each = 3L)]
hist_long[, year := year + rep(1:3, times = nrow(util_hist))]
hist_long <- hist_long[year %between% c(2015L, 2018L)]
draws_prior <- merge(hist_long, bene[, .(BENE_ID, year, county_fips)],
                     by = c("BENE_ID", "year"))

# Fallback where a bene-year has no prior use: Poisson prediction of each
# service count from pre-choice traits, fit where same-year realized use
# exists. One draw per such bene-year; the bene's own realized use never enters.
trait_cols <- c("age", "srh", "health_vs_year_ago", "dual_annual",
                "income_cat", "education_cat", "adi_raw")
traits <- fread(bene_path, select = c("BENE_ID", "year", trait_cols))
for (tc in trait_cols) traits[, (tc) := as.numeric(get(tc))]
for (tc in trait_cols)
  traits[is.na(get(tc)), (tc) := traits[, median(get(tc), na.rm = TRUE)]]

fit_dt   <- merge(bene[, .(BENE_ID, year)], util_hist, by = c("BENE_ID", "year"))
fit_dt   <- merge(fit_dt, traits, by = c("BENE_ID", "year"))
no_prior <- fsetdiff(bene[, .(BENE_ID, year)], unique(draws_prior[, .(BENE_ID, year)]))
draws_fb <- merge(no_prior, bene[, .(BENE_ID, year, county_fips)], by = c("BENE_ID", "year"))
draws_fb <- merge(draws_fb, traits, by = c("BENE_ID", "year"))
for (col in count_cols) {
  m <- glm(reformulate(trait_cols, response = col), data = fit_dt, family = poisson())
  draws_fb[, (col) := predict(m, newdata = draws_fb, type = "response")]
}
draws_fb[, (trait_cols) := NULL]

draws <- rbindlist(list(draws_prior, draws_fb), use.names = TRUE)
draws[, n_op_other := pmax(n_op_visits - n_op_er_visits, 0)]

# Expected cost per (bene, plan): mean of the bene's own draws priced under
# every plan in their market.
setkey(draws, county_fips, year)
setkey(pbp_sched, county_fips, year)
priced <- draws[pbp_sched, on = .(county_fips, year), nomatch = NULL, allow.cartesian = TRUE]
priced[, oop  := n_car_pcp_lines * pcp_per_event + n_car_spec_lines * spec_per_event +
                 n_op_other * op_per_event + n_op_er_visits * er_per_event +
                 n_ip_days * ip_per_day]
priced[, cost := premium_annual + pmin(ded_total + oop, moop_eff)]
bp <- priced[, .(EC = mean(cost)), by = .(BENE_ID, year, county_fips, plan_id)]

# Risk cell: decile of the bene's expected annual cost, from their own expected
# use. The variance below is the spread among cell-mates (Abaluck-Gruber risk
# cells), not the bene's own 2-3 prior years, which are too few to identify it.
bene_use <- draws[, lapply(.SD, mean), by = .(BENE_ID, year, county_fips),
                  .SDcols = count_cols]
bene_use[, use_index := n_car_pcp_lines * allowed_pcp + n_car_spec_lines * allowed_spec +
                        n_op_visits * allowed_op + n_op_er_visits * allowed_er +
                        n_ip_days * allowed_ip_day]
bene_use[, cell := cut(frank(use_index, ties.method = "first"), breaks = 10,
                       labels = FALSE, include.lowest = TRUE)]

# Cell cost variance: cell-mates' realized same-year use (up to 100 per cell),
# priced under each distinct cost-sharing schedule (premium drops out of a
# variance). The spread across peers is the cell's risk for that schedule.
peers <- merge(util_hist, bene_use[, .(BENE_ID, year, cell)], by = c("BENE_ID", "year"))
peers[, n_op_other := pmax(n_op_visits - n_op_er_visits, 0)]
setkey(peers, NULL)
set.seed(1)
peers <- peers[, .SD[sample(.N, min(.N, 100L))], by = cell,
               .SDcols = c(count_cols, "n_op_other")]

sched_cols <- c("pcp_per_event", "spec_per_event", "op_per_event", "er_per_event",
                "ip_per_day", "ded_total", "moop_eff")
sched <- unique(pbp_sched[, ..sched_cols])
sched[, sched_id := .I]

peers[, jk := 1L]; sched[, jk := 1L]
pv <- peers[sched, on = "jk", allow.cartesian = TRUE]
pv[, cost := pmin(ded_total +
             n_car_pcp_lines * pcp_per_event + n_car_spec_lines * spec_per_event +
             n_op_other * op_per_event + n_op_er_visits * er_per_event +
             n_ip_days * ip_per_day, moop_eff)]
schedvar <- pv[, .(Var_C_ij = var(cost)), by = .(cell, sched_id)]

# Attach EC and the cell variance, write.
pbp_v <- merge(pbp_sched, sched, by = sched_cols)
bp <- merge(bp, bene_use[, .(BENE_ID, year, cell)], by = c("BENE_ID", "year"))
bp <- merge(bp, pbp_v[, .(plan_id, county_fips, year, sched_id)],
            by = c("plan_id", "county_fips", "year"))
bp <- merge(bp, schedvar, by = c("cell", "sched_id"), all.x = TRUE)
bp[is.na(Var_C_ij), Var_C_ij := schedvar[, median(Var_C_ij, na.rm = TRUE)]]
bp <- merge(bp, bene[, .(BENE_ID, year, BASEID)], by = c("BENE_ID", "year"))

out <- bp[, .(BENE_ID, BASEID, year, county_fips, plan_id, EC, Var_C_ij, cell)]
fwrite(out, out_path)
cat(sprintf("Wrote %s: %d rows, %d benes\n", out_path, nrow(out), uniqueN(out$BENE_ID)))
