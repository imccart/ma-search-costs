# 4-construct-dominance.R — Construct dominated-plan measures
#
# A plan is "dominated" if another plan in the same county-year has weakly lower
# total annual cost at EVERY utilization profile and strictly lower at some.
#
# Utilization profiles are based on actual Medicare utilization data
# (CMS/RAND 2019, MedPAC 2024). Each profile specifies service counts,
# and enrollee cost = 12*premium + sum(copays for services used) + deductibles,
# capped at MOOP. Dominance must hold across all profiles, ensuring it applies
# regardless of the beneficiary's actual health care needs.
#
# See background/utilization/utilization-notes.md for sources and validation.
#
# Input:  data/output/plan_county_benefits.csv (from script 3)
# Output: data/output/dominance_plan.csv     (plan-county-year with dominated flag)
#         data/output/dominance_county.csv    (county-year summary)

# ---------------------------------------------------------------------------
# Utilization profiles
# ---------------------------------------------------------------------------

# Each profile is a named list of service counts per year.
# Sources: CMS/RAND MA Encounter Data Report (2019), Table 3.1;
#          Ganguli et al. (2021) for PCP/specialist split;
#          MedPAC Data Book (2024) for spending validation.
#
# Per-event costs (for validation only, not used in dominance):
#   PCP visit ~$200, specialist ~$350, outpatient ~$2,000,
#   ER ~$1,500, inpatient ~$15,000

profiles <- list(
  # Profile 1: No utilization — premium only
  list(name = "none",       pcp = 0, spec = 0, op = 0, er = 0, ip = 0, implied_spend = 0),
  # Profile 2: Minimal — 2 PCP visits (~$400)
  list(name = "minimal",    pcp = 2, spec = 0, op = 0, er = 0, ip = 0, implied_spend = 400),
  # Profile 3: Low/healthy — 3 PCP + 1 specialist (~$950)
  list(name = "low",        pcp = 3, spec = 1, op = 0, er = 0, ip = 0, implied_spend = 950),
  # Profile 4: Below average — adds outpatient visit (~$2,700)
  list(name = "below_avg",  pcp = 3, spec = 2, op = 1, er = 0, ip = 0, implied_spend = 2700),
  # Profile 5: Average — near MA mean utilization (~$9,600)
  list(name = "average",    pcp = 3, spec = 5, op = 3, er = 1, ip = 0, implied_spend = 9850),
  # Profile 6: Above average — more specialist + ER (~$11,350)
  list(name = "above_avg",  pcp = 3, spec = 5, op = 4, er = 1, ip = 0, implied_spend = 11850),
  # Profile 7: High — includes inpatient stay (~$26,850)
  list(name = "high",       pcp = 3, spec = 5, op = 4, er = 1, ip = 1, implied_spend = 26850),
  # Profile 8: Very high — 2 inpatient stays (~$41,850)
  list(name = "very_high",  pcp = 3, spec = 7, op = 5, er = 2, ip = 2, implied_spend = 44300)
  # Profile 9 (MOOP) is handled separately — cost = 12*premium + MOOP exactly
)

n_profiles <- length(profiles) + 1  # +1 for MOOP profile

# Probability weights over profiles.
# Based on MedPAC 2024 health status distribution:
#   50.2% excellent/very good health → profiles 1-4
#   45.1% good/fair health → profiles 5-6
#   4.7% poor health → profiles 7-9 (including MOOP)
#
# Within each group, weights are spread roughly evenly.
profile_weights <- c(
  0.10,   # none (some beneficiaries use almost nothing)
  0.15,   # minimal
  0.15,   # low/healthy
  0.10,   # below average
  0.22,   # average (largest single group)
  0.13,   # above average (not uncommon, some chronic conditions)
  0.08,   # high (inpatient stay — ~14% have any admission)
  0.04,   # very high (multiple admissions)
  0.03    # at MOOP (catastrophic)
)
# Weights sum to 1
stopifnot(abs(sum(profile_weights) - 1) < 1e-10)


# ---------------------------------------------------------------------------
# Cost computation
# ---------------------------------------------------------------------------

# Typical Medicare allowed charges by service type.
# Used to convert coinsurance percentages to dollar amounts.
# Sources: MedPAC, CMS Medicare Provider Utilization data.
allowed_charges <- list(
  pcp  = 200,    # E&M office visit (99213/99214)
  spec = 350,    # specialist E&M + minor procedures
  op   = 2000,   # outpatient hospital (facility + professional)
  er   = 1500,   # emergency department visit
  ip   = 15000   # inpatient stay (average DRG payment)
)

# Average Medicare inpatient length of stay (~5.4 days).
# Inpatient copays in PBP are typically per-day, not per-stay.
avg_ip_los <- 5

#' Compute per-event enrollee cost for a service
#'
#' Combines copay and coinsurance. If both are present, adds them.
#' Coinsurance is converted to dollars using the typical allowed charge.
#' For inpatient, copay is per-day and multiplied by avg LOS.
#'
#' @param copay Dollar copay (NA if plan doesn't use copay)
#' @param coins_pct Coinsurance percentage (NA if plan doesn't use coinsurance)
#' @param allowed Typical allowed charge for this service
#' @param is_inpatient If TRUE, copay is per-day (multiply by avg_ip_los)
per_event_cost <- function(copay, coins_pct, allowed, is_inpatient = FALSE) {
  cp <- ifelse(is.na(copay), 0, copay)
  if (is_inpatient) cp <- cp * avg_ip_los
  co <- ifelse(is.na(coins_pct), 0, coins_pct / 100 * allowed)
  # Use whichever is non-zero; if both present, take the larger
  # (plans typically use one or the other, but if both appear, the
  # larger value is the more conservative/realistic estimate)
  max(cp, co)
}

#' Compute annual enrollee cost for a single plan across all profiles
#'
#' For each profile, cost = 12*premium + min(OOP, MOOP), where:
#'   OOP = deductible + drug_deductible + sum(events * per_event_cost)
#'
#' The final profile is at MOOP: cost = 12*premium + MOOP.
#'
#' @return Numeric vector of length n_profiles
compute_plan_costs <- function(premium, deductible, moop,
                                pcp_copay, pcp_coins,
                                spec_copay, spec_coins,
                                er_copay, er_coins,
                                op_copay, op_coins,
                                ip_copay, ip_coins,
                                drug_ded) {

  annual_prem <- 12 * ifelse(is.na(premium), 0, premium)
  ded <- ifelse(is.na(deductible), 0, deductible)
  drug_d <- ifelse(is.na(drug_ded), 0, drug_ded)
  moop_val <- ifelse(is.na(moop) | moop == 0, 6700, moop)

  # Per-event enrollee costs combining copay + coinsurance
  pcp_c  <- per_event_cost(pcp_copay,  pcp_coins,  allowed_charges$pcp)
  spec_c <- per_event_cost(spec_copay, spec_coins, allowed_charges$spec)
  er_c   <- per_event_cost(er_copay,   er_coins,   allowed_charges$er)
  op_c   <- per_event_cost(op_copay,   op_coins,   allowed_charges$op)
  ip_c   <- per_event_cost(ip_copay,   ip_coins,   allowed_charges$ip, is_inpatient = TRUE)

  costs <- numeric(n_profiles)

  for (p in seq_along(profiles)) {
    pr <- profiles[[p]]

    oop_services <- pr$pcp * pcp_c + pr$spec * spec_c +
                    pr$op * op_c + pr$er * er_c + pr$ip * ip_c

    oop_total <- ded + drug_d + oop_services
    oop_capped <- min(oop_total, moop_val)

    costs[p] <- annual_prem + oop_capped
  }

  # Final profile: at MOOP (catastrophic)
  costs[n_profiles] <- annual_prem + moop_val

  costs
}


# ---------------------------------------------------------------------------
# Read merged data
# ---------------------------------------------------------------------------

df <- read_csv(
  "data/output/plan_county_benefits.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)
message("Read ", nrow(df), " rows")

df <- df %>%
  filter(!is.na(avg_enrollment), avg_enrollment > 0)
message("After filtering to positive enrollment: ", nrow(df), " rows")

# ---------------------------------------------------------------------------
# Filter to standard MA plan types and create comparison groups
# ---------------------------------------------------------------------------

# Keep only standard MA plan types (exclude PACE, MSA, Cost, MMP, Demo, etc.)
standard_types <- c("HMO/HMOPOS", "Local PPO", "Regional PPO", "PFFS")

df <- df %>%
  mutate(
    plan_category = case_when(
      plan_type %in% c("HMO/HMOPOS") ~ "HMO",
      plan_type %in% c("Local PPO", "Regional PPO") ~ "PPO",
      plan_type %in% c("PFFS", "RFB PFFS") ~ "PFFS",
      TRUE ~ "other"
    ),
    has_partd = (partd == "Yes")
  )

message("\nPlan type distribution:")
df %>% count(plan_category) %>% print()

df <- df %>% filter(plan_category != "other")
message("After keeping standard types (HMO, PPO, PFFS): ", nrow(df), " rows")

# ---------------------------------------------------------------------------
# Eligibility: need premium and MOOP at minimum
# ---------------------------------------------------------------------------

df <- df %>%
  mutate(eligible = !is.na(premium) & !is.na(moop) & moop > 0)

df_compare <- df %>% filter(eligible)
df_ineligible <- df %>% filter(!eligible)

message("Plans eligible for cost-curve comparison: ", nrow(df_compare),
        " (", round(nrow(df_compare) / nrow(df) * 100, 1), "%)")

# Dominance comparisons are within county-year-plantype-partd groups.
# A PPO is never dominated by an HMO (different network value).
# A plan with Part D is never dominated by one without (different benefit scope).
message("\nComparison groups: county_fips x year x plan_category x has_partd")

# ---------------------------------------------------------------------------
# Compute cost at each profile for every plan
# ---------------------------------------------------------------------------

message("Computing costs across ", n_profiles, " utilization profiles...")

cost_matrix <- matrix(NA_real_, nrow = nrow(df_compare), ncol = n_profiles)

for (r in seq_len(nrow(df_compare))) {
  cost_matrix[r, ] <- compute_plan_costs(
    premium    = df_compare$premium[r],
    deductible = df_compare$deductible[r],
    moop       = df_compare$moop[r],
    pcp_copay  = df_compare$pcp_copay_min[r],
    pcp_coins  = df_compare$pcp_coins_min[r],
    spec_copay = df_compare$specialist_copay_min[r],
    spec_coins = df_compare$specialist_coins_min[r],
    er_copay   = df_compare$er_copay_min[r],
    er_coins   = df_compare$er_coins_min[r],
    op_copay   = df_compare$outpatient_copay[r],
    op_coins   = df_compare$outpatient_coins[r],
    ip_copay   = df_compare$inpatient_copay[r],
    ip_coins   = df_compare$inpatient_coins_pct[r],
    drug_ded   = df_compare$drug_deductible[r]
  )
}

# Store key cost points for validation
profile_names <- c(sapply(profiles, `[[`, "name"), "at_moop")
df_compare$cost_none    <- cost_matrix[, 1]
df_compare$cost_average <- cost_matrix[, 5]
df_compare$cost_high    <- cost_matrix[, 7]
df_compare$cost_at_moop <- cost_matrix[, n_profiles]

# ---------------------------------------------------------------------------
# Validation: do simulated costs match observed spending?
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Validate per-event costs: are coinsurance plans being captured?
# ---------------------------------------------------------------------------

message("\n========== Per-Event Cost Validation ==========")
message("Checking how copay vs coinsurance drives per-event enrollee costs...")

# Compute per-event costs for each plan and service for diagnostics
df_compare <- df_compare %>%
  mutate(
    pev_pcp  = mapply(per_event_cost, pcp_copay_min, pcp_coins_min,
                      MoreArgs = list(allowed = allowed_charges$pcp)),
    pev_spec = mapply(per_event_cost, specialist_copay_min, specialist_coins_min,
                      MoreArgs = list(allowed = allowed_charges$spec)),
    pev_er   = mapply(per_event_cost, er_copay_min, er_coins_min,
                      MoreArgs = list(allowed = allowed_charges$er)),
    pev_op   = mapply(per_event_cost, outpatient_copay, outpatient_coins,
                      MoreArgs = list(allowed = allowed_charges$op)),
    pev_ip   = mapply(per_event_cost, inpatient_copay, inpatient_coins_pct,
                      MoreArgs = list(allowed = allowed_charges$ip, is_inpatient = TRUE))
  )

message("\nPer-event enrollee costs (mean, median, max):")
for (svc in c("pev_pcp", "pev_spec", "pev_er", "pev_op", "pev_ip")) {
  vals <- df_compare[[svc]]
  msg <- sprintf("  %-10s: mean=$%6.0f  median=$%6.0f  max=$%6.0f  pct_zero=%4.1f%%",
                 gsub("pev_", "", svc),
                 mean(vals, na.rm = TRUE), median(vals, na.rm = TRUE),
                 max(vals, na.rm = TRUE), mean(vals == 0, na.rm = TRUE) * 100)
  message(msg)
}

message("\nPlans using coinsurance (coins > 0) by service:")
message(sprintf("  PCP:         %d (%.1f%%)", sum(df_compare$pcp_coins_min > 0, na.rm = TRUE),
                mean(df_compare$pcp_coins_min > 0, na.rm = TRUE) * 100))
message(sprintf("  Specialist:  %d (%.1f%%)", sum(df_compare$specialist_coins_min > 0, na.rm = TRUE),
                mean(df_compare$specialist_coins_min > 0, na.rm = TRUE) * 100))
message(sprintf("  ER:          %d (%.1f%%)", sum(df_compare$er_coins_min > 0, na.rm = TRUE),
                mean(df_compare$er_coins_min > 0, na.rm = TRUE) * 100))
message(sprintf("  Outpatient:  %d (%.1f%%)", sum(df_compare$outpatient_coins > 0, na.rm = TRUE),
                mean(df_compare$outpatient_coins > 0, na.rm = TRUE) * 100))
message(sprintf("  Inpatient:   %d (%.1f%%)", sum(df_compare$inpatient_coins_pct > 0, na.rm = TRUE),
                mean(df_compare$inpatient_coins_pct > 0, na.rm = TRUE) * 100))

message("\n========== Cost Curve Validation ==========")
message("Benchmarks: MedPAC 2021 per-capita spending")
message("  Excellent/very good health: $8,935")
message("  Good/fair health:           $18,124")
message("  Poor health:                $39,962")
message("  Overall average:            $15,094")

message("\nSimulated enrollee costs (mean across plans):")
for (p in seq_len(n_profiles)) {
  pname <- profile_names[p]
  implied <- if (p <= length(profiles)) profiles[[p]]$implied_spend else NA
  msg <- sprintf("  %-12s: enrollee cost = $%s", pname,
                 format(round(mean(cost_matrix[, p])), big.mark = ","))
  if (!is.na(implied)) {
    msg <- paste0(msg, sprintf("  (implied total spending: $%s)",
                               format(implied, big.mark = ",")))
  }
  message(msg)
}

message("\nNote: enrollee cost = premium + OOP. Total spending includes")
message("insurer-paid amounts and is much higher than enrollee cost.")

# ---------------------------------------------------------------------------
# Compute mean and variance of cost for each plan
# ---------------------------------------------------------------------------

# Weighted mean cost: E[cost] = sum(w_s * cost_s)
# Weighted variance:  V[cost] = sum(w_s * (cost_s - E[cost])^2)

plan_mean_cost <- cost_matrix %*% profile_weights
plan_var_cost  <- (cost_matrix - as.vector(plan_mean_cost))^2 %*% profile_weights

df_compare$mean_cost <- as.vector(plan_mean_cost)
df_compare$var_cost  <- as.vector(plan_var_cost)
df_compare$sd_cost   <- sqrt(df_compare$var_cost)

message("\n========== Mean-Variance Summary ==========")
message(sprintf("Mean cost:  mean=$%.0f, median=$%.0f, sd=$%.0f",
                mean(df_compare$mean_cost), median(df_compare$mean_cost),
                sd(df_compare$mean_cost)))
message(sprintf("SD of cost: mean=$%.0f, median=$%.0f",
                mean(df_compare$sd_cost), median(df_compare$sd_cost)))

# ---------------------------------------------------------------------------
# Check dominance within county-year-plantype-partd groups
# ---------------------------------------------------------------------------
# Two dominance concepts:
#   1. Cost-curve dominance: B's cost <= A's cost at EVERY profile (strict on >= 1)
#   2. Mean-variance dominance: B has weakly lower mean AND weakly lower variance
#      (strict on >= 1). Any risk-averse EU maximizer prefers B.
#
# We report both. Mean-variance is the primary measure.

df_compare$row_idx <- seq_len(nrow(df_compare))

group_indices <- df_compare %>%
  group_by(county_fips, year, plan_category, has_partd) %>%
  summarize(rows = list(row_idx), .groups = "drop")

message("\nChecking dominance across ", nrow(group_indices),
        " county-year-type-partd groups...")

dom_curve <- rep(FALSE, nrow(df_compare))
dom_mv    <- rep(FALSE, nrow(df_compare))
dom_curve_by <- rep(NA_character_, nrow(df_compare))
dom_mv_by    <- rep(NA_character_, nrow(df_compare))

n_groups <- nrow(group_indices)
report_every <- max(1, floor(n_groups / 20))

for (g in seq_len(n_groups)) {
  if (g %% report_every == 0) {
    message("  Progress: ", round(g / n_groups * 100), "%")
  }

  idx <- group_indices$rows[[g]]
  n <- length(idx)
  if (n <= 1) next

  group_costs <- cost_matrix[idx, , drop = FALSE]
  group_means <- plan_mean_cost[idx]
  group_vars  <- plan_var_cost[idx]

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next

      # Cost-curve dominance
      if (!dom_curve[idx[i]]) {
        diff <- group_costs[i, ] - group_costs[j, ]
        if (all(diff >= 0) && any(diff > 0)) {
          dom_curve[idx[i]] <- TRUE
          dom_curve_by[idx[i]] <- paste0(df_compare$contractid[idx[j]], "_",
                                          df_compare$planid[idx[j]])
        }
      }

      # Mean-variance dominance
      if (!dom_mv[idx[i]]) {
        mean_diff <- group_means[i] - group_means[j]  # positive = j cheaper
        var_diff  <- group_vars[i]  - group_vars[j]    # positive = j less risky
        if (mean_diff >= 0 && var_diff >= 0 && (mean_diff > 0 || var_diff > 0)) {
          dom_mv[idx[i]] <- TRUE
          dom_mv_by[idx[i]] <- paste0(df_compare$contractid[idx[j]], "_",
                                       df_compare$planid[idx[j]])
        }
      }

      if (dom_curve[idx[i]] && dom_mv[idx[i]]) break
    }
  }
}

df_compare$dom_curve    <- dom_curve
df_compare$dom_curve_by <- dom_curve_by
df_compare$dom_mv       <- dom_mv
df_compare$dom_mv_by    <- dom_mv_by

# Primary dominance flag: mean-variance
df_compare$dominated    <- dom_mv
df_compare$dominated_by <- dom_mv_by

message("\n========== Dominance Results ==========")
message(sprintf("Cost-curve dominated:      %d (%.1f%%)",
                sum(dom_curve), mean(dom_curve) * 100))
message(sprintf("Mean-variance dominated:   %d (%.1f%%)",
                sum(dom_mv), mean(dom_mv) * 100))
message(sprintf("Both:                      %d (%.1f%%)",
                sum(dom_curve & dom_mv), mean(dom_curve & dom_mv) * 100))
message(sprintf("MV only (not cost-curve):  %d (%.1f%%)",
                sum(dom_mv & !dom_curve), mean(dom_mv & !dom_curve) * 100))

message("\nDominance rate by plan type and Part D (mean-variance):")
df_compare %>%
  group_by(plan_category, has_partd) %>%
  summarize(n = n(), n_dom = sum(dominated),
            pct = round(n_dom / n * 100, 1), .groups = "drop") %>%
  print(n = 20)

# ---------------------------------------------------------------------------
# Combine eligible + ineligible
# ---------------------------------------------------------------------------

df_ineligible <- df_ineligible %>%
  mutate(cost_none = NA_real_, cost_average = NA_real_,
         cost_high = NA_real_, cost_at_moop = NA_real_,
         mean_cost = NA_real_, var_cost = NA_real_, sd_cost = NA_real_,
         dom_curve = NA, dom_curve_by = NA_character_,
         dom_mv = NA, dom_mv_by = NA_character_,
         dominated = NA, dominated_by = NA_character_)

df_out <- bind_rows(
  df_compare %>% select(-row_idx, -starts_with("pev_"),
                        -any_of(c("eligible", "has_premium", "n_copay_nonmiss"))),
  df_ineligible %>% select(-any_of(c("eligible", "has_premium", "n_copay_nonmiss")))
)

# ---------------------------------------------------------------------------
# Write plan-level output
# ---------------------------------------------------------------------------

write_csv(df_out, "data/output/dominance_plan.csv")
message("\nWrote data/output/dominance_plan.csv")

# ---------------------------------------------------------------------------
# County-year summary
# ---------------------------------------------------------------------------

county_summary <- df_out %>%
  filter(!is.na(dominated)) %>%
  group_by(county_fips, year, state, county_name) %>%
  summarize(
    n_plans = n(),
    n_dominated = sum(dominated),
    pct_dominated = round(n_dominated / n_plans * 100, 1),
    total_enrollment = sum(avg_enrollment, na.rm = TRUE),
    enrollment_in_dominated = sum(avg_enrollment[dominated], na.rm = TRUE),
    pct_enrollment_dominated = round(enrollment_in_dominated / total_enrollment * 100, 1),
    .groups = "drop"
  )

message("\n========== County-Year Summary ==========")
message("County-years: ", nrow(county_summary))

message("\nDistribution of pct plans dominated:")
county_summary %>%
  summarize(min = min(pct_dominated), p25 = quantile(pct_dominated, 0.25),
            median = median(pct_dominated), mean = round(mean(pct_dominated), 1),
            p75 = quantile(pct_dominated, 0.75), max = max(pct_dominated)) %>%
  print()

message("\nDistribution of pct enrollment in dominated plans:")
county_summary %>%
  filter(total_enrollment > 0) %>%
  summarize(min = min(pct_enrollment_dominated),
            p25 = quantile(pct_enrollment_dominated, 0.25),
            median = median(pct_enrollment_dominated),
            mean = round(mean(pct_enrollment_dominated), 1),
            p75 = quantile(pct_enrollment_dominated, 0.75),
            max = max(pct_enrollment_dominated)) %>%
  print()

message("\nMean pct enrollment in dominated plans by year:")
county_summary %>%
  group_by(year) %>%
  summarize(mean_pct = round(mean(pct_enrollment_dominated), 1)) %>%
  print(n = 20)

write_csv(county_summary, "data/output/dominance_county.csv")
message("\nWrote data/output/dominance_county.csv")
