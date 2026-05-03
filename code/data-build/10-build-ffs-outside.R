# 10-build-ffs-outside.R — FFS outside-option expected cost and variance
#
# Constructs two FFS variants by applying FFS cost-sharing to the same
# utilization profiles used for MA dominance:
#
#   FFS_bare = Part A + Part B + standalone PDP, no Medigap
#              (high Var(C) from absent OOP cap)
#   FFS_supp = Part A + Part B + standalone PDP + Medigap Plan G
#              (low Var(C); Plan G covers Part A deductible/coinsurance and
#              Part B coinsurance; Part B deductible remains)
#
# All FFS cost-sharing parameters are national series (no county variation).
# Medigap Plan G premium varies by state/area in reality; we use a single
# national approximation here. Defer county-specific Medigap to a robustness
# extension.
#
# Input:  code/data-build/_utilization-profiles.R
# Output: data/output/ffs_outside.csv (year x variant: mean_cost, var_cost, sd_cost)

source("code/data-build/_utilization-profiles.R")

# ---------------------------------------------------------------------------
# National FFS / PDP / Medigap cost-sharing series, 2008-2018
# ---------------------------------------------------------------------------

# Sources: CMS Medicare Premiums History (Part A and Part B).
#          CMS National Base Beneficiary Premium series (Part D).
#          Standard Part D defined-benefit deductibles, CMS publication.
#          Medigap Plan G premium: approximate national median (~$150/mo)
#          held flat; varies by state/age/issue-age in reality.

ffs_series <- tibble(
  year                = 2008:2018,
  part_a_deductible   = c(1024, 1068, 1100, 1132, 1156, 1184, 1216, 1260, 1288, 1316, 1340),
  part_b_premium_mo   = c(96.40, 96.40, 110.50, 115.40, 99.90, 104.90, 104.90, 104.90, 121.80, 134.00, 134.00),
  part_b_deductible   = c(135, 135, 155, 162, 140, 147, 147, 147, 166, 183, 183),
  pdp_premium_mo      = c(27.93, 30.36, 31.94, 32.34, 31.08, 31.17, 32.42, 33.13, 34.10, 35.63, 35.02),
  pdp_deductible      = c(275, 295, 310, 310, 320, 325, 310, 320, 360, 400, 405),
  medigap_g_premium_mo = rep(150, 11)
)

# ---------------------------------------------------------------------------
# Cost computation by profile and variant
# ---------------------------------------------------------------------------

# Per-event enrollee cost under FFS_bare (no Medigap):
#   PCP, spec, op, er  -> 0.20 * allowed charge
#   ip                 -> Part A deductible (per stay, first 60 days)
#
# Per-event enrollee cost under FFS_supp (Plan G):
#   PCP, spec, op, er  -> 0 (Plan G covers Part B coinsurance)
#   ip                 -> 0 (Plan G covers Part A deductible and coinsurance)
#
# Annual fixed costs:
#   FFS_bare = 12 * (Part B premium + PDP premium) + Part B deductible (annual)
#                                                  + PDP deductible (annual)
#   FFS_supp = 12 * (Part B premium + PDP premium + Medigap G premium)
#              + Part B deductible (annual; Plan G does NOT cover this)
#              + PDP deductible (annual)

ffs_profile_costs <- function(yr_row, variant) {
  prem <- 12 * (yr_row$part_b_premium_mo + yr_row$pdp_premium_mo)
  if (variant == "supp") {
    prem <- prem + 12 * yr_row$medigap_g_premium_mo
  }
  fixed_oop <- yr_row$part_b_deductible + yr_row$pdp_deductible

  costs <- numeric(n_profiles)
  for (p in seq_along(profiles)) {
    pr <- profiles[[p]]
    if (variant == "bare") {
      services_oop <-
        0.20 * pr$pcp  * allowed_charges$pcp +
        0.20 * pr$spec * allowed_charges$spec +
        0.20 * pr$op   * allowed_charges$op +
        0.20 * pr$er   * allowed_charges$er +
        pr$ip * yr_row$part_a_deductible
    } else {
      services_oop <- 0
    }
    costs[p] <- prem + fixed_oop + services_oop
  }

  # Profile 9 ("at MOOP") for FFS_bare is unbounded — there is no cap.
  # We define it as the very-high profile cost scaled by 1.5 to represent
  # catastrophic utilization beyond the benchmark profile. This affects the
  # variance calculation only; FFS_bare's whole point is heavy right-tail risk.
  if (variant == "bare") {
    costs[n_profiles] <- prem + fixed_oop +
      1.5 * (
        0.20 * profiles[[length(profiles)]]$pcp  * allowed_charges$pcp +
        0.20 * profiles[[length(profiles)]]$spec * allowed_charges$spec +
        0.20 * profiles[[length(profiles)]]$op   * allowed_charges$op +
        0.20 * profiles[[length(profiles)]]$er   * allowed_charges$er +
        profiles[[length(profiles)]]$ip * yr_row$part_a_deductible
      )
  } else {
    costs[n_profiles] <- prem + fixed_oop
  }

  costs
}

# ---------------------------------------------------------------------------
# Build year x variant panel
# ---------------------------------------------------------------------------

ffs_outside <- ffs_series %>%
  rowwise() %>%
  group_split() %>%
  map_dfr(function(yr_row) {
    map_dfr(c("bare", "supp"), function(variant) {
      costs <- ffs_profile_costs(yr_row, variant)
      mean_cost <- sum(profile_weights * costs[seq_along(profile_weights)])
      var_cost  <- sum(profile_weights *
                       (costs[seq_along(profile_weights)] - mean_cost)^2)
      tibble(
        year      = yr_row$year,
        variant   = variant,
        mean_cost = mean_cost,
        var_cost  = var_cost,
        sd_cost   = sqrt(var_cost)
      )
    })
  })

message("\n========== FFS outside option ==========")
print(ffs_outside, n = Inf)

write_csv(ffs_outside, "data/output/ffs_outside.csv")
message("\nWrote data/output/ffs_outside.csv")
