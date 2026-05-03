# _utilization-profiles.R — Shared utilization profile definitions
#
# Used by:
#   5-construct-dominance.R      (MA plan cost-curves)
#   10-build-ffs-outside.R       (FFS outside-option cost-curves)
#
# Profiles describe service counts per year for representative utilization
# patterns. Each plan's annual enrollee cost is computed by applying the
# plan's cost-sharing schedule to every profile, then taking the
# probability-weighted mean and variance across profiles.
#
# Sources: CMS/RAND MA Encounter Data Report (2019), Table 3.1;
#          Ganguli et al. (2021) for PCP/specialist split;
#          MedPAC Data Book (2024) for spending validation and weights.

profiles <- list(
  list(name = "none",       pcp = 0, spec = 0, op = 0, er = 0, ip = 0, implied_spend = 0),
  list(name = "minimal",    pcp = 2, spec = 0, op = 0, er = 0, ip = 0, implied_spend = 400),
  list(name = "low",        pcp = 3, spec = 1, op = 0, er = 0, ip = 0, implied_spend = 950),
  list(name = "below_avg",  pcp = 3, spec = 2, op = 1, er = 0, ip = 0, implied_spend = 2700),
  list(name = "average",    pcp = 3, spec = 5, op = 3, er = 1, ip = 0, implied_spend = 9850),
  list(name = "above_avg",  pcp = 3, spec = 5, op = 4, er = 1, ip = 0, implied_spend = 11850),
  list(name = "high",       pcp = 3, spec = 5, op = 4, er = 1, ip = 1, implied_spend = 26850),
  list(name = "very_high",  pcp = 3, spec = 7, op = 5, er = 2, ip = 2, implied_spend = 44300)
)

# Profile 9 (MOOP) is appended in MA cost computation; FFS bare has no MOOP,
# FFS supp's MOOP is set by Medigap Plan G structure.
n_profiles <- length(profiles) + 1

profile_weights <- c(
  0.10, 0.15, 0.15, 0.10, 0.22, 0.13, 0.08, 0.04, 0.03
)
stopifnot(abs(sum(profile_weights) - 1) < 1e-10)

allowed_charges <- list(
  pcp  = 200,
  spec = 350,
  op   = 2000,
  er   = 1500,
  ip   = 15000
)

avg_ip_los <- 5
