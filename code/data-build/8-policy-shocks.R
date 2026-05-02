# 8-policy-shocks.R — Build policy-shock indicators for IV designs
#
# Two MA payment policies that exogenously shifted plan entry, available as
# instruments for choice-set complexity (n_plans):
#
#   1. Urban Floor (MMA 2003)  — county received a payment-floor bump if it was
#      in a CBSA with population >= 250K and had low FFS costs. Designation
#      frozen in 2004; predetermined relative to 2008+ search dynamics. Active
#      every year of the panel. CMS publishes the Urban Floor 2004 flag as a
#      column in every annual calculation-rate (risk) file.
#
#   2. Bonus County (ACA 2010, active 2012-2015) — county qualified for double
#      quality-bonus payments based on 2009 MA penetration >=25%, urban-floor
#      status, and per-capita spending below national median. CMS publishes
#      a "Qualifying County" flag in each annual risk file 2012-2015.
#
# Sources (all under D:/research-data/):
#   - medicare-advantage/benchmarks/calculationdataYYYY/riskYYYY.csv (2012-15)
#   - geography/Zip and MSA/cbsatocountycrosswalk2005.dta (SSA->FIPS)
#
# Output: data/output/policy_shocks.csv (county_fips x year, standalone file)
#         data/output/analysis_panel.csv (augmented in place with policy cols)

# ---------------------------------------------------------------------------
# Helper: read one risk file (CMS files have ~16 rows of metadata then a
# header row starting with "Code,State,County")
# ---------------------------------------------------------------------------

read_risk <- function(fpath) {
  raw   <- readLines(fpath, n = 30)
  start <- which(grepl("^Code,State,County", raw))
  if (length(start) == 0) stop("no header found in ", fpath)
  read_csv(fpath, skip = start - 1, show_col_types = FALSE,
           col_types = cols(.default = "c"))
}

# ---------------------------------------------------------------------------
# SSA -> FIPS crosswalk
# ---------------------------------------------------------------------------

ssa_fips <- haven::read_dta(
  "D:/research-data/geography/Zip and MSA/cbsatocountycrosswalk2005.dta"
) %>%
  transmute(
    ssa         = str_pad(as.character(ssacounty),  5, side = "left", pad = "0"),
    county_fips = str_pad(as.character(fipscounty), 5, side = "left", pad = "0")
  ) %>%
  distinct()

message("SSA-FIPS crosswalk: ", nrow(ssa_fips), " counties")

# ---------------------------------------------------------------------------
# Pull Urban Floor 2004 (time-invariant, read once from 2012 file)
# ---------------------------------------------------------------------------

risk_2012 <- read_risk(
  "D:/research-data/medicare-advantage/benchmarks/calculationdata2012/risk2012.csv"
)

uf_2012_col <- str_subset(names(risk_2012), regex("urban floor", ignore_case = TRUE))
message("Urban Floor column in 2012 file: '", uf_2012_col, "'")

urban_floor <- risk_2012 %>%
  transmute(
    ssa         = str_pad(Code, 5, side = "left", pad = "0"),
    urban_floor = as.integer(.data[[uf_2012_col]] == "Yes")
  ) %>%
  filter(!is.na(urban_floor)) %>%
  distinct()
message("Urban Floor 2004 counties (Yes): ", sum(urban_floor$urban_floor),
        " of ", nrow(urban_floor))

# ---------------------------------------------------------------------------
# Pull Bonus County (yearly, 2012-2015)
# ---------------------------------------------------------------------------

risk_path <- function(yr) {
  # 2015 only: file lives in a CSV/ subfolder; other years sit directly under
  # calculationdataYYYY/.
  candidates <- c(
    paste0("D:/research-data/medicare-advantage/benchmarks/calculationdata",
           yr, "/risk", yr, ".csv"),
    paste0("D:/research-data/medicare-advantage/benchmarks/calculationdata",
           yr, "/CSV/risk", yr, ".csv")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) stop("no risk", yr, ".csv found")
  hit[1]
}

read_bonus_year <- function(yr) {
  fpath <- risk_path(yr)
  d  <- read_risk(fpath)
  bc <- str_subset(names(d), regex("qualifying county", ignore_case = TRUE))
  if (length(bc) == 0) stop("no Qualifying County column in ", fpath)
  d %>%
    transmute(
      ssa          = str_pad(Code, 5, side = "left", pad = "0"),
      year         = as.integer(yr),
      bonus_county = as.integer(.data[[bc[1]]] == "YES" |
                                .data[[bc[1]]] == "Yes")
    ) %>%
    filter(!is.na(bonus_county))
}

bonus <- map_dfr(2012:2015, read_bonus_year)
message("\nBonus County designations 2012-2015:")
bonus %>% count(year, bonus_county) %>% print()

# ---------------------------------------------------------------------------
# Build the panel: county_fips x year (2008-2018) with both flags
# ---------------------------------------------------------------------------

# Cross-product of all FIPS in the crosswalk x panel years
panel_years <- 2008:2018

policy_panel <- ssa_fips %>%
  crossing(year = panel_years) %>%
  left_join(urban_floor, by = "ssa") %>%
  left_join(bonus,       by = c("ssa", "year")) %>%
  mutate(
    urban_floor  = replace_na(urban_floor,  0L),
    # bonus_county only defined for 2012-2015; outside that window, set to 0
    # so the panel is rectangular (consumer can filter to 2012-2015 if needed)
    bonus_county = if_else(year %in% 2012:2015, replace_na(bonus_county, 0L),
                           0L)
  ) %>%
  select(county_fips, year, urban_floor, bonus_county)

# A county can appear under multiple SSAs in the crosswalk (1:m mapping in
# rare cases). Take the max over duplicates so a county is "treated" if any
# of its SSAs is treated.
policy_panel <- policy_panel %>%
  group_by(county_fips, year) %>%
  summarise(
    urban_floor  = max(urban_floor,  na.rm = TRUE),
    bonus_county = max(bonus_county, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

message("\n========== Policy panel ==========")
message("Rows: ", nrow(policy_panel), " across ",
        n_distinct(policy_panel$county_fips), " counties (",
        min(policy_panel$year), "-", max(policy_panel$year), ")")

message("\nUrban Floor share (time-invariant, sample 2010):")
policy_panel %>%
  filter(year == 2010) %>%
  count(urban_floor) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

message("\nBonus County share by year (only 2012-2015 active):")
policy_panel %>%
  filter(year %in% 2012:2015) %>%
  count(year, bonus_county) %>%
  group_by(year) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print(n = Inf)

write_csv(policy_panel, "data/output/policy_shocks.csv")
message("\nWrote data/output/policy_shocks.csv")

# ---------------------------------------------------------------------------
# Augment analysis_panel.csv (built by script 7) with policy columns
# ---------------------------------------------------------------------------

if (file.exists("data/output/analysis_panel.csv")) {
  ap <- read_csv(
    "data/output/analysis_panel.csv", show_col_types = FALSE,
    col_types = cols(county_fips = col_character(), .default = col_guess())
  ) %>%
    select(-any_of(c("urban_floor", "bonus_county")))  # drop in case of re-run

  ap_aug <- ap %>%
    left_join(policy_panel, by = c("county_fips", "year"))

  if (nrow(ap_aug) != nrow(ap)) {
    stop("Row count changed after policy join: ap=", nrow(ap),
         " aug=", nrow(ap_aug))
  }

  message("\nAugmenting analysis_panel.csv: ", nrow(ap_aug), " rows; ",
          sum(ap_aug$urban_floor == 1), " urban-floor; ",
          sum(ap_aug$bonus_county == 1, na.rm = TRUE), " bonus-county-years")

  write_csv(ap_aug, "data/output/analysis_panel.csv")
  message("Wrote data/output/analysis_panel.csv (with policy columns)")
} else {
  message("\nanalysis_panel.csv not found — run 7-merge-acs.R first ",
          "to build it, then re-run this script to augment.")
}
