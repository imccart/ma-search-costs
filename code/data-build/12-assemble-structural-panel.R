# 12-assemble-structural-panel.R — Plan-attributes panel for the structural model
#
# Produces the county x plan x year panel that the Stigler-search structural
# model consumes. Two kinds of rows:
#
#   MA plans     — one row per (county_fips, year, contractid_planid).
#                  Carries mean_cost, var_cost, Star_Rating, plan_category,
#                  has_partd, parent_org, plus salience inputs (incumbent flag,
#                  parent-insurer market share).
#   FFS variants — two rows per (county_fips, year): FFS_bare and FFS_supp.
#                  Carries year-level mean_cost / var_cost from
#                  ffs_outside.csv. Salience inputs degenerate (always
#                  "considered", parent_org = "FFS").
#
# Every row also carries `total_eligibles` (county-year Medicare population
# from the penetration pull) and `ins_brokers_estab`, `ins_brokers_emp`
# (CBP NAICS 524210 broker density), so the structural code can derive
# market shares and apply broker density as a search-cost shifter directly
# from the panel.
#
# Input:  data/output/dominance_plan.csv
#         data/output/ffs_outside.csv
#         data/output/penetration.csv
#         data/output/cbp_broker.csv
# Output: data/output/structural_panel.csv

# ---------------------------------------------------------------------------
# Read MA plan-level data
# ---------------------------------------------------------------------------

ma <- read_csv(
  "data/output/dominance_plan.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
) %>%
  filter(!is.na(mean_cost), !is.na(avg_enrollment), avg_enrollment > 0)

message("MA plan rows (with cost-curve and positive enrollment): ", nrow(ma))

ma <- ma %>%
  mutate(plan_id = paste0(contractid, "_", planid))

# ---------------------------------------------------------------------------
# Incumbent flag: was this plan_id present in same county_fips in t-1?
# ---------------------------------------------------------------------------

incumbent <- ma %>%
  distinct(plan_id, county_fips, year) %>%
  mutate(prior_year = year - 1L) %>%
  inner_join(
    ma %>% distinct(plan_id, county_fips, year) %>%
      rename(prior_year = year),
    by = c("plan_id", "county_fips", "prior_year")
  ) %>%
  transmute(plan_id, county_fips, year, incumbent = TRUE)

ma <- ma %>%
  left_join(incumbent, by = c("plan_id", "county_fips", "year")) %>%
  mutate(incumbent = if_else(is.na(incumbent), FALSE, incumbent))

message("Incumbent rate (MA plan-county-years where present in t-1): ",
        sprintf("%.1f%%", mean(ma$incumbent) * 100))

# ---------------------------------------------------------------------------
# Parent-insurer market share within county-year
# ---------------------------------------------------------------------------

county_totals <- ma %>%
  group_by(county_fips, year) %>%
  summarize(county_enrollment = sum(avg_enrollment), .groups = "drop")

insurer_enrollment <- ma %>%
  group_by(county_fips, year, parent_org) %>%
  summarize(insurer_enrollment = sum(avg_enrollment), .groups = "drop") %>%
  left_join(county_totals, by = c("county_fips", "year")) %>%
  mutate(insurer_share = insurer_enrollment / county_enrollment) %>%
  select(county_fips, year, parent_org, insurer_share)

ma <- ma %>%
  left_join(insurer_enrollment, by = c("county_fips", "year", "parent_org"))

message("Insurer share summary (parent_org x county-year):")
ma %>%
  distinct(county_fips, year, parent_org, insurer_share) %>%
  summarize(
    mean   = mean(insurer_share),
    median = median(insurer_share),
    p90    = quantile(insurer_share, 0.90),
    p99    = quantile(insurer_share, 0.99)
  ) %>%
  print()

# ---------------------------------------------------------------------------
# Trim MA to the structural-panel column set
# ---------------------------------------------------------------------------

ma_panel <- ma %>%
  transmute(
    county_fips,
    year,
    plan_id,
    plan_kind      = "MA",
    plan_category,
    has_partd,
    parent_org,
    Star_Rating    = suppressWarnings(as.numeric(Star_Rating)),
    mean_cost,
    var_cost,
    sd_cost,
    avg_enrollment,
    incumbent,
    insurer_share,
    dominated
  )

message("MA panel rows: ", nrow(ma_panel))

# ---------------------------------------------------------------------------
# FFS rows
# ---------------------------------------------------------------------------

ffs <- read_csv("data/output/ffs_outside.csv", show_col_types = FALSE)

county_years <- ma_panel %>% distinct(county_fips, year)

ffs_panel <- county_years %>%
  inner_join(ffs, by = "year", relationship = "many-to-many") %>%
  transmute(
    county_fips,
    year,
    plan_id        = paste0("FFS_", variant),
    plan_kind      = "FFS",
    plan_category  = "FFS",
    has_partd      = TRUE,
    parent_org     = "FFS",
    Star_Rating    = NA_real_,
    mean_cost,
    var_cost,
    sd_cost,
    avg_enrollment = NA_real_,
    incumbent      = TRUE,
    insurer_share  = NA_real_,
    dominated      = NA
  )

message("FFS panel rows: ", nrow(ffs_panel),
        "  (", nrow(county_years), " county-years x 2 variants)")

# ---------------------------------------------------------------------------
# Join penetration (county-year total Medicare population)
# ---------------------------------------------------------------------------

penetration <- read_csv(
  "data/output/penetration.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)

cbp <- read_csv(
  "data/output/cbp_broker.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)

panel <- bind_rows(ma_panel, ffs_panel) %>%
  left_join(penetration, by = c("county_fips", "year")) %>%
  left_join(cbp, by = c("county_fips", "year")) %>%
  mutate(
    ins_brokers_estab = if_else(is.na(ins_brokers_estab), 0L, ins_brokers_estab),
    ins_brokers_emp   = if_else(is.na(ins_brokers_emp),   0L, ins_brokers_emp)
  ) %>%
  arrange(county_fips, year, plan_kind, plan_id)

unmatched_pen <- panel %>%
  filter(is.na(total_eligibles)) %>%
  distinct(county_fips, year)
message("County-years missing penetration data: ", nrow(unmatched_pen))

unmatched_cbp <- panel %>%
  filter(ins_brokers_estab == 0L) %>%
  distinct(county_fips, year)
message("County-years with zero broker establishments (CBP): ",
        nrow(unmatched_cbp))

# ---------------------------------------------------------------------------
# Diagnostics and write
# ---------------------------------------------------------------------------

message("\n========== Structural panel ==========")
message("Total rows: ", nrow(panel))
message("County-years: ", nrow(county_years))
message("Years: ", paste(sort(unique(panel$year)), collapse = ", "))

message("\nRow counts by plan_kind:")
panel %>% count(plan_kind) %>% print()

message("\nRow counts per county-year (MA only, distribution):")
panel %>%
  filter(plan_kind == "MA") %>%
  count(county_fips, year) %>%
  summarize(min = min(n), p25 = quantile(n, 0.25), median = median(n),
            mean = round(mean(n), 1), p75 = quantile(n, 0.75), max = max(n)) %>%
  print()

message("\nMA share of eligibles (county-year, MA enrollment / total_eligibles):")
panel %>%
  filter(plan_kind == "MA") %>%
  group_by(county_fips, year) %>%
  summarize(ma_enroll = sum(avg_enrollment, na.rm = TRUE),
            total = first(total_eligibles), .groups = "drop") %>%
  filter(!is.na(total), total > 0) %>%
  mutate(ma_share = ma_enroll / total) %>%
  summarize(
    median = round(median(ma_share), 3),
    mean   = round(mean(ma_share), 3),
    p90    = round(quantile(ma_share, 0.9), 3),
    over_1 = sum(ma_share > 1)
  ) %>%
  print()

write_csv(panel, "data/output/structural_panel.csv")
message("\nWrote data/output/structural_panel.csv")
