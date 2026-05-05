# 14-build-prominence-vars.R — Prominence-ordering inputs for η_ij
#
# Adds plan-level prominence components to the structural panel for use in
# the consideration-set / awareness ordering. None of these reference the
# focal county-year MA share (insurer_share is the endogenous version, kept
# in the panel only as a robustness comparator).
#
# Columns added to structural_panel.csv (MA plans only; FFS rows get NA):
#
#   pf_rank                 — Plan Finder TEAC-ascending rank within
#                              (county_fips, year). Lowest mean_cost = rank 1.
#   pf_rank_norm            — pf_rank / K_ct (rank position as fraction of
#                              market size; 0 = top, 1 = bottom).
#   pf_rank_score           — exp(-KAPPA * pf_rank_norm); use as the s^PF_ij
#                              prominence component in the structural model.
#   parent_org_loo_national — parent_org's share of national MA enrollment
#                              EXCLUDING focal county-year. Captures national
#                              footprint / brand recognition, exogenous to
#                              focal-county choice.
#   parent_org_loo_state    — same but within-state, excluding focal county.
#                              Captures state-level distribution / regulatory
#                              presence / broker pipeline maturation.
#   plan_tenure_national    — years contract+pbp has appeared in any market,
#                              rolling count from first observed year.
#   plan_tenure_county      — years plan has been in this specific county.
#
# Input:  data/output/structural_panel.csv (from script 13)
# Output: data/output/structural_panel.csv (in place; columns added)

KAPPA <- 1.0   # decay parameter for pf_rank_score; sensitivity-test downstream

panel <- read_csv(
  "data/output/structural_panel.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)
message("Loaded structural panel: ", nrow(panel), " rows")

ma <- panel %>% filter(plan_kind == "MA")
ffs <- panel %>% filter(plan_kind == "FFS")
message("MA rows: ", nrow(ma), "  FFS rows: ", nrow(ffs))


# ---------------------------------------------------------------------------
# 1. Plan Finder rank — TEAC-ascending within (county_fips, year)
# ---------------------------------------------------------------------------

ma <- ma %>%
  group_by(county_fips, year) %>%
  mutate(
    pf_rank      = rank(mean_cost, ties.method = "average"),
    K_ct         = n(),
    pf_rank_norm = (pf_rank - 1) / pmax(K_ct - 1, 1),
    pf_rank_score = exp(-KAPPA * pf_rank_norm)
  ) %>%
  ungroup()

message("\npf_rank distribution (MA plan-county-years):")
ma %>%
  summarize(
    K_ct_mean   = mean(K_ct),
    K_ct_median = median(K_ct),
    K_ct_max    = max(K_ct),
    rank_score_mean = mean(pf_rank_score),
    rank_score_p50  = median(pf_rank_score)
  ) %>% print()


# ---------------------------------------------------------------------------
# 2. Parent-org leave-one-out national share
# ---------------------------------------------------------------------------
# parent_org_loo_national_jct =
#     [sum over j' in same parent_org, all counties != c, year t of avg_enrollment_j'c't]
#   / [sum over all parent_org and counties != c, year t of avg_enrollment]
#
# Captures the parent insurer's national presence outside the focal county,
# which proxies advertising / broker pipeline / brand recognition without
# any mechanical dependence on focal-county shares.

parent_year_total <- ma %>%
  filter(!is.na(avg_enrollment)) %>%
  group_by(parent_org, year) %>%
  summarize(parent_year_enroll = sum(avg_enrollment), .groups = "drop")

national_year_total <- ma %>%
  filter(!is.na(avg_enrollment)) %>%
  group_by(year) %>%
  summarize(national_year_enroll = sum(avg_enrollment), .groups = "drop")

county_parent_year <- ma %>%
  filter(!is.na(avg_enrollment)) %>%
  group_by(county_fips, year, parent_org) %>%
  summarize(this_county_parent_enroll = sum(avg_enrollment), .groups = "drop")

county_year_total <- ma %>%
  filter(!is.na(avg_enrollment)) %>%
  group_by(county_fips, year) %>%
  summarize(this_county_total_enroll = sum(avg_enrollment), .groups = "drop")

ma <- ma %>%
  left_join(parent_year_total, by = c("parent_org", "year")) %>%
  left_join(national_year_total, by = "year") %>%
  left_join(county_parent_year, by = c("county_fips", "year", "parent_org")) %>%
  left_join(county_year_total, by = c("county_fips", "year")) %>%
  mutate(
    parent_org_loo_national =
      (parent_year_enroll - replace_na(this_county_parent_enroll, 0)) /
      pmax(national_year_enroll - replace_na(this_county_total_enroll, 0), 1)
  )


# ---------------------------------------------------------------------------
# 3. Parent-org leave-one-out within-state share
# ---------------------------------------------------------------------------

ma <- ma %>%
  mutate(state_fips = substr(county_fips, 1, 2))

parent_state_year_total <- ma %>%
  filter(!is.na(avg_enrollment)) %>%
  group_by(state_fips, year, parent_org) %>%
  summarize(parent_state_enroll = sum(avg_enrollment), .groups = "drop")

state_year_total <- ma %>%
  filter(!is.na(avg_enrollment)) %>%
  group_by(state_fips, year) %>%
  summarize(state_year_enroll = sum(avg_enrollment), .groups = "drop")

ma <- ma %>%
  left_join(parent_state_year_total, by = c("state_fips", "year", "parent_org")) %>%
  left_join(state_year_total, by = c("state_fips", "year")) %>%
  mutate(
    parent_org_loo_state =
      (parent_state_enroll - replace_na(this_county_parent_enroll, 0)) /
      pmax(state_year_enroll - replace_na(this_county_total_enroll, 0), 1)
  )


# ---------------------------------------------------------------------------
# 4. Plan tenure — years plan has been offered (cumulative)
# ---------------------------------------------------------------------------

plan_first_year_national <- ma %>%
  group_by(plan_id) %>%
  summarize(first_year_national = min(year), .groups = "drop")

plan_first_year_county <- ma %>%
  group_by(plan_id, county_fips) %>%
  summarize(first_year_county = min(year), .groups = "drop")

ma <- ma %>%
  left_join(plan_first_year_national, by = "plan_id") %>%
  left_join(plan_first_year_county, by = c("plan_id", "county_fips")) %>%
  mutate(
    plan_tenure_national = year - first_year_national + 1L,
    plan_tenure_county   = year - first_year_county   + 1L
  )

message("\nPlan tenure (national):")
ma %>%
  summarize(
    mean   = mean(plan_tenure_national),
    median = median(plan_tenure_national),
    p90    = quantile(plan_tenure_national, 0.9),
    max    = max(plan_tenure_national)
  ) %>% print()


# ---------------------------------------------------------------------------
# 5. Drop intermediates, recombine with FFS, write back
# ---------------------------------------------------------------------------

ma_out <- ma %>%
  select(
    -K_ct,
    -parent_year_enroll, -national_year_enroll,
    -this_county_parent_enroll, -this_county_total_enroll,
    -parent_state_enroll, -state_year_enroll,
    -first_year_national, -first_year_county,
    -state_fips
  )

# FFS rows: fill new prominence columns with NA so column counts match
ffs_out <- ffs %>%
  mutate(
    pf_rank                 = NA_real_,
    pf_rank_norm            = NA_real_,
    pf_rank_score           = NA_real_,
    parent_org_loo_national = NA_real_,
    parent_org_loo_state    = NA_real_,
    plan_tenure_national    = NA_integer_,
    plan_tenure_county      = NA_integer_
  )

panel_out <- bind_rows(ma_out, ffs_out) %>%
  arrange(county_fips, year, plan_kind, plan_id)

stopifnot(nrow(panel_out) == nrow(panel))

message("\nFinal column count: ", ncol(panel_out))
message("Distribution of pf_rank_score among MA plans:")
panel_out %>%
  filter(plan_kind == "MA", !is.na(pf_rank_score)) %>%
  summarize(
    mean   = mean(pf_rank_score),
    median = median(pf_rank_score),
    p10    = quantile(pf_rank_score, 0.1),
    p90    = quantile(pf_rank_score, 0.9)
  ) %>% print()

message("\nDistribution of parent_org_loo_national:")
panel_out %>%
  filter(plan_kind == "MA", !is.na(parent_org_loo_national)) %>%
  summarize(
    mean   = mean(parent_org_loo_national),
    median = median(parent_org_loo_national),
    p10    = quantile(parent_org_loo_national, 0.1),
    p90    = quantile(parent_org_loo_national, 0.9)
  ) %>% print()

write_csv(panel_out, "data/output/structural_panel.csv")
message("\nWrote data/output/structural_panel.csv with prominence columns")
