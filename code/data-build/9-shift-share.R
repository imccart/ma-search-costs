# 9-shift-share.R — Shift-share (Bartik) instruments for MA plan count
#
# Constructs a county-year shift-share IV for log(n_plans) by interacting
# baseline (2008) county-level plan-type composition with national plan-type
# growth rates. The headline instrument exploits the MIPPA 2008 PFFS network
# requirement, which forced PFFS plans to develop networks starting in 2011;
# most exited or converted between 2010 and 2014. Counties with higher 2008
# PFFS dependence saw larger plan-count drops after the rule bound.
#
# Bartik:   z_{c,t} = sum_k  s_{c,k,2008}  *  g_{k,t}
# where:    s_{c,k,2008} = share of plans in county c that were type k in 2008
#           g_{k,t}      = log(national_plans_k,t / national_plans_k,2008)
#
# We construct three separate shift-share variables (PFFS, HMO, PPO) so the
# PFFS-only design can be reported as the headline and the insurer-level
# generalization (task 2) can swap in parent-organization shares.
#
# Identification (Goldsmith-Pinkham, Sorkin, Swift 2020): validity follows
# from exogeneity of the 2008 baseline shares — i.e., 2008 county-level plan
# composition predetermined relative to 2010+ search-cost dynamics. Plausible
# but not airtight; a 2009 baseline could be considered for robustness.
#
# Input:  data/output/enrollment.csv (plan-county-year panel from script 1)
# Output: data/output/shift_share.csv (county_fips x year)
#         data/output/analysis_panel.csv (augmented in place with bartik cols)

# ---------------------------------------------------------------------------
# Read enrollment, restrict to standard MA plans
# ---------------------------------------------------------------------------

enr <- read_csv(
  "data/output/enrollment.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
) %>%
  filter(plan_type %in% c("HMO/HMOPOS", "Local PPO", "Regional PPO", "PFFS"),
         snp == "No", eghp == "No") %>%
  mutate(plan_category = case_when(
    plan_type == "HMO/HMOPOS"               ~ "HMO",
    plan_type %in% c("Local PPO", "Regional PPO") ~ "PPO",
    plan_type == "PFFS"                     ~ "PFFS"
  ))

# ---------------------------------------------------------------------------
# County x year x plan_category plan counts
# ---------------------------------------------------------------------------

cy_counts <- enr %>%
  count(county_fips, year, plan_category, name = "n_plans_k") %>%
  complete(county_fips, year, plan_category, fill = list(n_plans_k = 0))

# ---------------------------------------------------------------------------
# Baseline (2008) county-level shares by plan_category
# ---------------------------------------------------------------------------

base_shares <- cy_counts %>%
  filter(year == 2008) %>%
  group_by(county_fips) %>%
  mutate(total_2008 = sum(n_plans_k),
         share_2008 = if_else(total_2008 > 0, n_plans_k / total_2008, 0)) %>%
  ungroup() %>%
  select(county_fips, plan_category, share_2008)

message("Baseline (2008) shares: ",
        n_distinct(base_shares$county_fips), " counties")

# ---------------------------------------------------------------------------
# National plan-type growth (vs 2008 baseline)
# ---------------------------------------------------------------------------

natl <- enr %>%
  count(year, plan_category, name = "national_n") %>%
  group_by(plan_category) %>%
  mutate(g = log(national_n / national_n[year == 2008])) %>%
  ungroup() %>%
  select(year, plan_category, g)

message("\nNational growth (log) vs 2008 baseline:")
natl %>%
  pivot_wider(names_from = plan_category, values_from = g) %>%
  mutate(across(c(HMO, PPO, PFFS), ~ round(.x, 2))) %>%
  print(n = Inf)

# ---------------------------------------------------------------------------
# Build shift-share by plan_category, then sum to total Bartik
# ---------------------------------------------------------------------------

shift <- base_shares %>%
  crossing(year = unique(natl$year)) %>%
  left_join(natl, by = c("plan_category", "year")) %>%
  mutate(z_k = share_2008 * g) %>%
  pivot_wider(
    id_cols     = c(county_fips, year),
    names_from  = plan_category,
    values_from = c(share_2008, z_k),
    names_glue  = "{.value}_{plan_category}"
  ) %>%
  mutate(
    bartik_total = z_k_HMO + z_k_PPO + z_k_PFFS  # sum-of-types Bartik
  ) %>%
  rename(
    bartik_pffs    = z_k_PFFS,
    bartik_hmo     = z_k_HMO,
    bartik_ppo     = z_k_PPO,
    pffs_share2008 = share_2008_PFFS,
    hmo_share2008  = share_2008_HMO,
    ppo_share2008  = share_2008_PPO
  )

message("\nShift-share panel: ", nrow(shift), " county-years")

# ---------------------------------------------------------------------------
# Insurer-level (parent_org) shift-share — generalization of plan-type version
# ---------------------------------------------------------------------------
#
# Bartik:  z^I_{c,t} = sum_j  s_{c,j,2008}  *  (national_j,t / national_j,2008 - 1)
#
# Level shocks (rather than log) because individual parent firms can exit
# entirely (n_t = 0 → log undefined), whereas plan TYPES never go to zero in
# the panel. Level form bounded below by -1 (full exit) and unbounded above.

ins_cy <- enr %>%
  filter(!is.na(parent_org)) %>%
  count(county_fips, year, parent_org, name = "n_plans_j")

# Baseline 2008 county shares by parent
ins_share2008 <- ins_cy %>%
  filter(year == 2008) %>%
  group_by(county_fips) %>%
  mutate(total_2008 = sum(n_plans_j),
         share2008  = if_else(total_2008 > 0, n_plans_j / total_2008, 0)) %>%
  ungroup() %>%
  select(county_fips, parent_org, share2008)

# National parent x year plan counts; growth as level change vs 2008
ins_natl <- ins_cy %>%
  group_by(parent_org, year) %>%
  summarise(natl_n = sum(n_plans_j), .groups = "drop") %>%
  group_by(parent_org) %>%
  mutate(
    natl_2008 = sum(natl_n * (year == 2008)),
    g_level   = if_else(natl_2008 > 0, natl_n / natl_2008 - 1, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(g_level)) %>%
  select(parent_org, year, g_level)

bartik_ins <- ins_share2008 %>%
  inner_join(ins_natl, by = "parent_org",
             relationship = "many-to-many") %>%
  mutate(z_j = share2008 * g_level) %>%
  group_by(county_fips, year) %>%
  summarise(bartik_insurer = sum(z_j), .groups = "drop")

message("\nbartik_insurer summary by year:")
bartik_ins %>%
  group_by(year) %>%
  summarise(mean_b = round(mean(bartik_insurer), 3),
            min_b  = round(min(bartik_insurer), 3),
            max_b  = round(max(bartik_insurer), 3),
            sd_b   = round(sd(bartik_insurer), 3)) %>%
  print(n = Inf)

shift <- shift %>% left_join(bartik_ins, by = c("county_fips", "year"))

message("\nbartik_pffs distribution by year (high-share counties get more")
message("negative bartik_pffs as PFFS contracts nationally):")
shift %>%
  group_by(year) %>%
  summarise(mean_pffs_share2008 = round(mean(pffs_share2008), 2),
            mean_bartik_pffs    = round(mean(bartik_pffs), 3),
            min_bartik_pffs     = round(min(bartik_pffs), 3)) %>%
  print(n = Inf)

write_csv(shift, "data/output/shift_share.csv")
message("\nWrote data/output/shift_share.csv")

# ---------------------------------------------------------------------------
# Augment analysis_panel.csv with bartik columns
# ---------------------------------------------------------------------------

if (file.exists("data/output/analysis_panel.csv")) {
  ap <- read_csv(
    "data/output/analysis_panel.csv", show_col_types = FALSE,
    col_types = cols(county_fips = col_character(), .default = col_guess())
  ) %>%
    select(-any_of(c("bartik_pffs", "bartik_hmo", "bartik_ppo",
                     "bartik_total", "bartik_insurer",
                     "pffs_share2008", "hmo_share2008", "ppo_share2008")))

  ap_aug <- ap %>%
    left_join(shift, by = c("county_fips", "year"))

  if (nrow(ap_aug) != nrow(ap)) {
    stop("Row count changed after shift-share join: ap=", nrow(ap),
         " aug=", nrow(ap_aug))
  }

  message("\nAugmenting analysis_panel.csv with bartik columns: ",
          nrow(ap_aug), " rows; ",
          sum(!is.na(ap_aug$bartik_pffs)), " with bartik values")

  write_csv(ap_aug, "data/output/analysis_panel.csv")
  message("Wrote data/output/analysis_panel.csv (with shift-share columns)")
} else {
  message("\nanalysis_panel.csv not found — run earlier scripts first.")
}
