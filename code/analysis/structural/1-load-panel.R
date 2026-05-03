# 1-load-panel.R — Load structural panel + analysis panel, build market list
#
# Loads two CSVs and stitches them together:
#   data/output/structural_panel.csv  (plan-county-year, GMM input)
#   data/output/analysis_panel.csv    (county-year, demographics + Bartik
#                                      instruments, used for RF moments)
#
# Outputs in globalenv():
#   panel    — tidy plan-county-year rows. FFS_bare/FFS_supp collapsed into
#              one FFS row per county-year via omega_bare = 0.25. Each row
#              carries the county-year demographics needed by the structural
#              search-cost equation c_m = exp(gamma0 + gamma1' X_m).
#   markets  — list keyed by market_id, each element a tibble of plan rows.
#   cy_panel — county-year analysis panel (for RF moment construction).
#
# Markets without analysis-panel coverage are dropped (no demographics →
# c_m undefined under heterogeneity).

panel_raw <- read_csv(
  "data/output/structural_panel.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)

cy_panel <- read_csv(
  "data/output/analysis_panel.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
) %>%
  mutate(
    log_n_plans       = log(n_plans),
    log_pop           = log(total_pop),
    log_inc           = log(median_hh_income),
    methodology_shift = agg_val - agg_val_stable
  ) %>%
  filter(!is.na(log_inc), !is.na(pct_65plus),
         !is.na(pct_bachelors_p), !is.na(log_pop),
         !is.na(bartik_pffs), !is.na(total_enrollment), total_enrollment > 0)

message("Raw structural panel: ", nrow(panel_raw))
message("Analysis panel (county-year): ", nrow(cy_panel))

omega_bare <- 0.25

ffs_wide <- panel_raw %>%
  filter(plan_kind == "FFS") %>%
  select(county_fips, year, plan_id, mean_cost, var_cost,
         total_eligibles, ins_brokers_estab, ins_brokers_emp) %>%
  pivot_wider(
    id_cols     = c(county_fips, year, total_eligibles,
                    ins_brokers_estab, ins_brokers_emp),
    names_from  = plan_id,
    values_from = c(mean_cost, var_cost)
  )

ffs_collapsed <- ffs_wide %>%
  transmute(
    county_fips, year,
    plan_id        = "FFS",
    plan_kind      = "FFS",
    plan_category  = "FFS",
    has_partd      = TRUE,
    parent_org     = "FFS",
    Star_Rating    = NA_real_,
    mean_cost      = omega_bare * mean_cost_FFS_bare +
                     (1 - omega_bare) * mean_cost_FFS_supp,
    var_cost       = omega_bare * var_cost_FFS_bare +
                     (1 - omega_bare) * var_cost_FFS_supp,
    sd_cost        = sqrt(var_cost),
    avg_enrollment = NA_real_,
    incumbent      = TRUE,
    insurer_share  = NA_real_,
    dominated      = NA,
    total_eligibles, ins_brokers_estab, ins_brokers_emp
  )

ma <- panel_raw %>% filter(plan_kind == "MA")

panel <- bind_rows(ma, ffs_collapsed) %>%
  filter(!is.na(total_eligibles), total_eligibles > 0) %>%
  inner_join(
    cy_panel %>%
      select(county_fips, year, log_inc, pct_65plus,
             pct_bachelors_p, log_pop) %>%
      filter(!is.na(log_inc), !is.na(pct_65plus),
             !is.na(pct_bachelors_p), !is.na(log_pop)),
    by = c("county_fips", "year")
  )

message("Panel rows after demographic join: ", nrow(panel))

ma_enroll <- panel %>%
  filter(plan_kind == "MA") %>%
  group_by(county_fips, year) %>%
  summarize(ma_enroll = sum(avg_enrollment, na.rm = TRUE), .groups = "drop")

panel <- panel %>%
  left_join(ma_enroll, by = c("county_fips", "year")) %>%
  mutate(
    ma_enroll      = if_else(is.na(ma_enroll), 0, ma_enroll),
    outside_enroll = pmax(total_eligibles - ma_enroll, 0),
    observed_share = case_when(
      plan_kind == "FFS" ~ outside_enroll / total_eligibles,
      plan_kind == "MA"  ~ avg_enrollment / total_eligibles,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(observed_share))

panel <- panel %>%
  arrange(county_fips, year, plan_kind != "FFS", plan_id) %>%
  group_by(county_fips, year) %>%
  mutate(market_id = cur_group_id()) %>%
  ungroup()

n_markets <- max(panel$market_id)
message("Markets (after demographic filter): ", n_markets)
message("Panel rows: ", nrow(panel))
message("MA rows: ", sum(panel$plan_kind == "MA"))
message("FFS rows: ", sum(panel$plan_kind == "FFS"))

message("\nDemographic summary across markets:")
panel %>%
  distinct(market_id, log_inc, pct_65plus, pct_bachelors_p) %>%
  summarize(
    log_inc_mean = round(mean(log_inc), 2),
    pct_65_mean  = round(mean(pct_65plus), 3),
    pct_bach_mean = round(mean(pct_bachelors_p), 3)
  ) %>%
  print()

markets <- panel %>%
  group_by(market_id) %>%
  group_split() %>%
  set_names(seq_len(n_markets))

# Precomputed indices for fast county-year aggregation in 3b-rf-moments.R
PANEL_MA_IDX     <- which(panel$plan_kind == "MA")
PANEL_MA_DOM_IDX <- which(panel$plan_kind == "MA" &
                          !is.na(panel$dominated) & panel$dominated)
MA_MARKET_ID     <- panel$market_id[PANEL_MA_IDX]
MA_DOM_MARKET_ID <- panel$market_id[PANEL_MA_DOM_IDX]
N_MARKETS        <- n_markets
