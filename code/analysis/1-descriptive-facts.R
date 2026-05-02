# 1-descriptive-facts.R — Descriptive analysis of plan dominance and market structure
#
# Produces tables and figures documenting:
#   1. Market structure (plans per county, enrollment, plan types)
#   2. Dominance prevalence over time and across plan types
#   3. Choice set complexity and its relationship to dominated enrollment
#   4. Characteristics of dominated vs non-dominated plans
#
# Input:  data/output/dominance_plan.csv
#         data/output/dominance_county.csv
# Output: results/tables/ and results/figures/

# ---------------------------------------------------------------------------
# Read data
# ---------------------------------------------------------------------------

plan_df <- read_csv(
  "data/output/dominance_plan.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)
county_df <- read_csv(
  "data/output/dominance_county.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)

# Focus on plans with dominance classification
plan_df <- plan_df %>%
  filter(!is.na(dominated))

message("Plan-level data: ", nrow(plan_df), " observations")
message("County-year data: ", nrow(county_df), " observations")
message("Years: ", paste(sort(unique(plan_df$year)), collapse = ", "))

# =========================================================================
# 1. Market Structure
# =========================================================================

message("\n========== 1. Market Structure ==========")

# Plans per county-year
market_structure <- plan_df %>%
  group_by(county_fips, year, state, county_name) %>%
  summarize(
    n_plans = n(),
    n_hmo = sum(plan_category == "HMO"),
    n_ppo = sum(plan_category == "PPO"),
    n_pffs = sum(plan_category == "PFFS"),
    n_partd = sum(has_partd),
    total_enrollment = sum(avg_enrollment, na.rm = TRUE),
    .groups = "drop"
  )

message("\nPlans per county-year:")
market_structure %>%
  summarize(min = min(n_plans), p10 = quantile(n_plans, 0.1),
            p25 = quantile(n_plans, 0.25), median = median(n_plans),
            mean = round(mean(n_plans), 1),
            p75 = quantile(n_plans, 0.75), p90 = quantile(n_plans, 0.9),
            max = max(n_plans)) %>%
  print()

# Plans per county over time
plans_by_year <- market_structure %>%
  group_by(year) %>%
  summarize(
    mean_plans = round(mean(n_plans), 1),
    median_plans = median(n_plans),
    mean_enrollment = round(mean(total_enrollment), 0),
    n_counties = n()
  )

message("\nPlans per county by year:")
plans_by_year %>% print(n = 20)

# Figure: Mean plans per county over time
fig_plans_time <- ggplot(plans_by_year, aes(x = year, y = mean_plans)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Mean Plans per County",
       title = "Average Number of MA Plans per County") +
  theme_minimal() +
  scale_x_continuous(breaks = 2008:2018)

ggsave("results/figures/plans_per_county_time.png", fig_plans_time,
       width = 8, height = 5, dpi = 300)

# =========================================================================
# 2. Dominance Prevalence
# =========================================================================

message("\n========== 2. Dominance Prevalence ==========")

# Overall dominance rates by year
dom_by_year <- plan_df %>%
  group_by(year) %>%
  summarize(
    n_plans = n(),
    n_dominated = sum(dominated),
    pct_plans_dominated = round(n_dominated / n_plans * 100, 1),
    total_enrollment = sum(avg_enrollment, na.rm = TRUE),
    enrollment_dominated = sum(avg_enrollment[dominated], na.rm = TRUE),
    pct_enrollment_dominated = round(enrollment_dominated / total_enrollment * 100, 1),
    # Also report cost-curve dominance
    n_curve_dom = sum(dom_curve, na.rm = TRUE),
    pct_curve_dom = round(n_curve_dom / n_plans * 100, 1)
  )

message("\nDominance rates by year:")
dom_by_year %>%
  select(year, n_plans, pct_plans_dominated, pct_enrollment_dominated, pct_curve_dom) %>%
  print(n = 20)

# Figure: Dominance rates over time (both measures)
fig_dom_time <- dom_by_year %>%
  select(year, `Mean-Variance` = pct_enrollment_dominated,
         `Cost-Curve` = pct_curve_dom) %>%
  pivot_longer(-year, names_to = "measure", values_to = "pct") %>%
  ggplot(aes(x = year, y = pct, color = measure, linetype = measure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Percent of Enrollment in Dominated Plans",
       title = "Share of MA Enrollment in Dominated Plans",
       color = "Dominance Measure", linetype = "Dominance Measure") +
  theme_minimal() +
  scale_x_continuous(breaks = 2008:2018) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom")

ggsave("results/figures/dominance_over_time.png", fig_dom_time,
       width = 8, height = 5, dpi = 300)

# Dominance by plan type
dom_by_type <- plan_df %>%
  group_by(plan_category, has_partd) %>%
  summarize(
    n = n(),
    pct_dominated = round(mean(dominated) * 100, 1),
    pct_curve_dom = round(mean(dom_curve, na.rm = TRUE) * 100, 1),
    mean_enrollment = round(mean(avg_enrollment, na.rm = TRUE), 0),
    .groups = "drop"
  )

message("\nDominance by plan type:")
dom_by_type %>% print(n = 10)

# =========================================================================
# 3. Characteristics of Dominated vs Non-Dominated Plans
# =========================================================================

message("\n========== 3. Plan Characteristics ==========")

plan_chars_not_dom <- plan_df %>%
  filter(!dominated) %>%
  summarize(
    n = n(),
    mean_premium = round(mean(premium, na.rm = TRUE), 1),
    mean_deductible = round(mean(deductible, na.rm = TRUE), 1),
    mean_moop = round(mean(moop, na.rm = TRUE), 0),
    mean_pcp_copay = round(mean(pcp_copay_min, na.rm = TRUE), 1),
    mean_specialist_copay = round(mean(specialist_copay_min, na.rm = TRUE), 1),
    mean_er_copay = round(mean(er_copay_min, na.rm = TRUE), 1),
    mean_ip_copay = round(mean(inpatient_copay, na.rm = TRUE), 0),
    mean_drug_ded = round(mean(drug_deductible, na.rm = TRUE), 1),
    mean_enrollment = round(mean(avg_enrollment, na.rm = TRUE), 0),
    mean_star = round(mean(Star_Rating, na.rm = TRUE), 2),
    mean_cost = round(mean(mean_cost, na.rm = TRUE), 0),
    mean_sd_cost = round(mean(sd_cost, na.rm = TRUE), 0)
  ) %>%
  mutate(group = "Not Dominated")

plan_chars_dom <- plan_df %>%
  filter(dominated) %>%
  summarize(
    n = n(),
    mean_premium = round(mean(premium, na.rm = TRUE), 1),
    mean_deductible = round(mean(deductible, na.rm = TRUE), 1),
    mean_moop = round(mean(moop, na.rm = TRUE), 0),
    mean_pcp_copay = round(mean(pcp_copay_min, na.rm = TRUE), 1),
    mean_specialist_copay = round(mean(specialist_copay_min, na.rm = TRUE), 1),
    mean_er_copay = round(mean(er_copay_min, na.rm = TRUE), 1),
    mean_ip_copay = round(mean(inpatient_copay, na.rm = TRUE), 0),
    mean_drug_ded = round(mean(drug_deductible, na.rm = TRUE), 1),
    mean_enrollment = round(mean(avg_enrollment, na.rm = TRUE), 0),
    mean_star = round(mean(Star_Rating, na.rm = TRUE), 2),
    mean_cost = round(mean(mean_cost, na.rm = TRUE), 0),
    mean_sd_cost = round(mean(sd_cost, na.rm = TRUE), 0)
  ) %>%
  mutate(group = "Dominated")

message("\nDominated vs Non-Dominated plan characteristics:")
bind_rows(
  plan_chars_not_dom %>% pivot_longer(-c(group, n), names_to = "variable", values_to = "value"),
  plan_chars_dom %>% pivot_longer(-c(group, n), names_to = "variable", values_to = "value")
) %>%
  select(-n) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(diff = Dominated - `Not Dominated`) %>%
  print(n = 20)

# =========================================================================
# 4. Choice Set Complexity and Dominance
# =========================================================================

message("\n========== 4. Choice Complexity ==========")

# county_df already has n_plans from script 4
county_analysis <- county_df

# Bin counties by number of plans
county_analysis <- county_analysis %>%
  mutate(plan_bins = cut(n_plans,
                         breaks = c(0, 3, 5, 10, 15, 20, 30, Inf),
                         labels = c("1-3", "4-5", "6-10", "11-15",
                                    "16-20", "21-30", "31+"),
                         right = TRUE))

# Dominance by choice set size
dom_by_complexity <- county_analysis %>%
  group_by(plan_bins) %>%
  summarize(
    n_county_years = n(),
    mean_plans = round(mean(n_plans), 1),
    mean_pct_dominated = round(mean(pct_enrollment_dominated, na.rm = TRUE), 1),
    median_pct_dominated = round(median(pct_enrollment_dominated, na.rm = TRUE), 1),
    .groups = "drop"
  )

message("\nDominance by choice set size:")
dom_by_complexity %>% print(n = 10)

# Figure: Dominance vs choice set size
fig_complexity <- county_analysis %>%
  filter(!is.na(plan_bins)) %>%
  ggplot(aes(x = plan_bins, y = pct_enrollment_dominated)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  labs(x = "Number of Plans in County",
       y = "Percent of Enrollment in Dominated Plans",
       title = "Enrollment in Dominated Plans by Choice Set Size") +
  theme_minimal()

ggsave("results/figures/dominance_by_complexity.png", fig_complexity,
       width = 8, height = 5, dpi = 300)

# Scatter: n_plans vs pct dominated (county-year level)
fig_scatter <- county_analysis %>%
  filter(total_enrollment > 100) %>%
  ggplot(aes(x = n_plans, y = pct_enrollment_dominated)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "firebrick") +
  labs(x = "Number of Plans in County",
       y = "Percent of Enrollment in Dominated Plans",
       title = "More Plans, More Dominated Enrollment?") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 50))

ggsave("results/figures/dominance_vs_nplans_scatter.png", fig_scatter,
       width = 8, height = 5, dpi = 300)

# =========================================================================
# 5. Geographic Variation
# =========================================================================

message("\n========== 5. Geographic Variation ==========")

# State-level averages (pooling all years)
state_summary <- county_analysis %>%
  group_by(state) %>%
  summarize(
    n_county_years = n(),
    mean_plans = round(mean(n_plans), 1),
    mean_pct_dominated = round(mean(pct_enrollment_dominated, na.rm = TRUE), 1),
    mean_enrollment = round(mean(total_enrollment, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_pct_dominated))

message("\nTop 10 states by dominated enrollment share:")
state_summary %>% head(10) %>% print()

message("\nBottom 10 states:")
state_summary %>% tail(10) %>% print()

# =========================================================================
# 6. Summary Table for Paper
# =========================================================================

message("\n========== 6. Summary Statistics ==========")

# Panel A: Market structure by year
panel_a <- plans_by_year %>%
  select(year, mean_plans, median_plans, n_counties)

# Panel B: Dominance by year
panel_b <- dom_by_year %>%
  select(year, n_plans, pct_plans_dominated, pct_enrollment_dominated)

summary_table <- panel_a %>%
  left_join(panel_b, by = "year")

message("\nSummary table:")
summary_table %>% print(n = 20)

write_csv(summary_table, "results/tables/summary_stats.csv")
message("Wrote results/tables/summary_stats.csv")

message("\n========== Descriptive analysis complete ==========")
