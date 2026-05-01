# 3-merge-plan-county.R — Merge enrollment spine with plan benefits
# Creates plan-county-year panel with enrollment, characteristics, and benefits
#
# Input:  data/output/enrollment.csv (from script 1)
#         data/output/plan_benefits.csv (from script 2)
# Output: data/output/plan_county_benefits.csv

# ---------------------------------------------------------------------------
# 1. Read enrollment spine
# ---------------------------------------------------------------------------

enrollment <- read_csv("data/output/enrollment.csv", show_col_types = FALSE)
message("Enrollment spine: ", nrow(enrollment), " rows")

# ---------------------------------------------------------------------------
# 2. Read plan benefits
# ---------------------------------------------------------------------------

benefits <- read_csv("data/output/plan_benefits.csv", show_col_types = FALSE)
message("Benefits panel: ", nrow(benefits), " rows")

# Dedup to contract-plan-year: take segment 0 (base benefit) when multiple segments exist
benefits_dedup <- benefits %>%
  arrange(contractid, planid, year, segment_id) %>%
  group_by(contractid, planid, year) %>%
  slice(1) %>%
  ungroup() %>%
  select(-segment_id)

message("After dedup to contract-plan-year: ", nrow(benefits_dedup), " rows")

# ---------------------------------------------------------------------------
# 3. Merge
# ---------------------------------------------------------------------------

merged <- enrollment %>%
  left_join(benefits_dedup, by = c("contractid", "planid", "year"))

# Verify no row expansion
if (nrow(merged) != nrow(enrollment)) {
  warning("Row count changed after merge! Enrollment: ", nrow(enrollment),
          " -> Merged: ", nrow(merged))
} else {
  message("\nMerge complete: ", nrow(merged), " rows (no expansion)")
}

# ---------------------------------------------------------------------------
# 4. Validation
# ---------------------------------------------------------------------------

message("\n========== Validation ==========")

message("\nBenefits match rate by year:")
merged %>%
  mutate(has_benefits = !is.na(premium) | !is.na(deductible) | !is.na(er_copay_min)) %>%
  group_by(year) %>%
  summarize(n = n(), matched = sum(has_benefits),
            pct = round(matched / n * 100, 1)) %>%
  print(n = 20)

message("\nMissing rates for key variables:")
merged %>%
  summarize(
    across(c(avg_enrollment, premium, deductible, moop,
             er_copay_min, pcp_copay_min, specialist_copay_min,
             drug_deductible, Star_Rating),
           ~ mean(is.na(.x)) * 100, .names = "pct_na_{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  mutate(pct_na = round(pct_na, 1)) %>%
  print(n = 20)

# ---------------------------------------------------------------------------
# 5. Write output
# ---------------------------------------------------------------------------

write_csv(merged, "data/output/plan_county_benefits.csv")
message("\nWrote data/output/plan_county_benefits.csv")
message("Columns: ", ncol(merged))
message("Rows: ", nrow(merged))
