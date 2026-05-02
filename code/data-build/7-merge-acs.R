# 7-merge-acs.R — Build county-year analysis panel
#
# Joins the dominance summary (dominance_county.csv) with ACS demographics
# (acs_county.csv) and the product-differentiation cluster measure
# (cluster_county.csv) on (county_fips, year). 2008 is dropped explicitly
# because ACS 5-year endyears begin at 2009.
#
# Input:  data/output/dominance_county.csv
#         data/output/cluster_county.csv
#         data/output/acs_county.csv
# Output: data/output/analysis_panel.csv (county-year)

# ---------------------------------------------------------------------------
# Read inputs
# ---------------------------------------------------------------------------

dom <- read_csv(
  "data/output/dominance_county.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)
message("Dominance panel: ", nrow(dom), " county-years across ",
        n_distinct(dom$county_fips), " counties (",
        min(dom$year), "-", max(dom$year), ")")

acs <- read_csv(
  "data/output/acs_county.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
) %>%
  select(-county_name)  # keep county_name from dominance side
message("ACS panel: ", nrow(acs), " county-years (",
        min(acs$year), "-", max(acs$year), ")")

clust <- read_csv(
  "data/output/cluster_county.csv", show_col_types = FALSE,
  col_types = cols(county_fips = col_character(), .default = col_guess())
)
message("Cluster panel: ", nrow(clust), " county-years (",
        sum(!is.na(clust$agg_val)), " with agg_val)")

# ---------------------------------------------------------------------------
# Drop 2008 (no ACS coverage)
# ---------------------------------------------------------------------------

dom_2009p <- dom %>% filter(year >= 2009)
message("Dropped ", nrow(dom) - nrow(dom_2009p),
        " dominance rows for year 2008 (no ACS coverage)")

# ---------------------------------------------------------------------------
# Merge
# ---------------------------------------------------------------------------

panel <- dom_2009p %>%
  left_join(acs, by = c("county_fips", "year")) %>%
  left_join(clust, by = c("county_fips", "year"))

# Verify left-join did not expand rows
if (nrow(panel) != nrow(dom_2009p)) {
  stop("Row count changed after join: dom_2009p=", nrow(dom_2009p),
       " panel=", nrow(panel))
}

# Report unmatched dominance rows (counties present in MA data but missing ACS)
unmatched <- panel %>%
  filter(is.na(median_hh_income))
message("\nUnmatched dominance rows (no ACS match): ", nrow(unmatched))
if (nrow(unmatched) > 0) {
  unmatched %>%
    count(county_fips, state, county_name, sort = TRUE) %>%
    head(10) %>%
    print()
}

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

message("\n========== Analysis panel ==========")
message("Rows: ", nrow(panel), " across ",
        n_distinct(panel$county_fips), " counties (",
        min(panel$year), "-", max(panel$year), ")")

message("\nNA shares by column:")
panel %>%
  summarise(across(c(n_plans, pct_enrollment_dominated, total_pop,
                     median_hh_income, pct_65plus, pct_bachelors_p,
                     pct_broadband, agg_val),
                   ~ round(mean(is.na(.x)) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  print(n = Inf)

write_csv(panel, "data/output/analysis_panel.csv")
message("\nWrote data/output/analysis_panel.csv")
