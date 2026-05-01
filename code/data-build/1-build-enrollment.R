# 1-build-enrollment.R — Build enrollment spine from MA repo
# Reads processed MA repo yearly files, selects relevant columns, stacks into panel
#
# Input:  data/input/ma-repo/ma_data_YYYY.txt (symlink -> medicare-advantage repo output)
# Output: data/output/enrollment.csv

# ---------------------------------------------------------------------------
# Read MA repo data
# ---------------------------------------------------------------------------

years <- 2008:2018

read_ma_year <- function(yr) {
  fpath <- paste0("data/input/ma-repo/ma_data_", yr, ".txt")
  if (!file.exists(fpath)) {
    message("  MA repo file not found: ", fpath)
    return(NULL)
  }

  keep_cols <- c(
    # IDs
    "contractid", "planid", "fips", "year",
    # Enrollment
    "avg_enrollment", "first_enrollment", "last_enrollment",
    # Plan characteristics
    "state", "county", "county_name",
    "org_type", "plan_type", "partd", "snp", "eghp",
    "org_name", "org_marketing_name", "plan_name", "parent_org",
    # Quality
    "Star_Rating",
    # Premiums and payments
    "basic_premium", "bid", "ma_rate",
    # Market
    "avg_ffscost",
    # Risk
    "riskscore_partc"
  )

  df <- read_tsv(fpath, col_types = cols(.default = "c"), show_col_types = FALSE)
  available <- intersect(keep_cols, names(df))
  df <- df %>% select(all_of(available))

  num_cols <- c("planid", "avg_enrollment", "first_enrollment", "last_enrollment",
                "Star_Rating", "basic_premium", "bid", "ma_rate",
                "avg_ffscost", "riskscore_partc")
  for (col in intersect(num_cols, names(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  df$year <- as.integer(yr)
  df
}

message("Reading MA repo data...")
enrollment <- map(years, function(yr) {
  message("  Year: ", yr)
  read_ma_year(yr)
}) %>%
  bind_rows()

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

message("\n========== Validation ==========")
message("Total rows: ", nrow(enrollment))
message("Years: ", paste(sort(unique(enrollment$year)), collapse = ", "))

message("\nRows by year:")
enrollment %>% count(year) %>% print(n = 20)

message("\nMissing rates:")
enrollment %>%
  summarize(
    across(c(avg_enrollment, Star_Rating, basic_premium, bid, avg_ffscost),
           ~ mean(is.na(.x)) * 100, .names = "pct_na_{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  mutate(pct_na = round(pct_na, 1)) %>%
  print(n = 20)

message("\nEnrollment distribution:")
enrollment %>%
  filter(!is.na(avg_enrollment)) %>%
  summarize(min = min(avg_enrollment), p25 = quantile(avg_enrollment, 0.25),
            median = median(avg_enrollment), mean = round(mean(avg_enrollment), 1),
            p75 = quantile(avg_enrollment, 0.75), max = max(avg_enrollment)) %>%
  print()

# ---------------------------------------------------------------------------
# Write output
# ---------------------------------------------------------------------------

write_csv(enrollment, "data/output/enrollment.csv")
message("\nWrote data/output/enrollment.csv")
