# 5-pull-acs.R — Pull ACS 5-year county demographics via tidycensus
#
# Builds a county-year panel of demographics aligned with the MA dominance
# panel (endyears 2009-2018). Variables are pulled per year and combined; some
# variables (bachelor's degree, broadband) are only available in later years
# and appear as NA pre-introduction.
#
# One-time setup: store your Census API key in Windows Credential Manager.
#     keyring::key_set("census_api_key")
#   Get a free key at https://api.census.gov/data/key_signup.html
#
# Output: data/output/acs_county.csv (one row per county-year)

# ---------------------------------------------------------------------------
# API key
# ---------------------------------------------------------------------------

api_key <- tryCatch(
  keyring::key_get("census_api_key"),
  error = function(e) stop(
    "Census API key not found in Windows Credential Manager.\n",
    "Store it once with:  keyring::key_set(\"census_api_key\")\n",
    "Get a free key at https://api.census.gov/data/key_signup.html",
    call. = FALSE
  )
)
census_api_key(api_key, install = FALSE, overwrite = TRUE)

# ---------------------------------------------------------------------------
# Variable definitions
# ---------------------------------------------------------------------------

# Stable variables (available every year 2009-2018 ACS 5-year):
vars_stable <- c(
  median_hh_income = "B19013_001",
  median_age       = "B01002_001",
  total_pop        = "B01003_001"
)

# Age 65+ from B01001 (sex by age). Males 65+ = vars 20-25; females 65+ = 44-49.
vars_age65 <- c(
  paste0("B01001_0", 20:25),
  paste0("B01001_0", 44:49)
)

# Bachelor's+ from B15003 (educational attainment, 25+).
# B15003 introduced in ACS 2012 5-year; for 2009-2011, use B15002.
# B15003: bachelor's=022, master's=023, professional=024, doctorate=025; total=001
# B15002: male bachelor's+ = 015..018; female = 032..035; total = 001
vars_educ_b15003 <- c("B15003_001", "B15003_022", "B15003_023", "B15003_024", "B15003_025")
vars_educ_b15002 <- c("B15002_001",
                      "B15002_015", "B15002_016", "B15002_017", "B15002_018",
                      "B15002_032", "B15002_033", "B15002_034", "B15002_035")

# Broadband from B28011 (Internet Subscriptions in Household).
# Available 2013+ only. B28011_001 = total HH; B28011_004 = with broadband.
vars_broadband <- c(hh_total = "B28011_001", hh_broadband = "B28011_004")

# ---------------------------------------------------------------------------
# Helper: pull one year, return wide tibble keyed on GEOID
# ---------------------------------------------------------------------------

pull_year <- function(yr) {
  message("ACS 5-year endyear=", yr)

  # Stable block
  d_stable <- get_acs(
    geography = "county", variables = vars_stable, year = yr,
    survey = "acs5", output = "wide", cache_table = TRUE
  ) %>%
    select(GEOID, NAME,
           median_hh_income = median_hh_incomeE,
           median_age       = median_ageE,
           total_pop        = total_popE)

  # Age 65+ block
  d_age <- get_acs(
    geography = "county", variables = vars_age65, year = yr,
    survey = "acs5", output = "wide", cache_table = TRUE
  ) %>%
    select(GEOID, ends_with("E"), -NAME) %>%
    mutate(pop_65plus = rowSums(across(ends_with("E")), na.rm = TRUE)) %>%
    select(GEOID, pop_65plus)

  # Education block (B15003 from 2012 onward, otherwise B15002)
  if (yr >= 2012) {
    d_educ <- get_acs(
      geography = "county", variables = vars_educ_b15003, year = yr,
      survey = "acs5", output = "wide", cache_table = TRUE
    ) %>%
      mutate(
        pop_25plus      = B15003_001E,
        pop_bachelors_p = B15003_022E + B15003_023E + B15003_024E + B15003_025E
      ) %>%
      select(GEOID, pop_25plus, pop_bachelors_p)
  } else {
    d_educ <- get_acs(
      geography = "county", variables = vars_educ_b15002, year = yr,
      survey = "acs5", output = "wide", cache_table = TRUE
    ) %>%
      mutate(
        pop_25plus      = B15002_001E,
        pop_bachelors_p = B15002_015E + B15002_016E + B15002_017E + B15002_018E +
                          B15002_032E + B15002_033E + B15002_034E + B15002_035E
      ) %>%
      select(GEOID, pop_25plus, pop_bachelors_p)
  }

  # Broadband (B28011, available only in later 5-year vintages — try and fall
  # back to NA if the variable doesn't exist for this endyear).
  d_bb <- tryCatch(
    get_acs(
      geography = "county", variables = vars_broadband, year = yr,
      survey = "acs5", output = "wide", cache_table = TRUE
    ) %>%
      select(GEOID, hh_total = hh_totalE, hh_broadband = hh_broadbandE),
    error = function(e) {
      message("  no broadband for endyear=", yr, " (", conditionMessage(e), ")")
      tibble(GEOID = d_stable$GEOID, hh_total = NA_real_, hh_broadband = NA_real_)
    }
  )

  d_stable %>%
    left_join(d_age, by = "GEOID") %>%
    left_join(d_educ, by = "GEOID") %>%
    left_join(d_bb, by = "GEOID") %>%
    mutate(year = yr, .after = NAME)
}

# ---------------------------------------------------------------------------
# Run for all years and combine
# ---------------------------------------------------------------------------

years <- 2009:2018
acs_county <- map_dfr(years, pull_year)

# ---------------------------------------------------------------------------
# Derive shares and tidy
# ---------------------------------------------------------------------------

acs_county <- acs_county %>%
  mutate(
    pct_65plus      = 100 * pop_65plus      / total_pop,
    pct_bachelors_p = 100 * pop_bachelors_p / pop_25plus,
    pct_broadband   = 100 * hh_broadband    / hh_total
  ) %>%
  rename(county_fips = GEOID, county_name = NAME) %>%
  select(county_fips, county_name, year,
         total_pop, median_hh_income, median_age,
         pct_65plus, pct_bachelors_p, pct_broadband)

message("ACS panel: ", nrow(acs_county), " county-years across ",
        n_distinct(acs_county$county_fips), " counties")

write_csv(acs_county, "data/output/acs_county.csv")
