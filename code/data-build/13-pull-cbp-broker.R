# 13-pull-cbp-broker.R — Insurance broker density (CBP NAICS 524210)
#
# Pulls Census County Business Patterns county-year counts of establishments
# and employment in NAICS 524210 ("Insurance Agencies and Brokerages") via
# the Census API, 2008-2018. The Census API uses different NAICS revision
# parameters by year:
#
#   2008-2011 -> NAICS2007
#   2012-2016 -> NAICS2012
#   2017-2018 -> NAICS2017
#
# 524210 is stable across all three revisions ("Insurance Agencies and
# Brokerages"). The series covers all insurance lines (auto, home, life,
# P&C, health), so the measure is a noisy proxy for MA-specialist broker
# density. There is no public MA-specific broker registry; CBP 524210 is
# the standard fallback in the literature (e.g., ACA-marketplace papers).
#
# Used downstream as
#   (a) a search-cost shifter — counties with denser broker presence have
#       lower effective c, so brokers/eligible enters the X vector for c_i
#   (b) an exclusion-restriction-clean instrument for log(insurer_share)
#       in the Stigler salience identification, on the theory that broker
#       density affects which plans get into consideration sets but not
#       the plans' own cost-sharing or quality
#
# Census API key: Windows Credential Manager, service "census_api_key".
#
# Output: data/output/cbp_broker.csv
#         (county_fips, year, ins_brokers_estab, ins_brokers_emp)

cbp_year <- function(yr, key) {
  naics_param <- if (yr <= 2011) "NAICS2007" else if (yr <= 2016) "NAICS2012" else "NAICS2017"
  url <- paste0(
    "https://api.census.gov/data/", yr, "/cbp?get=EMP,ESTAB",
    "&for=county:*&", naics_param, "=524210&key=", key
  )
  resp <- httr::GET(url, httr::timeout(60))
  if (httr::status_code(resp) != 200) {
    stop("Census CBP API returned status ", httr::status_code(resp),
         " for year ", yr)
  }
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  raw <- jsonlite::fromJSON(txt)
  hdr  <- as.character(raw[1, ])
  body <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(body) <- make.unique(hdr)

  tibble::as_tibble(body) %>%
    mutate(
      year              = yr,
      county_fips       = paste0(state, county),
      ins_brokers_emp   = suppressWarnings(as.integer(EMP)),
      ins_brokers_estab = suppressWarnings(as.integer(ESTAB))
    ) %>%
    select(county_fips, year, ins_brokers_estab, ins_brokers_emp)
}

key <- keyring::key_get("census_api_key")

cbp <- map_dfr(2008:2018, cbp_year, key = key)

message("CBP NAICS 524210 rows: ", nrow(cbp),
        "  (", n_distinct(cbp$county_fips), " counties x ",
        n_distinct(cbp$year), " years)")

message("\nNon-zero coverage by year (counties with employment > 0):")
cbp %>%
  group_by(year) %>%
  summarize(
    n_counties     = n(),
    n_emp_positive = sum(ins_brokers_emp > 0, na.rm = TRUE),
    pct_positive   = round(mean(ins_brokers_emp > 0, na.rm = TRUE) * 100, 1),
    median_estab   = median(ins_brokers_estab, na.rm = TRUE),
    median_emp     = median(ins_brokers_emp, na.rm = TRUE)
  ) %>%
  print(n = Inf)

write_csv(cbp, "data/output/cbp_broker.csv")
message("\nWrote data/output/cbp_broker.csv")
