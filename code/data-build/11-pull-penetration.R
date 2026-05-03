# 12-pull-penetration.R — Total Medicare population by county-year
#
# Reads CMS State/County Penetration files (one CSV per month) and pulls
# the June snapshot for each year 2008-2018. Eligibles is total Medicare
# beneficiaries in the county; Enrolled is MA enrollment. FFS enrollment
# is the difference and is the size of the outside-option market.
#
# June chosen because (a) it is mid-year and (b) it is the earliest month
# available in 2008. Annual averaging would be slightly more refined; the
# stable snapshot is fine for the structural panel's market-size object.
#
# Source: CMS Medicare Advantage State/County Penetration files in the
#         medicare-advantage repo's input folder. Path is hard-coded
#         because the existing project symlink (`ma-repo`) targets the
#         repo's `data/output/` rather than `data/input/`.
#
# Output: data/output/penetration.csv (county_fips x year:
#                                      total_eligibles, ma_enrolled)

penetration_dir <- file.path(
  "C:/Users/immccar/SynologyDrive/work/research-data-repo",
  "medicare-advantage/data/input/ma/penetration/Extracted Data"
)

clean_int <- function(x) as.integer(gsub(",", "", x))

read_pen_year <- function(yr) {
  fpath <- file.path(penetration_dir,
                     sprintf("State_County_Penetration_MA_%d_06.csv", yr))
  read_csv(fpath, show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>%
    transmute(
      county_fips     = FIPS,
      year            = yr,
      total_eligibles = clean_int(Eligibles),
      ma_enrolled     = clean_int(Enrolled)
    ) %>%
    filter(!is.na(county_fips), nchar(county_fips) == 5)
}

penetration <- map_dfr(2008:2018, read_pen_year)

message("Penetration rows: ", nrow(penetration),
        "  (", n_distinct(penetration$county_fips), " counties x ",
        n_distinct(penetration$year), " years)")

message("\nSuppressed (*) MA-enrollment cells by year:")
penetration %>%
  group_by(year) %>%
  summarize(suppressed = sum(is.na(ma_enrolled))) %>%
  print(n = Inf)

message("\nTotal Medicare eligibles by year (millions; NA suppressed):")
penetration %>%
  group_by(year) %>%
  summarize(
    eligibles_M = round(sum(total_eligibles, na.rm = TRUE) / 1e6, 2),
    ma_M        = round(sum(ma_enrolled,     na.rm = TRUE) / 1e6, 2),
    ffs_M       = round((sum(total_eligibles, na.rm = TRUE) -
                         sum(ma_enrolled,     na.rm = TRUE)) / 1e6, 2),
    ma_pen      = round(sum(ma_enrolled,     na.rm = TRUE) /
                        sum(total_eligibles, na.rm = TRUE) * 100, 1)
  ) %>%
  print(n = Inf)

write_csv(penetration, "data/output/penetration.csv")
message("\nWrote data/output/penetration.csv")
