# 3-cluster.R — Compute county-year product-differentiation measure
#
# For each county-year, runs agglomerative hierarchical clustering on plans'
# CMS Star Ratings component measures (~30 quality dimensions, year-specific).
# Extracts the agglomerative coefficient (∈ [0, 1]) as a scalar summary of how
# strongly plans split into distinct quality clusters within the county.
#   ~ 0  → plans look homogeneous; little quality differentiation
#   ~ 1  → plans split into clearly distinct quality groups; high differentiation
#
# This complements n_plans (cardinality of the choice set) as a measure of
# choice complexity. Approach mirrors the prototype in
# research-projects/_future-ideas/ma-product-differentiation/analysis/.
#
# We compute agg_val twice per county-year:
#   - agg_val: clustering on year-t's full CMS measure roster
#   - agg_val_stable: clustering on the intersection of year-t and year-(t-1)
#                     rosters (measures present in both years)
# The difference (agg_val - agg_val_stable) isolates the methodology-driven
# shift in measured differentiation: if year-t added a high-variance measure,
# differentiation goes up mechanically without underlying quality change.
#
# Input:  data/input/ma-repo/ma_data_YYYY.txt (raw MA repo files, 2008–2018)
#         code/data-build/_rating-variables.R (year-specific component lists)
# Output: data/output/cluster_county.csv (county-year, agg_val + agg_val_stable)

source("code/data-build/_rating-variables.R")

# ---------------------------------------------------------------------------
# Cluster one county-year
# ---------------------------------------------------------------------------

cluster_county_year <- function(plans, vars) {
  # plans: a tibble of plans in this county-year, with one column per rating var
  # vars: vector of rating-variable names to cluster on
  # Returns: list(n_plans, agg_val) — agg_val is NA if not computable

  feat <- plans %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    drop_na()

  if (nrow(feat) < 2) return(list(n_plans = nrow(feat), agg_val = NA_real_))

  # Drop columns with zero variance (agnes errors on them) and re-check N
  feat <- feat %>% select(where(~ sd(.x) > 0))
  if (ncol(feat) < 2 || nrow(feat) < 2) {
    return(list(n_plans = nrow(feat), agg_val = NA_real_))
  }

  hc <- cluster::agnes(scale(feat), method = "ward")
  list(n_plans = nrow(feat), agg_val = hc$ac)
}

# ---------------------------------------------------------------------------
# Process one year
# ---------------------------------------------------------------------------

cluster_year <- function(yr) {
  message("Clustering ", yr)

  fpath <- paste0("data/input/ma-repo/ma_data_", yr, ".txt")
  if (!file.exists(fpath)) {
    message("  MA repo file not found: ", fpath)
    return(NULL)
  }

  raw_vars      <- rating_vars[[as.character(yr)]]
  raw_vars_prev <- rating_vars[[as.character(yr - 1)]]   # NULL if yr == 2008
  if (is.null(raw_vars)) {
    message("  No rating-var list defined for ", yr, " — skipping")
    return(NULL)
  }

  ma <- read_tsv(
    fpath, col_types = cols(.default = "c"), show_col_types = FALSE
  ) %>%
    filter(
      plan_type %in% c("HMO/HMOPOS", "Local PPO", "Regional PPO", "PFFS"),
      snp == "No",
      eghp == "No"
    ) %>%
    mutate(county_fips = str_pad(fips, 5, side = "left", pad = "0"))

  available_full <- intersect(raw_vars, names(ma))
  available_stable <- if (!is.null(raw_vars_prev)) {
    intersect(intersect(raw_vars, raw_vars_prev), names(ma))
  } else {
    character(0)
  }

  message("  Rating vars: full=", length(available_full),
          "; stable (∩ prior year)=", length(available_stable))

  # Cluster on the full year-t roster
  out_full <- ma %>%
    select(county_fips, all_of(available_full)) %>%
    nest(.by = county_fips) %>%
    mutate(
      result        = map(data, ~ cluster_county_year(.x, available_full)),
      n_plans_clust = map_int(result, "n_plans"),
      agg_val       = map_dbl(result, "agg_val")
    ) %>%
    select(county_fips, n_plans_clust, agg_val)

  # Cluster again on the intersection with year-(t-1) roster, if defined
  if (length(available_stable) >= 2) {
    out_stable <- ma %>%
      select(county_fips, all_of(available_stable)) %>%
      nest(.by = county_fips) %>%
      mutate(
        result         = map(data, ~ cluster_county_year(.x, available_stable)),
        n_plans_stable = map_int(result, "n_plans"),
        agg_val_stable = map_dbl(result, "agg_val")
      ) %>%
      select(county_fips, n_plans_stable, agg_val_stable)
    out <- left_join(out_full, out_stable, by = "county_fips")
  } else {
    out <- out_full %>%
      mutate(n_plans_stable = NA_integer_, agg_val_stable = NA_real_)
  }

  out <- out %>%
    mutate(year = as.integer(yr)) %>%
    select(county_fips, year, n_plans_clust, agg_val,
           n_plans_stable, agg_val_stable)

  message("  Counties: ", nrow(out),
          "; clustered (full): ", sum(!is.na(out$agg_val)),
          "; clustered (stable): ", sum(!is.na(out$agg_val_stable)),
          "; mean agg_val: ", round(mean(out$agg_val, na.rm = TRUE), 3),
          "; mean agg_val_stable: ",
          round(mean(out$agg_val_stable, na.rm = TRUE), 3))

  out
}

# ---------------------------------------------------------------------------
# Run all years and combine
# ---------------------------------------------------------------------------

years <- 2008:2018
cluster_county <- map_dfr(years, cluster_year)

message("\n========== Cluster panel ==========")
message("County-years: ", nrow(cluster_county),
        " (", sum(!is.na(cluster_county$agg_val)), " with agg_val)")

message("\nagg_val (full) and agg_val_stable by year:")
cluster_county %>%
  group_by(year) %>%
  summarise(
    n            = n(),
    n_full       = sum(!is.na(agg_val)),
    n_stable     = sum(!is.na(agg_val_stable)),
    mean_full    = round(mean(agg_val,        na.rm = TRUE), 3),
    mean_stable  = round(mean(agg_val_stable, na.rm = TRUE), 3),
    mean_shift   = round(mean(agg_val - agg_val_stable, na.rm = TRUE), 3)
  ) %>%
  print(n = Inf)

write_csv(cluster_county, "data/output/cluster_county.csv")
message("\nWrote data/output/cluster_county.csv")
