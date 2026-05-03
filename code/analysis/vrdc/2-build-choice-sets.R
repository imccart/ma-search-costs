# 2-build-choice-sets.R — Link bene to county-year choice set
#
# For each MCBS respondent in (county_fips, year), the choice set is FFS
# plus all MA plans active in our locally-built structural_panel.csv for
# that county-year. We attach plan attributes (mean_cost, var_cost,
# Star_Rating, dominated, incumbent, insurer_share) for both the chosen
# plan and the rest of the choice set.
#
# Output:
#   `markets`  — list of length N_markets: each element is a tibble with
#                rows = plans in that (county, year) choice set, columns =
#                plan attributes used by the simulator.
#   `bene_to_market` — vector mapping each row of `bene` to its market index.
#   `choice_in_market` — vector giving the index (within market) of the
#                       chosen plan (FFS or specific contract+PBP).

panel_path <- "/workspace/pl027710/upload/structural_panel.csv"
if (!file.exists(panel_path)) stop("structural_panel.csv not found at ", panel_path)

structural_panel <- fread(panel_path)
message("Loaded structural_panel: ", nrow(structural_panel), " rows")


# ---- Collapse FFS_bare and FFS_supp into one FFS row per county-year ----
# Same omega_bare = 0.25 as in code/analysis/structural/1-load-panel.R.

OMEGA_BARE <- 0.25
ffs_collapsed <- structural_panel %>%
  filter(plan_kind == "FFS") %>%
  mutate(
    weight = if_else(plan_category == "FFS_bare", OMEGA_BARE, 1 - OMEGA_BARE)
  ) %>%
  group_by(county_fips, year) %>%
  summarize(
    plan_id      = "FFS",
    plan_kind    = "FFS",
    contract_id  = NA_character_,
    pbp_id       = NA_character_,
    mean_cost    = sum(mean_cost * weight),
    var_cost     = sum(var_cost  * weight),
    Star_Rating  = NA_real_,
    dominated    = FALSE,
    incumbent    = NA_integer_,
    insurer_share = NA_real_,
    total_eligibles = first(total_eligibles),
    .groups = "drop"
  )

ma_plans <- structural_panel %>%
  filter(plan_kind == "MA") %>%
  select(
    county_fips, year, plan_id, plan_kind, contract_id, pbp_id,
    mean_cost, var_cost, Star_Rating, dominated,
    incumbent, insurer_share, total_eligibles
  )

choice_set_panel <- bind_rows(ffs_collapsed, ma_plans) %>%
  arrange(county_fips, year, plan_kind, plan_id)

message("Choice set panel: ",
        nrow(choice_set_panel), " plan-rows across ",
        n_distinct(paste(choice_set_panel$county_fips, choice_set_panel$year)), " markets")


# ---- Build per-market tibbles (one tibble per (county_fips, year)) ----

markets <- choice_set_panel %>%
  group_by(county_fips, year) %>%
  group_split()

market_keys <- choice_set_panel %>%
  group_by(county_fips, year) %>%
  group_keys() %>%
  mutate(market_id = row_number())

# Lookup: (county_fips, year) -> market_id
market_lookup <- market_keys %>%
  mutate(key = paste(county_fips, year, sep = "_"))


# ---- Map each bene row to its market_id ----

bene <- bene %>%
  mutate(key = paste(county_fips, year, sep = "_")) %>%
  left_join(
    market_lookup %>% select(key, market_id),
    by = "key"
  )

n_no_market <- sum(is.na(bene$market_id))
message(sprintf("Beneficiaries with no matching market in structural_panel: %d (%.1f%%)",
                n_no_market, 100 * n_no_market / nrow(bene)))

# Drop bene rows whose (county, year) isn't in the public panel
bene <- bene %>% filter(!is.na(market_id))


# ---- For each bene, identify the chosen plan within their market ----

# Chosen plan key:
#   - FFS bene (is_ffs_mbsf = 1)             -> plan_id = "FFS"
#   - MA bene with valid contract+pbp        -> contract_id+pbp_id in market panel
#   - MA bene without valid IDs              -> drop or flag

bene <- bene %>%
  mutate(
    chosen_plan_id = if_else(
      is_ffs_mbsf == 1,
      "FFS",
      paste0(ann_contract, "-", ann_pbp)
    )
  )

# Verify the chosen plan exists in the bene's market
bene_with_choice <- bene %>%
  rowwise() %>%
  mutate(
    market_plan_ids = list(markets[[market_id]]$plan_id),
    chosen_in_market = chosen_plan_id %in% market_plan_ids
  ) %>%
  ungroup()

n_unmatched <- sum(!bene_with_choice$chosen_in_market)
message(sprintf("Beneficiaries whose chosen MA plan is not in the public panel: %d (%.1f%%)",
                n_unmatched, 100 * n_unmatched / nrow(bene_with_choice)))

# These are typically SNPs / EGHPs / mid-year-only plans excluded from
# the public panel. Drop for v1; documenting in fit diagnostics.
bene <- bene_with_choice %>% filter(chosen_in_market) %>% select(-market_plan_ids)
message("After chosen-plan-match filter: ", nrow(bene), " rows")


# ---- Index of chosen plan within market (for likelihood lookup) ----

bene$choice_idx <- mapply(
  function(mid, pid) {
    which(markets[[mid]]$plan_id == pid)[1]
  },
  bene$market_id, bene$chosen_plan_id
)


# ---- Attach the bene-specific incumbent flag back into the choice set ----
# The market-level `incumbent` column reflects whether the plan was active
# in the prior year (at the market level). The bene-specific incumbent flag
# is whether this respondent was previously enrolled in this plan. The
# salience equation uses bene-specific within MCBS but market-level in the
# public RF; for the structural model we want bene-specific.
#
# We attach this by overriding the market panel's incumbent column with a
# bene-specific override at evaluation time (in 3-individual-likelihood.R).

message("Built choice sets for ", length(markets), " markets, ",
        nrow(bene), " bene-rows in the analysis sample")
