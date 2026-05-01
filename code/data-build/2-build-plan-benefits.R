# 2-build-plan-benefits.R — Extract key benefit variables from PBP files (2008–2018)
# Reads raw PBP zip/folder data via symlink, outputs harmonized panel
#
# Input:  data/input/plan-benefits/ (symlink -> D:/research-data/medicare-advantage/plan-benefits)
# Output: data/output/plan_benefits.csv

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Read a single file from a zip archive
read_from_zip <- function(zip_path, filename) {
  zip_contents <- unzip(zip_path, list = TRUE)$Name
  if (!(filename %in% zip_contents)) return(NULL)
  tmp <- tempdir()
  unzip(zip_path, files = filename, exdir = tmp, overwrite = TRUE)
  df <- read_tsv(file.path(tmp, filename), col_types = cols(.default = "c"),
                 show_col_types = FALSE)
  file.remove(file.path(tmp, filename))
  df
}

#' Read a tab-delimited file from a zip archive or extracted folder
#' @param year Integer year
#' @param filename Name of the .txt file inside the zip/folder
#' @param cols Character vector of columns to keep (plus key columns)
#' @return A tibble, or NULL if file not found
read_pbp <- function(year, filename, cols = NULL) {
  base_path <- "data/input/plan-benefits"

  # Key ID columns always kept

  id_cols <- c("pbp_a_hnumber", "pbp_a_plan_identifier", "segment_id")

  # Check for extracted folder first, then annual zip, then quarterly zips (Q4 preferred)
  folder_path <- file.path(base_path, paste0("PBP-Benefits-", year))
  zip_path <- file.path(base_path, paste0("PBP-Benefits-", year, ".zip"))

  # Quarterly zip patterns (prefer Q4, then Q3, etc.)
  quarterly_patterns <- c(
    paste0("PBP-Benefits-", year, "-Q4.zip"),
    paste0("PBP_Benefits_", year, "-Quarter-4.zip"),
    paste0("PBP Benefits - ", year, " - Quarter 4.zip"),
    paste0("PBP-Benefits-", year, "-Q3.zip"),
    paste0("PBP-Benefits-", year, "-Q2.zip"),
    paste0("PBP-Benefits-", year, "-Q1.zip")
  )

  if (dir.exists(folder_path)) {
    fpath <- file.path(folder_path, filename)
    if (!file.exists(fpath)) return(NULL)
    df <- read_tsv(fpath, col_types = cols(.default = "c"), show_col_types = FALSE)
  } else if (file.exists(zip_path)) {
    df <- read_from_zip(zip_path, filename)
    if (is.null(df)) return(NULL)
  } else {
    # Try quarterly zips
    found <- FALSE
    for (qzip in quarterly_patterns) {
      qpath <- file.path(base_path, qzip)
      if (file.exists(qpath)) {
        message("  Using quarterly zip: ", qzip)
        df <- read_from_zip(qpath, filename)
        found <- TRUE
        break
      }
    }
    if (!found) {
      message("  No data found for year ", year)
      return(NULL)
    }
    if (is.null(df)) return(NULL)
  }

  # Select requested columns (keep only those that exist)
  if (!is.null(cols)) {
    keep <- intersect(c(id_cols, cols), names(df))
    df <- df %>% select(all_of(keep))
  }

  df %>% mutate(year = year)
}


#' Convert character columns to numeric, suppressing warnings for empty/NA
to_numeric <- function(x) suppressWarnings(as.numeric(x))


# ---------------------------------------------------------------------------
# Section D: Premium, Deductible, MOOP
# ---------------------------------------------------------------------------

extract_section_d <- function(year) {
  message("Section D: ", year)

  d_cols <- c(
    # Premium
    "pbp_d_mplusc_premium",
    # Deductible (old naming, present all years)
    "pbp_d_comb_deduct_yn", "pbp_d_comb_deduct_amt",
    "pbp_d_inn_deduct_yn", "pbp_d_inn_deduct_amt",
    # Deductible (new naming, 2016+)
    "pbp_d_ann_deduct_yn", "pbp_d_ann_deduct_amt",
    # MOOP
    "pbp_d_out_pocket_amt_yn", "pbp_d_out_pocket_amt",
    "pbp_d_comb_max_enr_amt_yn", "pbp_d_comb_max_enr_amt",
    "pbp_d_maxenr_oopc_amt"
  )

  df <- read_pbp(year, "pbp_Section_D.txt", d_cols)
  if (is.null(df)) return(NULL)

  # Ensure columns exist (some years lack certain fields)
  for (col in d_cols) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      premium = to_numeric(pbp_d_mplusc_premium),
      # Deductible: use amount if available, else $0 if any _yn flag says "no deductible"
      # CMS coding: 1 = yes (has deductible), 2 = no (no deductible)
      deductible = case_when(
        !is.na(to_numeric(pbp_d_comb_deduct_amt)) ~ to_numeric(pbp_d_comb_deduct_amt),
        !is.na(to_numeric(pbp_d_inn_deduct_amt))  ~ to_numeric(pbp_d_inn_deduct_amt),
        !is.na(to_numeric(pbp_d_ann_deduct_amt))  ~ to_numeric(pbp_d_ann_deduct_amt),
        pbp_d_comb_deduct_yn == "2" ~ 0,
        pbp_d_inn_deduct_yn == "2"  ~ 0,
        pbp_d_ann_deduct_yn == "2"  ~ 0,
        TRUE ~ NA_real_
      ),
      # MOOP: use amount if available, else $0 if _yn flag says no
      moop = case_when(
        !is.na(to_numeric(pbp_d_out_pocket_amt))    ~ to_numeric(pbp_d_out_pocket_amt),
        !is.na(to_numeric(pbp_d_comb_max_enr_amt))  ~ to_numeric(pbp_d_comb_max_enr_amt),
        !is.na(to_numeric(pbp_d_maxenr_oopc_amt))   ~ to_numeric(pbp_d_maxenr_oopc_amt),
        pbp_d_out_pocket_amt_yn == "2"  ~ 0,
        pbp_d_comb_max_enr_amt_yn == "2" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(pbp_a_hnumber, pbp_a_plan_identifier, segment_id, year,
           premium, deductible, moop)
}


# ---------------------------------------------------------------------------
# B1a: Inpatient Hospital
# ---------------------------------------------------------------------------

extract_inpatient <- function(year) {
  message("Inpatient: ", year)

  # Variable names differ by era
  if (year >= 2016) {
    ip_cols <- c(
      "pbp_b1a_copay_yn", "pbp_b1a_copay_mcs_amt_t1",
      "pbp_b1a_copay_mcs_int_num_t1",
      "pbp_b1a_copay_mcs_amt_int1_t1",
      "pbp_b1a_coins_yn", "pbp_b1a_coins_mcs_pct_t1",
      "pbp_b1a_coins_mcs_int_num_t1",
      "pbp_b1a_coins_mcs_pct_int1_t1",
      "pbp_b1a_ded_yn", "pbp_b1a_ded_amt_t1"
    )
  } else {
    ip_cols <- c(
      "pbp_b1a_copay_yn", "pbp_b1a_copay_mcs_amt",
      "pbp_b1a_copay_mcs_intrvl_num",
      "pbp_b1a_copay_mcs_amt_intrvl1",
      "pbp_b1a_coins_yn", "pbp_b1a_coins_mcs_pct",
      "pbp_b1a_coins_mcs_intrvl_num",
      "pbp_b1a_coins_mcs_pct_intrvl1",
      "pbp_b1a_ded_yn", "pbp_b1a_ded_amt"
    )
  }

  df <- read_pbp(year, "pbp_b1a_inpat_hosp.txt", ip_cols)
  if (is.null(df)) return(NULL)

  # Harmonize to common names across eras
  if (year >= 2016) {
    # Tiered variables: map _t1 variants to standard names
    remap <- c(
      pbp_b1a_copay_mcs_amt = "pbp_b1a_copay_mcs_amt_t1",
      pbp_b1a_copay_mcs_amt_intrvl1 = "pbp_b1a_copay_mcs_amt_int1_t1",
      pbp_b1a_coins_mcs_pct = "pbp_b1a_coins_mcs_pct_t1",
      pbp_b1a_coins_mcs_pct_intrvl1 = "pbp_b1a_coins_mcs_pct_int1_t1",
      pbp_b1a_ded_amt = "pbp_b1a_ded_amt_t1"
    )
    for (new_name in names(remap)) {
      old_name <- remap[[new_name]]
      if (old_name %in% names(df)) df[[new_name]] <- df[[old_name]]
    }
  }

  # Ensure columns exist
  needed <- c("pbp_b1a_copay_yn", "pbp_b1a_copay_mcs_amt_intrvl1", "pbp_b1a_copay_mcs_amt",
              "pbp_b1a_coins_yn", "pbp_b1a_coins_mcs_pct_intrvl1", "pbp_b1a_coins_mcs_pct")
  for (col in needed) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      inpatient_copay = case_when(
        !is.na(to_numeric(pbp_b1a_copay_mcs_amt_intrvl1)) ~ to_numeric(pbp_b1a_copay_mcs_amt_intrvl1),
        !is.na(to_numeric(pbp_b1a_copay_mcs_amt)) ~ to_numeric(pbp_b1a_copay_mcs_amt),
        pbp_b1a_copay_yn == "2" ~ 0,
        TRUE ~ NA_real_
      ),
      inpatient_coins_pct = case_when(
        !is.na(to_numeric(pbp_b1a_coins_mcs_pct_intrvl1)) ~ to_numeric(pbp_b1a_coins_mcs_pct_intrvl1),
        !is.na(to_numeric(pbp_b1a_coins_mcs_pct)) ~ to_numeric(pbp_b1a_coins_mcs_pct),
        pbp_b1a_coins_yn == "2" ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(pbp_a_hnumber, pbp_a_plan_identifier, segment_id, year,
           inpatient_copay, inpatient_coins_pct)
}


# ---------------------------------------------------------------------------
# B4: Emergency Room
# ---------------------------------------------------------------------------

extract_er <- function(year) {
  message("ER: ", year)

  er_cols <- c(
    "pbp_b4a_copay_yn",
    "pbp_b4a_copay_amt_mc_min", "pbp_b4a_copay_amt_mc_max",
    "pbp_b4a_coins_yn",
    "pbp_b4a_coins_pct_mc_min", "pbp_b4a_coins_pct_mc_max"
  )

  df <- read_pbp(year, "pbp_b4_emerg_urgent.txt", er_cols)
  if (is.null(df)) return(NULL)

  for (col in er_cols) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      er_copay_min = ifelse(pbp_b4a_copay_yn == "2", 0, to_numeric(pbp_b4a_copay_amt_mc_min)),
      er_copay_max = ifelse(pbp_b4a_copay_yn == "2", 0, to_numeric(pbp_b4a_copay_amt_mc_max)),
      er_coins_min = ifelse(pbp_b4a_coins_yn == "2", 0, to_numeric(pbp_b4a_coins_pct_mc_min)),
      er_coins_max = ifelse(pbp_b4a_coins_yn == "2", 0, to_numeric(pbp_b4a_coins_pct_mc_max))
    ) %>%
    select(pbp_a_hnumber, pbp_a_plan_identifier, segment_id, year,
           er_copay_min, er_copay_max, er_coins_min, er_coins_max)
}


# ---------------------------------------------------------------------------
# B7: Physician (PCP + Specialist)
# ---------------------------------------------------------------------------

extract_physician <- function(year) {
  message("Physician: ", year)

  phys_cols <- c(
    # PCP (B7a)
    "pbp_b7a_copay_yn",
    "pbp_b7a_copay_amt_mc_min", "pbp_b7a_copay_amt_mc_max",
    "pbp_b7a_coins_yn",
    "pbp_b7a_coins_pct_mc_min", "pbp_b7a_coins_pct_mc_max",
    # Specialist (B7b)
    "pbp_b7b_copay_yn",
    "pbp_b7b_copay_mc_amt_min", "pbp_b7b_copay_mc_amt_max",
    "pbp_b7b_coins_yn",
    "pbp_b7b_coins_pct_mc_min", "pbp_b7b_coins_pct_mc_max"
  )

  # 2008–2011: B7 split into two files; B7a and B7b are in _1
  if (year <= 2011) {
    filename <- "pbp_b7_health_prof_1.txt"
  } else {
    filename <- "pbp_b7_health_prof.txt"
  }

  df <- read_pbp(year, filename, phys_cols)
  if (is.null(df)) return(NULL)

  for (col in phys_cols) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      pcp_copay_min = ifelse(pbp_b7a_copay_yn == "2", 0, to_numeric(pbp_b7a_copay_amt_mc_min)),
      pcp_copay_max = ifelse(pbp_b7a_copay_yn == "2", 0, to_numeric(pbp_b7a_copay_amt_mc_max)),
      pcp_coins_min = ifelse(pbp_b7a_coins_yn == "2", 0, to_numeric(pbp_b7a_coins_pct_mc_min)),
      pcp_coins_max = ifelse(pbp_b7a_coins_yn == "2", 0, to_numeric(pbp_b7a_coins_pct_mc_max)),
      specialist_copay_min = ifelse(pbp_b7b_copay_yn == "2", 0, to_numeric(pbp_b7b_copay_mc_amt_min)),
      specialist_copay_max = ifelse(pbp_b7b_copay_yn == "2", 0, to_numeric(pbp_b7b_copay_mc_amt_max)),
      specialist_coins_min = ifelse(pbp_b7b_coins_yn == "2", 0, to_numeric(pbp_b7b_coins_pct_mc_min)),
      specialist_coins_max = ifelse(pbp_b7b_coins_yn == "2", 0, to_numeric(pbp_b7b_coins_pct_mc_max))
    ) %>%
    select(pbp_a_hnumber, pbp_a_plan_identifier, segment_id, year,
           pcp_copay_min, pcp_copay_max, pcp_coins_min, pcp_coins_max,
           specialist_copay_min, specialist_copay_max,
           specialist_coins_min, specialist_coins_max)
}


# ---------------------------------------------------------------------------
# B9: Outpatient Hospital
# ---------------------------------------------------------------------------

extract_outpatient <- function(year) {
  message("Outpatient: ", year)

  op_cols <- c(
    "pbp_b9a_copay_yn",
    "pbp_b9a_copay_mc_amt", "pbp_b9a_copay_mc_amt_max",
    "pbp_b9a_coins_yn",
    "pbp_b9a_coins_pct_mc", "pbp_b9a_coins_pct_mcmax"
  )

  df <- read_pbp(year, "pbp_b9_outpat_hosp.txt", op_cols)
  if (is.null(df)) return(NULL)

  for (col in op_cols) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      outpatient_copay = ifelse(pbp_b9a_copay_yn == "2", 0, to_numeric(pbp_b9a_copay_mc_amt)),
      outpatient_copay_max = ifelse(pbp_b9a_copay_yn == "2", 0, to_numeric(pbp_b9a_copay_mc_amt_max)),
      outpatient_coins = ifelse(pbp_b9a_coins_yn == "2", 0, to_numeric(pbp_b9a_coins_pct_mc)),
      outpatient_coins_max = ifelse(pbp_b9a_coins_yn == "2", 0, to_numeric(pbp_b9a_coins_pct_mcmax))
    ) %>%
    select(pbp_a_hnumber, pbp_a_plan_identifier, segment_id, year,
           outpatient_copay, outpatient_copay_max,
           outpatient_coins, outpatient_coins_max)
}


# ---------------------------------------------------------------------------
# MRX: Drug Benefits (plan-level)
# ---------------------------------------------------------------------------

extract_drug <- function(year) {
  message("Drug: ", year)

  mrx_cols <- c(
    "mrx_drug_ben_yn",
    "mrx_benefit_type",
    "mrx_alt_ded_charge",
    "mrx_alt_ded_amount",
    "mrx_alt_gap_covg_yn",
    "mrx_formulary_tiers_num"
  )

  df <- read_pbp(year, "pbp_mrx.txt", mrx_cols)
  if (is.null(df)) return(NULL)

  for (col in mrx_cols) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  df %>%
    mutate(
      drug_benefit = mrx_drug_ben_yn,
      # Drug deductible: if no drug benefit, NA; if ded_charge says no, $0; else use amount
      # mrx_alt_ded_charge: 1 = charges deductible, 2 = no deductible
      drug_deductible = case_when(
        mrx_drug_ben_yn == "2" ~ NA_real_,
        !is.na(to_numeric(mrx_alt_ded_amount)) ~ to_numeric(mrx_alt_ded_amount),
        mrx_alt_ded_charge == "2" ~ 0,
        TRUE ~ 0  # if drug benefit exists but no deductible info, assume $0
      ),
      drug_gap_coverage = mrx_alt_gap_covg_yn,
      drug_tiers = to_numeric(mrx_formulary_tiers_num)
    ) %>%
    select(pbp_a_hnumber, pbp_a_plan_identifier, segment_id, year,
           drug_benefit, drug_deductible, drug_gap_coverage, drug_tiers)
}


# ---------------------------------------------------------------------------
# Main: Loop years, extract all components, join, and write
# ---------------------------------------------------------------------------

years <- 2008:2018

# For 2018, use Q4 as the final version
# The script checks for folder first, then single zip, then quarterly zips

results <- map(years, function(yr) {
  message("\n========== Year: ", yr, " ==========")

  sec_d <- extract_section_d(yr)
  ip    <- extract_inpatient(yr)
  er    <- extract_er(yr)
  phys  <- extract_physician(yr)
  op    <- extract_outpatient(yr)
  drug  <- extract_drug(yr)

  if (is.null(sec_d)) {
    message("  Skipping year ", yr, " — no Section D data")
    return(NULL)
  }

  # Join all components on plan keys
  join_keys <- c("pbp_a_hnumber", "pbp_a_plan_identifier", "segment_id", "year")

  out <- sec_d
  if (!is.null(ip))   out <- left_join(out, ip,   by = join_keys)
  if (!is.null(er))   out <- left_join(out, er,    by = join_keys)
  if (!is.null(phys)) out <- left_join(out, phys,  by = join_keys)
  if (!is.null(op))   out <- left_join(out, op,    by = join_keys)
  if (!is.null(drug)) out <- left_join(out, drug,  by = join_keys)

  out
})

# Stack all years
plan_benefits <- bind_rows(results)

# Rename ID columns to match MA repo conventions
plan_benefits <- plan_benefits %>%
  rename(
    contractid = pbp_a_hnumber,
    planid = pbp_a_plan_identifier
  ) %>%
  mutate(planid = as.numeric(planid),
         segment_id = as.numeric(segment_id))

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

message("\n========== Validation ==========")
message("Total rows: ", nrow(plan_benefits))
message("Years: ", paste(sort(unique(plan_benefits$year)), collapse = ", "))
message("Rows by year:")
plan_benefits %>% count(year) %>% print(n = 20)

message("\nMissing rates for key variables:")
plan_benefits %>%
  summarize(
    across(c(premium, deductible, moop,
             er_copay_min, pcp_copay_min, specialist_copay_min,
             outpatient_copay, inpatient_copay,
             drug_deductible),
           ~ mean(is.na(.x)), .names = "pct_na_{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") %>%
  mutate(pct_na = round(pct_na * 100, 1)) %>%
  print(n = 20)

message("\nPremium distribution:")
plan_benefits %>%
  filter(!is.na(premium)) %>%
  summarize(min = min(premium), p25 = quantile(premium, 0.25),
            median = median(premium), mean = mean(premium),
            p75 = quantile(premium, 0.75), max = max(premium)) %>%
  print()

# ---------------------------------------------------------------------------
# Write output
# ---------------------------------------------------------------------------

write_csv(plan_benefits, "data/output/plan_benefits.csv")
message("\nWrote data/output/plan_benefits.csv")
