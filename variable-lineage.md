# Variable lineage

One entry per variable that reaches the paper, whether through a table, a
figure, a summary statistic, an estimate or a counterfactual. Each entry gives
the raw file and field the variable starts from, each transformation in order
with the script that does it, its level and units in final form, and the code
files it appears in. Scripts are named without line numbers; functions are named
where the work happens inside one. Variables built by one rule from a set of
inputs (a family of interactions, a set of indicators) are documented as one
entry listing every member.

This document covers the structural model (Table 1 and the counterfactual). The
pipeline has three stages. SAS steps 1-5 (`code/data-build/vrdc/`) run on the
VRDC seat and write `PL027710.*` datasets that are exported as CSVs into the
seat's R workspace. The local data-build (`code/data-build/1`-`14`) builds the
public plan-attribute panel `structural_panel.csv` and the PBP cost-sharing file
`plan_county_benefits.csv`, which are uploaded to the seat. R steps 0-9
(`code/analysis/vrdc/`) run on the seat, and their result CSVs are exported back
to `results/`.

Raw inputs referred to below:

| File | Contents |
|---|---|
| `MBSF.MBSF_ABCD_<yr>` (seat, 2014-2018) | Master Beneficiary Summary File: demographics, coverage months, monthly Part C contract/PBP, monthly county FIPS |
| `MCBS<yr>.SURVEY_*_<yr>` (seat, 2015-2018) | MCBS Survey File segments DEMO, HISUMRY, MAPLANQX, MCREPLNQ, HITLINE, GENHLTH, CENWGTS |
| `MCBSXWLK.MCBSXWLK` (seat) | MCBS `BASE_ID` to Medicare `BENE_ID` crosswalk |
| `ENRFPL<yr>.*_ENC` (seat) | MA encounter records (IP, OP, carrier; SNF/HHA pulled but unused) |
| `RIF<yyyy>.*_CLAIMS_<MM>` (seat, 2012-2018) | FFS claims (inpatient, outpatient, carrier; SNF/HHA pulled but unused) |
| `data/input/ma-repo/ma_data_<yr>.txt` | Plan-county-year MA landscape from `research-data-repo/medicare-advantage`: enrollment, plan type, Part D flag, parent org, star rating |
| `D:/research-data/medicare-advantage/plan-benefits/` | CMS PBP benefit files (premium, deductible, MOOP, service copays and coinsurance) |
| CMS State/County Penetration files (June) | County-year Medicare eligibles |
| Census County Business Patterns API | County-year establishments in NAICS 524210 (insurance agencies and brokerages) |

---

## 1. Identifiers, sample and weights

### `BASEID`, `BENE_ID`
- Paper: unit of observation; the beneficiary count in the Table 1 note (13,667).
- Raw: MCBS DEMO `BASEID`; `MCBSXWLK.MCBSXWLK` maps `BASE_ID` to `BENE_ID`.
- Chain: `2-extract-mcbs.sas` keeps community-dwelling respondents aged 65+ (`INT_TYPE` in C/B, `H_AGE >= 65`) and inner-joins the segments on `BASEID` by year. `3-build-bene-panel.sas` attaches `BENE_ID` through the crosswalk and the MBSF year row, and sets `link_status`. `1-build-bene-choice-panel.R` keeps `link_status == "ok"`, `full_year_partAB == 1`, `not_esrd == 1`, `active_shopper == 1`, and non-missing county, income category and education category; it drops bene-years whose chosen plan is not in the public plan panel. `2-load-estimation-panel.R` drops duplicate bene-plan rows, bene-years with no in-panel chosen plan, and bene-years with missing or zero weight. The likelihood groups bene-years by `BASEID` (`idx_by_bene` in `3-individual-likelihood.R`).
- Level: beneficiary (`BASEID`); the estimation row is beneficiary-year (22,978).
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `0-project-bene-cost-sharing.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### Sample-restriction flags: `full_year_partAB`, `not_esrd`, `active_shopper`
- Paper: define the estimation sample.
- Raw: MBSF `BENE_HI_CVRAGE_TOT_MONS`, `BENE_SMI_CVRAGE_TOT_MONS`; MCBS HISUMRY `H_MEDSTA`; MCBS HITLINE `PLANTYPE`.
- Chain: `1-extract-mbsf.sas` already keeps only 12 months of Part A and Part B (`%mbsf_age65_filter`), so `full_year_partAB` in `3-build-bene-panel.sas` is 1 for every linked row. `2-extract-mcbs.sas` sets `not_esrd = medstatus in ("10","20")` (aged, not ESRD) and `active_shopper = 1` when the respondent has no employer, VA, Tricare or retiree-drug-subsidy coverage (`PLANTYPE` 20/21, 40, 50, 60). The MA-via-institution term in `active_shopper` is zero by construction because `S_OBTNP` is inapplicable on Medicare rows.
- Level: beneficiary-year; 0/1.
- Files: `1-extract-mbsf.sas`, `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `0-project-bene-cost-sharing.R`, `1-build-bene-choice-panel.R`.

### `wgt_full_sample`, `variance_stratum`, `variance_psu`
- Paper: weights every likelihood contribution and all fit and counterfactual shares; enters the sandwich standard errors.
- Raw: MCBS CENWGTS `CS1YRWGT` (2015-2016) or `CEYRSWGT` (2017-2018); `SUDSTRAT`; `SUDUNIT`.
- Chain: `2-extract-mcbs.sas` resolves the year-specific weight name. `2-load-estimation-panel.R` drops missing or zero weights and declares the `svydesign`. `3-individual-likelihood.R` multiplies each beneficiary's log-likelihood by the weight on that beneficiary's first bene-year row (`wgt_by_bene`). `9-counterfactual.R` weights bene-year outcomes by `wgt_full_sample`.
- Level: beneficiary-year; persons represented.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `5-fit-diagnostics.R`, `6-standard-errors.R`, `9-counterfactual.R`.

## 2. Choice and choice set

### `chosen_plan_id`, `is_chosen`, `choice_idx`
- Paper: the dependent variable of the choice stage; the FFS share and incumbent-given-MA rows of the fit diagnostics.
- Raw: MBSF `PTC_CNTRCT_ID_01`-`_12`, `PTC_PBP_ID_01`-`_12`.
- Chain: `1-extract-mbsf.sas` (`%modal_partc` in `_config.sas`) takes the December contract and PBP, or the first non-blank month, as `ann_contract`/`ann_pbp`, and sets `is_ffs = 1` when no month has a contract. `3-build-bene-panel.sas` carries these as `is_ffs_mbsf`, `ann_contract`, `ann_pbp`. `1-build-bene-choice-panel.R` builds `chosen_plan_id` ("FFS" or `contract_pbp`) and `is_chosen = plan_id == chosen_plan_id` on each bene-plan row. `2-load-estimation-panel.R` stores the within-market index of the chosen plan as `choice_idx`.
- Level: beneficiary-year; plan identifier.
- Files: `_config.sas`, `1-extract-mbsf.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### Market (`county_fips`, `year`) and plan set
- Paper: defines each beneficiary's choice set.
- Raw: MBSF `STATE_CNTY_FIPS_CD_12`; plan rows from `ma_data_<yr>.txt`.
- Chain: `1-extract-mbsf.sas` takes the December county FIPS. `1-build-bene-choice-panel.R` zero-pads it to `county_fips` and inner-joins every plan in `structural_panel.csv` for that county-year. `13-assemble-structural-panel.R` builds that panel from MA plans with positive enrollment in the standard types (HMO, local/regional PPO, PFFS; filtered in `5-construct-dominance.R`) plus FFS rows; `1-build-bene-choice-panel.R` collapses the two FFS variants into one "FFS" row. `2-load-estimation-panel.R` stores one plan list per market (`markets`).
- Level: county-year.
- Files: `1-extract-mbsf.sas`, `1-build-enrollment.R`, `5-construct-dominance.R`, `13-assemble-structural-panel.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`.

### Incumbent plan: `prior_plan_id`, `prior_plan_offered`
- Paper: Table 1 Panel A, `psi`; the incumbent plan is always in the considered set.
- Raw: MBSF prior-year `ann_contract`, `ann_pbp`, `is_ffs`.
- Chain: `3-build-bene-panel.sas` joins the MBSF row from year t-1 (`prior_contract`, `prior_pbp`, `prior_was_ffs`). `1-build-bene-choice-panel.R` builds `prior_plan_id` ("FFS" or `contract_pbp`) and the bene-plan flag `incumbent_bene`. `2-load-estimation-panel.R` collapses it to `incumbent_bene_year` and sets `has_prior_year`. `3-individual-likelihood.R` sets `prior_plan_offered = has_prior_year & incumbent_bene_year`, adds `psi` to the matching plan's utility, and fixes its inclusion probability at 1 (`compute_phi`).
- Level: beneficiary-year; 0/1 per plan.
- Files: `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

## 3. Plan utility regressors (Table 1, Panel A)

### `premium` (`bene_prem`)
- Paper: `alpha_prem`; the dollar conversion of all welfare figures.
- Raw: PBP `pbp_d_mplusc_premium` (monthly MA plus Part D premium); FFS premium from the national series in `0-project-bene-cost-sharing.R` (PDP premium and Medigap Plan G premium).
- Chain: `2-build-plan-benefits.R` reads the PBP premium; `4-merge-plan-county.R` merges it to plan-county-year in `plan_county_benefits.csv`. `0-project-bene-cost-sharing.R` sets `premium_annual = 12 * premium` for MA and, for FFS, `0.25 * 12 * PDP premium + 0.75 * 12 * (PDP + Medigap G premium)`; Part B premium is excluded from both. `1-build-bene-choice-panel.R` merges it onto bene-plan rows and fills an unpriced plan with the county-year mean priced premium. `2-load-estimation-panel.R` aligns it to market plan order. `compute_bene_utility` divides by 1,000.
- Level: beneficiary-plan-year; annual dollars, entered in thousands.
- Files: `2-build-plan-benefits.R`, `4-merge-plan-county.R`, `0-project-bene-cost-sharing.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### `oop_cost` (`bene_oop`)
- Paper: `alpha_oop`; `mean_EC_chosen` in the counterfactual uses `premium + oop_cost`.
- Raw: utilization from MA encounters (`ENRFPL`, script 4) and FFS claims (`RIF`, script 5); cost sharing from PBP service fields (`pbp_b7a` PCP, `pbp_b7b` specialist, `pbp_b9a` outpatient, `pbp_b4a` ER, inpatient, deductible, drug deductible, MOOP).
- Chain: `4-extract-ma-encounters.sas` and `5-extract-ffs-claims.sas` count per bene-year `n_ip_days`, `n_op_visits`, `n_op_er_visits` (revenue centers 0450-0459), `n_car_pcp_lines` (provider specialty 01, 08, 11, 38) and `n_car_spec_lines` (other non-blank specialties). `2-build-plan-benefits.R` builds the minimum in-network copay and coinsurance per service. `0-project-bene-cost-sharing.R` forms expected use as each realized prior year (up to three) or, with no prior use, a Poisson prediction from age, self-rated health, health change, dual status, income, education and ADI; prices each draw under every plan in the market at fixed allowed charges ($200 PCP, $350 specialist, $2,000 outpatient, $1,500 ER, $3,000 per inpatient day; `per_event` takes the larger of copay and coinsurance), adds deductibles, caps at MOOP ($6,700 when missing), and averages over draws (`EC_oop`). FFS uses the bare and Plan G regimes weighted 0.25/0.75. `1-build-bene-choice-panel.R` renames it `oop_cost` and fills an unpriced plan with `max(population cost - premium, 0)`.
- Level: beneficiary-plan-year; annual dollars, entered in thousands.
- Files: `4-extract-ma-encounters.sas`, `5-extract-ffs-claims.sas`, `2-build-plan-benefits.R`, `4-merge-plan-county.R`, `0-project-bene-cost-sharing.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### `Star_Rating`
- Paper: `beta`; the star-rating dollar value ($582 = 1000 `beta`/`alpha_prem`).
- Raw: `ma_data_<yr>.txt` field `Star_Rating`.
- Chain: `1-build-enrollment.R` reads it; it passes through `4-merge-plan-county.R` and `5-construct-dominance.R` to `13-assemble-structural-panel.R` (numeric; NA for FFS). `compute_bene_utility` enters `Star_Rating - 3.5`, with missing set to 0, for MA plans only.
- Level: plan-county-year; stars (1-5), centered at 3.5.
- Files: `1-build-enrollment.R`, `4-merge-plan-county.R`, `5-construct-dominance.R`, `13-assemble-structural-panel.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `has_partd`
- Paper: `partd` (Part D coverage).
- Raw: `ma_data_<yr>.txt` field `partd`.
- Chain: `5-construct-dominance.R` sets `has_partd = partd == "Yes"`. `13-assemble-structural-panel.R` carries it (TRUE for FFS rows, which do not use it). `compute_bene_utility` enters it for MA plans, missing as 0.
- Level: plan-county-year; 0/1.
- Files: `1-build-enrollment.R`, `5-construct-dominance.R`, `13-assemble-structural-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### `plan_category` (members: `cat_PPO`, `cat_PFFS`)
- Paper: PPO and PFFS rows of Panel A (HMO omitted).
- Raw: `ma_data_<yr>.txt` field `plan_type`.
- Chain: `5-construct-dominance.R` maps HMO/HMOPOS to HMO, local and regional PPO to PPO, PFFS and RFB PFFS to PFFS, and drops other types. `compute_bene_utility` adds `cat_PPO` or `cat_PFFS` to MA plans in those categories.
- Level: plan-county-year; category.
- Files: `1-build-enrollment.R`, `5-construct-dominance.R`, `13-assemble-structural-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### FFS indicator (`plan_kind == "FFS"`)
- Paper: `xi_FFS`.
- Raw: none; FFS rows are created in `13-assemble-structural-panel.R` for every county-year.
- Chain: `1-build-bene-choice-panel.R` collapses `FFS_bare` and `FFS_supp` into one "FFS" plan. `compute_bene_utility` adds `xi_FFS` to it. Its inclusion probability is fixed at 1.
- Level: county-year; 0/1.
- Files: `13-assemble-structural-panel.R`, `1-build-bene-choice-panel.R`, `3-individual-likelihood.R`.

## 4. Search-cost covariates (Table 1, Panel B)

All enter `compute_log_c_det` in `3-individual-likelihood.R`; any remaining NA is set to 0 at the top of that script.

### `log_inc_dm`
- Paper: `gamma_inc` (log income).
- Raw: MCBS DEMO `INCOME`.
- Chain: `2-extract-mcbs.sas` pulls it as `income_cat`. `1-build-bene-choice-panel.R` maps the 14 categories to bracket midpoints ($2,500 to $175,000), takes `log(max(midpoint, 1000))`, and demeans over the filtered sample.
- Level: beneficiary-year; log dollars, demeaned.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `educ_yrs_dm`
- Paper: `gamma_educ` (education, years).
- Raw: MCBS DEMO `SPDEGRCV`.
- Chain: `2-extract-mcbs.sas` pulls it as `education_cat`. `1-build-bene-choice-panel.R` maps the nine codes to years (0, 6, 11, 12, 13, 14, 14, 16, 19), demeans, and sets codes outside the map to 0.
- Level: beneficiary-year; years, demeaned.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `age_dm`
- Paper: `gamma_age`.
- Raw: MCBS DEMO `H_AGE`.
- Chain: `2-extract-mcbs.sas` pulls it as `age`. `1-build-bene-choice-panel.R` sets `age_dm = age - 75`.
- Level: beneficiary-year; years, centered at 75.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `is_dual`
- Paper: `gamma_dual`; the dual rows of `search_by_group.csv`.
- Raw: MCBS HISUMRY `H_OPMDCD`.
- Chain: `2-extract-mcbs.sas` pulls it as `dual_annual`. `1-build-bene-choice-panel.R` sets `is_dual = dual_annual in (1, 4)`.
- Level: beneficiary-year; 0/1.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `5-fit-diagnostics.R`.

### `adi_dm`
- Paper: `gamma_adi` (area deprivation).
- Raw: MCBS DEMO `ADI` (2015), `CENSADI` (2016-2017), `ADINATNL` (2018).
- Chain: `2-extract-mcbs.sas` resolves the year-specific name to `adi_raw`. `1-build-bene-choice-panel.R` coerces to numeric (suppression codes become NA), divides by 100, and fills NA with the non-missing mean. The three source variables are on different scales across years.
- Level: beneficiary-year; national rank / 100.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `0-project-bene-cost-sharing.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `book_understood_dm`
- Paper: `gamma_hb` (handbook comprehension).
- Raw: MCBS MCREPLNQ `KBOKUNDR`.
- Chain: `2-extract-mcbs.sas` pulls it as `book_understood`. `1-build-bene-choice-panel.R` demeans the raw code (1 very easy to 4 very difficult) and sets missing (non-readers) to 0.
- Level: beneficiary-year; survey code, demeaned.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `tenure_dm`
- Paper: `gamma_exp` (years enrolled in MA).
- Raw: MCBS MAPLANQX `MADVYRS`.
- Chain: `2-extract-mcbs.sas` pulls it as `madv_years_enrolled`. `1-build-bene-choice-panel.R` demeans it and sets missing (including FFS enrollees) to 0.
- Level: beneficiary-year; years, demeaned.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### `easy_compare_dm`, `enough_info_dm`
- Paper: `gamma_easycmp`, `gamma_infcmp`.
- Raw: MCBS MCREPLNQ `KNCOVOPT`, `KNCOVINF`.
- Chain: `2-extract-mcbs.sas` pulls them as `easy_compare_options`, `enough_info_to_compare`. `1-build-bene-choice-panel.R` sets code 5 ("doesn't make decisions") to NA, demeans, and sets NA to 0. Higher codes mean harder to compare or less information.
- Level: beneficiary-year; survey code, demeaned.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

## 5. Search actions (Table 1, Panels C and D)

### `act_info`, `act_web`, `act_phone`
- Paper: `kappa_info`, `kappa_web`, `kappa_phone` (action thresholds) and `b_info`, `b_web`, `b_phone` (consideration breadth); the search rate in the fit diagnostics and counterfactual.
- Raw: MCBS MCREPLNQ `KNINFMCR`, `KVSITWEB`, `KCPHINFO`.
- Chain: `2-extract-mcbs.sas` pulls them as `tried_find_info`, `visited_medicare_site`, `called_800_medicare`. `1-build-bene-choice-panel.R` sets each action to 1 when the code equals 1. `loglik_actions` models each as a logit in `B - c`; `compute_K` uses them to set consideration breadth.
- Level: beneficiary-year; 0/1.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### `book_read`
- Paper: `kappa_book`, `tau_gap`, `b_book`.
- Raw: MCBS MCREPLNQ `KBOKREAD`.
- Chain: `2-extract-mcbs.sas` pulls it as `book_read_amount`. `1-build-bene-choice-panel.R` maps code 1 (read thoroughly) to 2, code 2 (read parts) to 1, and code 3 (not at all), missing, don't know and refused to 0, per the MCBS 2015-2018 MCREPLNQ codebooks in `background/codebooks/`. `loglik_actions` models it as a three-level ordered logit; `compute_K` uses `book_read > 0`.
- Level: beneficiary-year; ordered 0/1/2.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

### `review_level`
- Paper: `kappa_review`, `tau_review_gap`.
- Raw: MCBS MCREPLNQ `KNCOVREV`.
- Chain: `2-extract-mcbs.sas` pulls it as `review_options_freq`. `1-build-bene-choice-panel.R` maps code 1 (at least yearly) to 2, codes 2-3 to 1, and everything else to 0. `loglik_actions` models it as a three-level ordered logit; it does not enter `compute_K`.
- Level: beneficiary-year; ordered 0/1/2.
- Files: `2-extract-mcbs.sas`, `3-build-bene-panel.sas`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`, `9-counterfactual.R`.

## 6. Prominence (Table 1, Panel E)

### `pf_rank_score`
- Paper: `lambda_PF` (Plan Finder cost rank).
- Raw: population expected cost `mean_cost` from `5-construct-dominance.R` (stylized utilization profiles priced through the PBP schedule).
- Chain: `14-build-prominence-vars.R` ranks MA plans by `mean_cost` within county-year, normalizes the rank to [0, 1], and sets `pf_rank_score = exp(-1 * rank_norm)`; FFS rows are NA. `compute_market_prominence` sets NA to 0 and restricts it to MA plans. `compute_salience` multiplies it by `lambda_PF_0`.
- Level: plan-county-year; score in [0.37, 1].
- Files: `5-construct-dominance.R`, `14-build-prominence-vars.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

### Broker prominence (`parent_org_loo_national` times `broker_density_per_k`)
- Paper: `lambda_broker` (broker channel).
- Raw: `ma_data_<yr>.txt` fields `parent_org`, `avg_enrollment`; CBP `ESTAB` for NAICS 524210; penetration-file county eligibles.
- Chain: `14-build-prominence-vars.R` computes the parent organization's share of national MA enrollment excluding the focal county (`parent_org_loo_national`). `12-pull-cbp-broker.R` pulls `ins_brokers_estab`; `11-pull-penetration.R` pulls June `total_eligibles`; `13-assemble-structural-panel.R` joins both (missing establishments set to 0). `1-build-bene-choice-panel.R` sets `broker_density_per_k = ins_brokers_estab / max(total_eligibles, 1) * 1000`. `compute_market_prominence` multiplies the two for MA plans; `compute_salience` multiplies the product by `lambda_broker_0`.
- Level: plan-county-year; share times establishments per 1,000 eligibles.
- Files: `1-build-enrollment.R`, `11-pull-penetration.R`, `12-pull-cbp-broker.R`, `13-assemble-structural-panel.R`, `14-build-prominence-vars.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `3-individual-likelihood.R`.

## 7. Counterfactual quantities

### `var_cost` (`Var_C_ij`)
- Paper: does not enter utility; used only by the dominance flag in the counterfactual.
- Raw: same utilization and PBP inputs as `oop_cost`.
- Chain: `0-project-bene-cost-sharing.R` assigns each bene-year to a decile of expected annual cost, samples up to 100 cell-mates' realized same-year use, prices it under each distinct cost-sharing schedule, and takes the variance; FFS uses the omega-weighted within-regime variance. `1-build-bene-choice-panel.R` renames it `var_cost`.
- Level: risk cell by cost-sharing schedule; dollars squared.
- Files: `0-project-bene-cost-sharing.R`, `1-build-bene-choice-panel.R`, `2-load-estimation-panel.R`, `9-counterfactual.R`.

### Dominated flag (`compute_bene_dominated`)
- Paper: `dominated_share` in `counterfactual.csv`.
- Raw: the beneficiary's own `mean_cost` (`premium + oop_cost`) and `var_cost` in each plan.
- Chain: `2-load-estimation-panel.R` aligns them to market plan order as `bene_mc` and `bene_vc`. `compute_bene_dominated` in `9-counterfactual.R` flags an MA plan as dominated for that beneficiary when another MA plan in the same `plan_category` by `has_partd` group has weakly lower `mean_cost` and `var_cost`, strictly lower on one.
- Level: beneficiary-plan-year; 0/1.
- Files: `2-load-estimation-panel.R`, `9-counterfactual.R`.

### Counterfactual outputs: `dominated_share`, `search_rate`, `mean_consideration`, `mean_EC_chosen`, `welfare_plan_value`, `welfare_search_cost`, `welfare_total`
- Paper: counterfactual section (to be drafted).
- Raw: model quantities at `theta_hat`.
- Chain: `9-counterfactual.R` scales `c_i` by 1, 0.75, 0.5 and 0; forms each beneficiary's empirical-Bayes weights over the random-effect draws from observed actions; computes choice-stage outcomes for the 16 consideration profiles of (info, web, phone, book > 0) and weights them by profile probabilities under each dose. `search_rate` includes `review_level > 0`. Welfare is relative to the 0% dose and converted to dollars by `1000 / alpha_prem`: `welfare_plan_value` from the change in the inclusive value over the considered set, `welfare_search_cost` from minus the change in `c_i` times the expected number of the five actions.
- Level: survey-weighted population means by dose; shares, plans, dollars per beneficiary-year.
- Files: `3-individual-likelihood.R`, `9-counterfactual.R`.

## 8. Outputs

| Output | Written by | Feeds |
|---|---|---|
| `results/theta_hat.csv` | `4-estimate-mle.R` | reloaded by `6`, `8`, `9` |
| `results/standard_errors.csv` | `6-standard-errors.R` | Table 1 (`tab:estimates`), hand-entered in `paper/paper.qmd` and checked in `notes/review-numbers.yaml` |
| `results/fit_diagnostics.csv` | `5-fit-diagnostics.R` | search rate, FFS share, incumbent given MA, predicted vs observed |
| `results/search_by_group.csv` | `5-fit-diagnostics.R` | search rate by dual and bachelor's degree, cells of at least 11 |
| `results/counterfactual.csv` | `9-counterfactual.R` | counterfactual section |

The seat writes these to `results/vrdc/`; the exported copies are in `results/`.
