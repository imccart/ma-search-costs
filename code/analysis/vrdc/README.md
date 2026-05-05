# VRDC analysis (`code/analysis/vrdc/`)

R-side individual-level structural estimation. Reads the bene panel exported by `code/data-build/vrdc/`, attaches plan attributes from the locally-built `structural_panel.csv`, and estimates the Stigler-search model via combined MLE + aggregate moments.

## Run order

```
_analyze-vrdc.R              master driver
0-build-bene-choice-panel.R  join bene panel × structural_panel, write checkpoint CSV
1-load-estimation-panel.R    read checkpoint, declare svydesign, build markets[] list
3-individual-likelihood.R    per-bene Stigler-search choice probability
4-aggregate-moments.R        survey-weighted aggregate moments
5-estimate-gmm.R             nloptr SBPLX optimization
6-fit-diagnostics.R          predicted vs observed by demographic group
7-mixture-extension.R        finite-mixture c_i (deferred)
```

Source `_analyze-vrdc.R` to run end-to-end.

The bene × plan panel is materialized as a checkpoint so (a) restarts after a crashed optimizer don't re-run the join, (b) diagnostics are easy on the canonical estimation object, and (c) counterfactuals are a `copy(bcp)` away.

## Inputs (must already be in VRDC seat)

- `/workspace/pl027710/export/bene_panel.csv` — produced by data-build (SAS)
- `/workspace/pl027710/upload/structural_panel.csv` — uploaded from local (with prominence columns from `code/data-build/14-build-prominence-vars.R`)
- `/workspace/pl027710/upload/analysis_panel.csv` — uploaded from local

## Checkpoint

- `/workspace/pl027710/export/bene_choice_panel.csv` — written by script 0; one row per (bene, plan in bene's market). This is the canonical estimation panel.

## Outputs

```
results/vrdc/
  theta_hat.csv                 point estimates
  moments_fit.csv               observed vs predicted aggregate moments
  search_by_group.csv           predicted vs observed search rate by demographic group
  ffs_by_group.csv              predicted vs observed FFS share by demographic group
  kstar_distribution.csv        distribution of K* across respondents
  c_distribution.csv            distribution of implied c
```

All cells with N < 11 are suppressed before output (CMS small-cell rule).

## Estimation strategy

Combined objective:
- Per-respondent log-likelihood of plan choice (Goeree-style consideration-set + multinomial logit on full-information utility within the considered set).
- Three aggregate moments:
  - M1: weighted mean of `searched` indicator (= weighted P(K* > 0) at theta)
  - M2: weighted mean of FFS choice (= weighted P(K* = 0))
  - M3: weighted mean of incumbent-MA choice among MA enrollees

Hyperparameter `LAMBDA` (in `4-aggregate-moments.R`) governs the relative weight of the moment block vs. the likelihood block. Default 1e3; tune if estimates drift.

Optimizer: `nloptr::nloptr` with `NLOPT_LN_SBPLX` (gradient-free; bounds-respecting). SLSQP would be faster but needs analytical gradients of the simulator — see `background/progress-and-next-steps.md` for that as a future unlock.

Standard errors: bootstrap clustered at `state_cnty_fips` (deferred). Or sandwich SEs from the Hessian × outer-product-of-gradient at theta_hat.

## Sample restrictions (already applied in script 1)

- `link_status == "ok"` (drop admin-vs-survey mismatches and unmatched MBSF)
- non-missing FIPS, income, education, internet
- bene's chosen MA plan (or FFS) appears in the public `structural_panel.csv` choice set

After all filters, the analysis sample N depends on coverage rates; expect ~30K–40K bene-years across 2015–2018.

## Variable provenance

- All MCBS variable names are verified against the 2023 MCBS Survey File codebooks (`~/Downloads/mcbs-survey-file-extracted/Codebooks/{demo,hisumry,myenroll,maplanqx,mcreplnq,genhlth,cenwgts}_2023.txt`).
- The 2015–2018 schema matches the 2023 codebook per the CMS user guide; if a variable is missing in earlier years (e.g., COVID items first appeared in 2020), the `2-extract-mcbs.sas` script will need a year-conditional pull. Sanity-check via `PROC CONTENTS` on the first run.
- MBSF variable names are verified against ResDAC documentation.
