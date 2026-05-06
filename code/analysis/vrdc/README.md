# VRDC analysis (`code/analysis/vrdc/`)

R-side individual-level structural estimation. Runs in a Jupyter analytic container (separate from SAS Enterprise Guide) under an RStudio project rooted at `ma-search/`. Reads the bene panel and utilization panels exported manually from SAS, projects bene utilization through PBP cost-sharing schedules, and estimates the Stigler-search model via combined MLE + aggregate moments.

## Run order

```
_analyze-vrdc.R                 master driver
0a-project-bene-cost-sharing.R  bene-specific EC[c|i,j] from util x PBP schedule
0-build-bene-choice-panel.R     join bene panel x structural_panel + EC, write checkpoint
1-load-estimation-panel.R       read checkpoint, declare svydesign, build markets[] list
3-individual-likelihood.R       per-bene Stigler-search choice probability
4-aggregate-moments.R           survey-weighted aggregate moments
5-estimate-gmm.R                nloptr SBPLX optimization
6-fit-diagnostics.R              predicted vs observed by demographic group
7-mixture-extension.R           finite-mixture c_i (deferred)
```

Source `_analyze-vrdc.R` from the project root to run end-to-end.

The bene × plan panel is materialized as a checkpoint so (a) restarts after a crashed optimizer don't re-run the join, (b) diagnostics are easy on the canonical estimation object, and (c) counterfactuals are a `copy(bcp)` away.

## Inputs (in `data/input/`, all CSV)

- `bene_panel.csv` — SAS-exported bene-year panel (data-build script 3)
- `ma_util_panel.csv` — SAS-exported MA encounter utilization (data-build script 4)
- `ffs_util_panel.csv` — SAS-exported FFS claims utilization (data-build script 5)
- `structural_panel.csv` — uploaded local; plan attributes (with prominence cols from `code/data-build/14-build-prominence-vars.R`)
- `plan_county_benefits.csv` — uploaded local; PBP cost-sharing schedule per (plan, county, year)

CSV exports from SAS Enterprise Guide are done manually (PROC EXPORT or right-click "Export") into `ma-search/data/input/` since the Jupyter container and the SAS Enterprise Guide workspace are separate environments.

## Checkpoints (in `data/output/`)

- `bene_cost_sharing.csv` — written by script 0a; one row per (bene, plan-in-market) with EC[c|i,j] and Var(C|j)
- `bene_choice_panel.csv` — written by script 0; one row per (bene, plan-in-market) with all attributes joined. Canonical estimation panel.

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

## Model spec

Three-stage Plan-Finder-anchored ordered search; full spec in `agents/model.md`. 21 free parameters:
- Utility (4): alpha, delta, beta, xi_FFS
- Search-cost heterogeneity (9): gamma_0, gamma_inc, gamma_educ, gamma_age, gamma_dual, gamma_adi, gamma_net (KVSITWEB), gamma_help (KCHIHELP=2), gamma_delegate (KCHIHELP=3)
- Awareness/prominence (8): lambda_PF_0, lambda_PF_online, lambda_PF_help, lambda_PF_delegate, lambda_broker_0, lambda_broker_help, lambda_broker_delegate, lambda_inc

`KCHIHELP=2` ("gets help") and `KCHIHELP=3` ("someone else decides") are entered as separate dummies, per the §1 mitigation in `agents/limitations.md`. Pooled-KCHIHELP estimation is a robustness check, not the baseline.

Standard errors: bootstrap clustered at `state_cnty_fips` (deferred). Or sandwich SEs from the Hessian × outer-product-of-gradient at theta_hat.

## Sample restrictions (already applied in script 1)

- `link_status == "ok"` (drop admin-vs-survey mismatches and unmatched MBSF)
- non-missing FIPS, income, education, internet
- bene's chosen MA plan (or FFS) appears in the public `structural_panel.csv` choice set

After all filters, the analysis sample N depends on coverage rates; expect ~30K–40K bene-years across 2015–2018.

## Variable provenance

- All MCBS variable names verified against the year-specific 2015/2016/2017/2018 LDS codebooks (archived in `background/codebooks/`). Several variables were renamed mid-decade (`H_URBRUR`→`H_CBSA`, `ADI`→`CENSADI`→`ADINATNL`, `CS1YRWGT`→`CEYRSWGT`); the year-conditional macro logic in `2-extract-mcbs.sas` resolves these. Full mapping in `agents/data.md`.
- MBSF variable names verified against ResDAC documentation.
- For the post-2018 redesign items (CMPRPLN, RVWCOST, INTERNET, MAMONPRM, MYENROLL segment, etc.), see `agents/data.md` "What's post-2018 only."
