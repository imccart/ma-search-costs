# VRDC analysis (`code/analysis/vrdc/`)

R-side individual-level structural estimation. Runs in a Jupyter analytic container (separate from SAS Enterprise Guide) under an RStudio project rooted at `ma-search/`. Reads the bene panel and utilization panels exported manually from SAS, projects bene utilization through PBP cost-sharing schedules, and estimates the three-stage search model by joint maximum likelihood over the observed search actions and the plan choice (spec in `agents/model.md`).

## Run order

```
_analyze-vrdc.R                master driver
0-project-bene-cost-sharing.R  bene-specific EC[c|i,j] from util x PBP schedule
1-build-bene-choice-panel.R    join bene panel x structural_panel + EC, write checkpoint
2-load-estimation-panel.R      read checkpoint, build markets[] + bene-specific EC vectors
3-individual-likelihood.R      joint search+choice likelihood (random effect, switching cost)
5-estimate-mle.R               simulated MLE (nloptr SBPLX) -> theta_hat
6-fit-diagnostics.R            predicted vs observed untargeted moments
7-standard-errors.R            observed-information SEs (numerical Hessian)
8-mixture-extension.R          finite-mixture c_i (deferred)
```

Source `_analyze-vrdc.R` from the project root to run end-to-end. (The retired
penalty/GMM scripts 4 and `5-estimate-gmm.R` were removed 2026-06-22.)

The bene × plan panel is materialized as a checkpoint so (a) restarts after a crashed optimizer don't re-run the join, (b) diagnostics are easy on the canonical estimation object, and (c) counterfactuals are a `copy(bcp)` away.

## Inputs (in `data/input/`, all CSV)

- `bene_panel.csv` — SAS-exported bene-year panel (data-build script 3)
- `ma_util_panel.csv` — SAS-exported MA encounter utilization (data-build script 4)
- `ffs_util_panel.csv` — SAS-exported FFS claims utilization (data-build script 5)
- `structural_panel.csv` — uploaded local; plan attributes (with prominence cols from `code/data-build/14-build-prominence-vars.R`)
- `plan_county_benefits.csv` — uploaded local; PBP cost-sharing schedule per (plan, county, year)

CSV exports from SAS Enterprise Guide are done manually (PROC EXPORT or right-click "Export") into `ma-search/data/input/` since the Jupyter container and the SAS Enterprise Guide workspace are separate environments.

## Checkpoints (in `data/output/`)

- `bene_cost_sharing.csv` — written by script 0; one row per (bene, plan-in-market) with EC[c|i,j] and Var(C|j)
- `bene_choice_panel.csv` — written by script 1; one row per (bene, plan-in-market) with all attributes joined. Canonical estimation panel.

## Outputs

```
results/vrdc/
  theta_hat.csv                 point estimates + bounds
  fit_diagnostics.csv           predicted vs observed untargeted moments
  search_by_group.csv           predicted vs observed search rate by subgroup
  standard_errors.csv           estimates with observed-information SEs
```

All cells with N < 11 are suppressed before output (CMS small-cell rule).

## Estimation strategy

Joint maximum likelihood over each beneficiary's observed search actions and
plan choice. No penalty and no aggregate-moment targeting — search enters the
likelihood directly. Per beneficiary,

```
L_b = ( prod_w P(choice_bw) ) * E_nu [ prod_w P(actions_bw | nu) ]
```

with `nu ~ N(0,1)` a beneficiary-level search-cost random effect shared across
that beneficiary's waves (its dispersion identified from the panel). The choice
probability is the Goeree consideration-set logit and is independent of `nu`, so
it is computed once per bene-year; only the action likelihood is integrated over
`nu` by simulation.

Search rate, FFS share, and incumbent retention are reported as untargeted fit
in `6-fit-diagnostics.R`, not matched in estimation.

Optimizer: `nloptr` `NLOPT_LN_SBPLX` (gradient-free; the simulated likelihood is
non-smooth in the action thresholds).

## Model spec

Three-stage Plan-Finder-anchored search; full spec in `agents/model.md`. 31 free
parameters:
- Utility (5): alpha, delta, beta, xi_FFS, psi (incumbent premium / switching cost)
- Search cost (8): gamma_0 + log-income, education, age, dual, ADI, handbook
  comprehension, MA tenure
- Dispersion (1): log_sigma_alpha (search-cost random-effect SD)
- Action baselines (4) + cutpoint (1): kappa_info / web / phone / book, tau_gap
  (handbook reading is an ordered 3-level action)
- Consideration breadth (5): b0, b_info, b_web, b_phone, b_book
- Awareness (7): lambda_PF_0 / web / help / delegate, lambda_broker_0 / help / delegate

Search actions are tried-to-find-info, website, and the 1-800 call (binary),
plus ordered handbook reading (none / parts / thorough). All are taken from the
SAS export and recoded in script 1; the handbook reading and comprehension
codings carry a seat-side verification flag. `KCHIHELP` enters consideration as
separate help (`=2`) and delegate (`=3`) terms.

Standard errors: observed-information (inverse numerical Hessian) in
`7-standard-errors.R`. County-clustered bootstrap is the gold standard but
re-estimates the model per replicate and is left as a long-run option.

## Sample restrictions (already applied in script 1)

- `link_status == "ok"` (drop admin-vs-survey mismatches and unmatched MBSF)
- non-missing FIPS, income, education, internet
- bene's chosen MA plan (or FFS) appears in the public `structural_panel.csv` choice set

After all filters, the analysis sample N depends on coverage rates; expect ~30K–40K bene-years across 2015–2018.

## Variable provenance

- All MCBS variable names verified against the year-specific 2015/2016/2017/2018 LDS codebooks (archived in `background/codebooks/`). Several variables were renamed mid-decade (`H_URBRUR`→`H_CBSA`, `ADI`→`CENSADI`→`ADINATNL`, `CS1YRWGT`→`CEYRSWGT`); the year-conditional macro logic in `2-extract-mcbs.sas` resolves these. Full mapping in `agents/data.md`.
- MBSF variable names verified against ResDAC documentation.
- For the post-2018 redesign items (CMPRPLN, RVWCOST, INTERNET, MAMONPRM, MYENROLL segment, etc.), see `agents/data.md` "What's post-2018 only."
