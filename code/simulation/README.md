# Simulation checks

From the project root, run `source("code/simulation/_simulate-all.R")` to reload
the helpers and run all three scripts. Run the toy script before either later
script when sourcing them individually; they reuse its fixtures.

- `1-toy-checks.R`: exact set enumeration, likelihood, parameter transforms,
  and full analytic-gradient checks.
- `2-shuffle-convergence.R`: retained comparison of the older shuffle method
  across counts and seeds. Lower-count FALSE rows are expected. The 40,000-draw
  reference must pass its finite benefit and profile-probability tolerances.
- `3-benefit-quadrature.R`: tests the default deterministic benefit calculation
  against exact enumeration and a finer/wider integration grid, including
  menus up to 80 plans and inclusion probabilities of zero and one; checks
  derivatives and benchmarks likelihood-plus-gradient evaluation against
  40,000 balanced shuffles. Timing is reported, not used as a test threshold.

Outputs are CSVs in `results/simulation/`.

## Parameter recovery

Run the recovery experiment separately from the fast numerical checks:

```r
source('code/simulation/4-parameter-recovery.R')
```

It uses the helpers loaded by the driver and defaults to one pilot panel of 80
beneficiaries, three waves, and three starts (truth plus two perturbed starts),
estimating all 32 parameters. The generator uses exact enumeration for perceived benefits,
independent Bernoulli consideration draws, and Gumbel choice shocks. One normal
search-cost draw persists across a beneficiary's waves; each simulated choice
becomes the next year's incumbent. Initial incumbency is independent of cost.
Premiums are annual dollars. Synthetic menus contain three to six MA plans.

The pilot uses unit survey weights and broad numerical bounds, saved to
`bounds.csv`. Active bounds and both raw and projected gradients are reported.
`converged` requires optimizer success and a projected gradient at most 1e-4.

Results are saved after each start in `results/simulation/recovery_pilot/`:

- `runs.csv`: objectives, convergence, gradients, bounds, evaluations, timing.
- `estimates_all_starts.csv`: truth and estimates for each starting point.
- `best_runs.csv`, `best_estimates.csv`: lowest-objective start in each panel.
- `recovery_summary.csv`: bias and RMSE across best-start estimates.
- `panel_<rep>_{pair,iy,draws}.csv`: the simulated panels.

The generator lives in `helpers/dgp.R` and is shared by scripts 4, 6, and 7.
Menu sizes are set by `J_min` and `J_max` (default 3 to 6; at most 12, since
the generator enumerates consideration sets).

For a larger experiment, configure before sourcing:

```r
recovery_config <- list(n_b = 1000L, waves = 3L, starts = 3L,
                       reps = 10L, maxit = 500L, label = 'mc')
source('code/simulation/4-parameter-recovery.R')
```

A label selects the output directory; rerunning the same label overwrites its
files. Results are checkpointed after each start; resuming is manual.

## Conditional recovery by parameter block

```r
source('code/simulation/6-block-recovery.R')
```

This runs three fits, each with three starts, on the same generated panel:

- Utility: 8 coefficients.
- Search cost: 10 covariate coefficients, persistent dispersion, 4 activity
  costs, and the profile-logit scale (16 parameters).
- Consideration: 4 intensity weights, baseline and intensity slope, and two
  source-exposure weights (8 parameters).

All other parameters stay exactly at truth. Defaults are 80 beneficiaries,
three waves, 31 random-effect nodes, and up to 500 iterations per start.
Set `block_recovery_config <- list(...)` before sourcing to override settings.

Outputs go to `results/simulation/recovery_block_utility/`,
`recovery_block_search_cost/`, and `recovery_block_consideration/`. A combined
`results/simulation/block_recovery_runs.csv` is written when all blocks finish.
Recovery summaries include only estimated parameters; full estimate files
mark fixed parameters with `estimated = FALSE`. Reported convergence gradients
and active-bound counts apply only to the estimated block.

To audit a block afterward:

```r
recovery_label <- 'block_consideration' # or block_utility / block_search_cost
source('code/simulation/5-recovery-diagnostics.R')
```

## Identification at the true parameters

```r
source('code/simulation/7-identification-at-truth.R')
```

Draws one large panel (default 3,000 beneficiaries, three waves, menus of 3 to
8 plans) and evaluates the likelihood at truth without optimizing:

- `score_test.csv`: summed score and its z-ratio for each parameter. Ratios
  far outside about ±3 indicate that the generator and the likelihood
  disagree.
- `standard_errors.csv`: standard errors implied by the Hessian at truth, at
  this sample size and scaled to the estimation sample's 13,667 beneficiaries.
- `eigen.csv`: loadings of the three weakest directions of the
  correlation-scaled Hessian.
- `slices.csv`: change in log-likelihood along those directions, in units of
  the implied standard deviation; a quadratic near -0.5 s^2 means the
  direction is identified at this sample size.

The Hessian takes 64 likelihood-and-gradient evaluations on the full panel.
Set `identification_config <- list(...)` before sourcing to change settings.

## Why shuffles are no longer needed by default

Write `e_j = exp(v_j) / sum_default exp(v)` and let `r` be the common perceived
inclusion probability for a profile. Independence gives the identity

```
E[log(1 + sum_j I_j e_j)]
  = integral_0^infinity exp(-t) [1 - product_j (1-r+r exp(-t e_j))] / t dt.
```

The change of variable `t = exp(x)` uses the existing fixed quadrature grid.
This integrates the same model as the uniform-subset/binomial shuffle method,
with deterministic numerical error instead of Monte Carlo error. Derivatives
are obtained by differentiating the finite quadrature sum; prefix and suffix
products avoid dividing by vanishing factors near full inclusion.

`make_model_data()` now defaults to `benefit_method = "quadrature"` and does
not allocate orderings in that mode. `n_shuffles` matters only when explicitly
using `benefit_method = "shuffle"`. Rebuild any in-memory model-data object
after reloading helpers to switch methods. The shuffle path remains available
for comparison.

The benchmark uses one beneficiary-year per menu size and three timed evaluations.
