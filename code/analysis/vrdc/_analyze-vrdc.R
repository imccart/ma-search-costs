# _analyze-vrdc.R — Master driver for VRDC structural estimation
#
# Runs the individual-level structural estimation on MCBS 2015-2018
# data extracted by code/data-build/vrdc/. See background/vrdc-plan.md
# for the full plan.
#
# Inputs (RStudio project root = ma-search/, run from there):
#   data/input/bene_panel.csv             SAS export from data-build script 3
#   data/input/ma_util_panel.csv          SAS export from data-build script 4
#   data/input/ffs_util_panel.csv         SAS export from data-build script 5
#   data/input/structural_panel.csv       uploaded local plan attributes
#   data/input/plan_county_benefits.csv   uploaded local PBP cost-sharing
#
# Outputs:
#   data/output/bene_cost_sharing.csv  bene-plan EC[c|i,j] (script 0)
#   data/output/bene_choice_panel.csv  estimation checkpoint (script 1)
#   results/vrdc/theta_hat.csv         point estimates + bounds
#   results/vrdc/fit_diagnostics.csv   predicted vs observed
#   results/vrdc/se_bootstrap.csv      clustered bootstrap SEs (deferred)

pacman::p_load(
  tidyverse, fixest, survey, nloptr, data.table
)

# 0 builds bene-specific EC[c|i,j] and Var(C|j) by projecting each bene's
#   claims utilization through every plan's PBP cost-sharing schedule.
# 1 builds the canonical bene × plan estimation panel (long format) joining
#   bene attributes, plan attributes, and the bene-specific cost-sharing.
# 2 reads the checkpoint and sets up survey design + per-market plan-sets.
# 3 onwards consume the loaded objects.
source("code/analysis/vrdc/0-project-bene-cost-sharing.R")
source("code/analysis/vrdc/1-build-bene-choice-panel.R")
source("code/analysis/vrdc/2-load-estimation-panel.R")
source("code/analysis/vrdc/3-individual-likelihood.R")
source("code/analysis/vrdc/4-aggregate-moments.R")
source("code/analysis/vrdc/5-estimate-gmm.R")
source("code/analysis/vrdc/6-fit-diagnostics.R")

# Mixture extension is opt-in (deferred to a second pass):
# source("code/analysis/vrdc/7-mixture-extension.R")

cat("\nVRDC analysis complete.\n")
