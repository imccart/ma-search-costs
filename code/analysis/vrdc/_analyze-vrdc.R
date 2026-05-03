# _analyze-vrdc.R — Master driver for VRDC structural estimation
#
# Runs the individual-level structural estimation on MCBS 2015-2018
# data extracted by code/data-build/vrdc/. See background/vrdc-plan.md
# for the full plan.
#
# Inputs (must already exist in the VRDC seat):
#   /workspace/pl027710/export/bene_panel.csv         — from data-build
#   /workspace/pl027710/upload/structural_panel.csv   — uploaded from local
#   /workspace/pl027710/upload/analysis_panel.csv     — uploaded from local
#
# Outputs:
#   results/vrdc/theta_hat.csv         — point estimates + bounds
#   results/vrdc/fit_diagnostics.csv   — predicted vs observed
#   results/vrdc/se_bootstrap.csv      — clustered bootstrap SEs (deferred)

pacman::p_load(
  tidyverse, fixest, survey, nloptr, data.table
)

# Run from project root: /workspace/pl027710/code/analysis/vrdc/.. -> ..
source("code/analysis/vrdc/1-load-bene-panel.R")
source("code/analysis/vrdc/2-build-choice-sets.R")
source("code/analysis/vrdc/3-individual-likelihood.R")
source("code/analysis/vrdc/4-aggregate-moments.R")
source("code/analysis/vrdc/5-estimate-gmm.R")
source("code/analysis/vrdc/6-fit-diagnostics.R")

# Mixture extension is opt-in (deferred to a second pass):
# source("code/analysis/vrdc/7-mixture-extension.R")

cat("\nVRDC analysis complete.\n")
