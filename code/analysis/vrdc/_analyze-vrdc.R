# _analyze-vrdc.R — Master driver for VRDC structural estimation
#
# Runs the individual-level structural estimation on MCBS 2015-2018
# data extracted by code/data-build/vrdc/. See background/vrdc-plan.md
# for the full plan.
#
# Inputs (must already exist in the VRDC seat):
#   /workspace/pl027710/export/bene_panel.csv         — from data-build
#   /workspace/pl027710/upload/structural_panel.csv   — uploaded from local
#
# Outputs:
#   /workspace/pl027710/export/bene_choice_panel.csv  — estimation checkpoint (script 0)
#   results/vrdc/theta_hat.csv         — point estimates + bounds
#   results/vrdc/fit_diagnostics.csv   — predicted vs observed
#   results/vrdc/se_bootstrap.csv      — clustered bootstrap SEs (deferred)

pacman::p_load(
  tidyverse, fixest, survey, nloptr, data.table
)

# Run from project root: /workspace/pl027710/code/analysis/vrdc/.. -> ..

# 0 builds the canonical bene × plan estimation panel (long format) and
# writes it as a checkpoint. 1 reads the checkpoint and sets up survey
# design + per-market plan-sets. 3 onwards consume the loaded objects.
source("code/analysis/vrdc/0-build-bene-choice-panel.R")
source("code/analysis/vrdc/1-load-estimation-panel.R")
source("code/analysis/vrdc/3-individual-likelihood.R")
source("code/analysis/vrdc/4-aggregate-moments.R")
source("code/analysis/vrdc/5-estimate-gmm.R")
source("code/analysis/vrdc/6-fit-diagnostics.R")

# Mixture extension is opt-in (deferred to a second pass):
# source("code/analysis/vrdc/7-mixture-extension.R")

cat("\nVRDC analysis complete.\n")
