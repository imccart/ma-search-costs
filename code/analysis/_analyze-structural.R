# _analyze-structural.R — Structural estimation (Stigler search GMM)
# Run from project root: source("code/analysis/_analyze-structural.R")
#
# Consumes:
#   data/output/structural_panel.csv  (from _build-structural.R)
#   data/output/analysis_panel.csv    (for moment targets: Bartik IVs,
#                                      demographic gradients)
#
# Produces parameter estimates, standard errors, fit diagnostics, and
# counterfactual outputs for the search-cost model documented in
# background/structural-model.md.

pacman::p_load(tidyverse, fixest)

source("code/analysis/structural/1-load-panel.R")
source("code/analysis/structural/2-simulate-shares.R")
source("code/analysis/structural/3b-rf-moments.R")
source("code/analysis/structural/3-moments.R")
source("code/analysis/structural/4-estimate-gmm.R")
source("code/analysis/structural/5-fit-diagnostics.R")

# Roadmap (next):
#   6-counterfactuals.R     Welfare + policy counterfactuals.
