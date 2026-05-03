# _build-rf.R — Build the reduced-form analysis panel
# Run from project root: source("code/data-build/_build-rf.R")
#
# Produces:
#   data/output/dominance_plan.csv     (shared with structural build)
#   data/output/analysis_panel.csv     (county-year panel + Bartik IVs)

pacman::p_load(tidyverse, modelsummary, kableExtra, fixest, tidycensus,
               keyring, cluster, httr, jsonlite)

source("code/data-build/1-build-enrollment.R")
source("code/data-build/2-build-plan-benefits.R")
source("code/data-build/3-cluster.R")
source("code/data-build/4-merge-plan-county.R")
source("code/data-build/5-construct-dominance.R")
source("code/data-build/6-pull-acs.R")
source("code/data-build/7-merge-acs.R")
source("code/data-build/8-policy-shocks.R")
source("code/data-build/9-shift-share.R")
