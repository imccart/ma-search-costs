# _build-structural.R — Build the structural plan-attributes panel
# Run from project root: source("code/data-build/_build-structural.R")
#
# Produces:
#   data/output/dominance_plan.csv     (shared with RF build)
#   data/output/ffs_outside.csv
#   data/output/penetration.csv
#   data/output/cbp_broker.csv
#   data/output/structural_panel.csv   (county x plan x year, GMM input)

pacman::p_load(tidyverse, modelsummary, kableExtra, fixest, tidycensus,
               keyring, cluster, httr, jsonlite)

source("code/data-build/1-build-enrollment.R")
source("code/data-build/2-build-plan-benefits.R")
source("code/data-build/3-cluster.R")
source("code/data-build/4-merge-plan-county.R")
source("code/data-build/5-construct-dominance.R")
source("code/data-build/10-build-ffs-outside.R")
source("code/data-build/11-pull-penetration.R")
source("code/data-build/12-pull-cbp-broker.R")
source("code/data-build/13-assemble-structural-panel.R")
source("code/data-build/14-build-prominence-vars.R")
