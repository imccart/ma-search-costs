# _build-all.R — Master data build script
# Run from project root: source("code/data-build/_build-all.R")

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
source("code/data-build/10-build-ffs-outside.R")
source("code/data-build/11-pull-penetration.R")
source("code/data-build/13-pull-cbp-broker.R")
source("code/data-build/12-assemble-structural-panel.R")
