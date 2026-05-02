# _build-all.R — Master data build script
# Run from project root: source("code/data-build/_build-all.R")

pacman::p_load(tidyverse, modelsummary, kableExtra, fixest, tidycensus, keyring)

source("code/data-build/1-build-enrollment.R")
source("code/data-build/2-build-plan-benefits.R")
source("code/data-build/3-merge-plan-county.R")
source("code/data-build/4-construct-dominance.R")
source("code/data-build/5-pull-acs.R")
source("code/data-build/6-merge-acs.R")
