# _analyze-rf.R — Reduced-form analysis
# Run from project root: source("code/analysis/_analyze-rf.R")
#
# Consumes:
#   data/output/analysis_panel.csv  (from _build-rf.R)
#
# Produces tables + figures for the descriptive section, OLS reduced-form,
# Bartik first-stage / RF / 2SLS, methodology-shift decomposition, and the
# consolidated 5-column paper table. Also exports hardcoded paper numbers.

pacman::p_load(tidyverse, modelsummary, kableExtra, fixest, broom)

source("code/analysis/1-descriptive-facts.R")
source("code/analysis/2-reduced-form.R")
source("code/analysis/3-shift-share-iv.R")
source("code/analysis/4-methodology-shift.R")
source("code/analysis/5-paper-table.R")
source("code/analysis/export-paper-numbers.R")
