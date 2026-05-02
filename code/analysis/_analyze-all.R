# _analyze-all.R — Master analysis script
# Run from project root: source("code/analysis/_analyze-all.R")

pacman::p_load(tidyverse, modelsummary, kableExtra, fixest, broom)

source("code/analysis/1-descriptive-facts.R")
source("code/analysis/2-reduced-form.R")
source("code/analysis/3-shift-share-iv.R")
source("code/analysis/4-methodology-shift.R")
source("code/analysis/export-paper-numbers.R")
