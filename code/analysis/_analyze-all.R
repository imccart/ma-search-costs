# _analyze-all.R — Master analysis script
# Run from project root: source("code/analysis/_analyze-all.R")

pacman::p_load(tidyverse, modelsummary, kableExtra, fixest)

source("code/analysis/1-descriptive-facts.R")
source("code/analysis/export-paper-numbers.R")
