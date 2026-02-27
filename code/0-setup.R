# 0-setup.R — Environment setup for MA Search Costs
# Run this script before any other scripts in the project

# Activate renv (when initialized)
# source("renv/activate.R")

# Load packages
library(tidyverse)
library(modelsummary)
library(kableExtra)
library(fixest)

# Options
options(modelsummary_factory_default = "kableExtra")
