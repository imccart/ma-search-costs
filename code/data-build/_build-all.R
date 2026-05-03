# _build-all.R — Full data build (RF + structural)
# Run from project root: source("code/data-build/_build-all.R")
#
# Runs the RF chain (scripts 1-9), then appends the structural-only tail
# (scripts 10-13). Use _build-rf.R or _build-structural.R for targeted
# rebuilds; this driver is the one-stop reproduce-from-scratch entry point.

source("code/data-build/_build-rf.R")

source("code/data-build/10-build-ffs-outside.R")
source("code/data-build/11-pull-penetration.R")
source("code/data-build/12-pull-cbp-broker.R")
source("code/data-build/13-assemble-structural-panel.R")
