# _simulate-all.R — Monte Carlo study.
# Helper functions are used both here and in main analysis (hence one location for both analyses)

pacman::p_load(tidyverse, nloptr, data.table, numDeriv)

source("code/analysis/vrdc/helpers/theta.R")
source("code/analysis/vrdc/helpers/quadrature.R")
source("code/analysis/vrdc/helpers/model-utility.R")
source("code/analysis/vrdc/helpers/model-search.R")
source("code/analysis/vrdc/helpers/model-choice.R")
source("code/analysis/vrdc/helpers/model-data.R")
source("code/analysis/vrdc/helpers/model-loglik.R")
source("code/simulation/helpers/dgp.R")

source("code/simulation/1-toy-checks.R")
source("code/simulation/2-shuffle-convergence.R")
source("code/simulation/3-benefit-quadrature.R")
# source("code/simulation/4-parameter-recovery.R")
# source("code/simulation/5-recovery-diagnostics.R")
# source("code/simulation/6-block-recovery.R")
source("code/simulation/7-identification-at-truth.R")


p2 <- generate_recovery_panel(3000L, 3L, recovery_truth, seed = 777L, J_range = 3:8)
f2 <- loglik(to_free(recovery_truth), make_model_data(p2$pair, p2$iy, n_nu = 31L))
z2 <- colSums(f2$scores) / sqrt(colSums(f2$scores^2))
fwrite(data.table(parameter = free_names, z = z2),
       "results/simulation/identification_base/score_test_seed777.csv")
round(sort(z2, decreasing = TRUE)[c(1:3, (n_theta - 2):n_theta)], 2)

for (b in c("search_cost", "consideration")) {
  recovery_config <- list(n_b = 3000L, waves = 3L, starts = 2L, reps = 1L,
                          maxit = 300L, n_nu = 31L, J_min = 3L, J_max = 8L,
                          block = b, label = paste0("block3000_", b))
  source("code/simulation/4-parameter-recovery.R")
}