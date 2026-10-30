# 7-identification-at-truth.R — Check the likelihood at the true parameters on
# a large panel from helpers/dgp.R, before any optimization.
#
# 1. Score test. If the DGP and the likelihood describe the same model, each
#    parameter's summed score at truth is mean zero, and its ratio to the
#    square root of the summed squared scores is approximately N(0, 1). A large
#    ratio points to a mismatch between the generator and the likelihood.
# 2. Curvature. The Hessian at truth, by central differences of the analytic
#    gradient, gives the standard error each parameter would have at this
#    sample size and at the estimation sample's 13,667 beneficiaries, and its
#    smallest eigenvalues give the directions the data pin down least.
# 3. Slices. The objective along the three weakest directions, to check that
#    truth is a maximum and to see how flat each direction is.
#
# Settings come from identification_config if it exists.
# Outputs (results/simulation/identification_<label>/): config.csv,
# score_test.csv, standard_errors.csv, eigen.csv, slices.csv

icfg <- modifyList(list(n_b = 3000L, waves = 3L, n_nu = 31L, seed = 4242L,
                        J_min = 3L, J_max = 8L, h = 1e-4, label = 'base'),
                   if (exists('identification_config')) identification_config else list())
stopifnot(grepl('^[A-Za-z0-9_-]+$', icfg$label))
idir <- file.path('results/simulation', paste0('identification_', icfg$label))
dir.create(idir, recursive = TRUE, showWarnings = FALSE)
fwrite(as.data.table(icfg), file.path(idir, 'config.csv'))

panel <- generate_recovery_panel(icfg$n_b, icfg$waves, recovery_truth, icfg$seed,
                                 icfg$J_min:icfg$J_max)
md_id <- make_model_data(panel$pair, panel$iy, n_nu = icfg$n_nu)
phi_true <- to_free(recovery_truth)
at_truth <- loglik(phi_true, md_id)

# ---- 1. Score test
S <- at_truth$scores
fwrite(data.table(parameter = free_names, score_sum = colSums(S),
                  z = colSums(S) / sqrt(colSums(S^2))),
       file.path(idir, 'score_test.csv'))

# ---- 2. Hessian of the total negative log-likelihood (unit weights)
n_total <- md_id$W_SUM
H <- matrix(0, n_theta, n_theta, dimnames = list(free_names, free_names))
for (k in seq_len(n_theta)) {
  e_k <- replace(numeric(n_theta), k, icfg$h)
  g_plus <- loglik(phi_true + e_k, md_id)$gradient
  g_minus <- loglik(phi_true - e_k, md_id)$gradient
  H[, k] <- (g_plus - g_minus) / (2 * icfg$h) * n_total
  cat(sprintf('Hessian column %d of %d\n', k, n_theta))
  flush.console()
}
H <- (H + t(H)) / 2
V_free <- solve(H)
G <- theta_jacobian(phi_true)
V_struct <- G %*% V_free %*% t(G)
scale_to_sample <- sqrt(icfg$n_b / 13667)
fwrite(data.table(parameter = theta_names, truth = recovery_truth,
                  free_parameter = free_names, se_free = sqrt(diag(V_free)),
                  se = sqrt(diag(V_struct)),
                  se_at_estimation_n = sqrt(diag(V_struct)) * scale_to_sample,
                  t_at_estimation_n = recovery_truth / (sqrt(diag(V_struct)) * scale_to_sample)),
       file.path(idir, 'standard_errors.csv'))

# Eigen-decomposition of the correlation-scaled Hessian, so that eigenvalues are
# comparable across parameters measured in different units.
d <- 1 / sqrt(diag(H))
eg <- eigen(H * outer(d, d), symmetric = TRUE)
weak <- order(eg$values)[1:3]
fwrite(rbindlist(lapply(seq_along(weak), function(r) {
  vec <- eg$vectors[, weak[r]]
  data.table(rank = r, eigenvalue = eg$values[weak[r]], parameter = free_names,
             loading = vec)[order(-abs(loading))]
})), file.path(idir, 'eigen.csv'))

# ---- 3. Slices along the weakest directions, in units of the implied
# standard deviation along each direction at this sample size.
slices <- list()
for (r in seq_along(weak)) {
  dir_free <- d * eg$vectors[, weak[r]]
  step <- 1 / sqrt(eg$values[weak[r]])
  for (s in seq(-3, 3, by = 0.5)) {
    val <- loglik(phi_true + s * step * dir_free, md_id)$value
    slices[[length(slices) + 1L]] <- data.table(rank = r, sd_units = s,
      objective = val, change_in_loglik = -(val - at_truth$value) * n_total)
  }
}
fwrite(rbindlist(slices), file.path(idir, 'slices.csv'))
