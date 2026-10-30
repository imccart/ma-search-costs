# 4-parameter-recovery.R — Estimate the model on panels drawn from the DGP in
# helpers/dgp.R. Settings come from recovery_config if it exists, e.g.
#   recovery_config <- list(n_b = 1000L, waves = 3L, starts = 3L, reps = 10L,
#                           maxit = 500L, label = 'mc', block = 'joint')
# block is 'joint' (all parameters), 'utility', 'search_cost', or
# 'consideration'; parameters outside the block stay at truth.
#
# Outputs (results/simulation/recovery_<label>/): config.csv, bounds.csv,
# panel_<rep>_{pair,iy,draws}.csv, runs.csv, estimates_all_starts.csv,
# best_runs.csv, best_estimates.csv, recovery_summary.csv

cfg <- modifyList(list(n_b = 80L, waves = 3L, starts = 3L, reps = 1L, maxit = 200L,
                       n_nu = 15L, seed = 1729L, label = 'pilot', block = 'joint',
                       J_min = 3L, J_max = 6L),
                  if (exists('recovery_config')) recovery_config else list())
stopifnot(cfg$n_b > 0, cfg$waves > 0, cfg$starts >= 2, cfg$reps > 0,
          grepl('^[A-Za-z0-9_-]+$', cfg$label))
outdir <- file.path('results/simulation', paste0('recovery_', cfg$label))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
fwrite(as.data.table(cfg), file.path(outdir, 'config.csv'))

# Optimizer bounds in free coordinates.
lo <- setNames(rep(-3, n_theta), free_names)
hi <- setNames(rep(3, n_theta), free_names)
lo[c('alpha_prem', 'alpha_oop')] <- .01
lo['b0'] <- -6
hi['b0'] <- 2
lo['gamma_0'] <- -4
hi['gamma_0'] <- 2
hi['log_b1'] <- 1.5
phi_true <- to_free(recovery_truth)
active <- switch(cfg$block,
  joint = seq_along(free_names),
  utility = match(utility_names, theta_names),
  search_cost = match(c(gamma_names, 'sigma_alpha', kappa_names, 'sigma_p'), theta_names),
  consideration = match(c(omega_names, 'b0', 'b1', 'lambda_PF', 'lambda_B'), theta_names),
  stop('Unknown recovery block: ', cfg$block))
fwrite(data.table(parameter = free_names, truth = phi_true, lower = lo, upper = hi),
       file.path(outdir, 'bounds.csv'))

all_runs <- all_estimates <- list()
for (rep_id in seq_len(cfg$reps)) {
  panel <- generate_recovery_panel(cfg$n_b, cfg$waves, recovery_truth, cfg$seed + rep_id,
                                   cfg$J_min:cfg$J_max)
  write_panel(panel, outdir, sprintf('%03d', rep_id))
  stopifnot(all(panel$pair[, sum(is_chosen), by = iy]$V1 == 1L),
            all(panel$draws$considered >= 1))
  md_rec <- make_model_data(panel$pair, panel$iy, n_nu = cfg$n_nu)
  truth_fit <- loglik(phi_true, md_rec)
  for (start in seq_len(cfg$starts)) {
    set.seed(cfg$seed + 10000L * rep_id + start)
    initial <- if (start == 1) phi_true else pmin(hi - .01, pmax(lo + .01, phi_true + rnorm(n_theta, 0, .4)))
    initial <- initial[active]
    cache_p <- NULL
    cache_result <- NULL
    evaluations <- 0L
    evaluate <- function(p) {
      if (is.null(cache_p) || !identical(p, cache_p)) {
        full_p <- phi_true
        full_p[active] <- p
        cache_result <<- loglik(full_p, md_rec)
        cache_p <<- p
        evaluations <<- evaluations + 1L
        if (evaluations %% 25 == 0) {
          cat(sprintf('rep %d start %d eval %d objective %.6f max|g| %.3g\n', rep_id, start,
                      evaluations, cache_result$value, max(abs(cache_result$gradient[active]))))
          flush.console()
        }
      }
      cache_result
    }
    elapsed <- system.time(fit <- optim(initial, function(p) evaluate(p)$value,
      function(p) evaluate(p)$gradient[active], method = 'L-BFGS-B', lower = lo[active],
      upper = hi[active], control = list(maxit = cfg$maxit, factr = 1e7, pgtol = 1e-5)))[['elapsed']]
    par <- phi_true
    par[active] <- fit$par
    final <- loglik(setNames(par, free_names), md_rec)
    projected <- (par - pmin(hi, pmax(lo, par - final$gradient)))[active]
    bound <- abs(par - lo) < 1e-5 | abs(par - hi) < 1e-5
    run <- data.table(replication = rep_id, start = start, block = cfg$block,
      objective = final$value, objective_at_truth = truth_fit$value,
      convergence_code = fit$convergence, message = if (is.null(fit$message)) '' else fit$message,
      max_gradient = max(abs(final$gradient[active])), max_projected_gradient = max(abs(projected)),
      converged = fit$convergence == 0 && max(abs(projected)) <= 1e-4,
      bounds_hit = sum(bound[active]), evaluations = evaluations, seconds = elapsed)
    est <- data.table(replication = rep_id, start = start, parameter = theta_names,
      truth = recovery_truth, estimate = to_struct(setNames(par, free_names)),
      free_parameter = free_names, free_estimate = par, bound_hit = bound,
      estimated = seq_along(theta_names) %in% active)
    all_runs[[length(all_runs) + 1L]] <- run
    all_estimates[[length(all_estimates) + 1L]] <- est
    fwrite(rbindlist(all_runs), file.path(outdir, 'runs.csv'))
    fwrite(rbindlist(all_estimates), file.path(outdir, 'estimates_all_starts.csv'))
  }
}
runs <- rbindlist(all_runs)
estimates <- rbindlist(all_estimates)
best <- runs[order(objective), .SD[1], by = replication]
best_estimates <- merge(estimates, best[, .(replication, start, converged)],
                        by = c('replication', 'start'))
recovery_summary <- best_estimates[estimated == TRUE, .(truth = first(truth),
  mean_estimate = mean(estimate), bias = mean(estimate - truth),
  rmse = sqrt(mean((estimate - truth)^2)), replications = .N,
  converged_replications = sum(converged)), by = parameter]
fwrite(best, file.path(outdir, 'best_runs.csv'))
fwrite(best_estimates, file.path(outdir, 'best_estimates.csv'))
fwrite(recovery_summary, file.path(outdir, 'recovery_summary.csv'))
