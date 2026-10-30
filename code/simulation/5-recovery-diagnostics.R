# 5-recovery-diagnostics.R — Integration accuracy and agreement across starts
# for one recovery run. Reads results/simulation/recovery_<label>/, where label
# is recovery_label if it exists and 'pilot' otherwise.
#
# Outputs: integration_diagnostics.csv, start_agreement.csv, parameter_spread.csv

label <- if (exists('recovery_label')) recovery_label else 'pilot'
stopifnot(grepl('^[A-Za-z0-9_-]+$', label))
folder <- file.path('results/simulation', paste0('recovery_', label))
configuration <- fread(file.path(folder, 'config.csv'))
runs <- fread(file.path(folder, 'runs.csv'))
bounds <- fread(file.path(folder, 'bounds.csv'))
all_estimates <- fread(file.path(folder, 'estimates_all_starts.csv'))
active <- which(all_estimates[replication == runs$replication[1] & start == runs$start[1], estimated])

diagnostics <- list()
for (rep_id in unique(runs$replication)) {
  best <- runs[replication == rep_id][which.min(objective)]
  phi <- all_estimates[replication == rep_id & start == best$start,
                       setNames(free_estimate, free_parameter)]
  panel <- read_panel(folder, sprintf('%03d', rep_id))
  for (point in c('truth', 'estimate')) {
    p <- if (point == 'truth') setNames(bounds$truth, bounds$parameter) else phi
    base <- loglik(p, make_model_data(panel$pair, panel$iy, n_nu = configuration$n_nu))
    for (nodes in c(31L, 61L)) {
      fine <- loglik(p, make_model_data(panel$pair, panel$iy, n_nu = nodes))
      diagnostics[[length(diagnostics) + 1L]] <- data.table(
        replication = rep_id, point = point, check = 'random_effect_nodes', setting = nodes,
        objective_difference = abs(base$value - fine$value),
        max_gradient_difference = max(abs(base$gradient[active] - fine$gradient[active])))
    }
    fine <- loglik(p, make_model_data(panel$pair, panel$iy, n_nu = configuration$n_nu,
      grid = choice_grid(h = .15, x_lo = -40)))
    diagnostics[[length(diagnostics) + 1L]] <- data.table(
      replication = rep_id, point = point, check = 'benefit_choice_grid', setting = .15,
      objective_difference = abs(base$value - fine$value),
      max_gradient_difference = max(abs(base$gradient[active] - fine$gradient[active])))
  }
}
fwrite(rbindlist(diagnostics), file.path(folder, 'integration_diagnostics.csv'))
fwrite(runs[, .(starts = .N, converged_starts = sum(converged),
                objective_spread = max(objective) - min(objective),
                min_projected_gradient = min(max_projected_gradient),
                max_bounds_hit = max(bounds_hit)), by = replication],
       file.path(folder, 'start_agreement.csv'))
fwrite(all_estimates[, .(min_estimate = min(estimate), max_estimate = max(estimate),
                         range = max(estimate) - min(estimate)), by = .(replication, parameter)],
       file.path(folder, 'parameter_spread.csv'))
