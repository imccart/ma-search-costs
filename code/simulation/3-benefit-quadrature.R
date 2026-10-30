# Run after the toy checks. Accuracy, boundary, and timing checks for the
# deterministic replacement of shuffled perceived inclusive values.
benefit_checks <- list()
benefit_add <- function(check, error, tolerance) {
  benefit_checks[[length(benefit_checks) + 1L]] <<- data.table(
    check = check, value = error, tolerance = tolerance,
    pass = is.finite(error) && error <= tolerance)
}
benefit_grid <- choice_grid()
benefit_fine <- choice_grid(h = 0.15, x_lo = -40)
rates <- c(0, 1e-8, seq(0.05, 0.95, length.out = 72), 1 - 1e-8, 1)
for (J in c(0L, 1L, 8L, 40L, 80L)) {
  ep <- if (J) exp(seq(-8, 8, length.out = J)) else numeric()
  a <- perceived_value_one(ep, rates, benefit_grid)
  fine <- perceived_value_one(ep, rates, benefit_fine)
  benefit_add(paste('value baseline vs fine grid, J =', J), max(abs(a$value - fine$value)), 1e-7)
  benefit_add(paste('rate derivative baseline vs fine grid, J =', J), max(abs(a$dr - fine$dr)), 1e-7)
  benefit_add(paste('zero exposure value, J =', J), abs(a$value[1]), 1e-12)
  benefit_add(paste('full exposure value, J =', J), abs(tail(a$value, 1) - log1p(sum(ep))), 1e-7)
  if (J <= 8) {
    exact <- vapply(rates, function(r) {
      sets <- brute_sets(J)
      # Explicit endpoint handling avoids 0 * log(0) in the reference.
      if (r == 0) return(0)
      if (r == 1) return(log1p(sum(ep)))
      brute_value(1, ep, rep(r, J))
    }, numeric(1))
    benefit_add(paste('value vs enumeration, J =', J), max(abs(a$value - exact)), 1e-7)
  }
  # Check the backward pass directly, including endpoint rates.
  if (J > 0) {
    adj <- cos(seq_along(rates))
    analytic <- perceived_value_one(ep, rates, benefit_grid, adj)$dloge
    # Three directions keep the large-menu derivative check inexpensive;
    # the full model's coordinate gradients are checked in the toy suite.
    directions <- cbind(1, sin(seq_len(J)), cos(seq_len(J)))
    for (d in 1:3) {
      direction <- directions[, d]
      numeric <- grad(function(t) sum(adj * perceived_value_one(
        exp(log(ep) + t * direction), rates, benefit_grid)$value), 0)
      projected <- sum(analytic * direction)
      benefit_add(paste('utility adjoint direction', d, 'vs numerical, J =', J),
                  abs(projected - numeric) / (1 + abs(projected)), 1e-6)
    }
  }
}

# Compare profile probabilities on the same three menus and cost/scale grid
# used in the shuffle study, now against exact set enumeration.
bpq <- brute_profiles(th0)
rq <- loglik(phi0, md8, components = TRUE)$parts$rhohat[1, ]
for (spread in c(1, 4, 8)) {
  ep <- exp(seq(-spread, spread, length.out = 8))
  approx <- perceived_value_one(ep, rq, benefit_grid)$value
  exact <- vapply(rq, function(r) brute_value(1, ep, rep(r, 8)), numeric(1))
  err <- 0
  for (cost in c(0.1, 1, 3)) for (scale in c(0.25, 1, 2)) {
    sm <- function(x) { z <- exp(x - max(x)); z / sum(z) }
    err <- max(err, abs(sm((approx - cost * bpq$k) / scale) - sm((exact - cost * bpq$k) / scale)))
  }
  benefit_add(paste('profile probabilities vs enumeration, spread =', spread), err, 1e-8)
}
benefit_checks <- rbindlist(benefit_checks)
fwrite(benefit_checks, 'results/simulation/checks_benefit_quadrature.csv')
print(benefit_checks)
stopifnot(all(benefit_checks$pass))

# Full likelihood + gradient, one beneficiary-year, three repeated evaluations.
# Do not turn machine-dependent timing into a pass/fail assertion.
benefit_timing <- list()
for (J in c(8L, 40L, 80L)) {
  pair_bench <- toy_rows(1, 0, J, '1')
  for (method in c('quadrature', 'shuffle')) {
    prep <- system.time(mb <- make_model_data(pair_bench, iy8, n_nu = 7L,
                                               benefit_method = method))[['elapsed']]
    elapsed <- replicate(3, system.time(loglik(phi0, mb))[['elapsed']])
    benefit_timing[[length(benefit_timing) + 1L]] <- data.table(
      J = J, method = method, setup_seconds = prep,
      median_eval_seconds = median(elapsed), data_bytes = as.numeric(object.size(mb)))
  }
}
benefit_timing <- rbindlist(benefit_timing)
fwrite(benefit_timing, 'results/simulation/benchmark_benefit.csv')
print(benefit_timing)
