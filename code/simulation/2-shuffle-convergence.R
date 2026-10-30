# Run after 1-toy-checks.R. Compare fixed shuffle approximations with exact
# enumeration on eight-plan menus. These are numerical, not statistical,
# tolerances: 0.01 choice-utility units and 0.002 absolute profile probability.
# The profile check includes sigma_p = 0.25, 1, 2 and costs = 0.1, 1, 3.

shuffle_counts <- c(50L, 200L, 1000L, 5000L, 20000L, 40000L)
shuffle_seeds <- 1:10
shuffle_value_tol <- 0.01
shuffle_profile_tol <- 0.002
shuffle_rows <- list()
softmax_rows <- function(z) {
  ez <- exp(z - apply(z, 1, max))
  ez / rowSums(ez)
}
bp <- brute_profiles(th0)
cases <- list(
  toy = exp(brute_utility(th0, pair8)[!pair8$is_default] -
              brute_utility(th0, pair8)[pair8$is_default]),
  spread4 = exp(seq(-4, 4, length.out = 8)),
  spread8 = exp(seq(-8, 8, length.out = 8))
)
# Source means are those of the original toy menu; all profiles are tested.
rh <- 1 - (1 - bp$s) * (1 - th0[['lambda_PF']] * mean(pair8$pf[!pair8$is_default]) * bp$R) *
  (1 - th0[['lambda_B']] * mean(pair8$b_score[!pair8$is_default]))
binom <- vapply(0:8, function(k) dbinom(k, 8, rh), numeric(length(bp$k)))
ind <- brute_sets(8)
sz <- rowSums(ind)
for (case in names(cases)) {
  ep <- cases[[case]]
  vals <- log1p(drop(ind %*% ep))
  exact_curve <- vapply(0:8, function(k) mean(vals[sz == k]), numeric(1))
  exact <- drop(binom %*% exact_curve)
  for (seed in shuffle_seeds) {
    # Use the estimator's ordering generator. Prefixes give nested samples.
    mdc <- make_model_data(pair8, iy8, n_shuffles = max(shuffle_counts),
                          n_nu = 1L, seed = seed, benefit_method = 'shuffle')
    g <- mdc$groups[[1]]
    # G contains global pair-row indices; ep is indexed within placed rows.
    base <- ep[match(as.vector(g$G), which(!pair8$is_default))]
    draws <- matrix(base[g$perm], nrow = 8)
    sums <- draws
    for (k in 2:8) sums[k, ] <- sums[k - 1, ] + draws[k, ]
    curves <- log1p(sums)
    for (S in shuffle_counts) {
      approx <- drop(binom %*% c(0, rowMeans(curves[, seq_len(S), drop = FALSE])))
      prob_error <- 0
      for (scale in c(0.25, 1, 2)) {
        ze <- (matrix(exact, 3, length(bp$k), byrow = TRUE) - outer(c(0.1, 1, 3), bp$k)) / scale
        za <- (matrix(approx, 3, length(bp$k), byrow = TRUE) - outer(c(0.1, 1, 3), bp$k)) / scale
        prob_error <- max(prob_error, abs(softmax_rows(za) - softmax_rows(ze)))
      }
      shuffle_rows[[length(shuffle_rows) + 1L]] <- data.table(
        menu = case, seed = seed, shuffles = S,
        value_error = max(abs(approx - exact)), profile_error = prob_error)
    }
  }
}
shuffle_results <- rbindlist(shuffle_rows)
shuffle_summary <- shuffle_results[, .(
  max_value_error = max(value_error), max_profile_error = max(profile_error)
), by = shuffles]
shuffle_summary[, pass := max_value_error <= shuffle_value_tol &
                            max_profile_error <= shuffle_profile_tol]
fwrite(shuffle_results, 'results/simulation/checks_shuffle.csv')
fwrite(shuffle_summary, 'results/simulation/checks_shuffle_summary.csv')
print(shuffle_summary)
default_shuffles <- eval(formals(make_model_data)$n_shuffles)
stopifnot(default_shuffles %in% shuffle_summary$shuffles,
          shuffle_summary[shuffles == default_shuffles, pass])

# Verify the fast convergence calculation against the actual likelihood's
# forward pass and check the position balance that supplies variance reduction.
balance <- balanced_perms(8L, 80L)
stopifnot(all(vapply(1:8, function(k) all(tabulate(balance[k, ], 8) == 10), logical(1))))
integration_checks <- list()
for (case in names(cases)) {
  mc <- make_model_data(pair8, iy8, n_nu = 1L, seed = 1L, benefit_method = 'shuffle')
  # Set the utility design directly to realize the test utilities (default 0).
  mc$pair$X[,] <- 0
  mc$pair$X[!pair8$is_default, 'xi_TM'] <- log(cases[[case]]) / th0[['xi_TM']]
  actual <- loglik(phi0, mc, components = TRUE)$parts$Vhat[1, ]
  exact <- vapply(rh, function(r) brute_value(1, cases[[case]], rep(r, 8)), numeric(1))
  expected_error <- shuffle_results[menu == case & seed == 1 & shuffles == default_shuffles,
                                    value_error]
  integration_checks[[case]] <- data.table(menu = case,
    discrepancy = abs(max(abs(actual - exact)) - expected_error))
}
integration_checks <- rbindlist(integration_checks)
fwrite(integration_checks, 'results/simulation/checks_shuffle_integration.csv')
stopifnot(all(integration_checks$discrepancy < 1e-10))

# The main toy gradient checks use quadrature. Also exercise the
# sampled, balanced-ordering path, using a small draw count for this derivative
# check (which tests differentiation, not approximation accuracy).
md_sampled <- make_model_data(pair8, iy8, n_shuffles = 200L, n_nu = 7L, seed = 1L,
                              benefit_method = 'shuffle')
analytic <- loglik(phi0, md_sampled)$gradient
numeric <- grad(function(p) loglik(setNames(p, free_names), md_sampled)$value, unname(phi0))
sampled_gradient <- data.table(parameter = free_names, analytic = analytic,
  numeric = numeric, rel_diff = abs(analytic - numeric) / (1 + abs(analytic)))
fwrite(sampled_gradient, 'results/simulation/checks_shuffle_gradient.csv')
stopifnot(max(sampled_gradient$rel_diff) < 1e-6)
