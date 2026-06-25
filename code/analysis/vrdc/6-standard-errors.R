# 6-standard-errors.R — observed-information standard errors at theta_hat
#
# Inverts the numerical Hessian of the survey-weighted negative log-likelihood.
# This is a practical, model-based standard error. The gold standard here is a
# county-clustered bootstrap, but that re-estimates the model per replicate and
# is left as a long-run option (loop 4-estimate-mle.R over resampled clusters).
# Parameters resting on a bound (alpha, delta, the lambdas at 0) have invalid
# Hessian-based standard errors and are reported as NA. Requires `theta_hat`,
# `nu_draws`, `theta_lower`, `theta_upper`, and numDeriv.

negll <- function(t) -compute_individual_loglik(t, nu_draws)

cat("\nComputing numerical Hessian for standard errors...\n")
t0 <- Sys.time()
H  <- numDeriv::hessian(negll, theta_hat)
cat(sprintf("  Hessian done in %.1f minutes\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

se <- tryCatch(sqrt(diag(solve(H))), error = function(e) {
  warning("Hessian not invertible; SEs unavailable: ", conditionMessage(e))
  rep(NA_real_, length(theta_hat))
})

at_bound <- theta_hat <= theta_lower + 1e-6 | theta_hat >= theta_upper - 1e-6
se[at_bound] <- NA_real_

out <- data.table(parameter = theta_names, estimate = theta_hat,
                  se = se, z = theta_hat / se)
cat("\n=== Estimates with standard errors ===\n"); print(out)

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(out, file.path(results_dir, "standard_errors.csv"))
cat("\nSaved standard_errors.csv\n")
