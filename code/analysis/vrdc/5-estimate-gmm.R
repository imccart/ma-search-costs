# 5-estimate-gmm.R — Joint MLE + aggregate-moment estimator
#
# Uses nloptr SLSQP with bounds (alpha, delta, eta_1, eta_2 >= 0). The
# objective is `compute_combined_objective` from 4-aggregate-moments.R,
# which is -loglik + LAMBDA * sum(M^2).

# ---- Initial values ------------------------------------------------
# Anchor near the local 7-moment public estimate where applicable, else 0.

theta0 <- c(
  alpha       = 0.59,
  delta       = 0.10,    # local was 0 at lower bound; start small positive
  beta        = 0.62,
  xi_FFS      = 5.99,
  gamma_0     = -2.32,
  gamma_inc   = 0.0,
  gamma_educ  = 0.0,
  gamma_age   = 0.0,
  gamma_inet  = -0.5,    # internet should reduce c
  gamma_dual  = 0.5,     # dual-eligible higher c (information-poor proxy)
  gamma_adi   = 0.5,     # higher ADI → higher c
  eta_1       = 0.50,
  eta_2       = 0.59
)
stopifnot(length(theta0) == length(theta_names))


# ---- Optimize ------------------------------------------------------

cat("\nStarting GMM estimation (nloptr SLSQP)...\n")
t0 <- Sys.time()

fit <- nloptr(
  x0     = theta0,
  eval_f = compute_combined_objective,
  lb     = theta_lower,
  ub     = theta_upper,
  opts   = list(
    algorithm   = "NLOPT_LN_SBPLX",   # SLSQP needs gradients; SBPLX is gradient-free
    xtol_rel    = 1e-5,
    ftol_rel    = 1e-6,
    maxeval     = 2000,
    print_level = 1
  )
)

t1 <- Sys.time()
cat(sprintf("\nElapsed: %.1f minutes\n",
            as.numeric(difftime(t1, t0, units = "mins"))))


# ---- Extract estimates --------------------------------------------

theta_hat <- setNames(fit$solution, theta_names)
final_obj <- fit$objective
final_ll  <- compute_individual_loglik(theta_hat)
final_m   <- compute_aggregate_moments(theta_hat)

cat("\n=== Estimates ===\n")
print(round(theta_hat, 4))
cat(sprintf("\nLog-likelihood : %.2f\n", final_ll))
cat("\nMoment residuals (predicted - observed):\n")
print(round(final_m, 4))


# ---- Save ----------------------------------------------------------

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

theta_df <- tibble(
  parameter = theta_names,
  estimate  = theta_hat,
  lower_bd  = theta_lower,
  upper_bd  = theta_upper
)
fwrite(theta_df, file.path(results_dir, "theta_hat.csv"))
cat("\nSaved theta_hat to ", file.path(results_dir, "theta_hat.csv"), "\n", sep = "")

moments_df <- tibble(
  moment = names(final_m),
  obs    = c(m1_obs, m2_obs, m3_obs),
  pred   = c(m1_obs, m2_obs, m3_obs) + final_m,
  resid  = final_m
)
fwrite(moments_df, file.path(results_dir, "moments_fit.csv"))
