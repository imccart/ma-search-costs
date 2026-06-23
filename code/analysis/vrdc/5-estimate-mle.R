# 5-estimate-mle.R — Simulated maximum likelihood (joint search + choice)
#
# Rewritten 2026-06-22. Replaces 5-estimate-gmm.R. No penalty, no LAMBDA. The
# objective is the negative survey-weighted simulated log-likelihood from
# 3-individual-likelihood.R, normalized to a per-weight mean so it is O(1).
# Optimized with nloptr SBPLX (derivative-free; the simulated likelihood is not
# smooth in the kappas/cutpoints, so a gradient method is inappropriate).

# ---- Common random numbers for the random effect --------------------------
# Deterministic quasi-MC draws (quantiles) so the objective is reproducible and
# does not jitter across optimizer evaluations. Shared across beneficiaries;
# each bene uses the same N_SIM_DRAWS draws, but the draw is the bene's nu_b.
nu_draws <- qnorm((seq_len(N_SIM_DRAWS) - 0.5) / N_SIM_DRAWS)

# Per-weight normalizer: sum of survey weights over unique beneficiaries.
W_SUM <- bene[!duplicated(bene$BASE_ID), sum(wgt_full_sample)]


# ---- Initial values -------------------------------------------------------
theta0 <- c(
  alpha = 0.60, delta = 0.10, beta = 0.50, xi_FFS = 6.00, psi = 1.00,
  gamma_0 = -2.00, gamma_inc = 0, gamma_educ = 0, gamma_age = 0,
  gamma_dual = 0.30, gamma_adi = 0.10, gamma_hb = 0.20, gamma_exp = -0.10,
  log_sigma_alpha = log(0.5),
  kappa_info = 0.50, kappa_web = 0.50, kappa_phone = 1.00, kappa_book = 0.50,
  tau_gap = 1.00,
  b0 = 0, b_info = 0.30, b_web = 0.80, b_phone = 0.30, b_book = 0.30,
  lambda_PF_0 = 0.50, lambda_PF_web = 1.00, lambda_PF_help = 0.30, lambda_PF_delegate = 0.20,
  lambda_broker_0 = 0.50, lambda_broker_help = 0.50, lambda_broker_delegate = 0.80
)
stopifnot(identical(names(theta0), theta_names))


# ---- Objective ------------------------------------------------------------
neg_ll <- function(theta) {
  -compute_individual_loglik(theta, nu_draws) / W_SUM
}

cat("\nStarting simulated MLE (nloptr SBPLX)...\n")
cat(sprintf("  beneficiaries: %d   bene-years: %d   draws: %d\n",
            uniqueN(bene$BASE_ID), nrow(bene), N_SIM_DRAWS))
t0 <- Sys.time()

fit <- nloptr(
  x0     = theta0,
  eval_f = neg_ll,
  lb     = theta_lower,
  ub     = theta_upper,
  opts   = list(algorithm   = "NLOPT_LN_SBPLX",
                xtol_rel    = 1e-5,
                ftol_rel    = 1e-6,
                maxeval     = 3000,
                print_level = 1)
)

cat(sprintf("\nElapsed: %.1f minutes\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))


# ---- Save -----------------------------------------------------------------
theta_hat <- setNames(fit$solution, theta_names)
cat("\n=== Estimates ===\n"); print(round(theta_hat, 4))
cat(sprintf("\nNeg per-weight LL: %.5f\n", fit$objective))

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(tibble(parameter = theta_names,
              estimate  = theta_hat,
              lower_bd  = theta_lower,
              upper_bd  = theta_upper),
       file.path(results_dir, "theta_hat.csv"))
cat("\nSaved theta_hat to ", file.path(results_dir, "theta_hat.csv"), "\n", sep = "")
