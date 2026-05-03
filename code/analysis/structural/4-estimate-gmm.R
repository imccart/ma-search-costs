# 4-estimate-gmm.R — GMM estimation, 12 moments / 10 parameters (overid)
#
# Per-iter cost: ~0.7 sec for simulation + ~0.05 sec for moment construction
# (no in-loop feols calls). L-BFGS-B with bounds; alpha, delta >= 0.
#
# SEs deferred — non-trivial for the RF orthogonality moments since they
# aren't separable into per-row contributions cleanly.

LOWER <- c(alpha = 0,    delta = 0,   beta = -2,  xi_ffs = -10,
           gamma0 = -6,  g_log_inc = -3, g_bach = -10, g_65 = -10,
           eta1 = -2,    eta2 = -2)
UPPER <- c(alpha = 5,    delta = 2,   beta = 5,   xi_ffs = 20,
           gamma0 = 0,   g_log_inc =  3, g_bach =  10, g_65 =  10,
           eta1 = 5,     eta2 = 5)

# Scaling: BLP residual moments are O(0.01-0.05); RF fractional moments are
# O(0.5-2). Bring BLP up by ~50 so identity W treats both groups roughly
# equally per moment.
MOMENT_SCALE <- c(rep(50, ncol(X_mat)), rep(1, 5))

gmm_obj_factory <- function(W) {
  function(v) {
    g_raw <- compute_moments(v, markets, observed, X_mat, panel)
    g <- g_raw * MOMENT_SCALE
    as.numeric(t(g) %*% W %*% g)
  }
}

W_id <- diag(length(MOMENT_SCALE))
obj  <- gmm_obj_factory(W_id)
v0   <- theta_to_vec(theta_start)

message("\n========== GMM (L-BFGS-B, identity W on scaled moments) ==========")
message("Starting theta:")
print(round(setNames(v0, PARAM_NAMES), 4))
message(sprintf("Objective at start: %.6e", obj(v0)))

t0 <- Sys.time()
fit <- optim(v0, obj, method = "L-BFGS-B",
             lower = LOWER, upper = UPPER,
             control = list(maxit = 80, factr = 1e9, trace = 1, REPORT = 2))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

message(sprintf("\nOptimization done in %.0f sec | obj = %.6e | convergence = %d",
                elapsed, fit$value, fit$convergence))
if (!is.null(fit$message)) message("Optim message: ", fit$message)

v_hat <- fit$par
names(v_hat) <- PARAM_NAMES
message("\nEstimates:")
print(round(v_hat, 4))

dir.create("results", showWarnings = FALSE, recursive = TRUE)
write_csv(
  tibble(param = PARAM_NAMES, est = as.numeric(v_hat)),
  "results/structural_theta_hat.csv"
)
message("Wrote results/structural_theta_hat.csv")

g_hat_raw <- compute_moments(v_hat, markets, observed, X_mat, panel)
names(g_hat_raw) <- MOMENT_NAMES

message("\n========== Moments at theta_hat (raw, unscaled) ==========")
print(round(g_hat_raw, 6))

theta_hat_list <- vec_to_theta(v_hat)
SE      <- rep(NA_real_, length(v_hat))
g_hat   <- g_hat_raw
