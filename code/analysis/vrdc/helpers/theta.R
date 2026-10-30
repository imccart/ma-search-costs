# theta.R — Parameter layout: 32 structural parameters and the unconstrained
# coordinates the optimizer works in. Normalizations kappa_info = omega_info = 1.

theta_names <- c(
  "alpha_prem", "alpha_oop", "beta", "xi_TM", "psi", "delta_PD", "cat_PPO", "cat_PFFS",
  "gamma_0", "gamma_inc", "gamma_educ", "gamma_age", "gamma_dual", "gamma_adi",
  "gamma_hb", "gamma_exp", "gamma_easycmp", "gamma_infcmp",
  "sigma_alpha",
  "kappa_web", "kappa_phone", "kappa_book1", "kappa_book2",
  "omega_web", "omega_phone", "omega_book1", "omega_book2",
  "b0", "b1", "lambda_PF", "lambda_B", "sigma_p"
)
n_theta <- length(theta_names)

utility_names <- theta_names[1:8]
gamma_names   <- theta_names[9:18]
kappa_names   <- c("kappa_web", "kappa_phone", "kappa_book1", "kappa_book2")
omega_names   <- c("omega_web", "omega_phone", "omega_book1", "omega_book2")

# The weight on reading all of the handbook is the weight on reading part of it
# plus a positive increment.
free_names <- c(
  theta_names[1:18],
  "log_sigma_alpha",
  paste0("log_", kappa_names),
  "log_omega_web", "log_omega_phone", "log_omega_book1", "log_omega_book2_inc",
  "b0", "log_b1", "logit_lambda_PF", "logit_lambda_B", "log_sigma_p"
)

exp_idx   <- match(c("sigma_alpha", kappa_names, "omega_web", "omega_phone", "omega_book1",
                     "b1", "sigma_p"), theta_names)
logit_idx <- match(c("lambda_PF", "lambda_B"), theta_names)
inc_idx   <- match("omega_book2", theta_names)
base_idx  <- match("omega_book1", theta_names)

to_struct <- function(phi) {
  if (!is.null(names(phi))) phi <- phi[free_names]
  phi <- unname(phi)
  th <- phi
  th[exp_idx] <- exp(phi[exp_idx])
  th[inc_idx] <- exp(phi[base_idx]) + exp(phi[inc_idx])
  th[logit_idx] <- plogis(phi[logit_idx])
  names(th) <- theta_names
  th
}

to_free <- function(th) {
  if (!is.null(names(th))) th <- th[theta_names]
  th <- unname(th)
  stopifnot(th[inc_idx] > th[base_idx], all(th[exp_idx] > 0),
            all(th[logit_idx] > 0 & th[logit_idx] < 1))
  phi <- th
  phi[exp_idx] <- log(th[exp_idx])
  phi[inc_idx] <- log(th[inc_idx] - th[base_idx])
  phi[logit_idx] <- qlogis(th[logit_idx])
  names(phi) <- free_names
  phi
}

# d theta / d phi (rows structural, columns free)
theta_jacobian <- function(phi) {
  th <- unname(to_struct(phi))
  d <- rep(1, n_theta)
  d[exp_idx] <- th[exp_idx]
  d[inc_idx] <- th[inc_idx] - th[base_idx]
  d[logit_idx] <- th[logit_idx] * (1 - th[logit_idx])
  G <- diag(d)
  G[inc_idx, base_idx] <- th[base_idx]
  dimnames(G) <- list(theta_names, free_names)
  G
}
