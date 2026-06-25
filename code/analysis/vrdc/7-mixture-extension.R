# 7-mixture-extension.R — Finite mixture in c_i (deferred / robustness)
#
# Replaces c_i = exp(gamma_0 + gamma_1' X_i) with a T-type mixture:
#   bene i is type tau in {1,...,T} with probability pi_tau(X_i),
#   where pi_tau is a multinomial logit on X_i.
#   Each type tau has its own scalar c_tau.
#
# Identifies non-demographic residual heterogeneity in search behavior —
# directly maps to the project's substantive argument that some bene's
# can be helped by information disclosure and others cannot.
#
# DEFERRED to a second-pass run. The single-c-per-X version (in
# 3-individual-likelihood.R) needs to converge cleanly first. Once that
# happens, switch the source line in _analyze-vrdc.R from the per-bene
# c calculation to the mixture version below.

T_TYPES <- 3L

# ---- Type-specific c parameters ----

# Use a separate theta vector for the mixture extension:
#   alpha, delta, beta, xi_FFS as before
#   c_1, c_2, c_3                     (T scalar search costs, sorted)
#   pi_logit_inc_2, pi_logit_inc_3    (income coef in mlogit for types 2,3)
#   pi_logit_inet_2, pi_logit_inet_3  (KVSITWEB coef in mixing for types 2,3)
#   ... (other X covariates in the mixing equation)
#   lambda_PF_*, lambda_broker_*, lambda_inc  (prominence weights, see
#     3-individual-likelihood.R; common across types in v1 mixture)

# ---- Mixing probabilities ----

compute_pi <- function(X_i, mix_pars) {
  # mlogit on X_i with type 1 as base; returns vector of length T summing to 1
  utility <- c(0,
               mix_pars$alpha2 + mix_pars$beta2 %*% X_i,
               mix_pars$alpha3 + mix_pars$beta3 %*% X_i)
  exp_u <- exp(utility - max(utility))
  exp_u / sum(exp_u)
}

# ---- Likelihood ----

# For each bene i:
#   L_i = sum_tau pi_tau(X_i) * P(j_i | c_tau, market(c,t))
#
# Iterate by EM: alternate between (a) computing posterior P(tau | i, theta)
# and (b) maximizing the complete-data likelihood.

# IMPLEMENTATION DEFERRED — see plan §6.4 for spec.
stop("Mixture extension not yet implemented; see background/vrdc-plan.md §6.4")
