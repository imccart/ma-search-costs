# model-choice.R — Choice probabilities and expected inclusive value for one
# beneficiary-year, by the one-dimensional integral representations
#
#   E[e_j / (e0 + e_j + S_-j)] = (e_j / a) int exp(x - exp(x)) prod_k f_k(exp(x) / a) dx,
#   E[log(e0 + S)] = log e0 + int exp(-exp(x)) (1 - prod_k f_k(exp(x) / e0)) dx,
#
# with f_k(t) = 1 - rho_k (1 - exp(-t e_k)) and a = e0 + e_j. Used by the
# counterfactuals and the toy checks; the likelihood has its own vectorized
# version of the same integrals.

choice_probs_one <- function(e_def, e_pl, rho, grid) {
  e0 <- sum(e_def)
  ep <- e_pl / e0
  J <- length(ep)
  lf_all <- log1p(rho * expm1(-outer(ep, grid$tau)))
  p_def <- (e_def / e0) * sum(grid$w * exp(colSums(lf_all)))
  p_pl <- vapply(seq_len(J), function(j) {
    a <- 1 + ep[j]
    lf <- log1p(rho[-j] * expm1(-outer(ep[-j] / a, grid$tau)))
    rho[j] * ep[j] / a * sum(grid$w * exp(colSums(lf)))
  }, numeric(1))
  list(default = p_def, placed = p_pl)
}

realized_value_one <- function(e_def, e_pl, rho, grid) {
  e0 <- sum(e_def)
  lf <- log1p(rho * expm1(-outer(e_pl / e0, grid$tau)))
  log(e0) - sum(grid$wv * expm1(colSums(lf)))
}

# Perceived value above the default inclusive value, for common inclusion r
# across placed plans. This is the same Bernoulli-set expectation as the
# shuffle/binomial construction, evaluated by deterministic quadrature.
# Prefix/suffix log-products avoid dividing by nearly zero inclusion factors.
# With adjoint supplied, return d(sum_p adjoint[p] V[p])/d log(e_j).
perceived_value_one <- function(ep, r, grid, adjoint = NULL) {
  J <- length(ep)
  P <- length(r)
  Q <- length(grid$tau)
  prefix <- vector('list', J + 1L)
  prefix[[1]] <- matrix(0, Q, P)
  em <- expm1(-outer(grid$tau, ep))
  for (j in seq_len(J)) {
    prefix[[j + 1L]] <- prefix[[j]] + log1p(outer(em[, j], r))
  }
  value <- -drop(crossprod(grid$wv, expm1(prefix[[J + 1L]])))
  suffix <- matrix(0, Q, P)
  dr <- numeric(P)
  dloge <- numeric(J)
  for (j in rev(seq_len(J))) {
    other <- exp(prefix[[j]] + suffix)
    if (is.null(adjoint)) {
      dr <- dr - drop(crossprod(grid$wv * em[, j], other))
    } else {
      dloge[j] <- sum(adjoint * r * drop(crossprod(
        grid$w * ep[j] * exp(-grid$tau * ep[j]), other)))
    }
    suffix <- suffix + log1p(outer(em[, j], r))
  }
  list(value = value, dr = dr, dloge = dloge)
}
