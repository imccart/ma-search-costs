# quadrature.R — Fixed integration rules.
#
# choice_grid: trapezoid rule on x for integrals of the form
#   int exp(x - exp(x)) g(exp(x)) dx  (weights w; choice probabilities)
#   int exp(-exp(x))    g(exp(x)) dx  (weights wv; expected inclusive value)
# The x-integrands are bounded, smooth, and decay double-exponentially, so the
# discretization error falls rapidly as h decreases. Finite-tail error still
# depends on the utility ratios and should be checked against a wider grid.
#
# gauss_hermite: nodes and weights for E[f(nu)], nu ~ N(0, 1), by Golub-Welsch.

# h = 0.3 keeps the weight-mass error below the toy suite's 1e-10 tolerance
# (about 3e-14); h = 0.4 leaves about 4e-10 of discretization error.
choice_grid <- function(h = 0.3, x_lo = -30, x_hi = 3.6) {
  x <- seq(x_lo, x_hi, by = h)
  tau <- exp(x)
  list(x = x, tau = tau, w = h * exp(x - tau), wv = h * exp(-tau), h = h)
}

gauss_hermite <- function(n) {
  if (n == 1) return(list(nodes = 0, weights = 1))
  off <- sqrt(seq_len(n - 1))
  jm <- matrix(0, n, n)
  jm[cbind(1:(n - 1), 2:n)] <- off
  jm[cbind(2:n, 1:(n - 1))] <- off
  ev <- eigen(jm, symmetric = TRUE)
  o <- order(ev$values)
  list(nodes = ev$values[o], weights = ev$vectors[1, o]^2)
}
