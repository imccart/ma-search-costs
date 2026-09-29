# supp2_components.R — Action (search-intensity) block estimated in isolation
#
# Second half of the component diagnostic. Fixes the utility block (hence the
# search benefit B_i) at theta_hat and estimates the search-cost parameters on
# the action likelihood alone — the same nu-integrated action algebra as
# compute_individual_loglik, with no choice term. Shows whether the search block
# identifies on its own and how its estimates compare to the joint fit.
#
# Runs STANDALONE after script 3 (assumes 1-build, 2-load, 3-individual-likelihood
# have been sourced, so their objects and functions are in memory); reloads
# theta_hat, nu_draws, and W_SUM from results/vrdc / the session. Not wired into
# a driver.
#
# Outputs (printed and written to results/vrdc/):
#   supp2_estimates.csv  action params, isolated vs joint, side by side
#   supp2_profiles.csv   objective profiled in log_sigma_alpha
#
# Runtime: a few minutes (vectorized nu integration).

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

if (!exists("nu_draws")) nu_draws <- qnorm((seq_len(N_SIM_DRAWS) - 0.5) / N_SIM_DRAWS)
if (!exists("W_SUM"))    W_SUM    <- bene[!duplicated(bene$BASEID), sum(wgt_full_sample)]
if (!exists("theta_hat")) {
  th_in     <- fread(file.path(results_dir, "theta_hat.csv"))
  theta_hat <- setNames(th_in$estimate, th_in$parameter)
}
if (is.null(names(theta_hat))) names(theta_hat) <- theta_names
theta_hat <- theta_hat[theta_names]
stopifnot(!any(is.na(theta_hat)), identical(names(theta_hat), theta_names))
R <- length(nu_draws)

# Search benefit at the fixed (theta_hat) utility — mirrors the B_init block in 4.
th_u <- unpack_theta(theta_hat)
B_vec <- numeric(nrow(bene))
for (i in seq_len(nrow(bene))) {
  brow <- bene_rows[[i]]; mkt <- markets[[brow$market_id]]
  v <- compute_bene_utility(mkt, bene_prem[[i]], bene_oop[[i]], th_u)
  inc <- brow$prior_plan_offered == 1L &
         !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
  v[inc] <- v[inc] + th_u$psi
  B_vec[i] <- compute_search_benefit(v, mkt$plan_kind == "FFS" | inc)
}

# Precompute: gamma design matrix, action outcomes, bene grouping.
Xc <- cbind(1, as.matrix(bene[, .(log_inc_dm, educ_yrs_dm, age_dm, is_dual, adi_dm,
                                   book_understood_dm, tenure_dm,
                                   easy_compare_dm, enough_info_dm)]))
ai <- bene$act_info; aw <- bene$act_web; ap <- bene$act_phone
brd <- bene$book_read; rvl <- bene$review_level
grp <- as.integer(factor(bene$BASEID, levels = names(idx_by_bene)))

gamma_names <- c("gamma_0", "gamma_inc", "gamma_educ", "gamma_age", "gamma_dual",
                 "gamma_adi", "gamma_hb", "gamma_exp", "gamma_easycmp", "gamma_infcmp")
act_free <- c(gamma_names, "log_sigma_alpha",
              "kappa_info", "kappa_web", "kappa_phone", "kappa_book", "tau_gap",
              "kappa_review", "tau_review_gap")

# Action-only negative per-weight log-likelihood: same nu-integrated algebra as
# compute_individual_loglik, vectorized over bene-years, no choice term.
action_negll <- function(u) {
  th <- as.list(setNames(u, act_free))
  gamma <- unlist(th[gamma_names]); sigma <- exp(th$log_sigma_alpha)
  logc_det <- as.vector(Xc %*% gamma)
  draws_mat <- matrix(0, length(wgt_by_bene), R)
  for (r in seq_len(R)) {
    z  <- B_vec - exp(logc_det + sigma * nu_draws[r])
    lp <- function(a, kap) { p <- plogis(z - kap); log(a * p + (1 - a) * (1 - p) + 1e-12) }
    ord <- function(lev, k1, gap) {
      hi <- plogis(z - (k1 + gap)); mid <- plogis(z - k1) - hi; lo <- 1 - plogis(z - k1)
      log(ifelse(lev == 2L, hi, ifelse(lev == 1L, mid, lo)) + 1e-12)
    }
    a_ll <- lp(ai, th$kappa_info) + lp(aw, th$kappa_web) + lp(ap, th$kappa_phone) +
            ord(brd, th$kappa_book, th$tau_gap) + ord(rvl, th$kappa_review, th$tau_review_gap)
    draws_mat[, r] <- rowsum(a_ll, grp)[, 1]
  }
  m <- apply(draws_mat, 1, max)
  ll_bene <- m + log(rowMeans(exp(draws_mat - m)))
  -sum(ll_bene * wgt_by_bene) / W_SUM
}

start <- theta_hat[act_free]
lower <- setNames(rep(-Inf, length(act_free)), act_free)
lower[c("tau_gap", "tau_review_gap")] <- 0
fit <- optim(start, action_negll, method = "L-BFGS-B", lower = lower,
             control = list(maxit = 800), hessian = TRUE)
est <- setNames(fit$par, act_free)
cat("\n=== Action block, estimated in isolation ===\n")
print(round(est, 4)); cat(sprintf("neg per-weight action LL: %.5f\n", fit$value))

# log_sigma_alpha profile: the random-effect dispersion is the action parameter
# most likely to be weakly identified.
lsgrid <- seq(-3, 1, by = 0.5)
pls <- sapply(lsgrid, function(x) { u <- est; u["log_sigma_alpha"] <- x; action_negll(u) })
cat("\nlog_sigma_alpha profile:\n")
print(data.frame(log_sigma_alpha = lsgrid, negLL = round(pls, 6)))

# Isolated vs joint (theta_hat), side by side.
joint <- theta_hat[act_free]
out <- data.table(parameter = act_free, isolated = as.numeric(est), joint = as.numeric(joint))
cat("\nIsolated vs joint:\n"); print(out)

fwrite(out, file.path(results_dir, "supp2_estimates.csv"))
fwrite(data.table(parameter = "log_sigma_alpha", value = lsgrid, negLL = pls),
       file.path(results_dir, "supp2_profiles.csv"))
cat("\nWrote results/vrdc/supp2_estimates.csv and supp2_profiles.csv\n")
