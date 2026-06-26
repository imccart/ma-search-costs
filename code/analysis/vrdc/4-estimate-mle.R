# 4-estimate-mle.R — Simulated maximum likelihood (joint search + choice)
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
W_SUM <- bene[!duplicated(bene$BASEID), sum(wgt_full_sample)]


# ---- Initial values via two fast first-stage MLEs -------------------------
# Stage 1: choice-only conditional logit (phi = 1) -> utility block. With every
#   plan considered, the Goeree choice prob collapses to a plain conditional
#   logit, globally concave in {alpha, delta, beta, xi_FFS, psi}.
# Stage 2: pooled action logit (random effect off) -> search-cost block. B_i is
#   fixed at the stage-1 utilities and hand-set awareness, so the action
#   likelihood is a vectorized set of logits over {gamma_*, kappa_*, tau_gap}.
# Awareness (lambda_*) and breadth (b_*) stay at hand values. These are STARTING
# VALUES only; the full joint MLE below re-optimizes all 31 from here. Perturb
# lam_hand / b_hand for multi-start.

lam_hand <- list(lambda_PF_0 = 0.50, lambda_PF_web = 1.00, lambda_PF_help = 0.30,
                 lambda_PF_delegate = 0.20, lambda_broker_0 = 0.50,
                 lambda_broker_help = 0.50, lambda_broker_delegate = 0.80)
b_hand   <- c(b0 = 0, b_info = 0.30, b_web = 0.80, b_phone = 0.30, b_book = 0.30)

# --- Stage 1: conditional logit on plan choice (phi = 1) ---
stage1_negll <- function(u) {
  th <- list(alpha = u[1], delta = u[2], beta = u[3], xi_FFS = u[4], psi = u[5])
  ll <- 0
  for (i in seq_len(nrow(bene))) {
    brow <- bene_rows[[i]]; mkt <- markets[[brow$market_id]]
    v <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th)
    inc <- brow$prior_plan_offered == 1L &
           !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + th$psi
    mx <- max(v)
    ll <- ll + (v[brow$choice_idx] - (mx + log(sum(exp(v - mx))))) *
          brow$wgt_full_sample
  }
  -ll / W_SUM
}
cat("\nStage 1: choice-only conditional logit...\n")
s1 <- optim(c(0.6, 0.1, 0.5, 6.0, 1.0), stage1_negll, method = "L-BFGS-B",
            lower = c(0, 0, -Inf, -Inf, -Inf))$par
names(s1) <- c("alpha", "delta", "beta", "xi_FFS", "psi")
cat("  "); print(round(s1, 4))

# --- Precompute B_i at the stage-1 utilities and hand-set awareness ---
th_B <- c(as.list(s1), lam_hand)
B_init <- numeric(nrow(bene))
for (i in seq_len(nrow(bene))) {
  brow <- bene_rows[[i]]; mkt <- markets[[brow$market_id]]
  v <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th_B)
  inc <- brow$prior_plan_offered == 1L &
         !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
  v[inc] <- v[inc] + th_B$psi
  sal <- compute_salience(mkt, market_prom[[brow$market_id]], brow, th_B)
  B_init[i] <- compute_search_benefit(mkt, v, sal)
}

# --- Stage 2: pooled action logit (sigma = 0, B fixed). Same action-likelihood
#     algebra as loglik_actions() in script 3, vectorized over bene-years. ---
Xc  <- as.matrix(bene[, .(log_inc_dm, educ_yrs_dm, age_dm, is_dual, adi_dm,
                          book_understood_dm, tenure_dm)])
ai  <- bene$act_info; aw <- bene$act_web; ap <- bene$act_phone; brd <- bene$book_read
wt  <- bene$wgt_full_sample
stage2_negll <- function(p) {
  c_i <- exp(p[1] + as.vector(Xc %*% p[2:8]))
  z   <- B_init - c_i
  lp  <- function(a, kap) { pp <- plogis(z - kap); log(a * pp + (1 - a) * (1 - pp) + 1e-12) }
  c1  <- p[12]; c2 <- c1 + p[13]
  pth <- plogis(z - c2); ppt <- plogis(z - c1) - pth; pno <- 1 - plogis(z - c1)
  pbook <- ifelse(brd == 2L, pth, ifelse(brd == 1L, ppt, pno))
  ll  <- lp(ai, p[9]) + lp(aw, p[10]) + lp(ap, p[11]) + log(pbook + 1e-12)
  -sum(ll * wt) / W_SUM
}
cat("Stage 2: pooled action logit...\n")
s2 <- optim(c(-2, 0, 0, 0, 0.30, 0.10, 0.20, -0.10,    # gamma_0 .. gamma_exp
              0.50, 0.50, 1.00, 0.50, 1.00),           # kappa_info/web/phone/book, tau_gap
            stage2_negll, method = "L-BFGS-B",
            lower = c(rep(-Inf, 12), 0))$par
names(s2) <- c("gamma_0", "gamma_inc", "gamma_educ", "gamma_age", "gamma_dual",
               "gamma_adi", "gamma_hb", "gamma_exp",
               "kappa_info", "kappa_web", "kappa_phone", "kappa_book", "tau_gap")
cat("  "); print(round(s2, 4))

# --- Assemble theta0 (order must match theta_names) ---
theta0 <- c(
  s1["alpha"], s1["delta"], s1["beta"], s1["xi_FFS"], s1["psi"],
  s2["gamma_0"], s2["gamma_inc"], s2["gamma_educ"], s2["gamma_age"],
  s2["gamma_dual"], s2["gamma_adi"], s2["gamma_hb"], s2["gamma_exp"],
  log(0.5),
  s2["kappa_info"], s2["kappa_web"], s2["kappa_phone"], s2["kappa_book"], s2["tau_gap"],
  b_hand["b0"], b_hand["b_info"], b_hand["b_web"], b_hand["b_phone"], b_hand["b_book"],
  lam_hand$lambda_PF_0, lam_hand$lambda_PF_web, lam_hand$lambda_PF_help,
  lam_hand$lambda_PF_delegate, lam_hand$lambda_broker_0,
  lam_hand$lambda_broker_help, lam_hand$lambda_broker_delegate
)
names(theta0) <- theta_names
stopifnot(identical(names(theta0), theta_names))
cat("\nAssembled theta0 from staged init:\n"); print(round(theta0, 4))


# ---- Objective ------------------------------------------------------------
neg_ll <- function(theta) {
  -compute_individual_loglik(theta, nu_draws) / W_SUM
}

cat("\nStarting simulated MLE (nloptr SBPLX)...\n")
cat(sprintf("  beneficiaries: %d   bene-years: %d   draws: %d\n",
            uniqueN(bene$BASEID), nrow(bene), N_SIM_DRAWS))
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
