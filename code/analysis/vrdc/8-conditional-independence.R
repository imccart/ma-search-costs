# 8-conditional-independence.R — Robustness for the Stage-2 measurement model
#
# The four search actions (info / web / phone / handbook) are treated as
# indicators of one latent search cost: each is a logit in the shared index
# B_i - c_i - kappa_a, and a bene-year's action likelihood is the PRODUCT of the
# four action probabilities conditional on the person random effect, integrated
# over it (agents/model.md, "Measurement structure"). The load-bearing
# assumption is CONDITIONAL INDEPENDENCE of the four actions given the search
# cost — they co-move only through the shared latent and the random effect, with
# no extra pairwise link (e.g. web/phone substitution).
#
# This script defends that assumption three ways. Point estimates only, no SEs,
# no per-variant counterfactual: if the search-cost gammas are stable, the
# counterfactual is stable by construction.
#
#   1. Pairwise co-occurrence fit (post-estimation on theta_hat; cheap). For
#      each action pair, observed P(both) vs the model's implied P(both) under
#      conditional independence. A large residual on a pair flags dependence the
#      shared latent does not capture.
#   2. Drop-one-action (re-estimate four times, each omitting one action). If
#      the search-cost gammas barely move, no single action is driving them.
#   3. Correlated-error relaxation on the web/phone pair (re-estimate once with
#      one extra parameter rho that lets those two actions' errors co-vary,
#      either sign). If rho is small and the gammas are unchanged, conditional
#      independence is not materially violated on the suspect pair.
#
# Runs STANDALONE after script 3: it reloads theta_hat, nu_draws, and W_SUM if
# script 4 was not run this session, so 4/5/6 can be commented out of the driver.
# Each re-estimation is warm-started at theta_hat (the optima are nearby) and
# every objective evaluation is cached to disk, so an interrupted seat session
# resumes on re-source instead of restarting. Delete the robust_*_cache.csv and
# robust_*.csv files if theta_hat or the sample changes.

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

MAXEVAL_ROBUST     <- 1200L   # ceiling per variant; ftol_rel usually stops early
N_SIM_DRAWS_OMEGA  <- 7L      # inner draws for the web/phone correlated-error factor


# ---- Reload theta_hat, nu_draws, W_SUM if script 4 was not run this session --
if (!exists("nu_draws"))
  nu_draws <- qnorm((seq_len(N_SIM_DRAWS) - 0.5) / N_SIM_DRAWS)

if (!exists("W_SUM"))
  W_SUM <- bene[!duplicated(bene$BASEID), sum(wgt_full_sample)]

if (!exists("theta_hat")) {
  th_in     <- fread(file.path(results_dir, "theta_hat.csv"))
  theta_hat <- setNames(th_in$estimate, th_in$parameter)
  cat("Reloaded theta_hat from disk.\n")
}
if (is.null(names(theta_hat))) names(theta_hat) <- theta_names
theta_hat <- theta_hat[theta_names]            # canonical order
stopifnot(!any(is.na(theta_hat)), identical(names(theta_hat), theta_names))
cat("\ntheta_hat in canonical order:\n"); print(round(theta_hat, 4))

om_draws <- qnorm((seq_len(N_SIM_DRAWS_OMEGA) - 0.5) / N_SIM_DRAWS_OMEGA)


# ---- Disk-cached objective (same idiom as 6-standard-errors.R) --------------
# SBPLX walks a deterministic path given (x0, initial_step, objective), so a
# theta-keyed cache replays completed evaluations after a crash and continues
# from the uncached frontier. Each variant gets its own cache file.
cached_objective <- function(raw_fn, cache_path) {
  env <- new.env(parent = emptyenv())
  if (file.exists(cache_path)) {
    old <- fread(cache_path, colClasses = c("character", "numeric"))
    for (r in seq_len(nrow(old))) assign(old$theta_key[r], old$value[r], envir = env)
    cat(sprintf("  reloaded %d cached evaluations\n", nrow(old)))
  } else {
    fwrite(data.table(theta_key = character(), value = numeric()), cache_path)
  }
  n_new <- 0L
  function(p) {
    k <- paste(sprintf("%.17g", p), collapse = "|")
    if (exists(k, envir = env, inherits = FALSE)) return(get(k, envir = env))
    v <- raw_fn(p)
    assign(k, v, envir = env)
    fwrite(data.table(theta_key = k, value = v), cache_path, append = TRUE, col.names = FALSE)
    n_new <<- n_new + 1L
    if (n_new %% 25L == 0L) {
      cat(sprintf("    %d new evaluations (%s)\n", n_new, format(Sys.time(), "%H:%M")))
      flush.console()
    }
    v
  }
}


# ---- Drop-one likelihood ----------------------------------------------------
# Mirrors compute_individual_loglik() in script 3 exactly, except the action
# product keeps only the actions flagged in `keep`. The choice stage is
# untouched (dropping a search action does not affect the plan-choice
# probability), so this reuses every choice-stage helper from script 3.
loglik_actions_keep <- function(ai, aw, ap, br, th, B, c, keep) {
  z  <- B - c
  lp <- function(act, kap) { p <- plogis(z - kap); log(act * p + (1 - act) * (1 - p) + 1e-12) }
  ll <- numeric(length(z))
  if (keep["info"])  ll <- ll + lp(ai, th$kappa_info)
  if (keep["web"])   ll <- ll + lp(aw, th$kappa_web)
  if (keep["phone"]) ll <- ll + lp(ap, th$kappa_phone)
  if (keep["book"]) {
    c1 <- th$kappa_book; c2 <- th$kappa_book + th$tau_gap
    p_th <- plogis(z - c2); p_pt <- plogis(z - c1) - p_th; p_no <- 1 - plogis(z - c1)
    p_book <- ifelse(br == 2L, p_th, ifelse(br == 1L, p_pt, p_no))
    ll <- ll + log(p_book + 1e-12)
  }
  ll
}

loglik_drop <- function(theta, nu_draws, keep) {
  th <- unpack_theta(theta)
  n  <- nrow(bene)
  ll_choice <- numeric(n); B_vec <- numeric(n); logc_det <- numeric(n)
  for (i in seq_len(n)) {
    brow <- bene_rows[[i]]; mid <- brow$market_id; mkt <- markets[[mid]]
    v    <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th)
    prom <- market_prom[[mid]]
    sal  <- compute_salience(mkt, prom, brow, th)
    inc  <- brow$prior_plan_offered == 1L &
            !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + th$psi
    phi <- compute_phi(mkt, sal, compute_K(brow, th), brow)
    p   <- compute_choice_prob(v, phi)
    ll_choice[i] <- log(pmax(p[brow$choice_idx], 1e-12))
    B_vec[i]     <- compute_search_benefit(mkt, v, sal)
    logc_det[i]  <- compute_log_c_det(brow, th)
  }
  sigma <- exp(th$log_sigma_alpha); R <- length(nu_draws)
  ll_bene <- numeric(length(idx_by_bene))
  for (bi in seq_along(idx_by_bene)) {
    rows <- idx_by_bene[[bi]]
    ch_sum <- sum(ll_choice[rows])
    ai <- bene$act_info[rows]; aw <- bene$act_web[rows]
    ap <- bene$act_phone[rows]; br <- bene$book_read[rows]
    Bw <- B_vec[rows]; ld <- logc_det[rows]
    draw <- numeric(R)
    for (r in seq_len(R)) {
      c_r <- exp(ld + sigma * nu_draws[r])
      draw[r] <- sum(loglik_actions_keep(ai, aw, ap, br, th, Bw, c_r, keep))
    }
    m <- max(draw)
    ll_bene[bi] <- ch_sum + m + log(mean(exp(draw - m)))
  }
  sum(ll_bene * wgt_by_bene)
}


# ---- Correlated-error likelihood (web/phone) --------------------------------
# Adds a bene-level factor omega ~ N(0,1) loading +rho on the web index and
# -rho on the phone index. rho > 0 makes web and phone SUBSTITUTES (the named
# concern); rho < 0 makes them complements; rho = 0 is exact conditional
# independence, at which this reduces to compute_individual_loglik() (asserted
# below). info and book do not load on omega, so they are integrated only over
# the search-cost draws; only the web/phone product is integrated over omega,
# which keeps the cost at R + R*S action evaluations rather than R*S.
loglik_corr <- function(theta_ext, nu_draws, om_draws) {
  th  <- unpack_theta(theta_ext[theta_names])
  rho <- theta_ext[["rho_webphone"]]
  n   <- nrow(bene)
  ll_choice <- numeric(n); B_vec <- numeric(n); logc_det <- numeric(n)
  for (i in seq_len(n)) {
    brow <- bene_rows[[i]]; mid <- brow$market_id; mkt <- markets[[mid]]
    v    <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th)
    prom <- market_prom[[mid]]
    sal  <- compute_salience(mkt, prom, brow, th)
    inc  <- brow$prior_plan_offered == 1L &
            !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + th$psi
    phi <- compute_phi(mkt, sal, compute_K(brow, th), brow)
    p   <- compute_choice_prob(v, phi)
    ll_choice[i] <- log(pmax(p[brow$choice_idx], 1e-12))
    B_vec[i]     <- compute_search_benefit(mkt, v, sal)
    logc_det[i]  <- compute_log_c_det(brow, th)
  }
  sigma <- exp(th$log_sigma_alpha); R <- length(nu_draws); S <- length(om_draws)
  lp  <- function(act, x) log(act * plogis(x) + (1 - act) * (1 - plogis(x)) + 1e-12)
  lme <- function(x) { m <- max(x); m + log(mean(exp(x - m))) }
  ll_bene <- numeric(length(idx_by_bene))
  for (bi in seq_along(idx_by_bene)) {
    rows <- idx_by_bene[[bi]]
    ch_sum <- sum(ll_choice[rows])
    ai <- bene$act_info[rows]; aw <- bene$act_web[rows]
    ap <- bene$act_phone[rows]; br <- bene$book_read[rows]
    Bw <- B_vec[rows]; ld <- logc_det[rows]
    draw <- numeric(R)
    for (r in seq_len(R)) {
      z_r  <- Bw - exp(ld + sigma * nu_draws[r])
      c1 <- th$kappa_book; c2 <- th$kappa_book + th$tau_gap
      p_th <- plogis(z_r - c2); p_pt <- plogis(z_r - c1) - p_th; p_no <- 1 - plogis(z_r - c1)
      lbk  <- sum(log(ifelse(br == 2L, p_th, ifelse(br == 1L, p_pt, p_no)) + 1e-12))
      la   <- sum(lp(ai, z_r - th$kappa_info)) + lbk
      lwp  <- numeric(S)
      for (s in seq_len(S)) {
        zw <- z_r + rho * om_draws[s]
        zp <- z_r - rho * om_draws[s]
        lwp[s] <- sum(lp(aw, zw - th$kappa_web)) + sum(lp(ap, zp - th$kappa_phone))
      }
      draw[r] <- la + lme(lwp)
    }
    ll_bene[bi] <- ch_sum + lme(draw)
  }
  sum(ll_bene * wgt_by_bene)
}


# ---- Model-implied pairwise action co-occurrence at a theta -----------------
# For each action pair, the model's P(both = 1) under conditional independence:
# integrate the PRODUCT of the two marginal action probabilities over the
# search-cost draws (given the cost, the two are independent). Handbook is
# binarized to read>0 (P = plogis(z - kappa_book)). Compared to the observed
# survey-weighted joint frequency. Per bene-year (the random effect is
# integrated marginally for this diagnostic), matching the observed side.
compute_pairwise_cooccurrence <- function(theta, nu_draws) {
  th <- unpack_theta(theta); n <- nrow(bene); R <- length(nu_draws)
  sigma <- exp(th$log_sigma_alpha)
  acts <- c("info", "web", "phone", "book")
  kap  <- c(th$kappa_info, th$kappa_web, th$kappa_phone, th$kappa_book)
  Aobs <- cbind(bene$act_info, bene$act_web, bene$act_phone,
                as.integer(bene$book_read > 0))
  colnames(Aobs) <- acts
  pairs <- combn(acts, 2, simplify = FALSE)
  Jmod  <- matrix(0, n, length(pairs))
  for (i in seq_len(n)) {
    brow <- bene_rows[[i]]; mid <- brow$market_id; mkt <- markets[[mid]]
    v    <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th)
    prom <- market_prom[[mid]]; sal <- compute_salience(mkt, prom, brow, th)
    inc  <- brow$prior_plan_offered == 1L &
            !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + th$psi
    B  <- compute_search_benefit(mkt, v, sal); ld <- compute_log_c_det(brow, th)
    Pa <- matrix(0, R, 4, dimnames = list(NULL, acts))
    for (r in seq_len(R)) {
      z <- B - exp(ld + sigma * nu_draws[r])
      Pa[r, ] <- plogis(z - kap)
    }
    for (pj in seq_along(pairs))
      Jmod[i, pj] <- mean(Pa[, pairs[[pj]][1]] * Pa[, pairs[[pj]][2]])
  }
  w <- bene$wgt_full_sample; W <- sum(w)
  out <- data.table(
    pair = vapply(pairs, function(p) paste(p, collapse = "_"), ""),
    obs  = vapply(pairs, function(p) sum(w * Aobs[, p[1]] * Aobs[, p[2]]) / W, 0.0),
    pred = vapply(seq_along(pairs), function(pj) sum(w * Jmod[, pj]) / W, 0.0)
  )
  out[, resid := obs - pred]
  out[]
}


# ---------------------------------------------------------------------------
# 1. Pairwise co-occurrence fit (cheap; always runs)
# ---------------------------------------------------------------------------
cat("\n=== Pairwise action co-occurrence (observed vs conditional-independence) ===\n")
cooc <- compute_pairwise_cooccurrence(theta_hat, nu_draws)
print(cooc)
fwrite(cooc, file.path(results_dir, "robust_cooccurrence.csv"))
cat("Saved robust_cooccurrence.csv\n")


# ---------------------------------------------------------------------------
# 2. Drop-one-action re-estimation
# ---------------------------------------------------------------------------
# Each variant fixes the dropped action's baseline (kappa_a, plus tau_gap when
# the handbook is dropped) at its theta_hat value — it is unidentified once its
# action leaves the likelihood — and re-optimizes the rest from the warm start.
drop_specs <- list(
  drop_info  = list(keep = c(info = FALSE, web = TRUE,  phone = TRUE,  book = TRUE),  fix = "kappa_info"),
  drop_web   = list(keep = c(info = TRUE,  web = FALSE, phone = TRUE,  book = TRUE),  fix = "kappa_web"),
  drop_phone = list(keep = c(info = TRUE,  web = TRUE,  phone = FALSE, book = TRUE),  fix = "kappa_phone"),
  drop_book  = list(keep = c(info = TRUE,  web = TRUE,  phone = TRUE,  book = FALSE), fix = c("kappa_book", "tau_gap"))
)

for (nm in names(drop_specs)) {
  res_file <- file.path(results_dir, paste0("robust_", nm, ".csv"))
  if (file.exists(res_file)) { cat(sprintf("\n%s already done; skipping.\n", nm)); next }
  spec     <- drop_specs[[nm]]
  keep     <- spec$keep
  free_idx <- setdiff(seq_along(theta_names), match(spec$fix, theta_names))
  x0 <- theta_hat[free_idx]; lb <- theta_lower[free_idx]; ub <- theta_upper[free_idx]
  raw_fn <- function(p) { th <- theta_hat; th[free_idx] <- p; -loglik_drop(th, nu_draws, keep) / W_SUM }
  negll  <- cached_objective(raw_fn, file.path(results_dir, paste0("robust_", nm, "_cache.csv")))
  step   <- pmax(0.05 * abs(x0), 0.02)
  cat(sprintf("\n=== %s: re-estimating %d free params (warm start = theta_hat) ===\n",
              nm, length(free_idx)))
  t0  <- Sys.time()
  fit <- nloptr(x0 = x0, eval_f = negll, lb = lb, ub = ub,
                opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-5,
                            ftol_rel = 1e-6, maxeval = MAXEVAL_ROBUST,
                            initial_step = step, print_level = 0))
  cat(sprintf("  done in %.1f min, neg per-weight LL %.5f\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins")), fit$objective))
  theta_v <- theta_hat; theta_v[free_idx] <- fit$solution
  fwrite(data.table(parameter = theta_names, estimate = theta_v), res_file)
  cat(sprintf("  saved %s\n", res_file))
}


# ---------------------------------------------------------------------------
# 3. Correlated-error relaxation (web/phone), one extra parameter rho
# ---------------------------------------------------------------------------
corr_file <- file.path(results_dir, "robust_corr_webphone.csv")
if (file.exists(corr_file)) {
  cat("\ncorr_webphone already done; skipping.\n")
} else {
  # Baseline check: at rho = 0 the correlated-error likelihood must equal the
  # main likelihood. Guards the two-latent integration against a coding error
  # before spending hours re-estimating.
  base_corr <- loglik_corr(c(theta_hat, rho_webphone = 0), nu_draws, om_draws)
  base_main <- compute_individual_loglik(theta_hat, nu_draws)
  cat(sprintf("\ncorr baseline check: rho=0 LL %.6f vs main LL %.6f\n", base_corr, base_main))
  stopifnot(abs(base_corr - base_main) / (abs(base_main) + 1) < 1e-8)

  theta_ext0 <- c(theta_hat, rho_webphone = 0)
  lb_e <- c(theta_lower, rho_webphone = -Inf); ub_e <- c(theta_upper, rho_webphone = Inf)
  raw_fn <- function(p) -loglik_corr(setNames(p, names(theta_ext0)), nu_draws, om_draws) / W_SUM
  negll  <- cached_objective(raw_fn, file.path(results_dir, "robust_corr_webphone_cache.csv"))
  step   <- pmax(0.05 * abs(theta_ext0), 0.02); step[["rho_webphone"]] <- 0.10
  cat(sprintf("\n=== corr_webphone: re-estimating %d params + rho (warm start = theta_hat) ===\n",
              length(theta_hat)))
  t0  <- Sys.time()
  fit <- nloptr(x0 = theta_ext0, eval_f = negll, lb = lb_e, ub = ub_e,
                opts = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-5,
                            ftol_rel = 1e-6, maxeval = MAXEVAL_ROBUST,
                            initial_step = step, print_level = 0))
  cat(sprintf("  done in %.1f min, neg per-weight LL %.5f, rho = %.4f\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              fit$objective, fit$solution[length(fit$solution)]))
  fwrite(data.table(parameter = names(theta_ext0), estimate = fit$solution), corr_file)
  cat(sprintf("  saved %s\n", corr_file))
}


# ---------------------------------------------------------------------------
# 4. Assemble the search-cost comparison across variants
# ---------------------------------------------------------------------------
# The search-cost gammas and dispersion are the object that has to be stable for
# conditional independence to be innocuous — they carry the counterfactual.
report_params <- c("gamma_0", "gamma_inc", "gamma_educ", "gamma_age", "gamma_dual",
                   "gamma_adi", "gamma_hb", "gamma_exp", "log_sigma_alpha")
summ <- data.table(parameter = report_params, main = theta_hat[report_params])
for (nm in c("drop_info", "drop_web", "drop_phone", "drop_book", "corr_webphone")) {
  f <- file.path(results_dir, paste0("robust_", nm, ".csv"))
  if (!file.exists(f)) next
  v <- fread(f); vv <- setNames(v$estimate, v$parameter)
  summ[[nm]] <- vv[report_params]
}
cat("\n=== Search-cost parameters across robustness variants ===\n")
print(summ)
fwrite(summ, file.path(results_dir, "robust_summary.csv"))
cat("Saved robust_summary.csv\n")

if (file.exists(corr_file)) {
  v   <- fread(corr_file)
  rho <- setNames(v$estimate, v$parameter)[["rho_webphone"]]
  cat(sprintf("\nCorrelated-error rho_webphone = %.4f (0 = conditional independence;\n", rho))
  cat("  positive = web/phone substitutes, negative = complements).\n")
}

cat("\nConditional-independence robustness complete.\n")
