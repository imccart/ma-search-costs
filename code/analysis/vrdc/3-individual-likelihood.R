# 3-individual-likelihood.R — Joint demand-side likelihood (search + choice)
#
# Consolidated spec in agents/model.md. The per-beneficiary likelihood is JOINT
# over the observed search actions and the plan choice, integrated over a
# beneficiary-level search-cost random effect:
#
#   L_b = ( prod_w P(choice_bw) ) * E_nu [ prod_w P(actions_bw | nu) ]
#
# The choice probability is independent of nu (utility + consideration breadth
# from the OBSERVED actions), so it is computed once per bene-year; only the
# action likelihood is integrated over nu (drawn once per BENE, shared across
# that bene's waves, which identifies sigma_alpha from the panel). Estimated by
# simulated ML in 4-estimate-mle.R.
#
# Search actions (all from the SAS export, recoded in script 1):
#   act_info  (KNINFMCR), act_web (KVSITWEB), act_phone (KCPHINFO)  — binary
#   book_read (KBOKREAD)  — ordered 0/1/2 (none/parts/thorough)
# Cost covariates: demographics + book_understood_dm (KBOKUNDR) + tenure_dm
#   (madv_years_enrolled). Consideration: help AND delegate (KCHIHELP=2 / =3).
# Utility uses bene-specific EC (bene_mc / bene_vc from script 2).

N_SIM_DRAWS <- 50L   # nu draws per beneficiary; common random numbers set in 5


# ---- One-time prep: incumbency flag + NA guards ---------------------------
bene[, prior_plan_offered := as.integer(has_prior_year == 1L & incumbent_bene_year == 1L)]
for (col in c("log_inc_dm","educ_yrs_dm","age_dm","is_dual","adi_dm",
              "book_understood_dm","tenure_dm","easy_compare_dm","enough_info_dm",
              "act_info","act_web","act_phone","book_read","review_level",
              "KCHIHELP_help","KCHIHELP_delegate"))
  set(bene, which(is.na(bene[[col]])), col, 0)


# ---- Parameter layout (25) ------------------------------------------------
theta_names <- c(
  # Utility (5): + delta on cost variance (risk term)
  "alpha", "beta", "xi_FFS", "psi", "delta",
  # Search-cost covariates (10): + comprehension (hb), tenure (exp),
  #   easy-to-compare (KNCOVOPT), enough-info-to-compare (KNCOVINF)
  "gamma_0", "gamma_inc", "gamma_educ", "gamma_age", "gamma_dual", "gamma_adi",
  "gamma_hb", "gamma_exp", "gamma_easycmp", "gamma_infcmp",
  # Search-cost dispersion (1)
  "log_sigma_alpha",
  # Action baselines (4) + handbook gap (1) + review-frequency indicator (2)
  "kappa_info", "kappa_web", "kappa_phone", "kappa_book", "tau_gap",
  "kappa_review", "tau_review_gap",
  # Consideration breadth (5)
  "b0", "b_info", "b_web", "b_phone", "b_book",
  # Awareness weights (2): market-level PF constant + broker constant
  "lambda_PF_0", "lambda_broker_0"
)

# Bounds: alpha, delta, both ordered gaps, and both lambda constants >= 0;
# everything else free.
theta_lower <- setNames(rep(-Inf, length(theta_names)), theta_names)
theta_lower[c("alpha","delta","tau_gap","tau_review_gap",
              "lambda_PF_0","lambda_broker_0")] <- 0
theta_upper <- setNames(rep(Inf, length(theta_names)), theta_names)

unpack_theta <- function(theta) {
  # Honor names when present so a theta stored in a different order (e.g. a
  # reloaded theta_hat) still maps each value to the right parameter. The
  # optimizer passes an unnamed vector already in theta_names order, unchanged.
  if (!is.null(names(theta))) theta <- theta[theta_names]
  setNames(as.list(theta), theta_names)
}


# ---- Stage 3 utility (bene-specific EC; incumbent psi added per-bene) ------
compute_bene_utility <- function(mkt, mc, vc, th) {
  v <- numeric(nrow(mkt))
  is_ffs <- mkt$plan_kind == "FFS"; is_ma <- !is_ffs
  mcs <- mc / 1e3        # cost in $000s
  vcs <- vc / 1e6        # cost variance in ($000s)^2
  v[is_ffs] <- -th$alpha * mcs[is_ffs] - th$delta * vcs[is_ffs] + th$xi_FFS
  v[is_ma]  <- -th$alpha * mcs[is_ma]  - th$delta * vcs[is_ma] +
                th$beta * ifelse(is.na(mkt$Star_Rating[is_ma]), 0,
                                 mkt$Star_Rating[is_ma] - 3.5)
  v
}

# ---- Stage 1 prominence over non-default MA plans -------------------------
compute_market_prominence <- function(mkt) {
  is_ma <- mkt$plan_kind == "MA"
  s_PF <- ifelse(is.na(mkt$pf_rank_score), 0, mkt$pf_rank_score) * is_ma
  loo   <- ifelse(is.na(mkt$parent_org_loo_national), 0, mkt$parent_org_loo_national)
  bdens <- ifelse(is.na(mkt$broker_density_per_k),    0, mkt$broker_density_per_k)
  s_broker <- loo * bdens * is_ma
  list(s_PF = s_PF, s_broker = s_broker, is_ma = is_ma)
}

# ---- Market-level salience over MA plans (constant PF + broker weights) -----
# lam_PF and lam_broker are now constants, so salience is common to all benes
# in a market (no act_web / KCHIHELP dependence). brow is retained in the
# signature only so call sites are unchanged.
compute_salience <- function(mkt, prom, brow, th) {
  if (!any(prom$is_ma))
    return(list(w = numeric(nrow(mkt)), W = 0, is_ma = prom$is_ma))
  lam_PF     <- th$lambda_PF_0
  lam_broker <- th$lambda_broker_0
  log_w <- lam_PF * prom$s_PF + lam_broker * prom$s_broker
  log_w[!prom$is_ma] <- -Inf
  mx <- max(log_w[prom$is_ma])
  w <- ifelse(prom$is_ma, exp(log_w - mx), 0)
  list(w = w, W = sum(w), is_ma = prom$is_ma)
}

# ---- Consideration breadth K from observed actions ------------------------
compute_K <- function(brow, th) {
  exp(th$b0 + th$b_info * brow$act_info + th$b_web * brow$act_web +
      th$b_phone * brow$act_phone + th$b_book * as.integer(brow$book_read > 0))
}

# ---- Inclusion probs: FFS & incumbent fixed at 1; free MA Goeree ----------
compute_phi <- function(mkt, sal, K_i, brow) {
  is_ffs <- mkt$plan_kind == "FFS"; is_ma <- sal$is_ma
  phi <- numeric(nrow(mkt)); phi[is_ffs] <- 1
  inc <- brow$prior_plan_offered == 1L &
         !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
  phi[inc] <- 1                      # incumbent (MA or FFS) always considered
  free <- is_ma & !inc
  if (K_i > 0 && any(free)) {
    Wf <- sum(sal$w[free])
    if (Wf > 0) { raw <- K_i * sal$w[free] / Wf; raw[raw > 1] <- 1; phi[free] <- raw }
  }
  phi
}

# ---- Goeree consideration-set choice probability --------------------------
compute_choice_prob <- function(v, phi) {
  considered <- phi > 0
  ev  <- exp(v - max(v[considered]))
  num <- phi * ev
  num / sum(num)
}

# ---- Search benefit B: value of considering the whole menu over the ---------
# no-search default (FFS + incumbent). E[max over all plans] - E[max over the
# default set], in log-sum-exp form; larger when the plans differ more from the
# default in how well they fit the bene. Built from expected-cost utilities, so
# it varies across counties with the local menu.
compute_search_benefit <- function(v, default) {
  iv <- function(x) { mx <- max(x); log(sum(exp(x - mx))) + mx }
  iv(v) - iv(v[default])
}

# ---- Deterministic part of log search cost (no nu) ------------------------
compute_log_c_det <- function(brow, th) {
  th$gamma_0 +
    th$gamma_inc  * brow$log_inc_dm + th$gamma_educ * brow$educ_yrs_dm +
    th$gamma_age  * brow$age_dm     + th$gamma_dual * brow$is_dual +
    th$gamma_adi  * brow$adi_dm     + th$gamma_hb   * brow$book_understood_dm +
    th$gamma_exp  * brow$tenure_dm  +
    th$gamma_easycmp * brow$easy_compare_dm + th$gamma_infcmp * brow$enough_info_dm
}

# ---- Action log-likelihood (vectorized over a bene's waves) ---------------
# ai/aw/ap binary; br ordered 0/1/2; B, c, and these are vectors over waves.
loglik_actions <- function(ai, aw, ap, br, rv, th, B, c) {
  z <- B - c
  lp <- function(act, kap) { p <- plogis(z - kap); log(act * p + (1 - act) * (1 - p) + 1e-12) }
  ordered_ll <- function(lev, k1, gap) {   # 3-level ordered: 0 / 1 / 2
    hi <- plogis(z - (k1 + gap)); mid <- plogis(z - k1) - hi; lo <- 1 - plogis(z - k1)
    log(ifelse(lev == 2L, hi, ifelse(lev == 1L, mid, lo)) + 1e-12)
  }
  lp(ai, th$kappa_info) + lp(aw, th$kappa_web) + lp(ap, th$kappa_phone) +
    ordered_ll(br, th$kappa_book,   th$tau_gap) +
    ordered_ll(rv, th$kappa_review, th$tau_review_gap)
}


# ---- Full simulated joint log-likelihood ----------------------------------
compute_individual_loglik <- function(theta, nu_draws, return_components = FALSE) {
  th <- unpack_theta(theta)

  n <- nrow(bene)
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
    B_vec[i]     <- compute_search_benefit(v, mkt$plan_kind == "FFS" | inc)
    logc_det[i]  <- compute_log_c_det(brow, th)
  }

  sigma <- exp(th$log_sigma_alpha); R <- length(nu_draws)
  ll_bene <- numeric(length(idx_by_bene))
  for (bi in seq_along(idx_by_bene)) {
    rows <- idx_by_bene[[bi]]
    ch_sum <- sum(ll_choice[rows])
    ai <- bene$act_info[rows]; aw <- bene$act_web[rows]
    ap <- bene$act_phone[rows]; br <- bene$book_read[rows]; rv <- bene$review_level[rows]
    Bw <- B_vec[rows]; ld <- logc_det[rows]
    draw <- numeric(R)
    for (r in seq_len(R)) {
      c_r <- exp(ld + sigma * nu_draws[r])
      draw[r] <- sum(loglik_actions(ai, aw, ap, br, rv, th, Bw, c_r))
    }
    m <- max(draw)
    ll_bene[bi] <- ch_sum + m + log(mean(exp(draw - m)))
  }
  ll_bene <- ll_bene * wgt_by_bene

  if (return_components)
    list(ll_bene = ll_bene, ll_choice = ll_choice, B = B_vec, bene_ids = names(idx_by_bene))
  else sum(ll_bene)
}


# ---- Model predictions at a parameter vector (for fit diagnostics) --------
compute_predictions <- function(theta, nu_draws) {
  th <- unpack_theta(theta)
  n <- nrow(bene); R <- length(nu_draws); sigma <- exp(th$log_sigma_alpha)
  p_ffs <- p_inc_ma <- p_search <- numeric(n)

  for (i in seq_len(n)) {
    brow <- bene_rows[[i]]; mid <- brow$market_id; mkt <- markets[[mid]]
    v    <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th)
    prom <- market_prom[[mid]]; sal <- compute_salience(mkt, prom, brow, th)
    inc  <- brow$prior_plan_offered == 1L &
            !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + th$psi
    p <- compute_choice_prob(v, compute_phi(mkt, sal, compute_K(brow, th), brow))
    is_ffs <- mkt$plan_kind == "FFS"; is_ma <- !is_ffs
    p_ffs[i] <- sum(p[is_ffs])
    pma <- sum(p[is_ma]); p_inc_ma[i] <- if (pma > 0) sum(p[inc & is_ma]) / pma else 0

    B <- compute_search_benefit(v, mkt$plan_kind == "FFS" | inc); ld <- compute_log_c_det(brow, th)
    pno <- numeric(R)
    for (r in seq_len(R)) {
      z <- B - exp(ld + sigma * nu_draws[r])
      pno[r] <- (1 - plogis(z - th$kappa_info)) * (1 - plogis(z - th$kappa_web)) *
                (1 - plogis(z - th$kappa_phone)) * (1 - plogis(z - th$kappa_book)) *
                (1 - plogis(z - th$kappa_review))
    }
    p_search[i] <- 1 - mean(pno)
  }

  data.table(
    wgt        = bene$wgt_full_sample, is_dual = bene$is_dual, has_bach = bene$has_bach,
    p_search   = p_search,
    obs_search = as.integer(bene$act_info == 1 | bene$act_web == 1 |
                            bene$act_phone == 1 | bene$book_read > 0 |
                            bene$review_level > 0),
    p_ffs      = p_ffs,    obs_ffs    = bene$is_ffs_admin,
    p_inc_ma   = p_inc_ma,
    obs_inc_ma = as.integer(bene$chosen_pid == bene$prior_plan_id),
    ma_prior   = as.integer(bene$is_ffs_admin == 0L & bene$has_prior_year == 1L &
                            bene$incumbent_bene_year == 1L & bene$prior_plan_id != "FFS")
  )
}


# ---------------------------------------------------------------------------
# One-time precompute (theta-independent; hoisted out of the objective)
# ---------------------------------------------------------------------------
# None of these depend on theta, so building them once here rather than on every
# optimizer evaluation inside compute_individual_loglik is a large speedup with
# no effect on any estimate. The two functions above reference these globals at
# call time (bene is fully finalized by the NA-guard block at the top of this
# script, so the precomputed rows match exactly).
market_prom <- lapply(markets, compute_market_prominence)
bene_rows   <- lapply(seq_len(nrow(bene)), function(i) as.list(bene[i]))
idx_by_bene <- split(seq_len(nrow(bene)), bene$BASEID)
wgt_by_bene <- bene$wgt_full_sample[vapply(idx_by_bene, `[`, integer(1), 1L)]
