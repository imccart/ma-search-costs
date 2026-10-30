# 2-toy-checks.R — Check the estimator's model functions against brute-force
# enumeration on hand-sized markets.
#
# The toy panel has three beneficiaries and five beneficiary-years: a new
# enrollee, MA incumbents, a traditional-Medicare incumbent, and a county with
# no placed plans. Every quantity is recomputed here from the model's
# definitions by listing all 2^J consideration sets, with no use of the
# estimator's integral or shuffle code.
#
# Output: results/simulation/checks_toy.csv (check, value, tolerance, pass)

dir.create("results/simulation", showWarnings = FALSE, recursive = TRUE)
set.seed(7)

# ---- Toy panel -------------------------------------------------------------
mk_plans <- function(n) {
  data.table(
    prem = round(rlnorm(n, 3.5, 1)) * rbinom(n, 1, 0.6),
    oop = round(runif(n, 500, 4000)),
    star = sample(seq(2.5, 5, 0.5), n, TRUE),
    has_pd = rbinom(n, 1, 0.8),
    category = sample(c("HMO", "PPO", "PFFS"), n, TRUE, c(0.6, 0.3, 0.1)),
    pf = exp(-runif(n)),
    b_score = runif(n, 0.05, 1)
  )
}

toy_rows <- function(iy_id, n_inc, J, chosen, tm_inc = FALSE) {
  tm <- data.table(prem = 0, oop = 2500, star = NA_real_, has_pd = 0,
                   category = NA_character_, pf = 0, b_score = 0)
  tm[, `:=`(is_tm = TRUE, is_inc = tm_inc, is_default = TRUE)]
  inc <- mk_plans(n_inc)[, `:=`(is_tm = FALSE, is_inc = TRUE, is_default = TRUE)]
  plc <- mk_plans(J)[, `:=`(is_tm = FALSE, is_inc = FALSE, is_default = FALSE)]
  d <- rbind(tm, inc, plc)
  d[, is_chosen := FALSE]
  pick <- if (chosen == "tm") 1L else if (chosen == "inc") 2L else 1L + n_inc + as.integer(chosen)
  d[pick, is_chosen := TRUE]
  d[, iy := iy_id]
  d
}

toy_pair <- rbind(
  toy_rows(1, 0, 3, "2"),
  toy_rows(2, 1, 3, "tm"),
  toy_rows(3, 1, 4, "inc"),
  toy_rows(4, 0, 0, "tm"),
  toy_rows(5, 0, 4, "4", tm_inc = TRUE)
)

toy_iy <- data.table(
  iy = 1:5, bene_id = c(1, 1, 2, 3, 2), year = c(2016, 2017, 2016, 2016, 2017),
  w = c(1.2, 1.2, 0.8, 1.5, 0.8), cluster_id = c("1_1", "1_1", "1_2", "2_1", "1_2"),
  info = c(1, 0, 1, 0, 1), web = c(1, 0, 1, 0, 0), phone = c(0, 0, 1, 0, 1),
  book = c(2, 0, 1, 0, 2)
)
xc <- matrix(rnorm(5 * length(model_xc_cols), 0, 0.5), 5)
colnames(xc) <- model_xc_cols
toy_iy <- cbind(toy_iy, xc)

th0 <- c(
  alpha_prem = 0.77, alpha_oop = 0.10, beta = 0.45, xi_TM = 1.0, psi = 1.5, delta_PD = 0.8,
  cat_PPO = -0.05, cat_PFFS = -0.45,
  gamma_0 = -0.5, gamma_inc = -0.05, gamma_educ = -0.06, gamma_age = 0.02, gamma_dual = 0.14,
  gamma_adi = 0.02, gamma_hb = -0.1, gamma_exp = 0.03, gamma_easycmp = -0.05, gamma_infcmp = -0.04,
  sigma_alpha = 0.5,
  kappa_web = 0.7, kappa_phone = 1.2, kappa_book1 = 0.4, kappa_book2 = 1.0,
  omega_web = 1.5, omega_phone = 0.8, omega_book1 = 0.5, omega_book2 = 1.2,
  b0 = -1.5, b1 = 0.6, lambda_PF = 0.5, lambda_B = 0.4, sigma_p = 1.0
)
phi0 <- to_free(th0)

md <- make_model_data(toy_pair, toy_iy, n_shuffles = 50L, n_nu = 7L)

# ---- Brute-force model -----------------------------------------------------
lv <- expand.grid(info = 0:1, web = 0:1, phone = 0:1, book = 0:2)

brute_profiles <- function(th) {
  wsum <- function(pre) {
    lv$info + th[[paste0(pre, "_web")]] * lv$web + th[[paste0(pre, "_phone")]] * lv$phone +
      th[[paste0(pre, "_book1")]] * (lv$book == 1) + th[[paste0(pre, "_book2")]] * (lv$book == 2)
  }
  n <- wsum("omega")
  list(n = n, k = wsum("kappa"), s = plogis(th[["b0"]] + th[["b1"]] * n), R = lv$web)
}

brute_sets <- function(J) {
  if (J == 0) return(matrix(0, 1, 0))
  as.matrix(expand.grid(rep(list(0:1), J)))
}

set_prob <- function(ind, rho) {
  if (ncol(ind) == 0) return(1)
  drop(exp(ind %*% log(rho) + (1 - ind) %*% log1p(-rho)))
}

brute_value <- function(e0, ep, rho) {
  ind <- brute_sets(length(ep))
  sum(set_prob(ind, rho) * log(e0 + drop(ind %*% ep)))
}

brute_choice <- function(e_def, ep, rho) {
  ind <- brute_sets(length(ep))
  pu <- set_prob(ind, rho)
  tot <- sum(e_def) + drop(ind %*% ep)
  list(default = vapply(e_def, function(x) sum(pu * x / tot), numeric(1)),
       placed = vapply(seq_along(ep), function(j) sum(pu * ind[, j] * ep[j] / tot), numeric(1)))
}

brute_utility <- function(th, d) {
  ma <- !d$is_tm
  cat <- ifelse(is.na(d$category), "", d$category)
  -th[["alpha_prem"]] * d$prem / 1000 - th[["alpha_oop"]] * d$oop / 1000 +
    th[["xi_TM"]] * d$is_tm + th[["psi"]] * d$is_inc +
    ma * (th[["beta"]] * ifelse(is.na(d$star), 0, d$star - 3.5) + th[["delta_PD"]] * d$has_pd +
            th[["cat_PPO"]] * (cat == "PPO") + th[["cat_PFFS"]] * (cat == "PFFS"))
}

brute_loglik <- function(th, pair, iyt, nu) {
  bp <- brute_profiles(th)
  lPF <- th[["lambda_PF"]]
  lB <- th[["lambda_B"]]
  res <- lapply(seq_len(nrow(iyt)), function(i) {
    d <- pair[iy == i]
    e <- exp(brute_utility(th, d))
    def <- d$is_default
    e_def <- e[def]
    ep <- e[!def]
    pf <- d$pf[!def]
    b <- d$b_score[!def]
    J <- length(ep)
    obs <- which(lv$info == iyt$info[i] & lv$web == iyt$web[i] & lv$phone == iyt$phone[i] &
                 lv$book == iyt$book[i])
    rho <- 1 - (1 - bp$s[obs]) * (1 - lPF * pf * bp$R[obs]) * (1 - lB * b)
    pc <- brute_choice(e_def, ep, rho)
    logP <- log(c(pc$default, pc$placed)[which(c(d$is_chosen[def], d$is_chosen[!def]))])
    pfb <- if (J) mean(pf) else 0
    bb <- if (J) mean(b) else 0
    Vh <- vapply(seq_len(nrow(lv)), function(p) {
      rh <- 1 - (1 - bp$s[p]) * (1 - lPF * pfb * bp$R[p]) * (1 - lB * bb)
      brute_value(sum(e_def), ep, rep(rh, J))
    }, numeric(1))
    xg <- sum(c(1, unlist(iyt[i, ..model_xc_cols])) * th[gamma_names])
    ell <- vapply(nu$nodes, function(x) {
      z <- (Vh - exp(xg + th[["sigma_alpha"]] * x) * bp$k) / th[["sigma_p"]]
      z[obs] - log(sum(exp(z)))
    }, numeric(1))
    list(logP = logP, ell = ell, Vh = Vh)
  })
  b_ids <- unique(iyt$bene_id)
  ll <- vapply(b_ids, function(b) {
    rows <- which(iyt$bene_id == b)
    ellb <- Reduce(`+`, lapply(res[rows], `[[`, "ell"))
    log(sum(nu$weights * exp(ellb))) + sum(vapply(res[rows], `[[`, numeric(1), "logP"))
  }, numeric(1))
  list(ll_b = ll, Vh = do.call(rbind, lapply(res, `[[`, "Vh")))
}

checks <- list()
add_check <- function(name, value, tol) {
  checks[[length(checks) + 1]] <<- data.table(check = name, value = value, tolerance = tol,
                                              pass = is.finite(value) & value <= tol)
}

# ---- 1. Profiles and inclusion ---------------------------------------------
sp <- search_profiles()
add_check("profile_index matches the profile grid",
          max(abs(profile_index(lv$info, lv$web, lv$phone, lv$book) - seq_len(nrow(lv)))), 0)
add_check("R(p) equals the website indicator", max(abs(sp$R - lv$web)), 0)
bp0 <- brute_profiles(th0)
add_check("intensity n(p) matches its definition",
          max(abs(drop(sp$act %*% c(1, th0[omega_names])) - bp0$n)), 1e-12)
add_check("inclusion_prob matches the product form",
          abs(inclusion_prob(0.3, 1, 0.6, 0.5, 0.5, 0.4) - (1 - 0.7 * (1 - 0.3) * (1 - 0.2))), 1e-15)

# ---- 2. Grid and single beneficiary-year integrals vs enumeration -----------
grid <- choice_grid()
grid_ref <- choice_grid(h = 0.2, x_lo = -40)
add_check("grid weights integrate to one", abs(sum(grid$w) - 1), 1e-10)

for (spread in c(1, 4, 8)) {
  J8 <- 8
  e_def8 <- c(1, 0.7)
  ep8 <- exp(seq(-spread, spread, length.out = J8))
  rho8 <- seq(0.1, 0.9, length.out = J8)
  q <- choice_probs_one(e_def8, ep8, rho8, grid)
  q_ref <- choice_probs_one(e_def8, ep8, rho8, grid_ref)
  bf <- brute_choice(e_def8, ep8, rho8)
  add_check(sprintf("choice probabilities vs enumeration, utility spread %d", spread),
            max(abs(c(q$default, q$placed) - c(bf$default, bf$placed))), 1e-9)
  add_check(sprintf("choice probabilities, baseline vs fine grid, spread %d", spread),
            max(abs(c(q$default, q$placed) - c(q_ref$default, q_ref$placed))), 1e-9)
  add_check(sprintf("choice probabilities sum to one, spread %d", spread),
            abs(sum(q$default, q$placed) - 1), 1e-9)
  add_check(sprintf("expected inclusive value vs enumeration, spread %d", spread),
            abs(realized_value_one(e_def8, ep8, rho8, grid) - brute_value(sum(e_def8), ep8, rho8)),
            1e-9)
}

# ---- 3. Default quadrature estimate of perceived benefit at J = 8 -----------
pair8 <- toy_rows(1, 0, 8, "3")
iy8 <- toy_iy[1][, `:=`(iy = 1L, bene_id = 1)]
md8 <- make_model_data(pair8, iy8, n_nu = 7L)
par8 <- loglik(phi0, md8, components = TRUE)$parts
bf8 <- brute_loglik(th0, pair8, iy8, md8$nu)
add_check("perceived benefit, quadrature vs enumeration at J = 8",
          max(abs(par8$Vhat[1, ] - bf8$Vh[1, ])), 1e-9)

# ---- 4. Full likelihood vs exact set enumeration ----------------------------
ll_est <- loglik(phi0, md)$ll_b
ll_bf <- brute_loglik(th0, toy_pair, toy_iy, md$nu)$ll_b
add_check("per-beneficiary log-likelihood vs enumeration", max(abs(ll_est - ll_bf)), 1e-8)

# ---- 5. Transform Jacobian ---------------------------------------------------
jn <- jacobian(function(p) to_struct(setNames(p, free_names)), unname(phi0))
add_check("transform Jacobian vs numerical", max(abs(theta_jacobian(phi0) - jn)), 1e-7)
add_check("to_free inverts to_struct", max(abs(to_struct(to_free(th0)) - th0)), 1e-12)

# ---- 6. Analytic gradient vs numerical ---------------------------------------
set.seed(11)
grad_rows <- list()
for (k in 1:3) {
  phi_k <- if (k == 1) phi0 else phi0 + rnorm(n_theta, 0, 0.3)
  out <- loglik(phi_k, md)
  gn <- grad(function(p) loglik(setNames(p, free_names), md)$value, unname(phi_k))
  rel <- abs(out$gradient - gn) / (1 + abs(out$gradient))
  grad_rows[[k]] <- data.table(point = k, parameter = free_names, analytic = out$gradient,
                               numeric = gn, rel_diff = rel)
  add_check(sprintf("gradient vs numerical, point %d", k), max(rel), 1e-6)
  add_check(sprintf("weighted scores sum to the gradient, point %d", k),
            max(abs(-colSums(md$w_b * out$scores) / md$W_SUM - out$gradient)), 1e-12)
}

checks <- rbindlist(checks)
fwrite(checks, "results/simulation/checks_toy.csv")
fwrite(rbindlist(grad_rows), "results/simulation/checks_gradient.csv")
print(checks)
stopifnot(all(checks$pass))
