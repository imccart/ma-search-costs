# model-loglik.R — Log-likelihood and analytic score of the redesigned model.
#
# For beneficiary b with waves t,
#   L_b = E_nu[ prod_t mu_t(p_t | nu) ] * prod_t P_t(j_t | p_t),
# where mu is the logit over the 24 profiles with value
#   (Vhat_p - c(nu) k_p) / sigma_p,
# and P is the probability of the chosen option given inclusion probabilities at
# the observed profile. The nu integral uses Gauss-Hermite nodes; P and Vhat use
# the integral (or optional reference shuffle) constructions in model-choice.R and
# model-data.R. Utilities are carried relative to the default-set inclusive
# value, which cancels from the profile logit.
#
# Returns the objective -sum_b w_b ll_b / W_SUM, its gradient in free
# coordinates, per-beneficiary log-likelihoods ll_b, and per-beneficiary
# unweighted scores (n_b x n_theta, free coordinates).

loglik <- function(phi, md, components = FALSE) {
  if (!is.null(names(phi))) phi <- phi[free_names]
  th <- to_struct(phi)
  pr <- md$pair
  N <- md$n_iy
  act <- md$prof$act
  Rp <- md$prof$R
  iyv <- pr$iy
  dr <- pr$is_default
  pl <- md$placed
  ipl <- iyv[pl]
  pg <- md$pl_groups
  pobs <- md$iy$p_obs
  ch <- md$iy$chosen_row
  chA <- md$iy$chosen_placed

  # ---- Utilities relative to the default-set inclusive value
  v <- drop(pr$X %*% th[utility_names])
  vt <- v[md$tm_row]
  L0 <- vt + log(rowsum(exp(v - vt[iyv]) * dr, iyv)[, 1])
  et <- exp(v - L0[iyv])

  # ---- Profiles
  n_p <- drop(act %*% c(1, th[omega_names]))
  k_p <- drop(act %*% c(1, th[kappa_names]))
  s_p <- plogis(th[["b0"]] + th[["b1"]] * n_p)
  ds_p <- s_p * (1 - s_p)
  lPF <- th[["lambda_PF"]]
  lB <- th[["lambda_B"]]
  b1 <- th[["b1"]]

  # ---- Choice stage at the observed profile
  s_i <- s_p[pobs]
  R_i <- Rp[pobs]
  pf <- pr$pf[pl]
  bs <- pr$b_score[pl]
  chiR <- 1 - lPF * pf * R_i[ipl]
  chiB <- 1 - lB * bs
  rho <- 1 - (1 - s_i[ipl]) * chiR * chiB
  a <- rep(1, N)
  a[chA] <- 1 + et[ch[chA]]
  cc <- et[pl] / a[ipl]
  keep <- as.numeric(!pr$is_chosen[pl])
  tau <- md$grid$tau
  Fq <- matrix(0, N, length(tau))
  for (qs in md$qchunks) {
    lf <- log1p(rho * expm1(-outer(cc, tau[qs])))
    Fq[pg, qs] <- rowsum(lf * keep, ipl)
  }
  Lam <- exp(Fq) * rep(md$grid$w, each = N)
  Phi <- rowSums(Lam)
  rho_all <- rep(1, length(v))
  rho_all[pl] <- rho
  logP <- log(rho_all[ch]) + log(et[ch] / a) + log(Phi)

  # ---- Expected inclusive value under perceived (common) inclusion rates
  qR <- lPF * md$iy$pf_bar
  qB <- lB * md$iy$b_bar
  rh <- 1 - (1 - qB) * sweep(1 - outer(qR, Rp), 2, 1 - s_p, "*")
  P <- nrow(act)
  Vh <- matrix(0, N, P)
  dV <- matrix(0, N, P)
  use_quadrature <- identical(md$settings$benefit_method, 'quadrature')
  for (g in md$groups) {
    J <- g$J
    nJ <- length(g$rows)
    E <- matrix(et[g$G], nJ, J)
    if (use_quadrature) {
      for (ii in seq_len(nJ)) {
        row <- g$rows[ii]
        pvq <- perceived_value_one(E[ii, ], rh[row, ], md$grid)
        Vh[row, ] <- pvq$value
        dV[row, ] <- pvq$dr
      }
      next
    }
    IV <- matrix(0, nJ, J)
    for (r in seq_len(ncol(g$perm))) IV <- IV + log1p(E[, g$perm[, r], drop = FALSE] %*% g$U)
    IV0 <- cbind(0, IV / ncol(g$perm))
    lr <- log(rh[g$rows, , drop = FALSE])
    l1r <- log1p(-rh[g$rows, , drop = FALSE])
    Vg <- matrix(0, nJ, P)
    dg <- matrix(0, nJ, P)
    for (K in 0:J) {
      Vg <- Vg + exp(lchoose(J, K) + K * lr + (J - K) * l1r) * IV0[, K + 1]
      if (K < J) dg <- dg + exp(lchoose(J - 1, K) + K * lr + (J - 1 - K) * l1r) *
          (IV0[, K + 2] - IV0[, K + 1])
    }
    Vh[g$rows, ] <- Vg
    dV[g$rows, ] <- J * dg
  }

  # ---- Profile logit over the nu nodes
  xg <- drop(md$iy$Xc %*% th[gamma_names])
  sa <- th[["sigma_alpha"]]
  sp <- th[["sigma_p"]]
  nu <- md$nu$nodes
  M <- length(nu)
  io <- cbind(seq_len(N), pobs)
  ell <- kbar <- zbar <- zob <- cm <- matrix(0, N, M)
  abar <- array(0, c(N, ncol(act), M))
  for (m in seq_len(M)) {
    c_m <- exp(xg + sa * nu[m])
    z <- (Vh - outer(c_m, k_p)) / sp
    mx <- z[cbind(seq_len(N), max.col(z, "first"))]
    ez <- exp(z - mx)
    se <- rowSums(ez)
    mu <- ez / se
    zob[, m] <- z[io]
    ell[, m] <- z[io] - mx - log(se)
    kbar[, m] <- drop(mu %*% k_p)
    zbar[, m] <- rowSums(mu * z)
    abar[, , m] <- mu %*% act
    cm[, m] <- c_m
  }
  bi <- md$b_of_iy
  Lb <- sweep(rowsum(ell, bi), 2, log(md$nu$weights), "+")
  mb <- Lb[cbind(seq_len(nrow(Lb)), max.col(Lb, "first"))]
  A_b <- mb + log(rowSums(exp(Lb - mb)))
  ll_b <- A_b + rowsum(logP, bi)[, 1]
  value <- -sum(md$w_b * ll_b) / md$W_SUM

  # =========================== Score ===========================

  # ---- Choice stage: adjoints in rho and in log relative utility
  LamN <- Lam / Phi
  Hr <- numeric(length(pl))
  Gc <- numeric(length(pl))
  for (qs in md$qchunks) {
    em <- expm1(-outer(cc, tau[qs]))
    f <- 1 + rho * em
    Lq <- LamN[ipl, qs, drop = FALSE]
    Hr <- Hr + rowSums(Lq * em / f)
    Gc <- Gc - rho * rowSums(Lq * sweep(em + 1, 2, tau[qs], "*") / f)
  }
  g_c <- Gc * cc * keep
  Hr <- Hr * keep
  sum_g <- numeric(N)
  sum_g[pg] <- rowsum(g_c, ipl)[, 1]
  dloga <- -1 - sum_g
  D <- numeric(length(v))
  D[pl] <- g_c
  D[ch[chA]] <- 1 + dloga[chA] * et[ch[chA]] / a[chA]
  sumD <- rowsum(D, iyv)[, 1]
  psi <- D
  psi[dr] <- -et[dr] * (sumD[iyv[dr]] + !chA[iyv[dr]])
  psi[ch[!chA]] <- psi[ch[!chA]] + 1
  gr <- Hr
  gr[md$chosen_pl_pos] <- gr[md$chosen_pl_pos] + 1 / rho[md$chosen_pl_pos]

  # ---- Profile stage: posterior-averaged residual dA_b / dVhat
  pii <- exp(Lb - A_b)[bi, , drop = FALSE]
  mbar <- matrix(0, N, P)
  for (m in seq_len(M)) {
    z <- (Vh - outer(cm[, m], k_p)) / sp
    mx <- z[cbind(seq_len(N), max.col(z, "first"))]
    ez <- exp(z - mx)
    mbar <- mbar + pii[, m] * ez / rowSums(ez)
  }
  Ab <- -mbar
  Ab[io] <- Ab[io] + 1
  Ab <- Ab / sp

  # ---- Expected inclusive value: backward pass through quadrature or orderings
  DV <- numeric(length(v))
  for (g in md$groups) {
    J <- g$J
    nJ <- length(g$rows)
    E <- matrix(et[g$G], nJ, J)
    if (use_quadrature) {
      for (ii in seq_len(nJ)) {
        row <- g$rows[ii]
        pvq <- perceived_value_one(E[ii, ], rh[row, ], md$grid, Ab[row, ])
        DV[g$G[ii, ]] <- pvq$dloge
      }
      next
    }
    lr <- log(rh[g$rows, , drop = FALSE])
    l1r <- log1p(-rh[g$rows, , drop = FALSE])
    Abr <- Ab[g$rows, , drop = FALSE]
    A <- matrix(0, nJ, J)
    for (K in 1:J) A[, K] <- rowSums(Abr * exp(lchoose(J, K) + K * lr + (J - K) * l1r))
    Dacc <- matrix(0, nJ, J)
    tU <- t(g$U)
    for (r in seq_len(ncol(g$perm))) {
      pm <- g$perm[, r]
      C <- E[, pm, drop = FALSE] %*% g$U
      Dacc[, pm] <- Dacc[, pm] + (A / (1 + C)) %*% tU
    }
    DV[g$G] <- Dacc / ncol(g$perm)
  }
  pv <- if (use_quadrature) DV else et * DV
  sum_pv <- rowsum(pv, iyv)[, 1]
  pv[dr] <- -et[dr] * sum_pv[iyv[dr]]
  psi <- psi + pv

  # ---- Assemble per beneficiary-year scores in structural coordinates
  Gs <- matrix(0, N, n_theta, dimnames = list(NULL, theta_names))
  Gs[, utility_names] <- rowsum(psi * pr$X, iyv)

  ex_names <- c("b0", "b1", omega_names)
  gs <- numeric(N)
  gPF <- numeric(N)
  gB <- numeric(N)
  if (length(pl)) {
    gs[pg]  <- rowsum(gr * chiR * chiB, ipl)[, 1]
    gPF[pg] <- rowsum(gr * (1 - s_i[ipl]) * pf * R_i[ipl] * chiB, ipl)[, 1]
    gB[pg]  <- rowsum(gr * (1 - s_i[ipl]) * chiR * bs, ipl)[, 1]
  }
  Gs[, ex_names] <- gs * ds_p[pobs] * cbind(1, n_p[pobs], b1 * act[pobs, -1, drop = FALSE])
  Gs[, "lambda_PF"] <- gPF
  Gs[, "lambda_B"] <- gB

  zeta <- Ab * dV
  Ue <- cbind(ds_p, ds_p * n_p, ds_p * b1 * act[, -1])
  Z1 <- zeta %*% Ue
  Z2 <- zeta %*% (Rp * Ue)
  Gs[, ex_names] <- Gs[, ex_names] + (1 - qB) * (Z1 - qR * Z2)
  zs <- drop(zeta %*% (1 - s_p))
  zsR <- drop(zeta %*% ((1 - s_p) * Rp))
  Gs[, "lambda_PF"] <- Gs[, "lambda_PF"] + md$iy$pf_bar * (1 - qB) * zsR
  Gs[, "lambda_B"] <- Gs[, "lambda_B"] + md$iy$b_bar * (zs - qR * zsR)

  dk <- k_p[pobs] - kbar
  wc <- pii * cm
  Gs[, gamma_names] <- -(md$iy$Xc * rowSums(wc * dk)) / sp
  Gs[, "sigma_alpha"] <- -rowSums(wc * dk * rep(nu, each = N)) / sp
  for (j in seq_along(kappa_names)) {
    Gs[, kappa_names[j]] <- -rowSums(wc * (act[pobs, j + 1] - abar[, j + 1, ])) / sp
  }
  Gs[, "sigma_p"] <- -rowSums(pii * (zob - zbar)) / sp

  scores <- rowsum(Gs, bi) %*% theta_jacobian(phi)
  dimnames(scores) <- list(NULL, free_names)
  out <- list(value = value, gradient = -colSums(md$w_b * scores) / md$W_SUM,
              ll_b = ll_b, scores = scores)
  if (components) {
    out$parts <- list(logP = logP, Vhat = Vh + L0, rhohat = rh, ell = ell, rho = rho,
                      et = et, L0 = L0, s_p = s_p, k_p = k_p, n_p = n_p)
  }
  out
}
