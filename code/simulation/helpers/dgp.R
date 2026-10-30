# dgp.R — Data-generating process for the recovery experiments.
#
# Written independently of the estimator: perceived benefits by exact
# enumeration of consideration sets, Bernoulli inclusion draws, and Gumbel
# choice shocks. One standard-normal search-cost draw persists across a
# beneficiary's waves, and each simulated choice becomes the next wave's
# current plan. Initial current plans are drawn independently of search cost.
# Menus hold J_range MA plans; enumeration limits J to about 12.

recovery_truth <- c(
  alpha_prem = .77, alpha_oop = .30, beta = .45, xi_TM = 1, psi = 1.5,
  delta_PD = .8, cat_PPO = -.05, cat_PFFS = -.45,
  gamma_0 = -.5, gamma_inc = -.3, gamma_educ = -.3, gamma_age = .2,
  gamma_dual = .3, gamma_adi = .2, gamma_hb = -.3, gamma_exp = -.2,
  gamma_easycmp = -.3, gamma_infcmp = -.3, sigma_alpha = .5,
  kappa_web = .7, kappa_phone = 1.2, kappa_book1 = .4, kappa_book2 = 1,
  omega_web = 1.5, omega_phone = .8, omega_book1 = .5, omega_book2 = 1.2,
  b0 = -1.5, b1 = .6, lambda_PF = .5, lambda_B = .4, sigma_p = 1)

generate_recovery_panel <- function(n_b, waves, th, seed, J_range = 3:6) {
  stopifnot(identical(names(th), theta_names), max(J_range) <= 12)
  set.seed(seed)
  profiles <- expand.grid(info = 0:1, web = 0:1, phone = 0:1, book = 0:2)
  action <- with(profiles, cbind(info, web, phone, book == 1, book == 2))
  intensity <- drop(action %*% c(1, th[omega_names]))
  cost <- drop(action %*% c(1, th[kappa_names]))
  own <- plogis(th[['b0']] + th[['b1']] * intensity)
  sets_by_J <- lapply(seq_len(max(J_range)), function(J) as.matrix(expand.grid(rep(list(0:1), J))))
  pair_rows <- iy_rows <- draw_rows <- list()
  id <- 0L
  for (b in seq_len(n_b)) {
    nu <- rnorm(1)
    persistent_x <- rnorm(9, 0, .7)
    J <- if (length(J_range) == 1) J_range else sample(J_range, 1)
    prior <- sample(0:J, 1)
    base_prem <- runif(J, 0, 2500) * rbinom(J, 1, .7)
    base_oop <- runif(J, 500, 5000)
    stars <- sample(seq(2.5, 5, .5), J, TRUE)
    pd <- rbinom(J, 1, .7)
    category <- sample(c('HMO', 'PPO', 'PFFS'), J, TRUE)
    broker <- runif(J, .02, 1)
    for (t in seq_len(waves)) {
      id <- id + 1L
      x <- persistent_x + rnorm(9, 0, .3)
      names(x) <- model_xc_cols
      prem <- pmax(0, base_prem + rnorm(J, 0, 100))
      oop <- pmax(100, base_oop + rnorm(J, 0, 600))
      pf <- exp(-rank(prem + base_oop) / J)
      d <- data.table(prem = c(0, prem), oop = c(runif(1, 1500, 4500), oop),
                      star = c(NA_real_, stars), has_pd = c(0, pd),
                      category = c(NA_character_, category), pf = c(0, pf),
                      b_score = c(0, broker), is_tm = c(TRUE, rep(FALSE, J)),
                      is_inc = 0:J == prior, is_default = (0:J == prior | 0:J == 0))
      v <- -th[['alpha_prem']] * d$prem / 1000 - th[['alpha_oop']] * d$oop / 1000 +
        th[['xi_TM']] * d$is_tm + th[['psi']] * d$is_inc
      v[-1] <- v[-1] + th[['beta']] * (stars - 3.5) + th[['delta_PD']] * pd +
        th[['cat_PPO']] * (category == 'PPO') + th[['cat_PFFS']] * (category == 'PFFS')
      placed <- which(!d$is_default)
      e0 <- sum(exp(v[d$is_default]))
      ep <- exp(v[placed])
      sets <- if (length(placed)) sets_by_J[[length(placed)]] else matrix(0, 1, 0)
      K <- rowSums(sets)
      r <- 1 - (1 - own) * (1 - th[['lambda_PF']] * mean(d$pf[placed]) * profiles$web) *
        (1 - th[['lambda_B']] * mean(d$b_score[placed]))
      probabilities <- exp(outer(K, log(r)) + outer(length(placed) - K, log1p(-r)))
      value <- drop(crossprod(log(e0 + drop(sets %*% ep)), probabilities))
      ci <- exp(th[['gamma_0']] + sum(x * th[gamma_names[-1]]) + th[['sigma_alpha']] * nu)
      z <- (value - ci * cost) / th[['sigma_p']]
      p <- sample.int(nrow(profiles), 1, prob = exp(z - max(z)))
      rho <- 1 - (1 - own[p]) * (1 - th[['lambda_PF']] * d$pf[placed] * profiles$web[p]) *
        (1 - th[['lambda_B']] * d$b_score[placed])
      considered <- d$is_default
      considered[placed] <- rbinom(length(placed), 1, rho) == 1
      u <- v - log(-log(runif(J + 1)))
      u[!considered] <- -Inf
      chosen <- which.max(u)
      d[, `:=`(iy = id, is_chosen = seq_len(.N) == chosen)]
      pair_rows[[id]] <- d
      iy_rows[[id]] <- cbind(data.table(iy = id, bene_id = b, year = 2015L + t,
        w = 1, cluster_id = as.character(b)), as.data.table(profiles[p, ]), as.data.table(as.list(x)))
      draw_rows[[id]] <- data.table(iy = id, nu = nu, profile = p, chosen = chosen,
        prior = prior, considered = sum(considered), cost = ci)
      prior <- chosen - 1L
    }
  }
  list(pair = rbindlist(pair_rows), iy = rbindlist(iy_rows), draws = rbindlist(draw_rows))
}

write_panel <- function(panel, folder, tag) {
  for (nm in c('pair', 'iy', 'draws')) {
    fwrite(panel[[nm]], file.path(folder, sprintf('panel_%s_%s.csv', tag, nm)))
  }
}

read_panel <- function(folder, tag) {
  pair <- fread(file.path(folder, sprintf('panel_%s_pair.csv', tag)),
                colClasses = list(character = 'category'))
  pair[category == '', category := NA_character_]
  list(pair = pair,
       iy = fread(file.path(folder, sprintf('panel_%s_iy.csv', tag)),
                  colClasses = list(character = 'cluster_id')),
       draws = fread(file.path(folder, sprintf('panel_%s_draws.csv', tag))))
}
