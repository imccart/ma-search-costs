# 9-counterfactual.R — Decision-support counterfactual (lower search cost)
#
# The deliverable of the structural model (agents/model.md, "Counterfactuals").
# A decision-support intervention lowers the per-person search cost c_i, which
# raises the probability of each search action, which widens consideration
# breadth K_i, which brings more (and better) plans into the considered set, which
# changes plan choice. Welfare is measured fully and decomposed into two channels:
# a plan-value channel (broader consideration brings better plans within reach)
# and a search-cost channel (the net effect of taking more actions at a lower
# per-action cost). The consideration benefit B_i drives action-taking but is
# realized through the plan-value channel, so the search-cost channel is measured
# as c_i * E[#actions] rather than an action-surplus term, which would count that
# benefit twice. The two channels are additive but, because the model is a
# measurement graft rather than one search-then-choice optimization, their sum is
# an accounting decomposition and is not sign-restricted.
#
# Dose-response: c_i is scaled by 1, 0.75, 0.50, and 0 (0%, 25%, 50%, 100% cuts);
# the 100% cut (c -> 0) is the frictionless upper bound.
#
# Heterogeneity is carried by the persistent search-cost random effect. For each
# beneficiary we form the empirical-Bayes posterior over the effect from their
# OBSERVED actions (a heavy searcher is a low-cost type and moves less under the
# intervention), then hold that posterior fixed and re-integrate the actions —
# which change because c changes — under each dose. Baseline and counterfactual
# are both model quantities integrated the same way, differing only in the cost
# scale, so the difference isolates the intervention.
#
# Solved once at theta_hat. Runs STANDALONE after script 3 (reloads theta_hat,
# nu_draws, W_SUM), no re-estimation and no SEs, so it is a single pass of a few
# minutes rather than a multi-hour seat job.
#
# Outcomes (survey-weighted over bene-years), per dose:
#   dominated_share      P(chosen plan is mean-variance dominated) — the RF fact
#   search_rate          P(any of the four search actions)
#   mean_consideration   expected number of plans in the considered set
#   mean_EC_chosen       expected out-of-pocket cost of the chosen plan ($)
#   welfare_plan_value   $ gain from broader consideration (inclusive-value change)
#   welfare_search_cost  $ from the change in search effort, net of the lower c_i
#   welfare_total        welfare_plan_value + welfare_search_cost
# Welfare columns are all relative to the 0% (no-intervention) dose.

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


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
theta_hat <- theta_hat[theta_names]
stopifnot(!any(is.na(theta_hat)), identical(names(theta_hat), theta_names))

th    <- unpack_theta(theta_hat)
sigma <- exp(th$log_sigma_alpha)
R     <- length(nu_draws)


# ---- Mean-variance dominance per market (replicates 5-construct-dominance.R) --
# Primary RF definition: within a county-year and a plan_category x has_partd
# group, plan a is dominated if some plan b has mean_cost_b <= mean_cost_a AND
# var_cost_b <= var_cost_a, strict on at least one. Computed inside each market
# (the bene's own choice set), among MA plans only; FFS is the outside option and
# never "dominated".
compute_market_dominated <- function(mkt) {
  dom   <- rep(FALSE, nrow(mkt))
  is_ma <- mkt$plan_kind == "MA"
  if (sum(is_ma) <= 1) return(dom)
  grp <- paste(mkt$plan_category, mkt$has_partd)
  for (g in unique(grp[is_ma])) {
    idx <- which(is_ma & grp == g & !is.na(mkt$mean_cost) & !is.na(mkt$var_cost))
    n   <- length(idx)
    if (n <= 1) next
    mc <- mkt$mean_cost[idx]; vc <- mkt$var_cost[idx]
    for (a in seq_len(n)) for (b in seq_len(n)) {
      if (a == b) next
      md <- mc[a] - mc[b]; vd <- vc[a] - vc[b]   # positive = b weakly better
      if (md >= 0 && vd >= 0 && (md > 0 || vd > 0)) { dom[idx[a]] <- TRUE; break }
    }
  }
  dom
}
market_dominated <- lapply(markets, compute_market_dominated)


# ---- Action profiles and dose grid ----------------------------------------
# Consideration breadth K depends on the four actions only through the binary
# (info, web, phone, book>0), so 16 profiles span everything the choice stage
# sees. K is bene-independent (a function of the breadth coefficients), so it is
# built once; the profile PROBABILITIES depend on the dose and the random-effect
# draw and are built per bene-year below.
profiles16 <- as.matrix(expand.grid(info = 0:1, web = 0:1, phone = 0:1, bookpos = 0:1))
K16    <- exp(th$b0 + th$b_info * profiles16[, "info"] + th$b_web * profiles16[, "web"] +
              th$b_phone * profiles16[, "phone"] + th$b_book * profiles16[, "bookpos"])
srch16 <- as.integer(rowSums(profiles16) > 0)          # 0 only for the all-inaction profile

scenarios <- c(cut0 = 1.0, cut25 = 0.75, cut50 = 0.50, cut100 = 0.0)   # c-multipliers
cut_map   <- c(cut0 = 0, cut25 = 25, cut50 = 50, cut100 = 100)


# ---- Per-beneficiary pass --------------------------------------------------
# Grouped by beneficiary so the empirical-Bayes posterior over the random effect
# uses all of a bene's waves, then applied to each of that bene's year-rows.
cat(sprintf("\nCounterfactual over %d bene-years (%d benes), %d draws, %d doses...\n",
            nrow(bene), length(idx_by_bene), R, length(scenarios)))
t0       <- Sys.time()
rows_out <- vector("list", nrow(bene))

for (bi in seq_along(idx_by_bene)) {
  rows  <- idx_by_bene[[bi]]
  cache <- vector("list", length(rows))
  draw_ll <- numeric(R)               # bene-level observed-action log-lik per draw

  for (jj in seq_along(rows)) {
    i    <- rows[jj]
    brow <- bene_rows[[i]]; mid <- brow$market_id; mkt <- markets[[mid]]
    v    <- compute_bene_utility(mkt, bene_mc[[i]], bene_vc[[i]], th)
    inc  <- brow$prior_plan_offered == 1L &
            !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + th$psi
    sal  <- compute_salience(mkt, market_prom[[mid]], brow, th)
    B    <- compute_search_benefit(mkt, v, sal)
    ld   <- compute_log_c_det(brow, th)

    # Observed-action log-lik at each draw (baseline cost) -> posterior weights.
    ai <- bene$act_info[i]; aw <- bene$act_web[i]; ap <- bene$act_phone[i]; br <- bene$book_read[i]
    oll <- numeric(R)
    for (r in seq_len(R)) {
      c_r <- exp(ld + sigma * nu_draws[r])
      oll[r] <- sum(loglik_actions(ai, aw, ap, br, th, B, c_r))
    }
    draw_ll <- draw_ll + oll

    # 16-profile choice-stage outcomes (dose- and draw-independent).
    dom_flag <- market_dominated[[mid]]; mc_i <- bene_mc[[i]]
    dom16 <- con16 <- ec16 <- iv16 <- numeric(16)
    for (k in seq_len(16)) {
      phi_c <- compute_phi(mkt, sal, K16[k], brow)
      p_c   <- compute_choice_prob(v, phi_c)
      dom16[k] <- sum(p_c * dom_flag)
      con16[k] <- sum(phi_c)
      ec16[k]  <- sum(p_c * mc_i)
      considered <- phi_c > 0; m <- max(v[considered])
      iv16[k]  <- log(sum(phi_c[considered] * exp(v[considered] - m))) + m
    }
    cache[[jj]] <- list(i = i, B = B, ld = ld,
                        dom = dom16, con = con16, ec = ec16, iv = iv16)
  }

  w <- exp(draw_ll - max(draw_ll)); w <- w / sum(w)     # EB posterior over draws

  for (jj in seq_along(rows)) {
    cc <- cache[[jj]]
    dt <- data.table(scenario = names(scenarios), wgt = bene$wgt_full_sample[cc$i],
                     dominated = 0, search = 0, consider = 0, ec = 0, iv = 0,
                     search_cost = 0)
    for (sc in seq_along(scenarios)) {
      c_r <- exp(cc$ld + sigma * nu_draws) * scenarios[sc]     # length R; 0 at 100% cut
      z   <- cc$B - c_r
      Pi <- plogis(z - th$kappa_info); Pw <- plogis(z - th$kappa_web)
      Pp <- plogis(z - th$kappa_phone); Pb <- plogis(z - th$kappa_book)
      PP <- matrix(0, 16, R)                                  # profile probs, 16 x R
      for (k in seq_len(16)) {
        pr <- (if (profiles16[k, "info"])    Pi else 1 - Pi) *
              (if (profiles16[k, "web"])     Pw else 1 - Pw) *
              (if (profiles16[k, "phone"])   Pp else 1 - Pp) *
              (if (profiles16[k, "bookpos"]) Pb else 1 - Pb)
        PP[k, ] <- pr
      }
      set(dt, sc, "dominated", sum(w * as.vector(cc$dom  %*% PP)))
      set(dt, sc, "search",    sum(w * as.vector(srch16  %*% PP)))
      set(dt, sc, "consider",  sum(w * as.vector(cc$con  %*% PP)))
      set(dt, sc, "ec",        sum(w * as.vector(cc$ec   %*% PP)))
      set(dt, sc, "iv",        sum(w * as.vector(cc$iv   %*% PP)))
      # Expected search cost: c_i (at this dose) times expected number of the four
      # actions taken, integrated over the posterior draws.
      set(dt, sc, "search_cost", sum(w * c_r * (Pi + Pw + Pp + Pb)))
    }
    rows_out[[cc$i]] <- dt
  }
}
cat(sprintf("  done in %.1f minutes\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))


# ---- Aggregate to survey-weighted population outcomes per dose --------------
long <- rbindlist(rows_out)
agg  <- long[, .(
  dominated_share    = sum(wgt * dominated)   / sum(wgt),
  search_rate        = sum(wgt * search)      / sum(wgt),
  mean_consideration = sum(wgt * consider)    / sum(wgt),
  mean_EC_chosen     = sum(wgt * ec)          / sum(wgt),
  mean_IV            = sum(wgt * iv)          / sum(wgt),
  mean_search_cost   = sum(wgt * search_cost) / sum(wgt)
), by = scenario]
agg[, cut_pct := cut_map[scenario]]
setorder(agg, cut_pct)

# Welfare decomposition vs the 0% baseline, in dollars via alpha (utils per
# $1,000). Plan-value channel: the inclusive-value gain as broader consideration
# brings better plans within reach. Search-cost channel: minus the change in
# expected search cost c_i * E[#actions], so a lower search burden is a gain.
iv_base <- agg[cut_pct == 0, mean_IV]
sc_base <- agg[cut_pct == 0, mean_search_cost]
agg[, welfare_plan_value  :=  1000 * (mean_IV - iv_base)         / th$alpha]
agg[, welfare_search_cost := -1000 * (mean_search_cost - sc_base) / th$alpha]
agg[, welfare_total       := welfare_plan_value + welfare_search_cost]

out <- agg[, .(cut_pct, dominated_share, search_rate, mean_consideration,
               mean_EC_chosen, welfare_plan_value, welfare_search_cost, welfare_total)]
cat("\n=== Decision-support counterfactual (dose-response) ===\n")
print(out)
fwrite(out, file.path(results_dir, "counterfactual.csv"))
cat("\nSaved counterfactual.csv\n")

cat(sprintf("\nBaseline model dominated-plan share: %.3f (RF target ~0.37)\n",
            agg[cut_pct == 0, dominated_share]))
cat("\nCounterfactual complete.\n")
