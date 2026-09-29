# supp1_components.R — Choice model estimated in isolation (nested logit)
#
# Isolated choice-side diagnostic with the corrected demand specification:
#   financial (Abaluck-Gruber split): -alpha_prem*premium - alpha_oop*E[OOP]
#                                      - delta*Var(OOP)
#   product characteristics: star, Part D coverage, plan-category fixed effects.
# FFS singleton nest against an MA nest holding every MA plan, dissimilarity
# lambda on the MA nest (lambda -> 1 is the flat conditional logit). Choice
# likelihood only: no action/search likelihood, no random effect, full
# consideration. delta is unbounded so its sign is visible; lambda in (0, 1].
#
# Brand (parent-org) fixed effects are deferred to a final round — they add ~30
# alternative-specific dummies, which this hand-rolled optimizer handles slowly;
# add them here or move to faster custom code once the spec is settled.
#
# Runs STANDALONE after script 3 (assumes 1-build, 2-load, 3-individual-likelihood
# have been sourced, so their objects are in memory); reloads W_SUM. The premium
# split means scripts 0, 1, 2 must be re-run first so bene_prem / bene_oop exist.
#
# Outputs (printed and written to results/vrdc/):
#   supp1_estimates.csv  one row per (spec, parameter)
#   supp1_profiles.csv   objective profiled in delta by spec

need_obj <- c("bene", "markets", "bene_prem", "bene_oop", "bene_vc", "bene_rows")
miss <- need_obj[!vapply(need_obj, exists, logical(1))]
if (length(miss))
  stop("Missing objects: ", paste(miss, collapse = ", "),
       ". Re-run scripts 0-3 first (premium split adds bene_prem / bene_oop).")

if (!exists("W_SUM")) W_SUM <- bene[!duplicated(bene$BASEID), sum(wgt_full_sample)]

# Plan-category levels across MA plans; first level is the reference. Star and
# Part D coverage enter as continuous / dummy characteristics.
cat_lv <- sort(unique(unlist(lapply(markets,
  function(m) m[plan_kind != "FFS", unique(as.character(plan_category))]))))
cat_lv <- cat_lv[!is.na(cat_lv)]
char_names <- c("star", "partd", if (length(cat_lv) > 1) paste0("cat_", cat_lv[-1]))

# Per-market characteristic design matrix (plans x char); the FFS row is all
# zeros (xi_FFS carries the outside option).
make_design <- function(m) {
  D <- matrix(0, nrow(m), length(char_names), dimnames = list(NULL, char_names))
  ma <- m$plan_kind != "FFS"
  D[ma, "star"]  <- ifelse(is.na(m$Star_Rating[ma]), 0, m$Star_Rating[ma] - 3.5)
  D[ma, "partd"] <- ifelse(is.na(m$has_partd[ma]), 0, as.numeric(m$has_partd[ma]))
  if (length(cat_lv) > 1)
    for (cl in cat_lv[-1]) {
      hit <- ma & !is.na(m$plan_category) & as.character(m$plan_category) == cl
      D[hit, paste0("cat_", cl)] <- 1
    }
  D
}
design <- lapply(markets, make_design)

# Nested-logit choice negative per-weight log-likelihood. pars is named with the
# financial coefficients, xi_FFS, psi, lambda, and the char_names coefficients.
nested_negll <- function(pars, risk = "var") {
  ap <- pars[["alpha_prem"]]; ao <- pars[["alpha_oop"]]; d <- pars[["delta"]]
  xf <- pars[["xi_FFS"]];     ps <- pars[["psi"]];       lam <- pars[["lambda"]]
  cc <- pars[char_names]
  clp <- lapply(design, function(D) as.vector(D %*% cc))
  ll <- 0
  for (i in seq_len(nrow(bene))) {
    brow <- bene_rows[[i]]; mid <- brow$market_id; mkt <- markets[[mid]]
    is_ffs <- mkt$plan_kind == "FFS"
    prem <- bene_prem[[i]] / 1e3
    oop  <- bene_oop[[i]]  / 1e3
    rt   <- if (risk == "var") bene_vc[[i]] / 1e6 else sqrt(pmax(bene_vc[[i]], 0)) / 1e3
    v <- clp[[mid]] - ap * prem - ao * oop - d * rt
    v[is_ffs] <- v[is_ffs] + xf
    inc <- brow$prior_plan_offered == 1L &
           !is.na(brow$prior_plan_id) & mkt$plan_id == brow$prior_plan_id
    v[inc] <- v[inc] + ps

    ci  <- brow$choice_idx
    vma <- v[!is_ffs]
    if (length(vma) == 0L) next
    mma    <- max(vma / lam)
    lse_ma <- mma + log(sum(exp(vma / lam - mma)))
    iv_ma  <- lam * lse_ma
    if (!any(is_ffs)) {
      logp <- v[ci] / lam - lse_ma
    } else {
      v_ffs  <- v[is_ffs][1]
      mup    <- max(v_ffs, iv_ma)
      lse_up <- mup + log(exp(v_ffs - mup) + exp(iv_ma - mup))
      logp   <- if (is_ffs[ci]) v_ffs - lse_up
                else (iv_ma - lse_up) + (v[ci] / lam - lse_ma)
    }
    ll <- ll + logp * brow$wgt_full_sample
  }
  -ll / W_SUM
}

pnames <- c("alpha_prem", "alpha_oop", "delta", "xi_FFS", "psi", "lambda", char_names)
start0 <- setNames(numeric(length(pnames)), pnames)
start0[c("alpha_prem", "alpha_oop", "delta", "xi_FFS", "psi", "lambda", "star", "partd")] <-
  c(0.6, 0.6, 0.05, 2.5, 1.0, 0.7, 0.4, 0.3)

fit_nested <- function(risk) {
  lower <- setNames(rep(-Inf, length(pnames)), pnames); lower["lambda"] <- 0.05
  upper <- setNames(rep(Inf, length(pnames)),  pnames); upper["lambda"] <- 1.0
  optim(start0, nested_negll, risk = risk, method = "L-BFGS-B",
        lower = lower, upper = upper, control = list(maxit = 500), hessian = TRUE)
}

est_rows <- list(); prof_rows <- list()
dgrid <- c(-0.5, -0.3, -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.3, 0.5)

# --- Nested logit, risk as variance ----------------------------------------
n_var <- fit_nested("var")
cat("\n=== Nested logit (FFS vs MA), risk = variance ===\n")
print(round(n_var$par, 4)); cat(sprintf("neg per-weight choice LL: %.5f\n", n_var$value))
est_rows[["nested_var"]] <- data.table(spec = "nested_var",
  parameter = names(n_var$par), estimate = as.numeric(n_var$par))

V <- tryCatch(solve(n_var$hessian), error = function(e) NULL)
if (!is.null(V)) {
  D <- sqrt(diag(V)); Rho <- V / outer(D, D)
  dimnames(Rho) <- list(names(n_var$par), names(n_var$par))
  keep <- c("alpha_prem", "alpha_oop", "delta", "xi_FFS", "psi", "lambda")
  cat("\nParameter correlation (financial block):\n"); print(round(Rho[keep, keep], 3))
}

pv <- sapply(dgrid, function(x) { p <- n_var$par; p["delta"] <- x; nested_negll(p, "var") })
cat("\nDelta profile (nested, variance):\n"); print(data.frame(delta = dgrid, negLL = round(pv, 6)))
prof_rows[["nested_var"]] <- data.table(spec = "nested_var", parameter = "delta",
                                        value = dgrid, negLL = pv)

# --- Nested logit, risk as SD ----------------------------------------------
n_sd <- fit_nested("sd")
cat("\n=== Nested logit, risk = SD (sqrt(var)/1e3) ===\n")
print(round(n_sd$par, 4)); cat(sprintf("neg per-weight choice LL: %.5f\n", n_sd$value))
est_rows[["nested_sd"]] <- data.table(spec = "nested_sd",
  parameter = names(n_sd$par), estimate = as.numeric(n_sd$par))
psd <- sapply(dgrid, function(x) { p <- n_sd$par; p["delta"] <- x; nested_negll(p, "sd") })
cat("\nDelta profile (nested, SD):\n"); print(data.frame(delta = dgrid, negLL = round(psd, 6)))
prof_rows[["nested_sd"]] <- data.table(spec = "nested_sd", parameter = "delta",
                                       value = dgrid, negLL = psd)

# --- Flat baseline (lambda = 1) for the nesting-gain comparison -------------
pflat  <- setdiff(pnames, "lambda")
startf <- start0[pflat]
flat <- optim(startf, function(p) nested_negll(c(p, lambda = 1)[pnames], "var"),
              method = "L-BFGS-B",
              lower = setNames(rep(-Inf, length(pflat)), pflat),
              control = list(maxit = 500))
cat("\n=== Flat logit (lambda = 1), risk = variance ===\n")
cat(sprintf("neg per-weight choice LL: %.5f   delta = %.4f\n",
            flat$value, flat$par[["delta"]]))
est_rows[["flat_var"]] <- data.table(spec = "flat_var",
  parameter = names(flat$par), estimate = as.numeric(flat$par))
cat(sprintf("\nlambda_hat = %.3f   nesting gain (flat - nested) = %.6f\n",
            n_var$par[["lambda"]], flat$value - n_var$value))

dir.create("results/vrdc", recursive = TRUE, showWarnings = FALSE)
fwrite(rbindlist(est_rows),  "results/vrdc/supp1_estimates.csv")
fwrite(rbindlist(prof_rows), "results/vrdc/supp1_profiles.csv")
cat("\nWrote results/vrdc/supp1_estimates.csv and supp1_profiles.csv\n")
