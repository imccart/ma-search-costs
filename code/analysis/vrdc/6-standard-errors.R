# 6-standard-errors.R — standard errors at theta_hat (no re-estimation)
#
# Runs standalone after script 3. It reloads theta_hat from results/vrdc and
# rebuilds nu_draws itself, so scripts 4 (the MLE) and 5 (diagnostics) can be
# skipped entirely.
#
# Two variance estimators:
#   OPG   outer product of the per-beneficiary scores. Costs 2p = 50 likelihood
#         evaluations (~15 min) and is written to disk immediately, so there are
#         usable standard errors long before the Hessian finishes.
#   HESS  observed information. Costs 1 + r*(2p + p(p-1)) evaluations; at p = 25
#         that is 2,601 at numDeriv's default r = 4 and 1,301 at r = 2. Every
#         evaluation is cached to hessian_cache.csv, so an interrupted session
#         resumes from the cache on the next source() instead of starting over.
# The reported standard error is the sandwich H^-1 J H^-1 when the Hessian is
# available, and the OPG otherwise. Parameters resting on a bound have invalid
# Hessian-based standard errors and are reported as NA.
#
# The gold standard is a county-clustered bootstrap, which re-estimates the
# model per replicate and is left as a long-run option.

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


# ---- Reload theta_hat and nu_draws if script 4 was not run this session ----
if (!exists("nu_draws"))
  nu_draws <- qnorm((seq_len(N_SIM_DRAWS) - 0.5) / N_SIM_DRAWS)

if (!exists("theta_hat")) {
  th_in     <- fread(file.path(results_dir, "theta_hat.csv"))
  theta_hat <- setNames(th_in$estimate, th_in$parameter)
  cat("Reloaded theta_hat from disk.\n")
}
if (is.null(names(theta_hat))) names(theta_hat) <- theta_names
theta_hat <- theta_hat[theta_names]   # canonical order; the output table pairs
stopifnot(!any(is.na(theta_hat)),     # parameter/estimate/se by position
          identical(names(theta_hat), theta_names))
cat("\ntheta_hat in canonical order:\n"); print(round(theta_hat, 4))

n_par <- length(theta_hat)


# ---- OPG: per-beneficiary scores by central differences -------------------
# ll_bene from compute_individual_loglik() is already survey-weighted, so the
# outer product of its numerical gradient is the correct sandwich meat.
cat(sprintf("\nOPG scores: %d likelihood evaluations...\n", 2 * n_par))
t0    <- Sys.time()
h_opg <- pmax(1e-4 * abs(theta_hat), 1e-5)
S     <- NULL
for (k in seq_len(n_par)) {
  tp <- tm <- theta_hat
  tp[k] <- tp[k] + h_opg[k]; tm[k] <- tm[k] - h_opg[k]
  ll_p <- compute_individual_loglik(tp, nu_draws, return_components = TRUE)$ll_bene
  ll_m <- compute_individual_loglik(tm, nu_draws, return_components = TRUE)$ll_bene
  if (is.null(S)) S <- matrix(0, length(ll_p), n_par)
  S[, k] <- (ll_p - ll_m) / (2 * h_opg[k])
  cat(sprintf("  %d/%d\n", k, n_par)); flush.console()
}
J <- crossprod(S)
se_opg <- tryCatch(sqrt(diag(solve(J))), error = function(e) {
  warning("OPG matrix not invertible: ", conditionMessage(e))
  rep(NA_real_, n_par)
})
cat(sprintf("  OPG done in %.1f minutes\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
fwrite(data.table(parameter = theta_names, estimate = theta_hat, se_opg = se_opg),
       file.path(results_dir, "standard_errors_opg.csv"))
cat("Saved standard_errors_opg.csv\n")


# ---- Disk-cached objective ------------------------------------------------
# numDeriv requests a deterministic sequence of parameter vectors, so replaying
# after a crash hits the cache for everything already computed.
cache_file <- file.path(results_dir, "hessian_cache.csv")
cache_env  <- new.env(parent = emptyenv())
key_of     <- function(t) paste(sprintf("%.17g", t), collapse = "|")

# The column is theta_key, not key: data.table() has a formal argument named
# key, so a column called key is swallowed as the key= argument instead.
if (file.exists(cache_file)) {
  old <- fread(cache_file, colClasses = c("character", "numeric"))
  for (r in seq_len(nrow(old))) assign(old$theta_key[r], old$value[r], envir = cache_env)
  cat(sprintf("\nReloaded %d cached likelihood evaluations.\n", nrow(old)))
} else {
  fwrite(data.table(theta_key = character(), value = numeric()), cache_file)
}

n_new <- 0L
negll <- function(t) {
  k <- key_of(t)
  if (exists(k, envir = cache_env, inherits = FALSE))
    return(get(k, envir = cache_env))
  v <- -compute_individual_loglik(t, nu_draws)
  assign(k, v, envir = cache_env)
  fwrite(data.table(theta_key = k, value = v), cache_file, append = TRUE, col.names = FALSE)
  n_new <<- n_new + 1L
  if (n_new %% 25L == 0L) {
    cat(sprintf("  %d new evaluations (%s)\n", n_new, format(Sys.time(), "%H:%M")))
    flush.console()
  }
  v
}


# ---- Hessian --------------------------------------------------------------
HESS_R <- 2L      # Richardson depth; 4 (numDeriv default) doubles the cost
HESS_D <- 0.02    # base step as a fraction of each parameter

n_expect <- 1L + HESS_R * (2L * n_par + n_par * (n_par - 1L))
cat(sprintf("\nHessian at r = %d: up to %d evaluations, %d already cached.\n",
            HESS_R, n_expect, length(ls(cache_env))))
t0 <- Sys.time()
H  <- hessian(negll, theta_hat, method.args = list(r = HESS_R, d = HESS_D))
cat(sprintf("  Hessian done in %.1f minutes (%d new evaluations)\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")), n_new))


# ---- Sandwich -------------------------------------------------------------
Hinv <- tryCatch(solve(H), error = function(e) {
  warning("Hessian not invertible: ", conditionMessage(e)); NULL
})
if (is.null(Hinv)) {
  se_hess <- se_sand <- rep(NA_real_, n_par)
} else {
  se_hess <- suppressWarnings(sqrt(diag(Hinv)))
  se_sand <- suppressWarnings(sqrt(diag(Hinv %*% J %*% Hinv)))
}

at_bound <- theta_hat <= theta_lower + 1e-6 | theta_hat >= theta_upper - 1e-6
se_hess[at_bound] <- NA_real_
se_sand[at_bound] <- NA_real_

se <- ifelse(is.na(se_sand), se_opg, se_sand)
out <- data.table(parameter = theta_names, estimate = theta_hat,
                  se = se, z = theta_hat / se,
                  se_opg = se_opg, se_hess = se_hess, se_sandwich = se_sand)
cat("\n=== Estimates with standard errors ===\n"); print(out)

fwrite(out, file.path(results_dir, "standard_errors.csv"))
cat("\nSaved standard_errors.csv\n")
