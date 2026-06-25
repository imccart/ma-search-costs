# 5-fit-diagnostics.R — model validation: predicted vs observed
#
# Rewritten 2026-06-22 for the joint-likelihood estimator. Search rate, FFS
# share, and incumbent retention among MA are NOT estimation targets under the
# new MLE; they are untargeted moments reported as fit. Requires `theta_hat`
# and `nu_draws` from 4-estimate-mle.R.

pred <- compute_predictions(theta_hat, nu_draws)

wm <- function(x, w) sum(x * w) / sum(w)
mp <- pred[ma_prior == 1L]

overall <- data.table(
  moment    = c("search rate", "FFS share", "incumbent | MA"),
  observed  = c(wm(pred$obs_search, pred$wgt),
                wm(pred$obs_ffs,    pred$wgt),
                wm(mp$obs_inc_ma,   mp$wgt)),
  predicted = c(wm(pred$p_search,   pred$wgt),
                wm(pred$p_ffs,      pred$wgt),
                wm(mp$p_inc_ma,     mp$wgt))
)
overall[, resid := predicted - observed]
cat("\n=== Fit diagnostics (untargeted moments) ===\n"); print(overall)

results_dir <- "results/vrdc"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(overall, file.path(results_dir, "fit_diagnostics.csv"))

# Predicted vs observed search rate by subgroup (CMS small-cell rule: N >= 11).
by_grp <- function(col, label) {
  g <- pred[, .(n = .N, obs = wm(obs_search, wgt), pred = wm(p_search, wgt)),
            by = c(col)][n >= 11]
  setnames(g, col, "level")
  g[, dim := label][]
}
grp <- rbind(by_grp("is_dual", "dual"), by_grp("has_bach", "bachelors"))
setcolorder(grp, c("dim", "level", "n", "obs", "pred"))
print(grp)
fwrite(grp, file.path(results_dir, "search_by_group.csv"))
cat("\nSaved fit_diagnostics.csv and search_by_group.csv\n")
