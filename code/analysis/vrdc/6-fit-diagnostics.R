# 6-fit-diagnostics.R — Predicted vs observed at theta_hat
#
# Tables / cleared outputs:
#   - search rate by demographic group (income tertile, education, age group, internet)
#   - FFS share by demographic group
#   - K* distribution
#   - Implied c distribution

components <- compute_individual_loglik(theta_hat, return_components = TRUE)
bene$searched_pred <- components$searched_pred
bene$K_star        <- components$K_star
bene$log_c         <- sapply(seq_len(nrow(bene)),
                             function(i) compute_log_c(bene[i, ], unpack_theta(theta_hat)))

results_dir <- "results/vrdc"


# ---- Search rate, observed vs predicted, by demographic group ----

by_group <- function(group_var) {
  bene %>%
    group_by(g = .data[[group_var]]) %>%
    summarize(
      n        = n(),
      obs      = weighted.mean(searched_obs,  w = wgt_full_sample),
      pred     = weighted.mean(searched_pred, w = wgt_full_sample),
      .groups  = "drop"
    ) %>%
    mutate(group = group_var) %>%
    select(group, level = g, n, obs, pred) %>%
    filter(n >= 11)   # CMS suppression threshold
}

groups <- c("has_inet", "is_dual", "has_bach", "race_black", "race_hisp")
search_by_group <- bind_rows(lapply(groups, by_group))
fwrite(search_by_group, file.path(results_dir, "search_by_group.csv"))


# ---- FFS share by demographic group ----

ffs_by_group <- function(group_var) {
  bene %>%
    group_by(g = .data[[group_var]]) %>%
    summarize(
      n    = n(),
      obs  = weighted.mean(is_ffs_mbsf,  w = wgt_full_sample),
      pred = weighted.mean(searched_pred == 0, w = wgt_full_sample),  # K*=0 -> FFS
      .groups = "drop"
    ) %>%
    mutate(group = group_var) %>%
    select(group, level = g, n, obs, pred) %>%
    filter(n >= 11)
}

ffs_groups <- bind_rows(lapply(groups, ffs_by_group))
fwrite(ffs_groups, file.path(results_dir, "ffs_by_group.csv"))


# ---- K* distribution and c distribution ----

k_dist <- bene %>%
  count(K_star) %>%
  mutate(pct = n / sum(n)) %>%
  filter(n >= 11)
fwrite(k_dist, file.path(results_dir, "kstar_distribution.csv"))

c_dist <- bene %>%
  summarize(
    c_p10  = quantile(exp(log_c), 0.10),
    c_p25  = quantile(exp(log_c), 0.25),
    c_p50  = quantile(exp(log_c), 0.50),
    c_p75  = quantile(exp(log_c), 0.75),
    c_p90  = quantile(exp(log_c), 0.90),
    c_mean = mean(exp(log_c)),
    log_c_sd = sd(log_c)
  )
fwrite(c_dist, file.path(results_dir, "c_distribution.csv"))


cat("\n=== Fit diagnostics summary ===\n")
cat("Search rate by internet:\n")
print(search_by_group %>% filter(group == "has_inet"))
cat("\nK* distribution:\n")
print(k_dist)
cat("\nc distribution:\n")
print(c_dist)
