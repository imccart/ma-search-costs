# 5-fit-diagnostics.R — Estimation table, share fit, K* distribution
#
# Reads v_hat, SE, theta_hat_list, g_hat from 4-estimate-gmm.R.
# Writes:
#   results/structural_params.csv
#   results/structural_share_fit.csv

# ---- Parameter table ----
param_tbl <- tibble(
  param = PARAM_NAMES,
  est   = as.numeric(v_hat),
  se    = SE,
  z     = as.numeric(v_hat) / SE
)

message("\n========== Parameter estimates ==========")
print(param_tbl, n = Inf)

dir.create("results", showWarnings = FALSE, recursive = TRUE)
write_csv(param_tbl, "results/structural_params.csv")
message("Wrote results/structural_params.csv")

# ---- Predicted share fit ----
out_hat <- compute_all_shares(markets, theta_hat_list)
panel$predicted_share <- out_hat$pred

share_tbl <- panel %>%
  mutate(plan_class = case_when(
    plan_kind == "FFS"                                            ~ "FFS_outside",
    plan_kind == "MA" & !is.na(dominated) & dominated             ~ "MA_dominated",
    plan_kind == "MA" & !is.na(dominated) & !dominated            ~ "MA_nondominated",
    plan_kind == "MA"                                              ~ "MA_unknown_dom",
    TRUE                                                           ~ "other"
  )) %>%
  group_by(plan_class) %>%
  summarize(
    obs_share  = weighted.mean(observed_share,  total_eligibles, na.rm = TRUE),
    pred_share = weighted.mean(predicted_share, total_eligibles, na.rm = TRUE),
    n_rows     = n(),
    .groups = "drop"
  )

message("\n========== Aggregate share fit ==========")
print(share_tbl)
write_csv(share_tbl, "results/structural_share_fit.csv")
message("Wrote results/structural_share_fit.csv")

# ---- MA take-up (eligibles-weighted across markets) ----
takeup_tbl <- panel %>%
  group_by(market_id) %>%
  summarize(
    obs_ma  = sum(observed_share[plan_kind == "MA"]),
    pred_ma = sum(predicted_share[plan_kind == "MA"]),
    elig    = first(total_eligibles),
    .groups = "drop"
  )
message(sprintf("\nMA take-up | observed: %.3f | predicted: %.3f",
                weighted.mean(takeup_tbl$obs_ma,  takeup_tbl$elig),
                weighted.mean(takeup_tbl$pred_ma, takeup_tbl$elig)))

# ---- K* distribution ----
message("\nK* distribution at theta_hat:")
print(summary(out_hat$K_star))
