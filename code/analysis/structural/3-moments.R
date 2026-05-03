# 3-moments.R — Combined moment vector (BLP + RF)
#
# 12 moments / 10 parameters:
#   1-7:  BLP-style orthogonality on share residuals
#   8-12: RF orthogonality moments (Bartik 2SLS dom + takeup, demographic
#         gradients on dominated-of-population share)

PARAM_NAMES <- c("alpha", "delta", "beta", "xi_ffs",
                 "gamma0", "g_log_inc", "g_bach", "g_65",
                 "eta1", "eta2")

theta_to_vec <- function(th) {
  c(th$alpha, th$delta, th$beta, th$xi_ffs,
    th$gamma0, th$g_log_inc, th$g_bach, th$g_65,
    th$eta1, th$eta2)
}

vec_to_theta <- function(v) {
  list(alpha     = v[1],  delta = v[2], beta = v[3], xi_ffs = v[4],
       gamma0    = v[5],  g_log_inc = v[6], g_bach = v[7], g_65 = v[8],
       eta1      = v[9],  eta2 = v[10])
}

build_attribute_matrix <- function(panel) {
  is_ffs <- panel$plan_kind == "FFS"
  is_ma  <- !is_ffs

  ish <- panel$insurer_share
  ish[is.na(ish) | ish < 1e-6] <- 1e-6
  log_ish <- log(ish)
  log_ish[log_ish < LOG_SHARE_FLOOR] <- LOG_SHARE_FLOOR

  cbind(
    mean_cost_ma = ifelse(is_ma, panel$mean_cost / 1000, 0),
    var_cost_ma  = ifelse(is_ma, panel$var_cost  / 1e6, 0),
    star_ma      = ifelse(is_ma & !is.na(panel$Star_Rating),
                          panel$Star_Rating, 0),
    is_ffs       = as.numeric(is_ffs),
    dominated_ma = as.numeric(is_ma & !is.na(panel$dominated) & panel$dominated),
    incumb_ma    = as.numeric(is_ma & panel$incumbent),
    log_share_ma = ifelse(is_ma, log_ish, 0)
  )
}

compute_residuals <- function(theta, markets) {
  out <- compute_all_shares(markets, theta)
  out$pred
}

compute_moments <- function(theta_v, markets, observed, X, panel) {
  theta <- vec_to_theta(theta_v)
  pred  <- compute_residuals(theta, markets)
  res   <- pred - observed
  m_blp <- as.numeric(crossprod(X, res) / length(res))

  panel$predicted_share <- pred
  m_rf <- compute_rf_moments(panel)

  c(m_blp, m_rf)
}

X_mat    <- build_attribute_matrix(panel)
observed <- panel$observed_share
MOMENT_NAMES <- c(colnames(X_mat),
                  "bartik_dom_rf", "bartik_takeup_rf",
                  "log_inc_dom_rf", "pct_65_dom_rf", "pct_bach_dom_rf")

theta_start <- list(
  alpha     = 0.5,
  delta     = 0.05,
  beta      = 0.5,
  xi_ffs    = 6,
  gamma0    = log(0.1),
  g_log_inc = 0,
  g_bach    = 0,
  g_65      = 0,
  eta1      = 0.5,
  eta2      = 0.6
)

g_start <- compute_moments(theta_to_vec(theta_start), markets, observed, X_mat, panel)
names(g_start) <- MOMENT_NAMES
message("\nMoments at starting theta:")
print(round(g_start, 5))
