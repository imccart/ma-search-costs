# model-search.R — The 24 search profiles and the exposure functions.
#
# Profile columns of act: info, web, phone, book1, book2, where
# book1 = 1{handbook read in part} and book2 = 1{read all of it}. Intensity
# n(p) = act %*% (1, omega); profile cost k(p) = act %*% (1, kappa); search
# exposure s(p) = logistic(b0 + b1 n(p)); R(p) = 1{visited Medicare.gov}.

search_profiles <- function() {
  g <- expand.grid(info = 0:1, web = 0:1, phone = 0:1, book = 0:2)
  act <- cbind(
    info  = g$info,
    web   = g$web,
    phone = g$phone,
    book1 = as.numeric(g$book == 1),
    book2 = as.numeric(g$book == 2)
  )
  list(levels = g, act = act, R = g$web)
}

profile_index <- function(info, web, phone, book) {
  as.integer(1 + info + 2 * web + 4 * phone + 8 * book)
}

# Probability that a plan enters the consideration set (product form).
inclusion_prob <- function(s, R, pf, b, lambda_PF, lambda_B) {
  1 - (1 - s) * (1 - lambda_PF * pf * R) * (1 - lambda_B * b)
}
