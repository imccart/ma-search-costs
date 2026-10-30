# model-data.R — Theta-independent estimation objects.
#
# make_model_data() takes two tables.
#   pair: one row per (beneficiary-year, option), sorted by iy, with columns
#     iy, prem, oop, star, has_pd, category, is_tm, is_inc, is_default,
#     is_chosen, pf, b_score. Default rows are traditional Medicare and, for an
#     incumbent, the prior-year MA plan; all other rows are placed plans.
#   iy: one row per beneficiary-year, iy = 1..N, with columns bene_id, year, w
#     (full-sample weight), cluster_id (stratum x PSU), info, web, phone, book,
#     and the search-cost covariates in model_xc_cols.
#
# Each beneficiary-year's placed plans get one random order, and S common column
# permutations are applied within each group of equal J, so every person has S
# position-balanced random orderings. When J! <= S the group uses all J! orderings, which makes
# the expected inclusive value exact.

model_xc_cols <- c("log_inc_dm", "educ_yrs_dm", "age_dm", "is_dual", "adi_dm",
                   "book_understood_dm", "tenure_dm", "easy_compare_dm", "enough_info_dm")

all_perms <- function(n) {
  if (n == 1) return(matrix(1L, 1, 1))
  sub <- all_perms(n - 1)
  do.call(cbind, lapply(seq_len(n), function(i) rbind(i, ifelse(sub >= i, sub + 1L, sub))))
}

# Each random ordering contributes all cyclic rotations, so every plan appears
# at each position once per block. Each prefix remains a uniform K-subset in
# expectation, while the K=1 and K=J-1 averages are exact for complete blocks.
balanced_perms <- function(J, S) {
  blocks <- ceiling(S / J)
  out <- matrix(0L, J, blocks * J)
  for (b in seq_len(blocks)) {
    p <- sample.int(J)
    for (k in 0:(J - 1L)) out[, (b - 1L) * J + k + 1L] <- p[(seq_len(J) + k - 1L) %% J + 1L]
  }
  out[, seq_len(S), drop = FALSE]
}

# Quadrature is the default; the shuffle path is retained for comparisons.
make_model_data <- function(pair, iy, n_shuffles = 40000L, n_nu = 20L, seed = 20261027L,
                            grid = choice_grid(), benefit_method = c('quadrature', 'shuffle')) {
  benefit_method <- match.arg(benefit_method)
  pair <- as.data.table(pair)
  iy <- as.data.table(iy)
  N <- nrow(iy)
  stopifnot(identical(as.integer(iy$iy), seq_len(N)), !is.unsorted(pair$iy),
            all(seq_len(N) %in% pair$iy))
  chk <- pair[, .(n_ch = sum(is_chosen), n_tm = sum(is_tm), n_def = sum(is_default)), by = "iy"]
  stopifnot(nrow(chk) == N, all(chk$n_ch == 1), all(chk$n_tm == 1),
            all(pair$is_default[pair$is_tm]))

  old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv()))
  set.seed(seed)

  n_row <- nrow(pair)
  iyv <- pair$iy
  placed <- which(!pair$is_default)
  ipl <- iyv[placed]
  J <- tabulate(ipl, nbins = N)
  pg <- sort(unique(ipl))
  pf_bar <- b_bar <- numeric(N)
  if (length(placed)) {
    pf_bar[pg] <- rowsum(pair$pf[placed], ipl)[, 1] / J[pg]
    b_bar[pg]  <- rowsum(pair$b_score[placed], ipl)[, 1] / J[pg]
  }
  chosen_row <- which(pair$is_chosen)
  chosen_placed <- !pair$is_default[chosen_row]
  pl_pos <- integer(n_row)
  pl_pos[placed] <- seq_along(placed)

  split_pl <- split(placed, ipl)
  groups <- lapply(sort(unique(J[J > 0])), function(Jv) {
    rows <- which(J == Jv)
    G <- matrix(unlist(lapply(split_pl[as.character(rows)], function(x)
      if (benefit_method == 'shuffle') x[sample.int(Jv)] else x)),
                ncol = Jv, byrow = TRUE)
    perm <- if (benefit_method != 'shuffle') NULL else if (factorial(Jv) <= n_shuffles) all_perms(Jv) else
      balanced_perms(Jv, n_shuffles)
    list(J = Jv, rows = rows, G = G, perm = perm, U = 1 * upper.tri(diag(Jv), diag = TRUE))
  })

  b_ids <- unique(iy$bene_id)
  first <- iy[, .(r = .I[which.min(year)]), by = bene_id]
  first <- first$r[match(b_ids, first$bene_id)]
  w_b <- iy$w[first]

  Q <- length(grid$tau)
  list(
    n_iy = N, n_b = length(b_ids),
    pair = list(
      iy = iyv,
      X = utility_design(pair$prem, pair$oop, pair$star, pair$has_pd, pair$category,
                         pair$is_tm, pair$is_inc),
      is_default = pair$is_default, is_chosen = pair$is_chosen, is_tm = pair$is_tm,
      pf = ifelse(pair$is_default, 0, pair$pf),
      b_score = ifelse(pair$is_default, 0, pair$b_score)
    ),
    placed = placed, pl_groups = pg, tm_row = which(pair$is_tm),
    chosen_pl_pos = pl_pos[chosen_row[chosen_placed]],
    iy = list(
      J = J,
      p_obs = profile_index(iy$info, iy$web, iy$phone, iy$book),
      chosen_row = chosen_row, chosen_placed = chosen_placed,
      pf_bar = pf_bar, b_bar = b_bar,
      Xc = cbind(1, as.matrix(iy[, ..model_xc_cols]))
    ),
    groups = groups,
    grid = grid,
    qchunks = split(seq_len(Q), ceiling(seq_len(Q) / 17)),
    nu = gauss_hermite(n_nu),
    prof = search_profiles(),
    b_ids = b_ids, b_of_iy = match(iy$bene_id, b_ids),
    w_b = w_b, cluster_b = iy$cluster_id[first], W_SUM = sum(w_b),
    src = list(pair = pair, iy = iy),
    settings = list(n_shuffles = n_shuffles, n_nu = n_nu, seed = seed,
                    benefit_method = benefit_method)
  )
}

# Rebuild the objects for a subset of beneficiaries (used for gradient checks).
subset_model_data <- function(md, bene_ids) {
  keep_iy <- md$src$iy$iy[md$src$iy$bene_id %in% bene_ids]
  iy_sub <- md$src$iy[iy %in% keep_iy]
  pair_sub <- md$src$pair[iy %in% keep_iy]
  new_id <- match(pair_sub$iy, iy_sub$iy)
  pair_sub[, iy := new_id]
  iy_sub[, iy := seq_len(.N)]
  make_model_data(pair_sub, iy_sub, md$settings$n_shuffles, md$settings$n_nu,
                  md$settings$seed, md$grid, md$settings$benefit_method)
}
