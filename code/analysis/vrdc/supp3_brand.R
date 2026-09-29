# supp3_brand.R — Brand-FE check on delta via a fixest conditional logit
#
# Final-round check: does adding parent-org (brand) fixed effects flip delta?
# Uses the Poisson / conditional-logit equivalence — a choice-occasion fixed
# effect makes a Poisson fit identical to McFadden's conditional logit — so brand
# and category enter as fixed effects that absorb cleanly and fast, which the
# hand-rolled nested optimizer could not do at scale. This is a FLAT logit (no
# nest); the nest moved delta ~0.007 in supp1, so it is not the lever on delta's
# sign. Financial terms use the premium / expected-OOP split. FFS is the outside
# option, carried by an is_ffs constant.
#
# Runs STANDALONE after script 2 (needs bcp with premium / oop_cost / var_cost
# and the plan attributes). fixest is loaded by the driver.
#
# Sign convention: in the Poisson-clogit a positive coefficient means more likely
# chosen, so premium and OOP coefficients are negative (disliked) and
# delta = -coef(var). delta > 0 is risk aversion.

if (!exists("bcp")) stop("bcp not found. Run scripts 1-2 first (warm session).")

d <- copy(bcp)
d[, is_ffs := as.integer(plan_kind == "FFS")]
d[, occ := .GRP, by = .(BASEID, year)]
d[, `:=`(parent_org = as.character(parent_org),
         plan_category = as.character(plan_category))]
d[is.na(parent_org),   parent_org   := "OTHER"]
d[is.na(plan_category), plan_category := "OTHER"]
# FFS shares an existing MA reference level for brand/category so it does not spawn
# an FFS-only fixed effect; is_ffs is its sole constant.
ref_brand <- d[is_ffs == 0L, parent_org][1]
ref_cat   <- d[is_ffs == 0L, plan_category][1]
d[is_ffs == 1L, `:=`(parent_org = ref_brand, plan_category = ref_cat)]
d[, `:=`(premium_k = premium / 1e3,
         oop_k     = oop_cost / 1e3,
         var_m     = var_cost / 1e6,
         sd_k      = sqrt(pmax(var_cost, 0)) / 1e3,
         star_c    = fifelse(is_ffs == 1L | is.na(Star_Rating), 0, Star_Rating - 3.5),
         partd     = fifelse(is_ffs == 1L, 0, as.numeric(has_partd)))]

wvar <- if ("wgt_full_sample" %in% names(d)) ~wgt_full_sample else NULL

report <- function(m, tag) {
  b <- coef(m)
  cat(sprintf("[%s]  delta = %.4f   alpha_prem = %.4f   alpha_oop = %.4f   prem/oop = %.2f\n",
              tag, -b[["var_m"]], -b[["premium_k"]], -b[["oop_k"]],
              b[["premium_k"]] / b[["oop_k"]]))
}

# Risk = variance, without vs with brand FE — the direct brand check on delta.
m_nobrand <- fepois(is_chosen ~ is_ffs + premium_k + oop_k + var_m + star_c + partd |
                    occ + plan_category, data = d, weights = wvar, cluster = ~occ)
m_brand   <- fepois(is_chosen ~ is_ffs + premium_k + oop_k + var_m + star_c + partd |
                    occ + parent_org + plan_category, data = d, weights = wvar, cluster = ~occ)
cat("=== Conditional logit (Poisson trick), risk = variance ===\n")
report(m_nobrand, "no brand FE")
report(m_brand,   "with brand FE")
print(summary(m_brand))

# Risk = SD, with brand FE.
m_brand_sd <- fepois(is_chosen ~ is_ffs + premium_k + oop_k + sd_k + star_c + partd |
                     occ + parent_org + plan_category, data = d, weights = wvar, cluster = ~occ)
cat(sprintf("\n[SD, with brand FE]  delta(SD) = %.4f\n", -coef(m_brand_sd)[["sd_k"]]))

est <- rbindlist(list(
  data.table(spec = "var_nobrand", parameter = names(coef(m_nobrand)), estimate = as.numeric(coef(m_nobrand))),
  data.table(spec = "var_brand",   parameter = names(coef(m_brand)),   estimate = as.numeric(coef(m_brand))),
  data.table(spec = "sd_brand",    parameter = names(coef(m_brand_sd)), estimate = as.numeric(coef(m_brand_sd)))
))
dir.create("results/vrdc", recursive = TRUE, showWarnings = FALSE)
fwrite(est, "results/vrdc/supp3_brand.csv")
cat("\nWrote results/vrdc/supp3_brand.csv\n")
