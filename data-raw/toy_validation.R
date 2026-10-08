# data-raw/toy_validation.R
# ---------------------------------------------------------------------------
# Generates `toy_validation`, a built-in example dataset for validatoR.
# Run once with: source("data-raw/toy_validation.R")
#                which will write data/toy_validation.rda via usethis::use_data() (at end of script).
#
# 'whole` is simulated FROM `partial` since the LR dispersion statistic is
#   cov(partial, whole) / var(partial),
# which is exactly the slope of the regression of WHOLE on PARTIAL.
# So by generating:
#   whole = intercept + slope * partial + noise
# the dataset has known target statistics (dispersion bias equals `slope`,
# level bias = intercept, and the correlation is also known), which can be used in
# tests. They are exact, because the sample moments of `partial` and of the noise
# are set exactly below.
#
# `pheno` is a pre-corrected phenotype built on top of `whole` (see the section
# "Pre-corrected phenotype" below) so that, with `h2` known, the accuracy
# estimated by validate_prediction(partial, pheno, h2 = h2) has a known value.
# As for `whole`, the random parts of `pheno` are set to exact moments, so this
# value is exact too. Its random draws come AFTER all the others, so adding it
# did not change the values of the other columns.
# ---------------------------------------------------------------------------

set.seed(123)

n <- 2000L

## ---- Target statistics ---------------
target_level_bias <- 0.75 # mean(partial) - mean(whole), in trait units
target_dispersion <- 0.90 # cov(p,w)/var(p); <1 = over-dispersed predictions
target_rho <- 0.85 # cor(partial, whole) = ratio of accuracies

sd_partial <- 10 # SD of partial EBVs
var_a <- 300 # true additive genetic variance (GSD ~ 17.3)
mean_inb <- 0.05 # average inbreeding
h2 <- 0.30 # heritability of the pre-corrected phenotype `pheno`

## ---- Derive simulation parameters from targets stats -------------------
# With:
# partial ~ N(0, sd_partial^2), and
# whole = a + b*partial + e:
#   dispersion  = b                                  -> b = target_dispersion
#   level bias  = mean(p) - mean(w) = -a             -> a = -target_level_bias
#   rho         = b*sd_p / sqrt(b^2*sd_p^2 + sd_e^2) -> solve for sd_e
b <- target_dispersion
a <- -target_level_bias
sd_e <- sqrt(b^2 * sd_partial^2 * (1 / target_rho^2 - 1))

## ---- Simulate -------------------------------------------------------------
# draw partial and the noise of whole
partial <- rnorm(n, mean = 0, sd = sd_partial)
e <- rnorm(n, mean = 0, sd = sd_e)

# force partial to have mean 0 and sd exactly sd_partial
partial <- as.numeric(scale(partial)) * sd_partial

# force the noise to have mean 0, sd exactly sd_e and no correlation with partial
e <- stats::resid(stats::lm(e ~ partial))
e <- as.numeric(scale(e)) * sd_e

# build whole from partial and the noise
whole <- a + b * partial + e

# group factor: simulate 4 cohorts (e.g. birth-year batches) for group-wise validation
group <- factor(sample(paste0("cohort_", 1:4), n, replace = TRUE))

# inbreeding coefficients (used for accuracy_partial and its bootstrap SE)
F_coef <- pmax(0, rnorm(n, mean = mean_inb, sd = 0.01))

## ---- Pre-corrected phenotype ---------------------------------------------
# Build a true breeding value (tbv) around `whole`, and a phenotype around tbv:
#   tbv   = whole + u,   u is uncorrelated with `whole` and `partial`
#           -> cov(whole, tbv) = var(whole), i.e. `whole` is an unbiased EBV of tbv
#           -> var(tbv) = var_a, so var(u) = var_a - var(whole)
#   pheno = tbv + e,     e uncorrelated error added, var(e) = var_a * (1 - h2) / h2
#           -> var(tbv) / var(pheno) = var_a / var(pheno) = h2
# "Uncorrelated" is exact here (see the code below), so the consequences are
# exact too, step by step:
#
# 1) cov(partial, pheno) = cov(partial, whole) = dispersion * var(partial)
#
#    pheno = whole + u + e                       (because tbv = whole + u)
#    cov(partial, pheno) = cov(partial, whole) + cov(partial, u) + cov(partial, e)
#                          (the covariance of a sum is the sum of the covariances)
#    u and e are independent of partial, so cov(partial, u) = cov(partial, e) = 0
#    -> cov(partial, pheno) = cov(partial, whole)
#    whole = a + b * partial + noise, with b = dispersion. The constant a and the
#    independent noise do not covary with partial, so
#    cov(partial, whole) = b * cov(partial, partial) = b * var(partial)
#    -> 0.90 * 10^2 = 90
#
# 2) cor(partial, pheno) = dispersion * sd_partial / sqrt(var_a / h2)
#
#    var(pheno) = var(tbv) + var(e) = var_a + var_a * (1 - h2) / h2 = var_a / h2
#    (tbv and e are independent, so the variances add)
#    -> sd(pheno) = sqrt(var_a / h2) = sqrt(300 / 0.3) = 31.6
#    cor(partial, pheno) = cov(partial, pheno) / (sd(partial) * sd(pheno))
#                        = dispersion * var(partial) / (sd_partial * sqrt(var_a / h2))
#                        = dispersion * sd_partial / sqrt(var_a / h2)
#    -> 90 / (10 * 31.6) = 0.285
#
# 3) cor(partial, pheno) / sqrt(h2) = dispersion * sd_partial / sqrt(var_a)
#
#    sqrt(var_a / h2) = sqrt(var_a) / sqrt(h2), so
#    cor(partial, pheno) = dispersion * sd_partial * sqrt(h2) / sqrt(var_a)
#    ->
#    cor(partial, pheno) / sqrt(h2) = dispersion * sd_partial / sqrt(var_a)
#    -> 0.285 / 0.548 = 0.52   (= 9 / 17.3)
#    This is cor(partial, tbv): cov(partial, tbv) = cov(partial, whole + u) = 90
#    by the same argument as in 1), and sd(tbv) = sqrt(var_a) = 17.3, so
#    cor(partial, tbv) = 90 / (10 * 17.3) = 0.52.
#    It is the accuracy that validate_prediction() should recover when h2 is
#    provided.
var_whole <- b^2 * sd_partial^2 + sd_e^2 # variance of `whole` (exact)
stopifnot(var_a > var_whole) # otherwise var(u) would be negative
sd_u <- sqrt(var_a - var_whole)
sd_e_pheno <- sqrt(var_a * (1 - h2) / h2)

# draw u and the error of pheno
u <- rnorm(n, mean = 0, sd = sd_u)
e_pheno <- rnorm(n, mean = 0, sd = sd_e_pheno)

# u: no correlation with partial and with the noise of whole (so with whole),
# mean 0 and sd exactly sd_u
u <- as.numeric(scale(stats::resid(stats::lm(u ~ partial + e)))) * sd_u

# e_pheno: no correlation with partial, the noise of whole and u,
# mean 0 and sd exactly sd_e_pheno
e_pheno <- as.numeric(scale(
  stats::resid(stats::lm(e_pheno ~ partial + e + u))
)) *
  sd_e_pheno

# build the true breeding value and the phenotype
tbv <- whole + u # not stored
pheno <- tbv + e_pheno

toy_validation <- data.frame(
  id = seq_len(n),
  partial = partial,
  whole = whole,
  pheno = pheno,
  group = group,
  inbreeding = F_coef
)

## ---- Sanity check: realised values should match the targets --------------
realised <- c(
  level_bias = mean(toy_validation$partial) - mean(toy_validation$whole),
  dispersion = cov(toy_validation$partial, toy_validation$whole) /
    var(toy_validation$partial),
  rho = cor(toy_validation$partial, toy_validation$whole),
  accuracy_p = sqrt(
    cov(toy_validation$partial, toy_validation$whole) /
      ((1 - mean(toy_validation$inbreeding)) * var_a)
  )
)
print(round(realised, 4))
# Targets: level_bias 0.75, dispersion 0.90, rho 0.85. They are exact, because
# the sample moments of partial and the noise are forced above.
# var_a above is the value to pass to the var_a argument in examples/tests.
# accuracy_p (0.562 with var_a = 300 and the inbreeding column) is the LR accuracy,
# sqrt(cov(partial, whole) / ((1 - mean F) * var_a)). It is higher than the
# correlation between partial and the true breeding value (0.52, see below),
# because partial is over-dispersed by construction (dispersion 0.90) and the LR
# accuracy assumes no over-dispersion.

realised_pheno <- c(
  cor_partial_pheno = cor(toy_validation$partial, toy_validation$pheno),
  cor_partial_pheno_scaled = cor(toy_validation$partial, toy_validation$pheno) /
    sqrt(h2)
)
print(round(realised_pheno, 4))
# Targets: cor_partial_pheno = b * sd_partial / sqrt(var_a / h2) = 0.2846,
#          accuracy_via_h2   = b * sd_partial / sqrt(var_a)      = 0.5196
# They are exact too, because u and e_pheno are set exactly above.
# Pass h2 = 0.3 to validate_prediction().

usethis::use_data(toy_validation, overwrite = TRUE)
