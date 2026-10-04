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
# the dataset has known target statistics (dispersion bias equals `slope` in expectation,
# level bias = intercept, and correlation is also known), which can be used in tests.
# ---------------------------------------------------------------------------

set.seed(123)

n <- 2000L

## ---- Target statistics ---------------
target_level_bias <- 0.75   # mean(partial) - mean(whole), in trait units
target_dispersion <- 0.90   # cov(p,w)/var(p); <1 = over-dispersed predictions
target_rho        <- 0.85   # cor(partial, whole) = ratio of accuracies

sd_partial <- 10            # SD of partial EBVs
VAR_A      <- 300           # true additive genetic variance (GSD ~ 17.3)
mean_inb   <- 0.05          # average inbreeding

## ---- Derive simulation parameters from targets stats -------------------
# With:
# partial ~ N(0, sd_partial^2), and
# whole = a + b*partial + e:
#   dispersion  = b                                  -> b = target_dispersion
#   level bias  = mean(p) - mean(w) = -a             -> a = -target_level_bias
#   rho         = b*sd_p / sqrt(b^2*sd_p^2 + sd_e^2) -> solve for sd_e
b  <- target_dispersion
a  <- -target_level_bias
sd_e <- sqrt(b^2 * sd_partial^2 * (1 / target_rho^2 - 1))

## ---- Simulate -------------------------------------------------------------
partial <- rnorm(n, mean = 0, sd = sd_partial) # draw the partial EBV vector
whole   <- a + b * partial + rnorm(n, mean = 0, sd = sd_e) # simultae the whole EBV vector from partial and noise

# group factor: simulate 4 cohorts (e.g. birth-year batches) for group-wise validation
group <- factor(sample(paste0("cohort_", 1:4), n, replace = TRUE))

# inbreeding coefficients (used for accuracy_partial and its bootstrap SE)
F_coef <- pmax(0, rnorm(n, mean = mean_inb, sd = 0.01))

toy_validation <- data.frame(
  id      = seq_len(n),
  partial = partial,
  whole   = whole,
  group   = group,
  inbreeding = F_coef
)

## ---- Sanity check: realised values should match the targets --------------
realised <- c(
  level_bias  = mean(toy_validation$partial) - mean(toy_validation$whole),
  dispersion  = cov(toy_validation$partial, toy_validation$whole) /
                  var(toy_validation$partial),
  rho         = cor(toy_validation$partial, toy_validation$whole),
  accuracy_p  = sqrt(cov(toy_validation$partial, toy_validation$whole) /
                       ((1 - mean(toy_validation$inbreeding)) * VAR_A))
)
print(round(realised, 4))
# Targets: level_bias 0.75, dispersion 0.90, rho 0.85 (sampling noise aside).
# VAR_A above is the value to pass to the VAR_A argument in examples/tests.

usethis::use_data(toy_validation, overwrite = TRUE)
