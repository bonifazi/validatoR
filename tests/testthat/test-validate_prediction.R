# Tests for validate_prediction(). The tests are grouped in blocks, each with a
# short description. The checks are exact: values that must equal a direct
# formula, a hand-calculated result or an exact target of `toy_validation` (see
# data-raw/toy_validation.R)

# ----------------------------------------------------------------------------
# Block 1. The statistics are right
# Tests: Hand-calculated values, the direct formulas and lm(), the rows of the table,
# and the mean squared error with its identities.
# ----------------------------------------------------------------------------

test_that("a perfect straight line gives the hand-calculated statistics", {
  x <- 1:10
  y <- 2 * x + 3
  res <- validate_prediction(x, y)$stats

  expect_equal(res["correlation", "value"], 1)
  expect_equal(res["slope", "value"], 2)
  expect_equal(res["intercept", "value"], 3)
  # mean(x) = 5.5, mean(y) = 14
  expect_equal(res["mean_diff", "value"], 5.5 - 14)
  # x - y = -(x + 3), so mse = mean((4:13)^2) = 805 / 10
  expect_equal(res["mse", "value"], 80.5)
  expect_equal(res["rmse", "value"], sqrt(80.5))
})

test_that("identical prediction and target give slope 1 and no bias", {
  x <- c(1.2, -0.4, 3.3, 0.8, -2.1, 1.9)
  res <- validate_prediction(x, x)$stats

  expect_equal(res["correlation", "value"], 1)
  expect_equal(res["slope", "value"], 1)
  expect_equal(res["intercept", "value"], 0)
  expect_equal(res["mean_diff", "value"], 0)
  expect_equal(res["mse", "value"], 0)
  expect_equal(res["rmse", "value"], 0)
})

test_that("statistics match the direct formulas and lm()", {
  x <- toy_validation$partial
  y <- toy_validation$pheno
  res <- validate_prediction(x, y, h2 = 0.3)$stats
  fit <- coef(lm(y ~ x))

  expect_equal(res["correlation", "value"], cor(x, y))
  expect_equal(res["slope", "value"], unname(fit[2]))
  expect_equal(res["intercept", "value"], unname(fit[1]))
  expect_equal(res["mean_diff", "value"], mean(x) - mean(y))
  expect_equal(res["accuracy", "value"], cor(x, y) / sqrt(0.3))
})

test_that("mse and rmse match the direct formulas and their identities", {
  x <- toy_validation$partial
  y <- toy_validation$whole
  res <- validate_prediction(x, y)$stats
  d <- x - y

  expect_equal(res["mse", "value"], mean(d^2))
  expect_equal(res["rmse", "value"], sqrt(mean(d^2)))
  expect_equal(res["rmse", "value"]^2, res["mse", "value"])
  # mse = squared level bias + variance of the differences (n as the divisor)
  expect_equal(
    res["mse", "value"],
    res["mean_diff", "value"]^2 + mean((d - mean(d))^2)
  )
})

test_that("rmse_in_GSD is only returned when var_a is given", {
  x <- toy_validation$partial
  y <- toy_validation$whole

  expect_false("rmse_in_GSD" %in% rownames(validate_prediction(x, y)$stats))

  res <- validate_prediction(x, y, var_a = 300)$stats
  expect_equal(res["rmse_in_GSD", "value"], res["rmse", "value"] / sqrt(300))
})

test_that("accuracy is only returned when h2 is given", {
  x <- toy_validation$partial
  y <- toy_validation$pheno

  expect_false("accuracy" %in% rownames(validate_prediction(x, y)$stats))
  expect_true("accuracy" %in% rownames(validate_prediction(x, y, h2 = 0.3)$stats))
})

test_that("the result has the documented structure", {
  res <- validate_prediction(toy_validation$partial, toy_validation$whole)

  expect_named(res, c("stats", "plot"))
  expect_s3_class(res$stats, "validatoR_stats")
  expect_s3_class(res$stats, "data.frame")
  expect_named(res$stats, "value")
  expect_identical(
    rownames(res$stats),
    c("n", "correlation", "slope", "intercept", "mean_diff", "mse", "rmse")
  )
  expect_null(res$plot)

  # the optional rows come after the fixed ones, accuracy last
  res_all <- validate_prediction(
    toy_validation$partial,
    toy_validation$pheno,
    h2 = 0.3,
    var_a = 300
  )
  expect_identical(
    rownames(res_all$stats),
    c(
      "n", "correlation", "slope", "intercept", "mean_diff", "mse", "rmse",
      "rmse_in_GSD", "accuracy"
    )
  )
})

test_that("n is the number of pairs used", {
  res <- validate_prediction(toy_validation$partial, toy_validation$whole)$stats
  expect_equal(res["n", "value"], nrow(toy_validation))

  small <- validate_prediction(1:7, c(2, 4, 3, 8, 7, 9, 12))$stats
  expect_equal(small["n", "value"], 7)
})

# ----------------------------------------------------------------------------
# Block 2. Known targets of the toy data
# Tests: The exact values the dataset was simulated to have, for partial vs whole
# and for partial vs pheno.
# ----------------------------------------------------------------------------

test_that("partial vs whole gives the exact simulated targets", {
  res <- validate_prediction(toy_validation$partial, toy_validation$whole)$stats

  # targets: rho 0.85, dispersion 0.90, level bias 0.75 (intercept -0.75)
  expect_equal(res["correlation", "value"], 0.85)
  expect_equal(res["slope", "value"], 0.90)
  expect_equal(res["mean_diff", "value"], 0.75)
  expect_equal(res["intercept", "value"], -0.75)
})

test_that("partial vs pheno gives the exact known accuracy when h2 is given", {
  res <- validate_prediction(
    toy_validation$partial,
    toy_validation$pheno,
    h2 = 0.3
  )$stats

  # cor(partial, pheno) = 0.90 * 10 / sqrt(300 / 0.3) and
  # accuracy = cor(partial, tbv) = 0.90 * 10 / sqrt(300)
  expect_equal(res["correlation", "value"], 9 / sqrt(1000))
  expect_equal(res["accuracy", "value"], 9 / sqrt(300))
  # pheno has the same slope and mean as whole
  expect_equal(res["slope", "value"], 0.90)
  expect_equal(res["mean_diff", "value"], 0.75)
  expect_equal(res["intercept", "value"], -0.75)
})

# ----------------------------------------------------------------------------
# Block 3. Bootstrap
# Tests: Standard errors of every row except n, and reproducibility with a seed.
# ----------------------------------------------------------------------------

test_that("bootstrap adds positive, finite SEs and keeps the estimates", {
  set.seed(1)
  res <- validate_prediction(
    toy_validation$partial,
    toy_validation$pheno,
    h2 = 0.3,
    var_a = 300,
    bootstrap = TRUE,
    n_boot = 50
  )$stats
  no_boot <- validate_prediction(
    toy_validation$partial,
    toy_validation$pheno,
    h2 = 0.3,
    var_a = 300
  )$stats

  expect_s3_class(res, "validatoR_stats")
  expect_named(res, c("value", "SE"))
  expect_equal(res$value, no_boot$value)

  # n is fixed, so it has no SE; all the other statistics do
  expect_true(is.na(res["n", "SE"]))
  se <- res[rownames(res) != "n", "SE"]
  expect_true(all(is.finite(se)))
  expect_true(all(se > 0))
})

test_that("bootstrap is reproducible with a seed", {
  run <- function() {
    set.seed(42)
    result <- validate_prediction(
      toy_validation$partial,
      toy_validation$whole,
      bootstrap = TRUE,
      n_boot = 30
    )$stats
    return(result)
  }

  expect_equal(run(), run())
})

# ----------------------------------------------------------------------------
# Block 4. Plot
# Tests: plot = TRUE returns a ggplot that can be built.
# ----------------------------------------------------------------------------

test_that("plot = TRUE returns a ggplot that can be built", {
  res <- validate_prediction(
    toy_validation$partial,
    toy_validation$whole,
    plot = TRUE
  )

  expect_s3_class(res$plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(res$plot))
})

# ----------------------------------------------------------------------------
# Block 5. Invalid input gives clear errors
# Tests: The vectors (type, length, missing values, variation, number of pairs) and
# the arguments (h2, var_a, flags, n_boot, ncpus).
# ----------------------------------------------------------------------------

test_that("non-numeric input is rejected", {
  expect_error(validate_prediction(letters[1:5], 1:5), "numeric")
  expect_error(validate_prediction(1:5, letters[1:5]), "numeric")
  expect_error(validate_prediction(as.data.frame(1:5), 1:5), "numeric")
})

test_that("vectors of different length are rejected", {
  expect_error(validate_prediction(1:5, 1:4), "same length")
})

test_that("missing values give an error that reports the counts", {
  x <- toy_validation$partial
  y <- toy_validation$whole
  x[1:3] <- NA
  y[5] <- NA

  expect_error(
    validate_prediction(x, toy_validation$whole),
    "`prediction` has 3 and `target` has 0"
  )
  expect_error(validate_prediction(x, y), "`prediction` has 3 and `target` has 1")
})

test_that("fewer than 3 pairs are rejected", {
  expect_error(validate_prediction(1:2, 2:3), "3 pairs")
})

test_that("a prediction or target with no variation is rejected", {
  expect_error(validate_prediction(rep(1, 5), 1:5), "`prediction` has no variation")
  expect_error(validate_prediction(1:5, rep(2, 5)), "`target` has no variation")
})

test_that("h2 must be a single number in (0, 1]", {
  x <- toy_validation$partial
  y <- toy_validation$pheno

  expect_error(validate_prediction(x, y, h2 = 0), "h2")
  expect_error(validate_prediction(x, y, h2 = 1.5), "h2")
  expect_error(validate_prediction(x, y, h2 = c(0.2, 0.3)), "h2")
  expect_error(validate_prediction(x, y, h2 = "0.3"), "h2")
  expect_no_error(validate_prediction(x, y, h2 = 1))
})

test_that("var_a must be a single positive number", {
  x <- toy_validation$partial
  y <- toy_validation$whole
  msg <- "`var_a` must be a single positive number."

  expect_error(validate_prediction(x, y, var_a = 0), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, var_a = -1), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, var_a = c(1, 2)), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, var_a = "300"), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, var_a = NA_real_), msg, fixed = TRUE)
  expect_no_error(validate_prediction(x, y, var_a = 300))
})

test_that("bootstrap and plot must be a single TRUE or FALSE", {
  x <- toy_validation$partial
  y <- toy_validation$whole

  expect_error(validate_prediction(x, y, bootstrap = NA), "bootstrap")
  expect_error(validate_prediction(x, y, bootstrap = 1), "bootstrap")
  expect_error(validate_prediction(x, y, bootstrap = c(TRUE, FALSE)), "bootstrap")
  expect_error(validate_prediction(x, y, plot = "yes"), "plot")
})

test_that("n_boot must be a single whole number of at least 2", {
  x <- toy_validation$partial
  y <- toy_validation$whole
  msg <- "`n_boot` must be a single whole number of at least 2."

  # checked even when bootstrap = FALSE, so a wrong value is found early
  expect_error(validate_prediction(x, y, n_boot = 1), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, n_boot = 2.5), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, n_boot = c(10, 20)), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, n_boot = NA), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, n_boot = "10"), msg, fixed = TRUE)
  expect_no_error(validate_prediction(x, y, n_boot = 2))
})

test_that("ncpus must be a single whole number of at least 1", {
  x <- toy_validation$partial
  y <- toy_validation$whole
  msg <- "`ncpus` must be a single whole number of at least 1."

  expect_error(validate_prediction(x, y, ncpus = 0), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, ncpus = 1.5), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, ncpus = c(1, 2)), msg, fixed = TRUE)
  expect_error(validate_prediction(x, y, ncpus = NA), msg, fixed = TRUE)
  expect_no_error(validate_prediction(x, y, ncpus = 1))
})
