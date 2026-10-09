# Tests for the `validatoR_stats` class: the values keep their full precision
# and only the printing is formatted.

lr_stats_example <- function(...) {
  res <- validate_lr(
    toy_validation[, c("id", "partial")],
    toy_validation[, c("id", "whole")],
    var_a = 300,
    inbreeding = toy_validation[, c("id", "inbreeding")],
    ...
  )
  return(res$stats)
}

# ----------------------------------------------------------------------------
# Block 1. The stored table
# Tests: The class and its data frame behaviour, the full precision of the
# stored values, and reading the values in the usual ways.
# ----------------------------------------------------------------------------

test_that("validate_lr() returns a validatoR_stats table that is a data frame", {
  stats <- lr_stats_example()

  expect_s3_class(stats, "validatoR_stats")
  expect_s3_class(stats, "data.frame")
  expect_true(is.data.frame(stats))
  expect_named(stats, "value")
})

test_that("the stored values keep their full precision", {
  stats <- lr_stats_example()
  x <- toy_validation$partial
  y <- toy_validation$whole

  expect_equal(stats["rho", "value"], cor(x, y))
  expect_equal(stats["dispersion_bias", "value"], cov(x, y) / var(x))
  # level bias in genetic standard deviations has more digits than the printed
  # table shows (rho and the dispersion are round numbers in toy_validation)
  in_gsd <- (mean(x) - mean(y)) / sqrt(300)
  expect_equal(stats["level_bias_in_GSD", "value"], in_gsd)
  expect_false(isTRUE(all.equal(
    stats["level_bias_in_GSD", "value"],
    signif(in_gsd, 4),
    tolerance = 1e-12
  )))
})

test_that("the values can be read in the usual ways", {
  stats <- lr_stats_example()

  expect_type(stats$value, "double")
  expect_length(stats[1:3, "value"], 3L)
  expect_s3_class(stats[1:3, , drop = FALSE], "validatoR_stats")
  expect_false(inherits(as.data.frame(stats), "validatoR_stats"))

  file <- tempfile(fileext = ".csv")
  utils::write.csv(stats, file)
  back <- utils::read.csv(file, row.names = 1)
  expect_equal(back$value, stats$value, tolerance = 1e-12)
})

# ----------------------------------------------------------------------------
# Block 2. Printing
# Tests: Fixed notation and significant digits, tiny values, the digits
# argument, extra arguments, the invisible return, and the bootstrap SE column.
# ----------------------------------------------------------------------------

test_that("printing uses fixed notation with 4 significant digits", {
  stats <- lr_stats_example()
  shown <- capture.output(print(stats))

  expect_false(any(grepl("e[+-][0-9]", shown)))
  expect_true(any(grepl("^n +2000", shown)))
  expect_true(any(grepl(
    format(signif(stats["rho", "value"], 4)),
    shown,
    fixed = TRUE
  )))
})

test_that("printing does not turn a tiny value into 0", {
  tiny <- .new_stats(data.frame(
    value = c(2000, 4.922e-08, 0.8609343),
    row.names = c("n", "average_inbreeding", "rho")
  ))
  shown <- capture.output(print(tiny))

  expect_true(any(grepl("0.00000004922", shown, fixed = TRUE)))
  expect_false(any(grepl("e[+-][0-9]", shown)))
  expect_equal(tiny["average_inbreeding", "value"], 4.922e-08)
})

test_that("digits changes the display and is checked", {
  stats <- lr_stats_example()
  shown_7 <- capture.output(print(stats, digits = 7))

  expect_true(any(grepl(
    format(signif(stats["rho", "value"], 7)),
    shown_7,
    fixed = TRUE
  )))
  expect_error(print(stats, digits = 0), "digits")
  expect_error(print(stats, digits = 2.5), "digits")
  expect_error(print(stats, digits = c(2, 3)), "digits")
  expect_error(print(stats, digits = NA), "digits")
})

test_that("extra arguments are passed on to print.data.frame()", {
  stats <- lr_stats_example()
  default <- capture.output(print(stats))
  no_names <- capture.output(print(stats, row.names = FALSE))

  # row.names = FALSE is an argument of print.data.frame(), not of our method
  expect_true(any(grepl("^rho", default)))
  expect_false(any(grepl("rho", no_names, fixed = TRUE)))
  # the values are still formatted by our method
  expect_true(any(grepl(
    format(signif(stats["rho", "value"], 4)),
    no_names,
    fixed = TRUE
  )))
  # and they can be combined with digits
  expect_true(any(grepl(
    format(signif(stats["rho", "value"], 7)),
    capture.output(print(stats, digits = 7, row.names = FALSE)),
    fixed = TRUE
  )))
})

test_that("print() returns the table invisibly and unchanged", {
  stats <- lr_stats_example()
  out <- NULL
  capture.output(out <- withVisible(print(stats)))

  expect_false(out$visible)
  expect_identical(out$value, stats)
})

test_that("the table also prints with a bootstrap SE column", {
  set.seed(1)
  stats <- lr_stats_example(bootstrap = TRUE, n_boot = 20)
  shown <- capture.output(print(stats))

  expect_named(stats, c("value", "SE"))
  expect_false(any(grepl("e[+-][0-9]", shown)))
  expect_true(any(grepl("NA", shown)))
})
