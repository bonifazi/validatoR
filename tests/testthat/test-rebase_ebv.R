# Tests for rebase_ebv(). The tests are grouped in blocks, each with a short
# description.

ebv <- toy_validation[, c("id", "partial", "whole")]
# the animals of the first cohort are the base population
base_df <- toy_validation[
  toy_validation$group == "cohort_1",
  "id",
  drop = FALSE
]
in_base <- toy_validation$id %in% base_df$id

# ----------------------------------------------------------------------------
# Block 1. Rebasing is right
# Tests: The base population ends with a mean of 0, every animal moves by the same
# amount, and the shape of the data is kept.
# ----------------------------------------------------------------------------

test_that("the base population has a mean of 0 in every column", {
  res <- rebase_ebv(ebv, base_pop = base_df)$rebased_ebv

  expect_equal(mean(res$partial[in_base]), 0)
  expect_equal(mean(res$whole[in_base]), 0)
})

test_that("every animal moves by the mean of the base population", {
  res <- rebase_ebv(ebv, base_pop = base_df)$rebased_ebv

  # each column with its own base-population mean
  expect_equal(
    ebv$partial - res$partial,
    rep(mean(ebv$partial[in_base]), nrow(ebv))
  )
  expect_equal(
    ebv$whole - res$whole,
    rep(mean(ebv$whole[in_base]), nrow(ebv))
  )
  # so the differences between animals do not change
  expect_equal(cor(res$partial, res$whole), cor(ebv$partial, ebv$whole))
})

test_that("a small example gives the hand-calculated values", {
  d <- data.frame(
    id = 1:6,
    a = c(10, 12, 14, 20, 22, 30),
    b = c(1, 2, 3, 4, 5, 6)
  )
  # animals 1 to 3 are the base population: mean(a) = 12, mean(b) = 2
  res <- rebase_ebv(d, base_pop = data.frame(id = 1:3))$rebased_ebv

  expect_equal(res$a, c(-2, 0, 2, 8, 10, 18))
  expect_equal(res$b, c(-1, 0, 1, 2, 3, 4))
})

test_that("constant_value is paired with the EBV columns in order", {
  # the first value goes with the first EBV column, the second with the second
  res <- rebase_ebv(ebv, constant_value = c(100, 90))$rebased_ebv

  expect_equal(res$partial, ebv$partial - 100)
  expect_equal(res$whole, ebv$whole - 90)

  # the order of the values follows the order of the columns of data
  swapped <- rebase_ebv(
    ebv[, c("id", "whole", "partial")],
    constant_value = c(100, 90)
  )$rebased_ebv
  expect_equal(swapped$whole, ebv$whole - 100)
  expect_equal(swapped$partial, ebv$partial - 90)
})

test_that("a data frame with one EBV column takes one constant_value", {
  res <- rebase_ebv(ebv[, c("id", "partial")], constant_value = 100)$rebased_ebv

  expect_equal(res$partial, ebv$partial - 100)
})

test_that("the IDs, the column names and the row order are kept", {
  set.seed(1)
  shuffled <- ebv[sample(nrow(ebv)), ]
  res <- rebase_ebv(shuffled, base_pop = base_df)$rebased_ebv

  expect_s3_class(res, "data.frame")
  expect_identical(names(res), names(shuffled))
  expect_identical(res$id, shuffled$id)
  expect_equal(nrow(res), nrow(shuffled))
})

test_that("the result is always a list, with plots only when requested", {
  res <- rebase_ebv(ebv, base_pop = base_df)

  expect_type(res, "list")
  expect_named(res, c("rebased_ebv", "plots"))
  expect_s3_class(res$rebased_ebv, "data.frame")
  expect_null(res$plots)

  res_const <- rebase_ebv(ebv, constant_value = c(100, 90))
  expect_named(res_const, c("rebased_ebv", "plots"))
  expect_null(res_const$plots)
})

# ----------------------------------------------------------------------------
# Block 2. Invalid input gives clear errors
# Tests: base_pop and constant_value, the shape of data, the IDs, and missing or
# infinite EBVs.
# ----------------------------------------------------------------------------

test_that("exactly one of base_pop and constant_value must be provided", {
  expect_error(
    rebase_ebv(ebv),
    "One of `base_pop` and `constant_value` must be provided.",
    fixed = TRUE
  )
  expect_error(
    rebase_ebv(ebv, base_pop = base_df, constant_value = 1),
    "`base_pop` and `constant_value` must not both be provided.",
    fixed = TRUE
  )
})

test_that("constant_value must be finite numbers", {
  msg <- "`constant_value` must be a vector of finite numbers."

  expect_error(rebase_ebv(ebv, constant_value = NA_real_), msg, fixed = TRUE)
  expect_error(rebase_ebv(ebv, constant_value = c(1, NA)), msg, fixed = TRUE)
  expect_error(rebase_ebv(ebv, constant_value = Inf), msg, fixed = TRUE)
  expect_error(rebase_ebv(ebv, constant_value = "1"), msg, fixed = TRUE)
  expect_error(rebase_ebv(ebv, constant_value = numeric(0)), msg, fixed = TRUE)
  expect_no_error(rebase_ebv(ebv, constant_value = c(0, 0)))
})

test_that("constant_value must have exactly one number per EBV column", {
  # ebv has 2 EBV columns: a single number is not repeated for both
  expect_error(
    rebase_ebv(ebv, constant_value = 1),
    "one number per EBV column (2), but it has 1.",
    fixed = TRUE
  )
  expect_error(
    rebase_ebv(ebv, constant_value = c(1, 2, 3)),
    "one number per EBV column (2), but it has 3.",
    fixed = TRUE
  )
  expect_no_error(rebase_ebv(ebv, constant_value = c(1, 2)))

  # 4 EBV columns: 2 values are not repeated twice
  four_cols <- cbind(ebv, extra_1 = ebv$partial, extra_2 = ebv$whole)
  expect_error(
    rebase_ebv(four_cols, constant_value = c(1, 2)),
    "one number per EBV column (4), but it has 2.",
    fixed = TRUE
  )
  expect_no_error(rebase_ebv(four_cols, constant_value = c(1, 2, 3, 4)))

  # a data frame with a single EBV column takes exactly one value
  one_col <- ebv[, c("id", "partial")]
  expect_error(
    rebase_ebv(one_col, constant_value = c(1, 2)),
    "one number per EBV column (1), but it has 2.",
    fixed = TRUE
  )
})

test_that("plot and verbose must be a single TRUE or FALSE", {
  expect_error(rebase_ebv(ebv, constant_value = 1, plot = NA), "plot")
  expect_error(rebase_ebv(ebv, constant_value = 1, verbose = "yes"), "verbose")
})

test_that("data must be a data frame with numeric EBV columns", {
  expect_error(
    rebase_ebv(1:5, constant_value = 1),
    "`data` must be a data frame",
    fixed = TRUE
  )
  expect_error(
    rebase_ebv(ebv[, "id", drop = FALSE], constant_value = 1),
    "`data` must have at least 2 columns",
    fixed = TRUE
  )

  d <- ebv
  d$whole <- as.character(d$whole)
  expect_error(
    rebase_ebv(d, constant_value = 1),
    "not numeric: whole",
    fixed = TRUE
  )
})

test_that("base_pop must be a data frame with IDs that are in data", {
  expect_error(
    rebase_ebv(ebv, base_pop = 1:3),
    "`base_pop` must be a data frame",
    fixed = TRUE
  )

  unknown <- data.frame(id = c(base_df$id[1:2], "not_an_animal"))
  expect_error(
    rebase_ebv(ebv, base_pop = unknown),
    "`base_pop` must only have IDs that are in `data`. Not found (1), e.g. not_an_animal.",
    fixed = TRUE
  )
})

test_that("duplicated or missing IDs are an error", {
  # duplicated base-population IDs would weight the base mean
  dup_base <- data.frame(id = c(base_df$id, base_df$id[1]))
  expect_error(
    rebase_ebv(ebv, base_pop = dup_base),
    "`base_pop` has duplicated IDs",
    fixed = TRUE
  )

  dup_data <- ebv
  dup_data$id[2] <- dup_data$id[1]
  expect_error(
    rebase_ebv(dup_data, constant_value = c(1, 1)),
    "`data` has duplicated IDs",
    fixed = TRUE
  )

  na_data <- ebv
  na_data$id[1] <- NA
  expect_error(
    rebase_ebv(na_data, constant_value = c(1, 1)),
    "`data` has missing IDs.",
    fixed = TRUE
  )
  na_base <- data.frame(id = c(base_df$id, NA))
  expect_error(
    rebase_ebv(ebv, base_pop = na_base),
    "`base_pop` has missing IDs.",
    fixed = TRUE
  )
})

test_that("missing EBVs are an error with the counts per column", {
  d <- ebv
  d$partial[which(in_base)[1:2]] <- NA
  d$whole[which(in_base)[1]] <- NA

  expect_error(
    rebase_ebv(d, base_pop = base_df),
    "`data` must not have missing or infinite EBVs: `partial` has 2, `whole` has 1.",
    fixed = TRUE
  )
})

test_that("missing or infinite EBVs are an error for any animal", {
  msg <- "`data` must not have missing or infinite EBVs"
  outside <- which(!in_base)[1]

  # an animal that is not in the base population
  d_na <- ebv
  d_na$partial[outside] <- NA
  expect_error(rebase_ebv(d_na, base_pop = base_df), msg, fixed = TRUE)
  expect_error(rebase_ebv(d_na, constant_value = c(1, 1)), msg, fixed = TRUE)

  # Inf, -Inf and NaN too, in the base population or outside it
  d_inf <- ebv
  d_inf$whole[which(in_base)[1]] <- Inf
  expect_error(
    rebase_ebv(d_inf, base_pop = base_df),
    "`whole` has 1.",
    fixed = TRUE
  )
  d_ninf <- ebv
  d_ninf$partial[outside] <- -Inf
  expect_error(rebase_ebv(d_ninf, base_pop = base_df), msg, fixed = TRUE)
  d_nan <- ebv
  d_nan$partial[outside] <- NaN
  expect_error(rebase_ebv(d_nan, base_pop = base_df), msg, fixed = TRUE)
})

test_that("an empty base_pop is an error", {
  # a base population without animals has no mean, so the result would be NaN
  empty <- data.frame(id = character(0))
  expect_error(
    rebase_ebv(ebv, base_pop = empty),
    "`base_pop` must have at least one animal.",
    fixed = TRUE
  )
  expect_error(
    rebase_ebv(ebv, base_pop = base_df[0, , drop = FALSE]),
    "`base_pop` must have at least one animal.",
    fixed = TRUE
  )
})

# ----------------------------------------------------------------------------
# Block 3. IDs and classes
# Tests: Numeric and text IDs match, and a data.table gives the same result.
# ----------------------------------------------------------------------------

test_that("numeric and text IDs match, also for large numbers", {
  # as.character(100000) is "1e+05", so the IDs must be written in full
  d <- data.frame(id = c(100000, 200000, 300000, 400000), x = c(1, 2, 3, 4))
  res <- rebase_ebv(
    d,
    base_pop = data.frame(id = c("100000", "200000"))
  )$rebased_ebv

  # the mean of the base population is 1.5
  expect_equal(res$x, c(-0.5, 0.5, 1.5, 2.5))
})

test_that("a data.table gives the same result as a data frame", {
  skip_if_not_installed("data.table")

  plain <- rebase_ebv(ebv, base_pop = base_df)$rebased_ebv
  with_dt <- rebase_ebv(
    data.table::as.data.table(ebv),
    base_pop = data.table::as.data.table(base_df)
  )$rebased_ebv

  expect_equal(with_dt, plain)
  expect_false(inherits(with_dt, "data.table"))
})

# ----------------------------------------------------------------------------
# Block 4. Messages
# Tests: Silent by default; verbose = TRUE reports what was done.
# ----------------------------------------------------------------------------

test_that("there are no messages by default", {
  expect_no_message(rebase_ebv(ebv, base_pop = base_df))
  expect_no_message(rebase_ebv(ebv, constant_value = c(100, 90)))
})

test_that("verbose = TRUE reports the animals and the subtracted values", {
  # collect all the messages (expect_message() only takes the first one)
  collect_messages <- function(expr) {
    messages <- character(0)
    withCallingHandlers(
      expr,
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    return(messages)
  }

  with_base <- collect_messages(
    rebase_ebv(ebv, base_pop = base_df, verbose = TRUE)
  )
  # one message for the animals and one for each EBV column
  expect_length(with_base, 3L)
  expect_match(
    with_base[1],
    paste0(sum(in_base), " in the base population"),
    fixed = TRUE
  )
  expect_match(with_base[2], "`partial`: mean EBV of the base population")
  expect_match(with_base[3], "`whole`: mean EBV of the base population")

  with_constant <- collect_messages(
    rebase_ebv(ebv, constant_value = c(100, 90), verbose = TRUE)
  )
  expect_length(with_constant, 3L)
  expect_match(with_constant[1], "constant value(s) subtracted", fixed = TRUE)
  expect_match(with_constant[2], "`partial`: 100 subtracted.", fixed = TRUE)
  expect_match(with_constant[3], "`whole`: 90 subtracted.", fixed = TRUE)
})

# ----------------------------------------------------------------------------
# Block 5. Plot
# Tests: The list with the plots, the regression line of each plot, and no
# ggplot2 deprecations.
# ----------------------------------------------------------------------------

test_that("plot = TRUE returns the data and one plot per EBV column", {
  res <- rebase_ebv(ebv, base_pop = base_df, plot = TRUE)

  expect_named(res, c("rebased_ebv", "plots"))
  expect_named(res$plots, c("partial", "whole"))
  expect_s3_class(res$plots$partial, "ggplot")
  expect_s3_class(res$plots$whole, "ggplot")
  # the data is the same with or without the plots
  expect_equal(
    res$rebased_ebv,
    rebase_ebv(ebv, base_pop = base_df)$rebased_ebv
  )
})

test_that("the blue line has slope 1 and intercept minus the subtracted value", {
  res <- rebase_ebv(ebv, base_pop = base_df, plot = TRUE)
  # layers: grey slope-1 line, points, blue regression line
  line <- res$plots$partial$layers[[3]]$data
  expect_equal(line$slope, 1)
  expect_equal(line$intercept, -mean(ebv$partial[in_base]))

  # with one constant per column, each plot has its own intercept
  res_const <- rebase_ebv(ebv, constant_value = c(100, 90), plot = TRUE)
  line_partial <- res_const$plots$partial$layers[[3]]$data
  line_whole <- res_const$plots$whole$layers[[3]]$data
  expect_equal(line_partial$slope, 1)
  expect_equal(line_partial$intercept, -100)
  expect_equal(line_whole$slope, 1)
  expect_equal(line_whole$intercept, -90)
})

test_that("the plots build without ggplot2 deprecations", {
  build_plots <- function() {
    # a deprecated call becomes an error
    old <- options(lifecycle_verbosity = "error")
    on.exit(options(old), add = TRUE)

    res <- rebase_ebv(ebv, base_pop = base_df, plot = TRUE)
    for (p in res$plots) {
      ggplot2::ggplot_build(p)
    }
    return(TRUE)
  }
  expect_no_error(build_plots())
})
