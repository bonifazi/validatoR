# Tests for validate_lr(). The full list is in PLAN.md, Block 4. The tests are
# grouped in blocks, each with a short description of what it tests.

partial_all <- toy_validation[, c("id", "partial")]
whole_all <- toy_validation[, c("id", "whole")]

# ----------------------------------------------------------------------------
# Block 1. The statistics are right
# Tests: The numbers match the direct formulas and the exact dataset targets, the
# internal function agrees with validate_lr(), and a tiny average inbreeding is kept.
# ----------------------------------------------------------------------------

test_that("the statistics match the direct formulas and the exact targets", {
  p <- toy_validation$partial
  w <- toy_validation$whole
  res <- validate_lr(
    partial_all,
    whole_all,
    var_a = 300,
    inbreeding = toy_validation[, c("id", "inbreeding")]
  )
  stats <- res$stats

  # exact checks against the formulas
  expect_equal(stats["n", "value"], nrow(toy_validation))
  expect_equal(stats["level_bias", "value"], mean(p) - mean(w))
  expect_equal(stats["level_bias_in_GSD", "value"], (mean(p) - mean(w)) / sqrt(300))
  expect_equal(stats["dispersion_bias", "value"], cov(p, w) / var(p))
  expect_equal(stats["rho", "value"], cor(p, w))
  expect_equal(stats["inc_acc", "value"], 1 / cor(p, w))
  expect_equal(stats["average_F", "value"], mean(toy_validation$inbreeding))
  expect_equal(stats["var_a", "value"], 300)
  expect_equal(
    stats["accuracy_partial", "value"],
    sqrt(cov(p, w) / ((1 - mean(toy_validation$inbreeding)) * 300))
  )

  # the dataset targets are exact (see data-raw/toy_validation.R)
  expect_equal(stats["level_bias", "value"], 0.75)
  expect_equal(stats["dispersion_bias", "value"], 0.90)
  expect_equal(stats["rho", "value"], 0.85)
  expect_equal(stats["inc_acc", "value"], 1 / 0.85)
  expect_equal(stats["level_bias_in_GSD", "value"], 0.75 / sqrt(300))
})

test_that(".lr_stats() on all rows gives the numbers of validate_lr()", {
  d <- toy_validation[, c("id", "partial", "whole", "inbreeding")]
  direct <- .lr_stats(d, seq_len(nrow(d)), var_a = 300)
  res <- validate_lr(
    partial_all,
    whole_all,
    var_a = 300,
    inbreeding = toy_validation[, c("id", "inbreeding")]
  )

  expect_equal(names(direct), rownames(res$stats))
  expect_equal(unname(direct), res$stats$value)
})

test_that("a tiny average inbreeding is kept, not rounded to 0", {
  inb <- toy_validation[, c("id", "inbreeding")]
  inb$inbreeding <- seq(0, 2e-7, length.out = nrow(inb))

  res <- validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb)
  expect_gt(res$stats["average_F", "value"], 0)
  expect_equal(res$stats["average_F", "value"], mean(inb$inbreeding))

  set.seed(1)
  res_boot <- validate_lr(
    partial_all,
    whole_all,
    var_a = 300,
    inbreeding = inb,
    bootstrap = TRUE,
    n_boot = 20
  )
  expect_equal(res_boot$stats["average_F", "value"], mean(inb$inbreeding))
})

# ----------------------------------------------------------------------------
# Block 2. Which animals are used
# Tests: Animals found in only one evaluation, the validation group (val_group) and
# the order of the rows.
# ----------------------------------------------------------------------------

test_that("animals found in only one evaluation are reported for each vector", {
  # partial holds rows 1:1990 and whole rows 11:2000, so 10 animals are only
  # in `partial` and 10 are only in `whole`
  expect_message(
    expect_message(
      validate_lr(partial_all[1:1990, ], whole_all[11:2000, ]),
      "10 animal(s) in `partial` are not in `whole`",
      fixed = TRUE
    ),
    "10 animal(s) in `whole` are not in `partial`",
    fixed = TRUE
  )
})

test_that("only the vector with extra animals gets a message", {
  expect_message(
    validate_lr(partial_all[1:1990, ], whole_all),
    "10 animal(s) in `whole` are not in `partial`",
    fixed = TRUE
  )
  expect_message(
    validate_lr(partial_all, whole_all[1:1990, ]),
    "10 animal(s) in `partial` are not in `whole`",
    fixed = TRUE
  )
})

test_that("there is no message when both vectors hold the same animals", {
  expect_no_message(validate_lr(partial_all, whole_all))
})

test_that("val_group subsets the animals and unknown IDs are an error", {
  vg <- data.frame(id = toy_validation$id[1:300])
  res <- validate_lr(partial_all, whole_all, val_group = vg)
  sub <- toy_validation[1:300, ]

  expect_equal(res$stats["n", "value"], 300)
  expect_equal(
    res$stats["dispersion_bias", "value"],
    cov(sub$partial, sub$whole) / var(sub$partial)
  )

  vg_unknown <- data.frame(id = c(toy_validation$id[1:5], "not_an_animal"))
  expect_error(
    validate_lr(partial_all, whole_all, val_group = vg_unknown),
    "ID(s) in `val_group` are not in both",
    fixed = TRUE
  )
})

test_that("the row order of the inputs does not change the result", {
  inb <- toy_validation[, c("id", "inbreeding")]
  shuffle <- function(d) {
    return(d[sample(nrow(d)), ])
  }

  ref <- validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb)
  set.seed(42)
  shuffled <- validate_lr(
    shuffle(partial_all),
    shuffle(whole_all),
    var_a = 300,
    inbreeding = shuffle(inb)
  )
  expect_equal(shuffled$stats, ref$stats)
})

# ----------------------------------------------------------------------------
# Block 3. Inbreeding
# Tests: The accepted range of the coefficients, and the accuracy when the average
# inbreeding is 1 or more.
# ----------------------------------------------------------------------------

test_that("inbreeding must be at least 0 and not missing", {
  inb <- toy_validation[, c("id", "inbreeding")]

  inb_negative <- inb
  inb_negative$inbreeding[1] <- -0.01
  expect_error(
    validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb_negative),
    "at least 0 and not missing"
  )

  inb_na <- inb
  inb_na$inbreeding[1] <- NA_real_
  expect_error(
    validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb_na),
    "at least 0 and not missing"
  )
})

test_that("inbreeding of 1 or more is accepted", {
  inb <- toy_validation[, c("id", "inbreeding")]
  inb$inbreeding[1:3] <- c(1, 1.2, 1.5)

  res <- validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb)
  expect_equal(res$stats["average_F", "value"], mean(inb$inbreeding))
  expect_false(is.nan(res$stats["accuracy_partial", "value"]))
})

test_that("accuracy is NaN with a warning when the average inbreeding is 1 or more", {
  inb <- toy_validation[, c("id", "inbreeding")]
  inb$inbreeding <- 1.5

  expect_warning(
    res <- validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb),
    "accuracy_partial` is NaN"
  )
  expect_true(is.nan(res$stats["accuracy_partial", "value"]))

  # exactly 1 divides by zero; it must give NaN too, not an error
  inb$inbreeding <- 1
  expect_warning(
    res <- validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb),
    "accuracy_partial` is NaN"
  )
  expect_true(is.nan(res$stats["accuracy_partial", "value"]))
})

# ----------------------------------------------------------------------------
# Block 4. Invalid input gives clear errors
# Tests: Unusable partial, whole and val_group inputs, no shared animals, the
# flags and numbers, the average_F and inbreeding arguments, missing, constant or
# too few EBVs, and bootstrap with average_F.
# ----------------------------------------------------------------------------

test_that("partial and whole must be data frames with IDs and numeric EBVs", {
  # not a data frame
  expect_error(
    validate_lr(partial_all$partial, whole_all),
    "`partial` must be a data frame",
    fixed = TRUE
  )
  expect_error(
    validate_lr(partial_all, "whole"),
    "`whole` must be a data frame",
    fixed = TRUE
  )

  # fewer than 2 columns
  expect_error(
    validate_lr(partial_all["id"], whole_all),
    "`partial` must have at least 2 columns (ID, value).",
    fixed = TRUE
  )

  # EBVs that are not numbers
  partial_text <- partial_all
  partial_text$partial <- as.character(partial_text$partial)
  expect_error(
    validate_lr(partial_text, whole_all),
    "`partial` must have numeric values in column 2.",
    fixed = TRUE
  )
})

test_that("missing or duplicated IDs are errors", {
  partial_na_id <- partial_all
  partial_na_id$id[1] <- NA
  expect_error(
    validate_lr(partial_na_id, whole_all),
    "`partial` has missing IDs.",
    fixed = TRUE
  )

  whole_dup <- whole_all
  whole_dup$id[2] <- whole_dup$id[1]
  expect_error(
    validate_lr(partial_all, whole_dup),
    "`whole` has duplicated IDs",
    fixed = TRUE
  )
})

test_that("partial and whole without a shared animal are an error", {
  whole_other <- whole_all
  whole_other$id <- paste0("other_", whole_other$id)
  expect_error(
    validate_lr(partial_all, whole_other),
    "have no animal IDs in common",
    fixed = TRUE
  )
})

test_that("missing, constant and too few validation animals are errors", {
  # missing EBVs, counted per vector
  partial_na <- partial_all
  partial_na$partial[1] <- NA_real_
  expect_error(
    validate_lr(partial_na, whole_all),
    "Missing EBVs are not allowed for validation animals: `partial` has 1"
  )

  # ... but only for the validation animals: this NA is outside `val_group`
  whole_na <- whole_all
  whole_na$whole[500] <- NA_real_
  vg <- data.frame(id = toy_validation$id[1:300])
  expect_no_error(validate_lr(partial_all, whole_na, val_group = vg))

  # constant EBVs
  partial_const <- partial_all
  partial_const$partial <- 1
  expect_error(validate_lr(partial_const, whole_all), "must vary")
  whole_const <- whole_all
  whole_const$whole <- 1
  expect_error(validate_lr(partial_all, whole_const), "must vary")

  # fewer than 3 animals
  expect_error(
    validate_lr(partial_all[1:2, ], whole_all[1:2, ]),
    "At least 3 validation animals"
  )
})

test_that("bootstrap = TRUE with average_F is an error", {
  expect_error(
    validate_lr(partial_all, whole_all, var_a = 300, average_F = 0.05, bootstrap = TRUE),
    "`inbreeding` (one value per animal) must be provided instead of `average_F`",
    fixed = TRUE
  )
})

test_that("val_group must be a data frame with unique, present IDs", {
  ids <- toy_validation$id

  # not a data frame
  expect_error(
    validate_lr(partial_all, whole_all, val_group = ids[1:300]),
    "`val_group` must be a data frame",
    fixed = TRUE
  )

  # duplicated or missing IDs
  vg_dup <- data.frame(id = c(ids[1:5], ids[1]))
  expect_error(
    validate_lr(partial_all, whole_all, val_group = vg_dup),
    "`val_group` IDs must be unique and not missing.",
    fixed = TRUE
  )
  vg_na <- data.frame(id = c(ids[1:5], NA))
  expect_error(
    validate_lr(partial_all, whole_all, val_group = vg_na),
    "`val_group` IDs must be unique and not missing.",
    fixed = TRUE
  )
})

test_that("plot_subgroups needs a group label in column 2 of val_group", {
  msg <- "`val_group` must have a group label in column 2 when `plot_subgroups = TRUE`."

  # no val_group at all
  expect_error(
    validate_lr(partial_all, whole_all, plot_subgroups = TRUE),
    msg,
    fixed = TRUE
  )

  # a val_group with the IDs only
  vg_ids <- data.frame(id = toy_validation$id[1:300])
  expect_error(
    validate_lr(partial_all, whole_all, val_group = vg_ids, plot_subgroups = TRUE),
    msg,
    fixed = TRUE
  )
})

test_that("average_F must be a single number in [0, 1), and not with inbreeding", {
  msg <- "`average_F` must be a single number in [0, 1)."
  expect_error(validate_lr(partial_all, whole_all, average_F = 1), msg, fixed = TRUE)
  expect_error(validate_lr(partial_all, whole_all, average_F = -0.1), msg, fixed = TRUE)
  expect_error(validate_lr(partial_all, whole_all, average_F = NA_real_), msg, fixed = TRUE)
  expect_error(
    validate_lr(partial_all, whole_all, average_F = c(0.1, 0.2)),
    msg,
    fixed = TRUE
  )

  # average_F and inbreeding are alternatives
  expect_error(
    validate_lr(
      partial_all,
      whole_all,
      var_a = 300,
      average_F = 0.05,
      inbreeding = toy_validation[, c("id", "inbreeding")]
    ),
    "`average_F` and `inbreeding` must not both be provided.",
    fixed = TRUE
  )
})

test_that("inbreeding must be a data frame covering every validation animal", {
  inb <- toy_validation[, c("id", "inbreeding")]

  # not a data frame
  expect_error(
    validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb$inbreeding),
    "`inbreeding` must be a data frame",
    fixed = TRUE
  )

  # an animal without a coefficient
  expect_error(
    validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb[-1, ]),
    "1 validation animal(s) are missing from `inbreeding`",
    fixed = TRUE
  )
})

test_that("flags and numbers are checked in validate_lr()", {
  # every flag must be a single TRUE or FALSE
  flags <- c(
    "plot",
    "plot_verbose",
    "plot_subgroups",
    "plot_in_gsd",
    "bootstrap",
    "verbose"
  )
  for (flag in flags) {
    args <- list(partial_all, whole_all)
    args[[flag]] <- NA
    expect_error(
      do.call(validate_lr, args),
      paste0("`", flag, "` must be TRUE or FALSE."),
      fixed = TRUE
    )
  }

  # the numbers
  expect_error(
    validate_lr(partial_all, whole_all, n_boot = 1),
    "`n_boot` must be a single whole number of at least 2.",
    fixed = TRUE
  )
  expect_error(
    validate_lr(partial_all, whole_all, ncpus = 0),
    "`ncpus` must be a single whole number of at least 1.",
    fixed = TRUE
  )
  expect_error(
    validate_lr(partial_all, whole_all, var_a = -1),
    "`var_a` must be a single positive number.",
    fixed = TRUE
  )
  expect_error(
    validate_lr(partial_all, whole_all, var_a = "300"),
    "`var_a` must be a single positive number.",
    fixed = TRUE
  )
})

# ----------------------------------------------------------------------------
# Block 5. Bootstrap
# Tests: Standard errors of the statistics.
# ----------------------------------------------------------------------------

test_that("bootstrap SEs are positive, with NA for n and 0 for var_a", {
  inb <- toy_validation[, c("id", "inbreeding")]
  plain <- validate_lr(partial_all, whole_all, var_a = 300, inbreeding = inb)
  set.seed(1)
  res <- validate_lr(
    partial_all,
    whole_all,
    var_a = 300,
    inbreeding = inb,
    bootstrap = TRUE,
    n_boot = 50
  )
  stats <- res$stats

  expect_named(stats, c("value", "SE"))
  # the estimates are those of the original data
  expect_equal(stats$value, plain$stats$value)
  expect_true(is.na(stats["n", "SE"]))
  expect_equal(stats["var_a", "SE"], 0)
  varying <- setdiff(rownames(stats), c("n", "var_a"))
  expect_true(all(stats[varying, "SE"] > 0))
})

# ----------------------------------------------------------------------------
# Block 6. Plots
# Tests: Group labels, plot_in_gsd, the regression line, and no ggplot2
# deprecations.
# ----------------------------------------------------------------------------

test_that("group labels follow the animals, whatever the order of val_group", {
  # val_group in reverse order, with a label that depends on the ID
  ids <- toy_validation$id[1:200]
  vg <- data.frame(id = rev(ids))
  vg$group <- ifelse(as.numeric(vg$id) %% 2 == 0, "even", "odd")

  res <- validate_lr(
    partial_all,
    whole_all,
    val_group = vg,
    plot = TRUE,
    plot_subgroups = TRUE
  )

  # the plot data has no IDs, so go back to the animal through its partial EBV
  plotted <- res$plot$data
  animal <- toy_validation$id[match(plotted$partial, toy_validation$partial)]
  expected <- ifelse(as.numeric(animal) %% 2 == 0, "even", "odd")
  expect_equal(nrow(plotted), 200L)
  expect_identical(as.character(plotted$group), expected)
})

test_that("plot_in_gsd needs var_a and only changes the plot", {
  expect_error(
    validate_lr(partial_all, whole_all, plot = TRUE, plot_in_gsd = TRUE),
    "`var_a` must be provided when `plot_in_gsd = TRUE`",
    fixed = TRUE
  )

  plain <- validate_lr(partial_all, whole_all, var_a = 300, plot = TRUE)
  gsd <- validate_lr(
    partial_all,
    whole_all,
    var_a = 300,
    plot = TRUE,
    plot_in_gsd = TRUE
  )

  # labels: "(GSD)" only when requested, even if var_a is provided
  expect_false(grepl("GSD", plain$plot$labels$x))
  expect_false(grepl("GSD", plain$plot$labels$y))
  expect_match(gsd$plot$labels$x, "(GSD)", fixed = TRUE)
  expect_match(gsd$plot$labels$y, "(GSD)", fixed = TRUE)
  # the axes are the EBVs divided by sqrt(var_a)
  expect_equal(
    range(gsd$plot$data$partial),
    range(toy_validation$partial) / sqrt(300)
  )
  # the statistics are the same either way
  expect_equal(gsd$stats, plain$stats)
})

test_that("the plotted regression line is the fit of whole on partial", {
  fit <- coef(lm(whole ~ partial, data = toy_validation))
  plain <- validate_lr(partial_all, whole_all, var_a = 300, plot = TRUE)
  gsd <- validate_lr(
    partial_all,
    whole_all,
    var_a = 300,
    plot = TRUE,
    plot_in_gsd = TRUE
  )

  # layers: grey slope-1 line, points, blue regression line
  line_plain <- plain$plot$layers[[3]]$data
  line_gsd <- gsd$plot$layers[[3]]$data
  expect_equal(line_plain$slope, unname(fit[2]))
  expect_equal(line_plain$intercept, unname(fit[1]))
  # dividing both axes by sqrt(var_a) keeps the slope and rescales the intercept
  expect_equal(line_gsd$slope, unname(fit[2]))
  expect_equal(line_gsd$intercept, unname(fit[1]) / sqrt(300))
})

test_that("the plots build without ggplot2 deprecations", {
  vg <- data.frame(
    id = toy_validation$id[1:200],
    group = rep(c("A", "B"), 100)
  )
  build_plots <- function() {
    # a deprecated call becomes an error
    old <- options(lifecycle_verbosity = "error")
    on.exit(options(old), add = TRUE)

    plain <- validate_lr(partial_all, whole_all, plot = TRUE)
    groups <- validate_lr(
      partial_all,
      whole_all,
      val_group = vg,
      plot = TRUE,
      plot_subgroups = TRUE
    )
    gsd <- validate_lr(
      partial_all,
      whole_all,
      var_a = 300,
      plot = TRUE,
      plot_in_gsd = TRUE,
      plot_verbose = TRUE
    )
    for (res in list(plain, groups, gsd)) {
      ggplot2::ggplot_build(res$plot)
    }
    return(TRUE)
  }
  expect_no_error(build_plots())
})

# ----------------------------------------------------------------------------
# Block 7. Input classes
# Tests: A data.table gives the same result as a data frame.
# ----------------------------------------------------------------------------

test_that("data.table inputs give the same result as data frames", {
  skip_if_not_installed("data.table")
  as_dt <- function(d) {
    return(data.table::as.data.table(d))
  }
  inb <- toy_validation[, c("id", "inbreeding")]
  vg <- data.frame(id = toy_validation$id[1:300])

  plain <- validate_lr(
    partial_all,
    whole_all,
    val_group = vg,
    var_a = 300,
    inbreeding = inb
  )
  with_dt <- validate_lr(
    as_dt(partial_all),
    as_dt(whole_all),
    val_group = as_dt(vg),
    var_a = 300,
    inbreeding = as_dt(inb)
  )
  expect_equal(with_dt$stats, plain$stats)
})
