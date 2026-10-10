# Tests for validate_lr_by_group(). The engine and the reader have their own
# tests in test-by_group.R.

# create the evaluations, the inbreeding and a groups table for the tests
p <- toy_validation[, c("id", "partial")]
w <- toy_validation[, c("id", "whole")]
inb <- toy_validation[, c("id", "inbreeding")]
groups <- data.frame(
  id = toy_validation$id,
  cohort_1 = toy_validation$group == "cohort_1",
  cohort_2 = toy_validation$group == "cohort_2",
  cohort = toy_validation$group
)

# create a function that returns the values of one group as a named vector
group_values <- function(res, group) {
  rows <- res$stats[res$stats$group == group, ]
  return(stats::setNames(rows$value, rows$statistic))
}

# create a function that returns the values of a stats table as a named vector
plain_values <- function(stats_table) {
  return(stats::setNames(stats_table$value, rownames(stats_table)))
}

# ----------------------------------------------------------------------------
# Block 1. Group "all" and other groups against plain calls
# Tests: The group "all" gives the same numbers as a plain validate_lr() call
# for each argument that is passed on, and so does a group of animals.
# ----------------------------------------------------------------------------

test_that("the group all equals a plain validate_lr() call", {
  res <- validate_lr_by_group(p, w, groups, var_a = 300, verbose = FALSE)
  plain <- validate_lr(p, w, var_a = 300)$stats

  expect_equal(group_values(res, "all"), plain_values(plain))
})

test_that("the group all equals a plain call with inbreeding", {
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    var_a = 300,
    inbreeding = inb,
    verbose = FALSE
  )
  plain <- validate_lr(p, w, var_a = 300, inbreeding = inb)$stats

  expect_equal(group_values(res, "all"), plain_values(plain))
})

test_that("the group all equals a plain call with average_inbreeding", {
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    var_a = 300,
    average_inbreeding = 0.05,
    verbose = FALSE
  )
  plain <- validate_lr(p, w, var_a = 300, average_inbreeding = 0.05)$stats

  expect_equal(group_values(res, "all"), plain_values(plain))
})

test_that("the group all equals a plain call with the bootstrap", {
  set.seed(1)
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    var_a = 300,
    inbreeding = inb,
    bootstrap = TRUE,
    n_boot = 30,
    verbose = FALSE
  )
  set.seed(1)
  plain <- validate_lr(
    p,
    w,
    var_a = 300,
    inbreeding = inb,
    bootstrap = TRUE,
    n_boot = 30
  )$stats

  expect_equal(group_values(res, "all"), plain_values(plain))
  rows <- res$stats[res$stats$group == "all", ]
  expect_equal(stats::setNames(rows$SE, rows$statistic), {
    stats::setNames(plain$SE, rownames(plain))
  })
})

test_that("a group equals a plain call on the same animals", {
  res <- validate_lr_by_group(p, w, groups, var_a = 300, verbose = FALSE)
  members <- data.frame(id = toy_validation$id[groups$cohort_1])
  plain <- validate_lr(p, w, val_group = members, var_a = 300)$stats

  expect_equal(group_values(res, "cohort_1"), plain_values(plain))
})

# ----------------------------------------------------------------------------
# Block 2. The arguments follow validate_lr()
# Tests: Every argument of validate_lr() is in validate_lr_by_group() with the
# same default, so the two functions cannot drift apart unnoticed.
# ----------------------------------------------------------------------------

test_that("validate_lr_by_group() has the arguments of validate_lr()", {
  lr_args <- formals(validate_lr)
  by_group_args <- formals(validate_lr_by_group)
  # val_group is replaced by groups
  passed_on <- setdiff(names(lr_args), "val_group")

  expect_true(all(passed_on %in% names(by_group_args)))
  # check that the only arguments not in validate_lr() are the ones for groups
  expect_setequal(
    setdiff(names(by_group_args), passed_on),
    c("groups", "split_labels", "include_all")
  )
})

test_that("validate_lr_by_group() has the defaults of validate_lr()", {
  lr_args <- formals(validate_lr)
  by_group_args <- formals(validate_lr_by_group)
  # verbose is TRUE on purpose in the by-group function
  same <- setdiff(names(lr_args), c("val_group", "verbose"))

  expect_identical(by_group_args[same], lr_args[same])
  expect_true(by_group_args$verbose)
})

# ----------------------------------------------------------------------------
# Block 3. Groups
# Tests: include_all, a column called all, split_labels, overlapping groups and
# the counts in the groups table.
# ----------------------------------------------------------------------------

test_that("the groups table counts the animals of each group", {
  res <- validate_lr_by_group(p, w, groups, var_a = 300, verbose = FALSE)

  expect_identical(res$groups$group, c("all", "cohort_1", "cohort_2", "cohort"))
  # count the animals of each group expected in the groups table
  expected <- c(
    nrow(groups),
    sum(groups$cohort_1),
    sum(groups$cohort_2),
    nrow(groups)
  )
  expect_equal(res$groups$n_provided, expected)
  expect_equal(res$groups$n, expected)
  expect_identical(res$groups$status, rep("ok", 4))
})

test_that("include_all = FALSE leaves out the group all", {
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    var_a = 300,
    include_all = FALSE,
    plot = TRUE,
    verbose = FALSE
  )

  expect_identical(res$groups$group, c("cohort_1", "cohort_2", "cohort"))
  expect_false("all" %in% res$stats$group)
  expect_false("all" %in% names(res$plots))
})

test_that("a column called all is an error only with include_all = TRUE", {
  with_all <- groups
  names(with_all)[2] <- "all"

  expect_error(
    validate_lr_by_group(p, w, with_all, verbose = FALSE),
    "`groups` must not have a column named \"all\"",
    fixed = TRUE
  )
  res <- validate_lr_by_group(
    p,
    w,
    with_all,
    include_all = FALSE,
    verbose = FALSE
  )
  expect_identical(res$groups$group[1], "all")
})

test_that("split_labels adds a group for each label of a text column", {
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    var_a = 300,
    split_labels = TRUE,
    verbose = FALSE
  )

  expect_identical(
    res$groups$group,
    c(
      "all",
      "cohort_1",
      "cohort_2",
      "cohort",
      paste0("cohort: ", sort(unique(as.character(toy_validation$group))))
    )
  )
  # the group of one label has the same animals as the logical column
  expect_equal(
    group_values(res, "cohort: cohort_1"),
    group_values(res, "cohort_1")
  )
})

test_that("an animal can be in several groups", {
  overlap <- data.frame(
    id = toy_validation$id,
    a = toy_validation$group %in% c("cohort_1", "cohort_2"),
    b = toy_validation$group %in% c("cohort_2", "cohort_3")
  )
  res <- validate_lr_by_group(p, w, overlap, verbose = FALSE)

  # check that the two groups share animals
  shared <- sum(overlap$a & overlap$b)
  expect_gt(shared, 0)
  # each group keeps all its animals, including the ones in both groups
  expect_equal(res$groups$n, c(nrow(overlap), sum(overlap$a), sum(overlap$b)))
  expect_identical(res$groups$status, rep("ok", 3))
  # count the animals in both groups once for each group, so the two group sizes together are larger than the number of different animals in the two groups
  expect_gt(sum(res$groups$n[2:3]), sum(overlap$a | overlap$b))
})

test_that("IDs of a group that are in no evaluation are counted but not used", {
  # add three IDs to cohort_1 that are neither in partial nor in whole
  extra <- data.frame(
    id = paste0("x", 1:3),
    cohort_1 = TRUE,
    cohort_2 = FALSE,
    cohort = NA
  )
  res <- validate_lr_by_group(
    p,
    w,
    rbind(groups, extra),
    var_a = 300,
    verbose = FALSE
  )
  row <- res$groups[res$groups$group == "cohort_1", ]

  expect_equal(row$n_provided, sum(groups$cohort_1) + 3)
  expect_equal(row$n, sum(groups$cohort_1))
  expect_identical(row$status, "ok")
  # the statistics are those of the animals that could be used
  expect_equal(group_values(res, "cohort_1")[["n"]], sum(groups$cohort_1))
})

# ----------------------------------------------------------------------------
# Block 4. Failing groups and warnings
# Tests: A group that fails or warns does not stop the others, and is reported
# in the groups table and in one warning.
# ----------------------------------------------------------------------------

test_that("a group with too few animals fails and the others still run", {
  groups_tiny <- cbind(
    groups,
    tiny = toy_validation$id %in% toy_validation$id[1:2]
  )

  expect_warning(
    res <- validate_lr_by_group(
      p,
      w,
      groups_tiny,
      var_a = 300,
      verbose = FALSE
    ),
    "tiny (failed)",
    fixed = TRUE
  )
  tiny_row <- res$groups[res$groups$group == "tiny", ]
  expect_identical(tiny_row$status, "failed")
  expect_match(tiny_row$message, "At least 3 validation animals")
  expect_false("tiny" %in% res$stats$group)
  expect_true(all(c("all", "cohort_1") %in% res$stats$group))
})

test_that("a group with a warning keeps its statistics", {
  ids <- paste0("A", 1:20)
  p2 <- data.frame(id = ids, partial = 1:20)
  w2 <- data.frame(id = ids, whole = c(1:10, 20:11))
  groups2 <- data.frame(
    id = ids,
    up = rep(c(TRUE, FALSE), each = 10),
    down = rep(c(FALSE, TRUE), each = 10)
  )

  expect_warning(
    res <- validate_lr_by_group(
      p2,
      w2,
      groups2,
      var_a = 1,
      average_inbreeding = 0,
      verbose = FALSE
    ),
    "down (warning)",
    fixed = TRUE
  )
  down_row <- res$groups[res$groups$group == "down", ]
  expect_identical(down_row$status, "warning")
  expect_match(down_row$message, "accuracy_partial")
  expect_true("down" %in% res$stats$group)
})

# ----------------------------------------------------------------------------
# Block 5. Plots
# Tests: One plot for each group that ran, coloured by the labels of a text
# column when plot_subgroups is TRUE.
# ----------------------------------------------------------------------------

test_that("plots are only built when plot = TRUE", {
  res <- validate_lr_by_group(p, w, groups, var_a = 300, verbose = FALSE)

  expect_length(res$plots, 0)
})

test_that("plot = TRUE gives one plot for each group", {
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    var_a = 300,
    plot = TRUE,
    verbose = FALSE
  )

  expect_named(res$plots, c("all", "cohort_1", "cohort_2", "cohort"))
  expect_s3_class(res$plots$cohort, "ggplot")
  expect_no_error(ggplot2::ggplot_build(res$plots$cohort_1))
})

test_that("a group that failed has no plot", {
  groups_tiny <- cbind(
    groups,
    tiny = toy_validation$id %in% toy_validation$id[1:2]
  )
  res <- suppressWarnings(
    validate_lr_by_group(p, w, groups_tiny, plot = TRUE, verbose = FALSE)
  )

  expect_false("tiny" %in% names(res$plots))
})

test_that("plot_subgroups colours a text column and not a logical one", {
  res <- validate_lr_by_group(
    p,
    w,
    groups,
    plot = TRUE,
    plot_subgroups = TRUE,
    verbose = FALSE
  )

  # the points are the second layer of the plot
  coloured <- ggplot2::ggplot_build(res$plots$cohort)
  plain <- ggplot2::ggplot_build(res$plots$cohort_1)
  expect_length(
    unique(coloured$data[[2]]$colour),
    nlevels(toy_validation$group)
  )
  expect_equal(res$plots$cohort$labels$colour, "Group")
  expect_length(unique(plain$data[[2]]$colour), 1L)
  expect_null(res$plots$cohort_1$labels$colour)
})

# ----------------------------------------------------------------------------
# Block 6. Errors
# Tests: The messages for wrong arguments, which stop the function before any
# group runs.
# ----------------------------------------------------------------------------

test_that("partial and whole must share some animals", {
  p_other <- p
  p_other$id <- paste0("x", p$id)

  expect_error(
    validate_lr_by_group(p_other, w, groups),
    "have no animal IDs in common",
    fixed = TRUE
  )
})

test_that("groups must be a data frame", {
  expect_error(
    validate_lr_by_group(p, w, "cohort_1"),
    "`groups` must be a data frame",
    fixed = TRUE
  )
})

test_that("plot_subgroups needs a text column", {
  logical_only <- groups[, c("id", "cohort_1", "cohort_2")]

  expect_error(
    validate_lr_by_group(p, w, logical_only, plot_subgroups = TRUE),
    "`groups` must have at least one text column",
    fixed = TRUE
  )
})

test_that("the flags of the by-group function are checked", {
  expect_error(
    validate_lr_by_group(p, w, groups, split_labels = NA),
    "`split_labels` must be TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    validate_lr_by_group(p, w, groups, include_all = "yes"),
    "`include_all` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("a mistake in a shared argument is one error before the groups", {
  expect_error(
    validate_lr_by_group(p, w, groups, n_boot = 1),
    "`n_boot` must be a single whole number",
    fixed = TRUE
  )
  expect_error(
    validate_lr_by_group(p, w, groups, var_a = -1),
    "`var_a` must be a single positive number",
    fixed = TRUE
  )
  expect_error(
    validate_lr_by_group(
      p,
      w,
      groups,
      average_inbreeding = 0.05,
      inbreeding = inb
    ),
    "must not both be provided",
    fixed = TRUE
  )
})

# ----------------------------------------------------------------------------
# Block 7. Messages while running
# Tests: The group names and the summary shown when verbose is TRUE, and the
# silence when it is FALSE.
# ----------------------------------------------------------------------------

test_that("verbose = TRUE names each group and shows the summary", {
  msgs <- capture_messages(validate_lr_by_group(p, w, groups, var_a = 300))

  expect_true(
    any(grepl("Validating 4 groups with validate_lr()", msgs, fixed = TRUE))
  )
  expect_true(any(grepl("[1/4] all", msgs, fixed = TRUE)))
  expect_true(any(grepl("[2/4] cohort_1", msgs, fixed = TRUE)))
  expect_true(any(grepl("4 groups: 4 ok", msgs, fixed = TRUE)))
})

test_that("verbose = FALSE is silent when every group is ok", {
  expect_length(
    capture_messages(
      validate_lr_by_group(p, w, groups, var_a = 300, verbose = FALSE)
    ),
    0
  )
})
