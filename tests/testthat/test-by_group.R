# Tests for .read_groups() and .by_group(), the internals shared by the
# by-group functions.

# create the animals that can be used and a groups table for the reader tests
ids_all <- c("A1", "A2", "A3", "A4", "A5", "A6")
groups <- data.frame(
  id = c("A1", "A2", "A3", "A4", "A5", "A6", "A7"),
  young = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE, TRUE),
  genotyped = c("male", "female", NA, "male", "female", NA, "male")
)

# ----------------------------------------------------------------------------
# Block 1. Reading the group columns
# Tests: Members, labels and counts of logical and text columns, and the
# conversions (TRUE/FALSE text, factors, numeric IDs).
# ----------------------------------------------------------------------------

test_that(".read_groups reads a logical column: members, n_provided and n", {
  units <- .read_groups(groups, ids_all)

  expect_named(units, c("young", "genotyped"))
  expect_named(units$young$members, "id")
  expect_identical(units$young$members$id, c("A1", "A2", "A5"))
  # A7 is a member but not in ids_all, so it is provided but not used
  expect_equal(units$young$n_provided, 4)
  expect_equal(units$young$n, 3)
})

test_that(".read_groups reads a text column: ids with their labels", {
  units <- .read_groups(groups, ids_all)

  expect_named(units$genotyped$members, c("id", "label"))
  expect_identical(units$genotyped$members$id, c("A1", "A2", "A4", "A5"))
  expect_identical(
    units$genotyped$members$label,
    c("male", "female", "male", "female")
  )
  expect_equal(units$genotyped$n_provided, 5)
  expect_equal(units$genotyped$n, 4)
})

test_that(".read_groups keeps each label with its ID when the rows are shuffled", {
  original <- .read_groups(groups, ids_all)$genotyped$members
  shuffled <- groups[c(7, 3, 1, 5, 2, 4, 6), ]
  members <- .read_groups(shuffled, ids_all)$genotyped$members

  expect_identical(
    members$label[match(original$id, members$id)],
    original$label
  )
})

test_that(".read_groups reads text with only TRUE and FALSE as logical", {
  chr <- data.frame(
    id = ids_all,
    g = c("TRUE", "FALSE", "TRUE", NA, "FALSE", "TRUE")
  )
  units <- .read_groups(chr, ids_all)

  expect_named(units$g$members, "id")
  expect_identical(units$g$members$id, c("A1", "A3", "A6"))
})

test_that(".read_groups reads a factor column as text labels", {
  fct <- data.frame(id = ids_all, g = factor(c("x", "y", NA, "x", NA, NA)))
  units <- .read_groups(fct, ids_all)

  expect_named(units$g$members, c("id", "label"))
  expect_identical(units$g$members$label, c("x", "y", "x"))
})

test_that(".read_groups matches numeric IDs with text IDs", {
  num <- data.frame(id = 1:5, g = c(TRUE, TRUE, FALSE, TRUE, NA))
  units <- .read_groups(num, c("1", "2", "3"))

  expect_identical(units$g$members$id, c("1", "2"))
  expect_equal(units$g$n_provided, 3)
  expect_equal(units$g$n, 2)
})

# ----------------------------------------------------------------------------
# Block 2. Splitting by label
# Tests: split_labels adds one group for each label of a text column, keeps the
# group of the whole column (all its labels together), and leaves logical
# columns alone.
# ----------------------------------------------------------------------------

test_that("split_labels adds a group for each label after the whole column", {
  units <- .read_groups(groups, ids_all, split_labels = TRUE)

  expect_named(
    units,
    c("young", "genotyped", "genotyped: female", "genotyped: male")
  )
  expect_identical(units$`genotyped: female`$members$id, c("A2", "A5"))
  expect_identical(units$`genotyped: male`$members$id, c("A1", "A4"))
})

test_that("split_labels counts provided and used IDs for each label", {
  units <- .read_groups(groups, ids_all, split_labels = TRUE)

  expect_equal(units$`genotyped: male`$n_provided, 3)
  expect_equal(units$`genotyped: male`$n, 2)
  expect_equal(units$`genotyped: female`$n, 2)
})

test_that("split_labels keeps the labels in the group of the whole column", {
  units <- .read_groups(groups, ids_all, split_labels = TRUE)

  expect_named(units$genotyped$members, c("id", "label"))
  expect_named(units$`genotyped: male`$members, "id")
})

test_that("split_labels keeps a label whose animals cannot be used", {
  # the only animal with the label "other" is A7, which is not in ids_all
  other <- groups
  other$genotyped[7] <- "other"
  units <- .read_groups(other, ids_all, split_labels = TRUE)

  expect_true("genotyped: other" %in% names(units))
  expect_equal(units$`genotyped: other`$n_provided, 1)
  expect_equal(units$`genotyped: other`$n, 0)
  expect_equal(nrow(units$`genotyped: other`$members), 0)
})

test_that("split_labels = FALSE adds no group for single labels", {
  units <- .read_groups(groups, ids_all, split_labels = FALSE)

  expect_named(units, c("young", "genotyped"))
})

# ----------------------------------------------------------------------------
# Block 3. Errors of the reader
# Tests: The messages for a wrong table, wrong column names, a wrong column
# type and wrong IDs.
# ----------------------------------------------------------------------------

test_that(".read_groups needs a data frame with a group column", {
  expect_error(
    .read_groups(list(1), ids_all),
    "`groups` must be a data frame with the IDs in column 1",
    fixed = TRUE
  )
  expect_error(
    .read_groups(data.frame(id = ids_all), ids_all),
    "one column per group",
    fixed = TRUE
  )
})

test_that(".read_groups reports duplicated group names", {
  dup <- data.frame(id = ids_all, a = TRUE, b = FALSE)
  names(dup)[3] <- "a"

  expect_error(.read_groups(dup, ids_all), "Duplicated: a", fixed = TRUE)
})

test_that(".read_groups reports the column of an empty group name", {
  empty <- data.frame(id = ids_all, a = TRUE, b = FALSE)
  names(empty)[3] <- ""

  expect_error(
    .read_groups(empty, ids_all),
    "Missing or empty name in column(s): 3",
    fixed = TRUE
  )
})

test_that(".read_groups reports a column named like a split-label group", {
  clash <- data.frame(
    id = ids_all,
    genotyped = c("male", "female", NA, "male", "female", NA),
    "genotyped: male" = TRUE,
    check.names = FALSE
  )

  expect_error(
    .read_groups(clash, ids_all, split_labels = TRUE),
    "`groups` has a column named `genotyped: male`",
    fixed = TRUE
  )
  # without split_labels the two column names are different, so there is no clash
  expect_no_error(.read_groups(clash, ids_all, split_labels = FALSE))
})

test_that(".read_groups needs logical or text columns", {
  bad <- data.frame(id = ids_all, g = 1:6)

  expect_error(
    .read_groups(bad, ids_all),
    "`groups` column `g` must be TRUE/FALSE or text labels.",
    fixed = TRUE
  )
})

test_that(".read_groups needs IDs that are present and unique", {
  dup_id <- data.frame(id = c("A1", "A1", "A3"), g = TRUE)
  na_id <- data.frame(id = c("A1", NA, "A3"), g = TRUE)

  expect_error(
    .read_groups(dup_id, ids_all),
    "has duplicated IDs",
    fixed = TRUE
  )
  expect_error(.read_groups(na_id, ids_all), "has missing IDs", fixed = TRUE)
})

# ----------------------------------------------------------------------------
# Block 4. The engine
# Tests: The tables and plots collected from the groups, and the three
# statuses (ok, warning, failed) with the summary warning.
# ----------------------------------------------------------------------------

# create a stand-in for the core function: fails below 3 animals, warns at 4
fake_run <- function(unit) {
  if (unit$n < 3) {
    stop("At least 3 validation animals are needed.", call. = FALSE)
  }
  if (unit$n == 4) {
    warning("odd group", call. = FALSE)
  }
  return(list(
    stats = data.frame(value = c(n = unit$n, rho = 0.5)),
    plot = paste("plot", unit$n)
  ))
}

# create units: a is ok, b fails, c has a warning, d is ok
units_all <- list(
  a = list(n_provided = 5L, n = 5L),
  b = list(n_provided = 2L, n = 2L),
  c = list(n_provided = 4L, n = 4L),
  d = list(n_provided = 6L, n = 6L)
)
units_ok <- units_all[c("a", "d")]

test_that(".by_group collects the long table, the groups table and the plots", {
  res <- .by_group(units_ok, fake_run, verbose = FALSE)

  expect_named(res, c("stats", "groups", "plots"))
  expect_named(res$stats, c("group", "statistic", "value", "SE"))
  expect_identical(res$stats$group, c("a", "a", "d", "d"))
  expect_identical(res$stats$statistic, c("n", "rho", "n", "rho"))
  expect_identical(rownames(res$stats), as.character(1:4))
  expect_named(res$groups, c("group", "n_provided", "n", "status", "message"))
  expect_identical(res$groups$status, c("ok", "ok"))
  expect_equal(res$groups$n, c(5, 6))
  expect_named(res$plots, c("a", "d"))
})

test_that(".by_group sets SE to NA when the stats have no SE", {
  res <- .by_group(units_ok, fake_run, verbose = FALSE)

  expect_true(all(is.na(res$stats$SE)))
})

test_that(".by_group keeps the SE when the stats have one", {
  with_se <- function(unit) {
    return(list(
      stats = data.frame(value = c(rho = 0.5), SE = c(rho = 0.1)),
      plot = NULL
    ))
  }
  res <- .by_group(units_ok, with_se, verbose = FALSE)

  expect_equal(res$stats$SE, c(0.1, 0.1))
  expect_length(res$plots, 0)
})

test_that(".by_group gives no warning when every group is ok", {
  expect_length(capture_warnings(.by_group(units_ok, fake_run, FALSE)), 0)
})

test_that(".by_group lets a failing group fail without stopping the others", {
  expect_warning(
    res <- .by_group(units_all[c("a", "b")], fake_run, verbose = FALSE),
    "2 groups: 1 ok, 0 with warnings, 1 failed",
    fixed = TRUE
  )

  expect_identical(res$groups$status, c("ok", "failed"))
  expect_match(res$groups$message[2], "At least 3 validation animals")
  expect_identical(unique(res$stats$group), "a")
  expect_named(res$plots, "a")
})

test_that(".by_group keeps the stats of a group that only had warnings", {
  expect_warning(
    res <- .by_group(units_all[c("a", "c")], fake_run, verbose = FALSE),
    "1 with warnings",
    fixed = TRUE
  )

  expect_identical(res$groups$status, c("ok", "warning"))
  expect_match(res$groups$message[2], "odd group")
  expect_true("c" %in% res$stats$group)
  expect_named(res$plots, c("a", "c"))
})

test_that(".by_group gives one summary warning that names the groups", {
  w <- capture_warnings(.by_group(units_all, fake_run, verbose = FALSE))

  expect_length(w, 1)
  expect_match(w, "See `$groups` for: b (failed), c (warning).", fixed = TRUE)
})

test_that(".by_group returns empty tables when every group fails", {
  expect_warning(
    res <- .by_group(units_all["b"], fake_run, verbose = FALSE),
    "1 failed",
    fixed = TRUE
  )

  expect_equal(nrow(res$stats), 0)
  expect_named(res$stats, c("group", "statistic", "value", "SE"))
  expect_length(res$plots, 0)
  expect_identical(res$groups$status, "failed")
})

# ----------------------------------------------------------------------------
# Block 5. Messages while running
# Tests: The header, the counter, the indented messages and the summary shown
# when verbose is TRUE, and the silence when it is FALSE.
# ----------------------------------------------------------------------------

test_that(".by_group starts with a header naming the function and the groups", {
  msgs <- capture_messages(
    .by_group(units_ok, fake_run, verbose = TRUE, core_name = "fake_run()")
  )

  expect_match(
    msgs[1],
    "Validating 2 groups with fake_run(): a, d",
    fixed = TRUE
  )
})

test_that(".by_group says group and not groups for a single group", {
  msgs <- capture_messages(.by_group(units_ok["a"], fake_run, verbose = TRUE))

  expect_match(msgs[1], "Validating 1 group with", fixed = TRUE)
  expect_true(any(grepl("1 group: 1 ok, 0 with warnings, 0 failed.", msgs, fixed = TRUE)))
})

test_that(".by_group numbers each group as it runs", {
  msgs <- capture_messages(.by_group(units_ok, fake_run, verbose = TRUE))

  expect_true(any(grepl("[1/2] a", msgs, fixed = TRUE)))
  expect_true(any(grepl("[2/2] d", msgs, fixed = TRUE)))
})

test_that(".by_group shows the summary below a titled line, after the last group", {
  msgs <- capture_messages(.by_group(units_ok, fake_run, verbose = TRUE))
  summary_title <- grep("---- Summary", msgs, fixed = TRUE)
  counts <- grep(
    "2 groups: 2 ok, 0 with warnings, 0 failed.",
    msgs,
    fixed = TRUE
  )
  last_group <- max(grep("[2/2] d", msgs, fixed = TRUE))

  expect_length(summary_title, 1)
  expect_gt(summary_title, last_group)
  expect_gt(counts, summary_title)
})

test_that(".by_group lists the groups that need attention in the summary", {
  msgs <- suppressWarnings(
    capture_messages(.by_group(units_all, fake_run, verbose = TRUE))
  )
  summary_title <- grep("---- Summary", msgs, fixed = TRUE)
  failed <- grep("  b: failed. At least 3", msgs, fixed = TRUE)
  warned <- grep("  c: warning. odd group", msgs, fixed = TRUE)

  expect_gt(failed, summary_title)
  expect_gt(warned, summary_title)
})

test_that(".by_group shows the problem of a group under its name", {
  msgs <- suppressWarnings(
    capture_messages(.by_group(units_all, fake_run, verbose = TRUE))
  )

  expect_true(any(grepl("  failed: At least 3", msgs, fixed = TRUE)))
  expect_true(any(grepl("  warning: odd group", msgs, fixed = TRUE)))
})

test_that(".by_group shows the messages of the core function under its group", {
  chatty <- function(unit) {
    message("hello from the core function")
    return(fake_run(unit))
  }
  msgs <- capture_messages(.by_group(units_ok, chatty, verbose = TRUE))
  hello <- grep("hello from the core function", msgs, fixed = TRUE)
  first_group <- grep("[1/2] a", msgs, fixed = TRUE)

  expect_length(hello, 2)
  # indented by two spaces, and after the name of its group
  expect_true(all(startsWith(msgs[hello], "  hello")))
  expect_gt(hello[1], first_group)
})

test_that(".by_group is silent when verbose is FALSE", {
  chatty <- function(unit) {
    message("hello from the core function")
    return(fake_run(unit))
  }

  expect_length(capture_messages(.by_group(units_ok, chatty, FALSE)), 0)
})
