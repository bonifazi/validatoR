# R/by_group.R
# Internal functions shared by the by-group functions, such as validate_lr_by_group():
# reading the group columns, and running one validation per group.

#' Read the group columns of `groups` into a list of units
#'
#' A group is one column of `groups`: the animals that belong to it. A unit is
#' what the engine runs once: one group, as a list with a data frame of its
#' members that can be used (`members`: column `id`, and column `label` for text
#' columns, so each ID keeps its label), how many IDs were provided
#' (`n_provided`) and how many can be used for validation (`n`). With
#' `split_labels`, each label of a text column is added as a unit of its own.
#'
#' @param groups Data frame: IDs in column 1, one column per group. A column is
#'   one of two types:
#'   1) logical (or text holding only "TRUE" and "FALSE"): `TRUE` for the
#'   members;
#'   2) text: a label for the members and `NA` for the rest.
#' @param ids IDs that can be used, that is, the animals that have all the data
#'   the validation needs. For instance, for `validate_lr()`, the IDs found in both `partial`
#'   and `whole`. Group members that are not in `ids` are counted in
#'   `n_provided` but not used for the validation statistics.
#' @param split_labels Logical. Add one unit per label of a text column.
#' @return Named list of units, each a list with `members`, `n_provided` and
#'   `n`.
#' @noRd
.read_groups <- function(groups, ids, split_labels = FALSE) {
  # check that groups is a data frame with an ID column and a group column
  if (!is.data.frame(groups) || ncol(groups) < 2L) {
    stop(
      "`groups` must be a data frame with the IDs in column 1 and one ",
      "column per group.",
      call. = FALSE
    )
  }
  # convert to data frame (in case it is a tibble or data.table)
  groups <- as.data.frame(groups)
  # check the IDs and write them as text
  group_ids <- .as_checked_ids(groups[[1]], "groups")
  # store the names of the group columns, excluding the ID column
  group_names <- names(groups)[-1]
  # find group columns with a missing or empty name, and duplicated names
  no_name <- which(is.na(group_names) | group_names == "")
  named <- group_names[!is.na(group_names) & group_names != ""]
  dup_names <- unique(named[duplicated(named)])
  # check that every group column has its own name, and report the problems
  if (length(no_name) > 0L || length(dup_names) > 0L) {
    stop(
      "`groups` must have a unique name for each group column.",
      if (length(no_name) > 0L) {
        paste0(
          " Missing or empty name in column(s): ",
          .show_some(no_name + 1L, 5L),
          "."
        )
      },
      if (length(dup_names) > 0L) {
        paste0(" Duplicated: ", .show_some(dup_names, 5L), ".")
      },
      call. = FALSE
    )
  }

  # loop over the group columns to create one unit per group column (and per label when split_labels is TRUE)
  units <- list()
  for (name in group_names) {
    # get values of this group column: TRUE/FALSE or labels, one per animal
    col <- groups[[name]]
    # convert factors to text
    if (is.factor(col)) {
      col <- as.character(col)
    }
    # convert text that only holds "TRUE" and "FALSE" to logical
    if (is.character(col) && all(col[!is.na(col)] %in% c("TRUE", "FALSE"))) {
      col <- as.logical(col)
    }
    # check that the column is logical or text labels
    if (!(is.logical(col) || is.character(col))) {
      stop(
        "`groups` column `",
        name,
        "` must be TRUE/FALSE or text labels.",
        call. = FALSE
      )
    }
    # find the members: the TRUE values of a logical column, or any label of a text column
    if (is.logical(col)) {
      is_member <- col %in% TRUE
      labels <- NULL
    } else {
      is_member <- !is.na(col)
      labels <- col
    }
    # keep the members that can be used, that is, whose ID is in ids
    keep <- is_member & group_ids %in% ids
    # create the members of the group, which are the IDs that can be used
    members <- data.frame(id = group_ids[keep], stringsAsFactors = FALSE)
    # add the labels next to their IDs, for text columns only
    if (!is.null(labels)) {
      members$label <- labels[keep]
    }
    # store the members and the counts of the group as a unit
    units[[name]] <- list(
      members = members,
      n_provided = sum(is_member),
      n = sum(keep)
    )
    # add each label of a text column as a unit of its own when split_labels is TRUE
    if (isTRUE(split_labels) && !is.null(labels)) {
      # loop over the unique labels of the group column, sorted alphabetically, including labels whose animals cannot be used
      for (lab in sort(unique(labels[is_member]))) {
        # name the group of this label, and check that no column already has that name
        label_name <- paste0(name, ": ", lab)
        if (label_name %in% group_names) {
          stop(
            "`groups` has a column named `",
            label_name,
            "`, which is also the name of the group that `split_labels = TRUE` ",
            "makes for the label `",
            lab,
            "` of column `",
            name,
            "`. Rename the column.",
            call. = FALSE
          )
        }
        # find the members of the group that have this label
        in_label <- is_member & labels %in% lab
        # store the members and the counts of this label as a unit
        units[[label_name]] <- list(
          members = data.frame(
            id = group_ids[in_label & keep],
            stringsAsFactors = FALSE
          ),
          n_provided = sum(in_label),
          n = sum(in_label & keep)
        )
      }
    }
  }
  return(units)
}

#' Run one function per group and collect the results
#'
#' This function runs `run_one` on each group and collects the results.
#' A failing group never stops the other groups. The error or the warnings of a
#' group are kept in the `groups` table, and a single warning at the end names
#' the groups that need attention.
#'
#' @param units Named list of units with `n_provided` and `n` (from
#'   `.read_groups()`, plus the `"all"` unit).
#' @param run_one Function of one unit that returns a list with `stats` and
#'   `plot`, such as the result of `validate_lr()`.
#' @param verbose Logical. If `TRUE`, show a header, name each group as it runs
#'   with a counter, show its messages (indented), warnings and errors, and show
#'   a summary at the end. If `FALSE`, the messages while running each group
#'   are suppressed. Defaults to `TRUE`.
#' @param core_name Text: the name of the function that is run on each group,
#'   used in the header. Defaults to `"validate_lr()"`.
#' @return List with `stats` (long table), `groups` (one row per group, with
#'   `status` "ok", "warning" or "failed") and `plots` (named list).
#' @noRd
.by_group <- function(
  units,
  run_one,
  verbose = TRUE,
  core_name = "validate_lr()"
) {
  # initialize one status and one message per group (empty text for now), and empty lists for the statistics and the plots
  n_units <- length(units)
  # choose the word for the number of groups, so that one group is not called "1 groups"
  if (n_units == 1L) {
    groups_word <- "group"
  } else {
    groups_word <- "groups"
  }
  group_status <- character(n_units)
  group_message <- character(n_units)
  stats_by_group <- list()
  plots <- list()

  # initialize the run function based on the verbose setting so that
  # messages of the core function are shown, indented, only when verbose
  if (isTRUE(verbose)) {
    run <- function(unit) {
      return(
        withCallingHandlers(
          run_one(unit),
          message = function(m) {
            # show the message of the core function two spaces in, under the group
            message("  ", conditionMessage(m), appendLF = FALSE)
            invokeRestart("muffleMessage")
          }
        )
      )
    }
    # show the header: the function that is run, and on which groups
    group_names <- .show_some(names(units), 6L)
    if (n_units > 6L) {
      group_names <- paste0(group_names, ", ...")
    }
    message(
      "Validating ",
      n_units,
      " ",
      groups_word,
      " with ",
      core_name,
      ": ",
      group_names,
      "\n"
    )
  } else {
    run <- function(unit) {
      # suppress the messages of the core function (its warnings and errors are not suppressed)
      return(suppressMessages(run_one(unit)))
    }
  }

  # loop over the units to run the core function on each group
  for (i in seq_len(n_units)) {
    # get the name of the group
    group_name <- names(units)[i]
    if (isTRUE(verbose)) {
      message("[", i, "/", n_units, "] ", group_name)
    }
    # collect the warnings of this group instead of showing them one by one
    # (the handler below adds each warning to this vector with <<-)
    group_warnings <- character(0)
    group_result <- tryCatch(
      withCallingHandlers(
        run(units[[i]]),
        warning = function(w) {
          group_warnings <<- c(group_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )

    # store the status of the group (failed, warning or ok) and its message, which is the error or the warnings and is empty when the status is ok
    if (inherits(group_result, "error")) {
      group_status[i] <- "failed"
      group_message[i] <- conditionMessage(group_result)
    } else if (length(group_warnings) > 0L) {
      group_status[i] <- "warning"
      group_message[i] <- paste(group_warnings, collapse = " | ")
    } else {
      group_status[i] <- "ok"
    }
    # show the problem under the name of the group
    if (isTRUE(verbose) && group_status[i] != "ok") {
      message("  ", group_status[i], ": ", group_message[i])
    }

    # store the statistics and the plot of the group, unless the group failed
    if (group_status[i] != "failed") {
      group_stats <- as.data.frame(group_result$stats)
      # add an SE column of NA when the bootstrap was not run, because SE only exists with the bootstrap
      if (!"SE" %in% names(group_stats)) {
        group_stats$SE <- NA_real_
      }
      # store the statistics of the group in a data frame with columns: group, statistic, value, SE
      stats_by_group[[group_name]] <- data.frame(
        group = group_name,
        statistic = rownames(group_stats),
        value = group_stats$value,
        SE = group_stats$SE,
        stringsAsFactors = FALSE
      )
      # store the plot of the group in a named list, if it exists
      if (!is.null(group_result$plot)) {
        plots[[group_name]] <- group_result$plot
      }
    }
  }

  # combine the statistics of all groups into one long table, or create an empty table when every group failed
  if (length(stats_by_group) > 0L) {
    all_groups_stats <- do.call(rbind, stats_by_group)
  } else {
    all_groups_stats <- data.frame(
      group = character(0),
      statistic = character(0),
      value = numeric(0),
      SE = numeric(0)
    )
  }
  # reset the row names to avoid duplicates; rbind() rebuilt them from the group names (e.g., genotyped.1, genotyped.2, ...)
  rownames(all_groups_stats) <- NULL

  # create the output groups table with one row per group containing info about:
  # the number of IDs provided and used, the status and the message
  groups_table <- data.frame(
    group = names(units),
    n_provided = vapply(units, function(unit) unit$n_provided, numeric(1)),
    n = vapply(units, function(unit) unit$n, numeric(1)),
    status = group_status,
    message = group_message,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # create the summary text that counts the outcomes of all groups
  summary_text <- paste0(
    n_units,
    " ",
    groups_word,
    ": ",
    sum(group_status == "ok"),
    " ok, ",
    sum(group_status == "warning"),
    " with warnings, ",
    sum(group_status == "failed"),
    " failed."
  )
  # flag the groups with a warning or a failure
  needs_attention <- group_status != "ok"
  # show the summary below a line of dashes with the title Summary, so it is not read as a message of the core function
  if (isTRUE(verbose)) {
    message("\n---- Summary ", strrep("-", 36))
    message(summary_text)
    # list each group that needs attention with its status and message
    for (i in which(needs_attention)) {
      message(
        "  ",
        names(units)[i],
        ": ",
        group_status[i],
        ". ",
        group_message[i]
      )
    }
  }
  # warn with the names of the groups to look at, which also works when not verbose
  if (any(needs_attention)) {
    warning(
      summary_text,
      " See `$groups` for: ",
      .show_some(
        paste0(
          names(units)[needs_attention],
          " (",
          group_status[needs_attention],
          ")"
        ),
        5L
      ),
      ".",
      call. = FALSE
    )
  }
  # return the statistics, the groups table and the plots as a list
  return(list(stats = all_groups_stats, groups = groups_table, plots = plots))
}
