#' Validate EBVs with the LR method for several groups
#'
#' Runs [validate_lr()] on each group of animals and collects the statistics
#' and the plots in one result. A group that fails never stops the others.
#'
#' @inheritParams validate_lr
#' @param groups Data frame with the animal IDs in column 1 and one column per
#'   group. The column name is the name of the group. A column is either
#'   logical (`TRUE` for the animals in the group, `FALSE` or `NA` for the
#'   rest) or text (a label for the animals in the group, `NA` for the rest).
#'   Text columns that only holds "TRUE" and "FALSE" is read as logical.
#'   An animal can be in several groups by being labelled in different columns.
#' @param split_labels Logical. If `TRUE`, each label of a text column is also
#'   run as a group of its own, named `column: label`, next to the group of the
#'   whole column (see the example under Groups in the details). Defaults to
#'   `FALSE`.
#' @param include_all Logical. If `TRUE`, the group `"all"` is also run, with
#'   all animals found in both `partial` and `whole`. Only when `TRUE`, no
#'   column of `groups` can be named `"all"`. Defaults to `TRUE`.
#' @param plot Logical. If `TRUE`, also build a ggplot2 scatter plot of the
#'   whole EBVs on the partial EBVs for each group. Defaults to `FALSE`.
#' @param plot_subgroups Logical. If `TRUE`, the points of a group from a text
#'   column are coloured by its labels in the plot. At least one group column
#'   must be text. Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, name each group as it runs, show its
#'   messages, warnings and errors, and show a summary at the end. Defaults to
#'   `TRUE`.
#' @returns A list with three elements:
#' * `stats`: a data frame in long format, one row for each group and
#'   statistic, with the columns `group`, `statistic`, `value` and `SE` (with
#'   `SE` being `NA` unless `bootstrap = TRUE`). A group that failed has no rows.
#' * `groups`: a data frame with one row for each group, with the columns
#'   `group`, `n_provided` (IDs listed in the group), `n` (animals used, i.e. animals being found
#'   in both evaluations), `status` ("ok", "warning" or "failed") and
#'   `message` (the associated error or the warnings, empty when the status is "ok").
#' * `plots`: a named list with one ggplot for each group. It is empty unless
#'   `plot = TRUE`, and has no entry for a group that failed.
#'
#' @details
#' # Groups
#' Each group is validated on its own with [validate_lr()], so the statistics
#' are those described there. A text column keeps its labels for the plot only:
#' the statistics of the group use all its animals together. To get statistics
#' for each label, use `split_labels = TRUE`. For example, a text column
#' `genotyped` with the labels `male` and `female` is run as the group
#' `genotyped` (all its animals together). With `split_labels = TRUE`, it is
#' also run as the groups `genotyped: male` and `genotyped: female`.
#'
#' # The group "all"
#' With `include_all = TRUE`, the first group is `"all"`: every animal found in
#' both `partial` and `whole`, whether or not it is in any group column. Its
#' statistics are those of a plain `validate_lr()` call. Because `"all"` is the
#' name of this group, a column of `groups` cannot be named `"all"` while
#' `include_all = TRUE`. Set `include_all = FALSE` when the validation of all
#' animals together (the group `"all"`) is not wanted, for example to save time
#' with the bootstrap.
#'
#' # Errors and warnings
#' A group that stops with an error gets the status "failed" and its message
#' is stored in `groups`. Other groups will still run. A group that finishes with
#' warnings gets "warning", keeps its statistics, and its warnings are stored in
#' `message`.
#' When any group needs attention from the user, one warning at the end names
#' these groups.
#'
#' # Variance and inbreeding
#' One variance `var_a` and one set of inbreeding values are used for all groups.
#'
#' # IDs
#' IDs are compared as text, so numeric and text IDs match. IDs in a group that
#' are not in both `partial` and `whole` are counted in `n_provided` but not
#' used. The IDs of `groups` must be unique and not missing.
#'
#' @examples
#' p <- toy_validation[, c("id", "partial")]
#' w <- toy_validation[, c("id", "whole")]
#' # one logical column for two cohorts, and a text column with all cohorts
#' groups <- data.frame(
#'   id = toy_validation$id,
#'   cohort_1 = toy_validation$group == "cohort_1",
#'   cohort_2 = toy_validation$group == "cohort_2",
#'   cohort = toy_validation$group
#' )
#' res <- validate_lr_by_group(p, w, groups, var_a = 300, verbose = FALSE)
#' res$groups
#' res$stats[res$stats$statistic == "rho", ]
#'
#' # one plot for each group; the group "cohort" is coloured by its labels
#' res_plot <- validate_lr_by_group(
#'   p, w, groups, var_a = 300, plot = TRUE, plot_subgroups = TRUE,
#'   verbose = FALSE
#' )
#' res_plot$plots$cohort
#' @export
validate_lr_by_group <- function(
  partial,
  whole,
  groups,
  var_a = NULL,
  average_inbreeding = NULL,
  inbreeding = NULL,
  split_labels = FALSE,
  include_all = TRUE,
  plot = FALSE,
  plot_verbose = FALSE,
  plot_subgroups = FALSE,
  plot_in_gsd = FALSE,
  bootstrap = FALSE,
  n_boot = 10000L,
  ncpus = 1L,
  verbose = TRUE
) {
  # 1. check flags and arguments
  .check_flag(split_labels, "split_labels")
  .check_flag(include_all, "include_all")
  # check the arguments that every group passes on to validate_lr(), once
  # this avoids that a mistake is not reported as the failure of every group
  .check_lr_arguments(
    var_a = var_a,
    average_inbreeding = average_inbreeding,
    inbreeding = inbreeding,
    plot = plot,
    plot_verbose = plot_verbose,
    plot_subgroups = plot_subgroups,
    plot_in_gsd = plot_in_gsd,
    bootstrap = bootstrap,
    n_boot = n_boot,
    ncpus = ncpus,
    verbose = verbose
  )

  # 2. animals found in both evaluations
  p_df <- .lr_input(partial, "partial")
  w_df <- .lr_input(whole, "whole")
  # find the IDs that are in both evaluations
  shared_ids <- intersect(p_df$id, w_df$id)
  # check that partial and whole share some animals
  if (length(shared_ids) == 0L) {
    stop(
      "`partial` and `whole` have no animal IDs in common. Check that the ",
      "IDs are in column 1 of both and are the same kind of ID.",
      call. = FALSE
    )
  }

  # 3. groups
  # read the group columns into one unit per group
  units <- .read_groups(groups, shared_ids, split_labels = split_labels)
  # check that no group column is called "all", which is the name of the group with all animals
  if (isTRUE(include_all) && "all" %in% names(units)) {
    stop(
      "`groups` must not have a column named \"all\" when ",
      "`include_all = TRUE`.",
      call. = FALSE
    )
  }
  # find the groups that have labels, which are the ones from text columns
  has_labels <- vapply(
    units,
    function(unit) "label" %in% names(unit$members),
    logical(1)
  )
  # check that plot_subgroups has a text column to take the labels from
  if (isTRUE(plot_subgroups) && !any(has_labels)) {
    stop(
      "`groups` must have at least one text column when ",
      "`plot_subgroups = TRUE`.",
      call. = FALSE
    )
  }
  # add the group "all" in front, where no members means every animal found in both evaluations
  if (isTRUE(include_all)) {
    all_unit <- list(
      all = list(
        members = NULL,
        n_provided = length(shared_ids),
        n = length(shared_ids)
      )
    )
    units <- c(all_unit, units)
  }

  # 4. validation of each group
  # create the function that runs validate_lr() on one group
  run_one <- function(unit) {
    return(
      validate_lr(
        partial,
        whole,
        val_group = unit$members,
        var_a = var_a,
        average_inbreeding = average_inbreeding,
        inbreeding = inbreeding,
        plot = plot,
        plot_verbose = plot_verbose,
        plot_subgroups = isTRUE(plot_subgroups) &&
          "label" %in% names(unit$members),
        plot_in_gsd = plot_in_gsd,
        bootstrap = bootstrap,
        n_boot = n_boot,
        ncpus = ncpus,
        verbose = verbose
      )
    )
  }

  # run validate_lr() on each group and return the statistics, the groups table and the plots
  return(.by_group(
    units,
    run_one,
    verbose = verbose,
    core_name = "validate_lr()"
  ))
}
