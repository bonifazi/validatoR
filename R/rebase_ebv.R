# R/rebase_ebv.R
# rebase_ebv(): express EBVs relative to a base population or to a constant,
# plus its internal helpers.

#' Rebase estimated breeding values
#'
#' Subtracts from the EBVs of all animals the mean EBV of a base population, so
#' that the base population has a mean EBV of 0. This is done for each EBV
#' column of `data`, each with its own corresponding base-population mean.
#' Alternatively, a fixed `constant_value` is subtracted from all EBVs (one per column).
#' Optionally, scatter plots of the rebased EBVs on the original EBVs are
#' returned.
#'
#' @param data A data frame with animal IDs in column 1 and EBVs in the other
#'   columns, one column per trait, all numeric. A data.table or a
#'   tibble also works.
#' @param base_pop Optional data frame with the IDs of the base-population
#'   animals in column 1. Every ID must be present in `data`. Provide either
#'   `base_pop` or `constant_value`, not both. Defaults to `NULL`.
#' @param constant_value Optional numeric vector: the values subtracted from
#'   the EBVs, with one value per EBV column, in the order of the columns of
#'   `data` (the first value is subtracted from the first EBV column, and so
#'   on). It must have exactly as many values as there are EBV columns.
#'   Provide either `base_pop` or `constant_value`, not both. Defaults to `NULL`.
#' @param plot Logical. If `TRUE`, also return a ggplot2 scatter plot of the
#'   rebased EBVs on the original EBVs for each column. Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, report with messages the number of animals
#'   and, per column, what was subtracted (and, with `base_pop`, the mean EBV of
#'   the base population before and after rebasing). Defaults to `FALSE`.
#'
#' @details
#' # Base population
#' For each EBV column, the mean EBV of the base-population animals is
#' subtracted from the EBVs of all animals.
#' Every animal's EBV is shifted by the same amount, so the relative
#' differences between the EBVs of animals do not change. Similarly, the
#' correlations and the regression slopes do not change. Thus, only the mean
#' EBV of each trait shifts, not the relative values. The base population ends
#' with a mean EBV of 0 in every column.
#'
#' # Constant value
#' With `constant_value`, each EBV column has its own value subtracted, in the
#' order of the columns: the first value from the first EBV column, the second
#' from the second, and so on. There must be exactly one value per EBV column;
#' values are never repeated for several columns.
#' Use with care: as no base population is used, the function does not set any group
#' of animals to a mean of 0. A group of animals will have a mean of 0 in a column
#' only when the value subtracted from that column is the mean EBV of the group,
#' for example a base-population mean computed beforehand.
#'
#' # Plot
#' The grey line has slope 1. The blue line is the regression of the rebased
#' EBVs on the original EBVs: its slope is 1 and its intercept is minus the value
#' that was subtracted.
#'
#' # Animals, IDs and errors
#' IDs are matched as text, with numbers written in full (`100000`, never
#' `1e+05`), so `1` and `"1"` are the same animal. The function stops with an
#' error when the IDs in `data` or in `base_pop` are missing or duplicated
#' (a duplicated ID would silently weight the base-population mean), when
#' `base_pop` has no animals or has IDs that are not in `data`, or when any EBV
#' in `data` is missing or infinite (a missing EBV would also leave the
#' base-population mean undefined).
#'
#' @returns
#' A list with two elements, `rebased_ebv` and `plots`.
#'
#' `rebased_ebv` is a data frame with the same columns and row order as `data`,
#' with the rebased EBVs (a plain data frame, also when `data` is a data.table
#' or a tibble).
#'
#' `plots` is `NULL` when `plot = FALSE`. When `plot = TRUE`, it is a named list
#' with one ggplot object per EBV column, so the plot of a column called
#' `partial` is `res$plots$partial`.
#'
#' @export
#' @examples
#' ebv <- toy_validation[, c("id", "partial", "whole")]
#' # the animals of the first cohort are the base population
#' base <- toy_validation[toy_validation$group == "cohort_1", "id", drop = FALSE]
#'
#' res_base <- rebase_ebv(ebv, base_pop = base)
#' rebased <- res_base$rebased_ebv
#' head(rebased)
#'
#' # the base population has a mean of 0 in every column
#' round(colMeans(rebased[rebased$id %in% base$id, c("partial", "whole")]), 8)
#'
#' # rebase against one constant value per column (100 for `partial`, 90 for
#' # `whole`), with a plot for each column
#' res <- rebase_ebv(ebv, constant_value = c(100, 90), plot = TRUE)
#' res$plots$partial
rebase_ebv <- function(
  data,
  base_pop = NULL,
  constant_value = NULL,
  plot = FALSE,
  verbose = FALSE
) {
  # 1. flags and scalar arguments
  # check that flags are a single TRUE or FALSE
  .check_flag(plot, "plot")
  .check_flag(verbose, "verbose")
  # check that one, and only one, of base_pop and constant_value is provided
  if (is.null(base_pop) && is.null(constant_value)) {
    stop(
      "One of `base_pop` and `constant_value` must be provided.",
      call. = FALSE
    )
  }
  if (!is.null(base_pop) && !is.null(constant_value)) {
    stop(
      "`base_pop` and `constant_value` must not both be provided.",
      call. = FALSE
    )
  }
  # check that constant_value is a vector of finite numbers
  if (
    !is.null(constant_value) &&
      !(is.numeric(constant_value) &&
        length(constant_value) >= 1L &&
        all(is.finite(constant_value)))
  ) {
    stop("`constant_value` must be a vector of finite numbers.", call. = FALSE)
  }

  # 2. EBVs as a plain data frame, with the IDs written as text for matching
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame with animal IDs in column 1 and EBVs in ",
      "the other columns.",
      call. = FALSE
    )
  }
  # plain data.frame whatever the class provided (tibble, data.table)
  data <- as.data.frame(data)
  # check for an ID column and at least one EBV column
  if (ncol(data) < 2L) {
    stop("`data` must have at least 2 columns (ID, EBV).", call. = FALSE)
  }
  ebv_cols <- names(data)[-1]
  # check that all EBV columns are numeric
  not_numeric <- ebv_cols[!vapply(data[-1], is.numeric, logical(1))]
  if (length(not_numeric) > 0L) {
    stop(
      "`data` must have numeric EBVs in columns 2 onward; not numeric: ",
      paste(not_numeric, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  # check for missing or infinite EBVs, in any animal
  n_bad <- vapply(
    data[-1],
    function(column) {
      return(sum(!is.finite(column)))
    },
    integer(1)
  )
  if (any(n_bad > 0L)) {
    stop(
      "`data` must not have missing or infinite EBVs: ",
      paste0(
        "`",
        ebv_cols[n_bad > 0L],
        "` has ",
        n_bad[n_bad > 0L],
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }
  # check that constant_value has exactly one value per EBV column
  if (!is.null(constant_value) && length(constant_value) != length(ebv_cols)) {
    stop(
      "`constant_value` must have one number per EBV column (",
      length(ebv_cols),
      "), but it has ",
      length(constant_value),
      ".",
      call. = FALSE
    )
  }
  ids <- .as_checked_ids(data[[1]], "data")

  # 3. define the shift (the value to subtract, one per EBV column)
  if (!is.null(base_pop)) {
    # check that base_pop is a data frame, then read its IDs
    if (!is.data.frame(base_pop)) {
      stop(
        "`base_pop` must be a data frame with the base-population IDs in ",
        "column 1.",
        call. = FALSE
      )
    }
    base_ids <- .as_checked_ids(as.data.frame(base_pop)[[1]], "base_pop")
    # check that base_pop has at least one animal (an empty base gives no mean)
    if (length(base_ids) == 0L) {
      stop("`base_pop` must have at least one animal.", call. = FALSE)
    }
    # check that all base-population IDs are in data
    not_found <- setdiff(base_ids, ids)
    if (length(not_found) > 0L) {
      stop(
        "`base_pop` must only have IDs that are in `data`. Not found (",
        length(not_found),
        "), e.g. ",
        .show_some(not_found),
        ".",
        call. = FALSE
      )
    }
    in_base <- ids %in% base_ids
    # define the shift when rebasing is based on the mean EBV of the base population
    # (one value per EBV column, in the order of the columns of data)
    shift <- vapply(data[in_base, -1, drop = FALSE], mean, numeric(1))
  } else {
    # define the shift when rebasing is based on a constant value
    # (one value per EBV column, in the order of constant_value)
    in_base <- NULL
    shift <- constant_value
  }

  # 4. rebase: subtract the value of each column from all animals
  rebased <- data
  for (j in seq_along(ebv_cols)) {
    rebased[[j + 1L]] <- data[[j + 1L]] - shift[[j]]
  }

  # report what was done
  if (isTRUE(verbose)) {
    if (is.null(in_base)) {
      message(
        "rebase_ebv(): ",
        nrow(data),
        " animals, constant value(s) subtracted."
      )
    } else {
      message(
        "rebase_ebv(): ",
        nrow(data),
        " animals, ",
        sum(in_base),
        " in the base population."
      )
    }
    # report what was subtracted, per EBV column
    for (j in seq_along(ebv_cols)) {
      if (is.null(in_base)) {
        message("`", ebv_cols[j], "`: ", round(shift[[j]], 4), " subtracted.")
      } else {
        message(
          "`",
          ebv_cols[j],
          "`: mean EBV of the base population before rebasing ",
          round(shift[[j]], 4),
          ", after ",
          round(mean(rebased[[j + 1L]][in_base]), 4),
          "."
        )
      }
    }
  }

  # 5. (optional) plots of the rebased on the original EBVs
  plots <- NULL
  if (isTRUE(plot)) {
    plots <- lapply(seq_along(ebv_cols), function(j) {
      return(.plot_rebase(data[[j + 1L]], rebased[[j + 1L]], ebv_cols[j]))
    })
    names(plots) <- ebv_cols
  }
  return(list(rebased_ebv = rebased, plots = plots))
}

#' Scatter plot of the rebased on the original EBVs
#'
#' Grey line: slope 1 reference. Blue line: regression of the rebased on the
#' original EBVs, whose intercept is minus the value that was subtracted.
#'
#' @param original EBVs before rebasing.
#' @param rebased EBVs after rebasing, in the same order.
#' @param title Plot title, the name of the EBV column.
#' @return A ggplot object.
#' @importFrom ggplot2 .data
#' @noRd
.plot_rebase <- function(original, rebased, title) {
  plot_data <- data.frame(original = original, rebased = rebased)
  # regression of the rebased on the original EBVs
  fit <- stats::coef(stats::lm(rebased ~ original, data = plot_data))
  # scatter plot with the slope-1 line and the fitted regression
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$original, y = .data$rebased)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(
      slope = fit[[2]],
      intercept = fit[[1]],
      colour = "blue"
    ) +
    ggplot2::labs(x = "Original EBV", y = "Rebased EBV", title = title) +
    ggplot2::theme_bw()
  return(p)
}
