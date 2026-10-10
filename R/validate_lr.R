# R/validate_lr.R
# validate_lr(): LR method of Legarra and Reverter (2018), comparing EBVs from
# a partial and a whole evaluation, plus its internal helpers.

#' Validate predictions with the LR method
#'
#' Computes the statistics of the LR method (Legarra and Reverter, 2018) by
#' comparing EBVs from a "partial" evaluation with EBVs from a "whole"
#' evaluation, for a group of validation animals (the "focal group").
#' Optionally, standard errors are estimated by bootstrapping over animals, and
#' a scatter plot of the whole EBVs on the partial EBVs is returned.
#'
#' @param partial A data frame with animal IDs in column 1 and EBVs from the
#'   partial evaluation in column 2. Other columns are ignored. A data.table or
#'   a tibble also works.
#' @param whole A data frame with animal IDs in column 1 and EBVs from the whole
#'   evaluation in column 2. Other columns are ignored. A data.table or
#'   a tibble also works.
#' @param val_group Optional data frame with the IDs of the validation animals
#'   in column 1 and, optionally, a group label in column 2 (used by
#'   `plot_subgroups`). Every ID must be present in both `partial` and
#'   `whole`. Defaults to `NULL`: all animals present in both are used.
#' @param var_a Optional single positive number: the additive genetic variance.
#'   Needed for the level bias in genetic standard deviations, for the accuracy
#'   of the partial EBVs, and for `plot_in_gsd`. Defaults to `NULL`.
#' @param average_inbreeding Optional single number in \[0, 1): the average
#'   inbreeding of the validation animals as a coefficient, not a percentage
#'   (`0.05`, not `5`), used for the accuracy of the partial EBVs. Provide
#'   either `average_inbreeding` or `inbreeding`, not both. Defaults to `NULL`.
#' @param inbreeding Optional data frame with animal IDs in column 1 and
#'   inbreeding coefficients (numbers of at least 0, as coefficients and not
#'   percentages) in column 2. It must include every validation animal; other
#'   animals are ignored. Use it instead of `average_inbreeding` when
#'   `bootstrap = TRUE`, so that the average inbreeding is recomputed in every
#'   resample. Defaults to `NULL`.
#' @param plot Logical. If `TRUE`, also return a ggplot2 scatter plot of the
#'   whole EBVs on the partial EBVs. Defaults to `FALSE`.
#' @param plot_verbose Logical. If `TRUE` (and `plot = TRUE`), add the number of
#'   animals and the main statistics to the plot. The level bias is shown in
#'   genetic standard deviations when `plot_in_gsd = TRUE`, and in the units of
#'   the EBVs otherwise. Defaults to `FALSE`.
#' @param plot_subgroups Logical. If `TRUE` (and `plot = TRUE`), colour the
#'   points by the group label in column 2 of `val_group`, which helps to spot
#'   heterogeneous groups. Defaults to `FALSE`.
#' @param plot_in_gsd Logical. If `TRUE` (and `plot = TRUE`), both axes are
#'   divided by the genetic standard deviation, `sqrt(var_a)`, so that the plot
#'   is in genetic standard deviations. It needs `var_a`. Defaults to `FALSE`:
#'   the axes are in the units of the EBVs.
#' @param bootstrap Logical. If `TRUE`, standard errors are estimated by
#'   bootstrapping over animals with replacement. Defaults to `FALSE`.
#' @param n_boot A single whole number of at least 2: the number of bootstrap
#'   resamples. Defaults to `10000`.
#' @param ncpus A single whole number of at least 1: the number of CPUs used
#'   for the bootstrap. Defaults to `1`, which runs serially.
#' @param verbose Logical. If `TRUE`, report the number of validation animals
#'   and the bootstrap settings. Defaults to `FALSE`.
#'
#' @details
#' # The LR method
#' The partial evaluation leaves out the records (or, more in general, a source of
#' information) for the validation animals (for example the youngest generations),
#' while the whole evaluation instead uses all the records (or, more in general,
#' all information or more information than partial).
#' The LR method compares the EBVs of the validation animals in the two
#' evaluations to estimate the level bias, the dispersion bias, and accuracies
#' (Legarra and Reverter, 2018).
#'
#' # Standard errors
#' With `bootstrap = TRUE`, the validation animals are resampled with
#' replacement `n_boot` times and all statistics are recomputed in each
#' resample. The `SE` column is the standard deviation of the resampled values.
#' The average inbreeding is recomputed in every resample from `inbreeding`,
#' which is why `average_inbreeding` cannot be used with the bootstrap. `n` has
#' no `SE`, and `var_a` has an `SE` of 0 because it is a constant.
#'
#' # Plot
#' The grey line has slope 1. The blue line is the regression of the whole on
#' the partial EBVs, so its slope is the dispersion bias; the level bias is not
#' its intercept.
#'
#' # Animals, IDs and errors
#' IDs are matched as text, with numbers written in full (`100000`, never
#' `1e+05`), so `1` and `"1"`, or `100000` and `"100000"`, are the same animal.
#' Without `val_group`, animals found in only one of `partial` and `whole` are
#' left out, with a message. The function stops with an error when validation
#' animals have missing EBVs or inbreeding coefficients, when there are fewer
#' than 3 validation animals, or when the EBVs have no variation.
#'
#' @returns
#' A list with two elements, `stats` and `plot`.
#'
#' `stats` is a data frame with one row per statistic and a column `value`.
#' With `bootstrap = TRUE` it also has a column `SE`, the bootstrap standard
#' error. It keeps the full precision and prints with a few significant digits
#' (see [print.validatoR_stats()]). The rows are, with `p` being the partial and
#' `w` the whole EBVs of the validation animals:
#'
#' * `n`: the number of validation animals.
#' * `level_bias`: `mean(p) - mean(w)`. 0 means no level bias.
#' * `level_bias_in_GSD`: `level_bias / sqrt(var_a)`. Only when `var_a` is
#'   provided.
#' * `dispersion_bias`: `cov(p, w) / var(p)`, the regression of `w` on `p`.
#'   1 means no dispersion bias, values below 1 mean the partial EBVs are
#'   over-dispersed, and values above 1 mean they are under-dispersed.
#' * `accuracy_partial`: `sqrt(cov(p, w) / ((1 - F) * var_a))`, with `F` being the
#'   average inbreeding of the validation animals. Only when `var_a` and
#'   `average_inbreeding` or `inbreeding` are provided. It is `NaN`, with a
#'   warning, when `cov(p, w)` is negative or the average inbreeding is 1 or
#'   more.
#' * `average_inbreeding` and `var_a`: the values used for `accuracy_partial`,
#'   returned with it so that tables from several groups can be compared.
#' * `rho`: `cor(p, w)`, the ratio of the accuracies of the partial and the
#'   whole EBVs.
#' * `inc_acc`: `1 / rho`, the increase in accuracy obtained with the whole
#'   evaluation, as a ratio to the accuracy of the partial evaluation (see Bonifazi
#'   et al., 2022). 1 means no gain. For the increase relative to the partial
#'   evaluation as a percentage, use `(inc_acc - 1) * 100`. For example, a `rho` of 0.80 gives
#'   an `inc_acc` of 1.25, which corresponds to an increase of 25%.
#'
#' `plot` is a ggplot object when `plot = TRUE`, and `NULL` otherwise.
#'
#' @seealso [validate_prediction()] to validate predictions against a target
#'   that is not a whole evaluation, such as phenotypes.
#'
#' @references
#' Legarra, A. and Reverter, A. (2018). Semi-parametric estimates of
#' population accuracy and bias of predictions of breeding values and future
#' phenotypes using the LR method. Genetics Selection Evolution.
#' \doi{10.1186/s12711-018-0426-6}
#'
#' Bonifazi, R., Calus, M. P. L., ten Napel, J., Veerkamp, R. F., Michenet, A.,
#' Savoia, S., Cromie, A. and Vandenplas, J. (2022). International single-step
#' SNPBLUP beef cattle evaluations for Limousin weaning weight. Genetics
#' Selection Evolution 54:57. \doi{10.1186/s12711-022-00748-0}
#'
#' @export
#' @examples
#' p <- toy_validation[, c("id", "partial")]
#' w <- toy_validation[, c("id", "whole")]
#' inb <- toy_validation[, c("id", "inbreeding")]
#'
#' # all animals, with the accuracy of the partial EBVs
#' res <- validate_lr(p, w, var_a = 300, inbreeding = inb)
#' res$stats
#'
#' # increase in accuracy relative to the partial evaluation, as a
#' # percentage (equal to 17.6 for this toy data)
#' (res$stats["inc_acc", "value"] - 1) * 100
#'
#' # two cohorts as the validation group, plot coloured by cohort, with the
#' # axes in genetic standard deviations
#' vg <- toy_validation[toy_validation$group %in% c("cohort_1", "cohort_2"),
#'                      c("id", "group")]
#' res_vg <- validate_lr(p, w, val_group = vg, var_a = 300, plot = TRUE,
#'                       plot_subgroups = TRUE, plot_verbose = TRUE,
#'                       plot_in_gsd = TRUE)
#' res_vg$plot
#'
#' # bootstrap SEs (few resamples, to keep the example fast)
#' validate_lr(p, w, var_a = 300, inbreeding = inb,
#'             bootstrap = TRUE, n_boot = 50)$stats
validate_lr <- function(
  partial,
  whole,
  val_group = NULL,
  var_a = NULL,
  average_inbreeding = NULL,
  inbreeding = NULL,
  plot = FALSE,
  plot_verbose = FALSE,
  plot_subgroups = FALSE,
  plot_in_gsd = FALSE,
  bootstrap = FALSE,
  n_boot = 10000L,
  ncpus = 1L,
  verbose = FALSE
) {
  # 1. flags and scalar arguments
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

  # 2. EBVs as (id, value) data frames, merged by ID
  p_df <- .lr_input(partial, "partial")
  w_df <- .lr_input(whole, "whole")
  # keep the animals found in both evaluations
  data <- merge(p_df, w_df, by = "id")
  # check that partial and whole share some animals
  if (nrow(data) == 0L) {
    stop(
      "`partial` and `whole` have no animal IDs in common. Check that the ",
      "IDs are in column 1 of both and are the same kind of ID. IDs found, ",
      "partial: ",
      .show_some(p_df$id),
      "; whole: ",
      .show_some(w_df$id),
      ".",
      call. = FALSE
    )
  }

  # 3. validation animals
  if (is.null(val_group)) {
    # count the animals left out of each evaluation
    n_only_partial <- nrow(p_df) - nrow(data)
    n_only_whole <- nrow(w_df) - nrow(data)
    # report the animals left out of either partial or whole
    if (n_only_partial > 0L) {
      message(
        n_only_partial,
        " animal(s) in `partial` are not in `whole` and were left out."
      )
    }
    if (n_only_whole > 0L) {
      message(
        n_only_whole,
        " animal(s) in `whole` are not in `partial` and were left out."
      )
    }
  } else {
    # check that val_group is a data frame
    if (!is.data.frame(val_group)) {
      stop(
        "`val_group` must be a data frame with the validation IDs in ",
        "column 1.",
        call. = FALSE
      )
    }
    # plain data.frame whatever the class provided (tibble, data.table)
    val_group <- as.data.frame(val_group)
    # convert the IDs to text, so that numeric and text IDs match
    val_ids <- .as_id(val_group[[1]])
    # check for missing or duplicated IDs in val_group
    if (anyNA(val_ids) || anyDuplicated(val_ids) > 0L) {
      stop(
        "`val_group` IDs must be unique and not missing.",
        call. = FALSE
      )
    }
    # check that all val_group IDs are in both partial and whole
    not_found <- setdiff(val_ids, data$id)
    if (length(not_found) > 0L) {
      stop(
        "`val_group` must only have IDs that are in both `partial` and ",
        "`whole`. Not found (",
        length(not_found),
        "), e.g. ",
        .show_some(not_found),
        ".",
        call. = FALSE
      )
    }
    # keep only the validation animals
    data <- data[data$id %in% val_ids, , drop = FALSE]
  }
  # check plotting arguments
  if (isTRUE(plot_subgroups) && (is.null(val_group) || ncol(val_group) < 2L)) {
    stop(
      "`val_group` must have a group label in column 2 when ",
      "`plot_subgroups = TRUE`.",
      call. = FALSE
    )
  }

  # 4. inbreeding is added as a column to the data, so resampling keeps it aligned
  if (!is.null(inbreeding)) {
    # read inbreeding as an (id, value) data frame
    inb_df <- .lr_input(inbreeding, "inbreeding")
    # check that every validation animal has an inbreeding coefficient
    no_inb <- setdiff(data$id, inb_df$id)
    if (length(no_inb) > 0L) {
      stop(
        "`inbreeding` must include every validation animal. Missing (",
        length(no_inb),
        "), e.g. ",
        .show_some(no_inb),
        ".",
        call. = FALSE
      )
    }
    # add inbreeding as a column, then check that it is not missing or negative
    data <- merge(data, inb_df, by = "id")
    if (anyNA(data$inbreeding) || any(data$inbreeding < 0)) {
      stop(
        "`inbreeding` values of the validation animals must be ",
        "at least 0 and not missing.",
        call. = FALSE
      )
    }
  }

  # 5. checks on the validation animals
  # check there are no missing EBVs (partial or whole)
  n_na_partial <- sum(is.na(data$partial))
  n_na_whole <- sum(is.na(data$whole))
  if (n_na_partial > 0L || n_na_whole > 0L) {
    stop(
      "Missing EBVs are not allowed for validation animals: `partial` has ",
      n_na_partial,
      " and `whole` has ",
      n_na_whole,
      ".",
      call. = FALSE
    )
  }
  # check that there are enough animals
  if (nrow(data) < 3L) {
    stop("At least 3 validation animals are needed.", call. = FALSE)
  }
  # check that both sets of EBVs vary
  if (length(unique(data$partial)) < 2L || length(unique(data$whole)) < 2L) {
    stop(
      "The `partial` and `whole` EBVs of the validation animals must vary ",
      "(not all equal), otherwise dispersion and rho are not defined.",
      call. = FALSE
    )
  }
  if (isTRUE(verbose)) {
    message("validate_lr(): ", nrow(data), " validation animals.")
  }

  # 6. statistics, with or without bootstrap SEs
  if (isTRUE(bootstrap)) {
    if (isTRUE(verbose)) {
      message(
        "Bootstrapping: ",
        n_boot,
        " resamples on ",
        ncpus,
        " CPU(s)."
      )
    }
    # bootstrap: SEs from resampling the validation animals
    stats_df <- .run_bootstrap(
      data,
      .lr_stats,
      n_boot = n_boot,
      ncpus = ncpus,
      var_a = var_a,
      average_inbreeding = average_inbreeding
    )
    stats_df["n", "SE"] <- NA_real_ # n (a fixed value) has no sampling error
  } else {
    # stats without bootstrap (no SEs)
    stats_df <- data.frame(
      value = .lr_stats(
        data,
        seq_len(nrow(data)),
        var_a = var_a,
        average_inbreeding = average_inbreeding
      )
    )
  }

  # 7. give a warning once, on the observed value, when the accuracy is not defined
  if (
    "accuracy_partial" %in%
      rownames(stats_df) &&
      is.nan(stats_df["accuracy_partial", "value"])
  ) {
    warning(
      "`accuracy_partial` is NaN because cov(partial, whole) is negative ",
      "or the average inbreeding is 1 or more.",
      call. = FALSE
    )
  }

  # 8. (optional) plot
  p <- if (isTRUE(plot)) {
    # add the group label by ID
    plot_data <- data
    group <- NULL
    if (isTRUE(plot_subgroups)) {
      labels <- data.frame(
        id = .as_id(val_group[[1]]),
        group = val_group[[2]]
      )
      plot_data <- merge(plot_data, labels, by = "id")
      group <- plot_data$group
    }
    .plot_lr(
      plot_data,
      stats_df,
      in_gsd = plot_in_gsd,
      var_a = var_a,
      verbose = plot_verbose,
      group = group
    )
  } else {
    NULL
  }

  return(list(stats = .new_stats(stats_df), plot = p))
}

#' Check the flags and scalar arguments of the LR validation
#'
#' The checks of `validate_lr()` that do not need the data. They are in their
#' own function so that `validate_lr_by_group()` runs them once, before the
#' groups, and a mistake is not reported as a failure of every group.
#'
#' @inheritParams validate_lr
#' @return `NULL`, invisibly. Stops with a message when an argument is wrong.
#' @noRd
.check_lr_arguments <- function(
  var_a,
  average_inbreeding,
  inbreeding,
  plot,
  plot_verbose,
  plot_subgroups,
  plot_in_gsd,
  bootstrap,
  n_boot,
  ncpus,
  verbose
) {
  .check_flag(plot, "plot")
  .check_flag(plot_verbose, "plot_verbose")
  .check_flag(plot_subgroups, "plot_subgroups")
  .check_flag(plot_in_gsd, "plot_in_gsd")
  .check_flag(bootstrap, "bootstrap")
  .check_flag(verbose, "verbose")
  # check the bootstrap settings
  .check_n_boot(n_boot)
  .check_ncpus(ncpus)

  # check for var_a being a single positive number
  if (!is.null(var_a)) {
    .check_positive_number(var_a, "var_a")
  }
  # check that plot_in_gsd = TRUE comes with var_a
  if (isTRUE(plot_in_gsd) && is.null(var_a)) {
    stop(
      "`var_a` must be provided when `plot_in_gsd = TRUE`.",
      call. = FALSE
    )
  }
  # check for average_inbreeding being a single number in [0, 1)
  if (
    !is.null(average_inbreeding) &&
      !(is.numeric(average_inbreeding) &&
        length(average_inbreeding) == 1L &&
        !is.na(average_inbreeding) &&
        average_inbreeding >= 0 &&
        average_inbreeding < 1)
  ) {
    stop(
      "`average_inbreeding` must be a single number in [0, 1).",
      call. = FALSE
    )
  }
  # check that either average_inbreeding or inbreeding is provided, not both
  if (!is.null(average_inbreeding) && !is.null(inbreeding)) {
    stop(
      "`average_inbreeding` and `inbreeding` must not both be provided.",
      call. = FALSE
    )
  }
  # check that inbreeding is provided with bootstrap = TRUE instead of average_inbreeding
  if (isTRUE(bootstrap) && !is.null(average_inbreeding)) {
    stop(
      "`inbreeding` (one value per animal) must be provided instead of ",
      "`average_inbreeding` when `bootstrap = TRUE`, so that the average ",
      "inbreeding is recomputed in every resample.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

#' Turn an ID + value input into a two-column data frame
#'
#' Accepts data.frame, data.table or tibble; keeps column 1 (IDs, as text) and
#' column 2 (values), named `id` and `arg`. Missing or duplicated IDs are
#' checked here. Missing values in column 2 are not checked here; they are
#' checked later, and only for the validation animals.
#'
#' @param x The user input.
#' @param arg Argument name, used for the value column and in messages.
#' @return Data frame with columns `id` and `<arg>`.
#' @noRd
.lr_input <- function(x, arg) {
  # check that the input is a data frame
  if (!is.data.frame(x)) {
    stop(
      "`",
      arg,
      "` must be a data frame with IDs in column 1 and values in ",
      "column 2.",
      call. = FALSE
    )
  }
  x <- as.data.frame(x) # data.table / tibble: x[[i]] then returns a vector
  # check for an ID column and a value column
  if (ncol(x) < 2L) {
    stop(
      "`",
      arg,
      "` must have at least 2 columns (ID, value).",
      call. = FALSE
    )
  }
  # check that the values are numeric
  if (!is.numeric(x[[2]])) {
    stop("`", arg, "` must have numeric values in column 2.", call. = FALSE)
  }
  # IDs as text (numeric and text IDs match), stopping when missing or duplicated
  ids <- .as_checked_ids(x[[1]], arg)
  # two columns: id, and the values named after the argument
  out <- data.frame(id = ids, value = x[[2]], stringsAsFactors = FALSE)
  names(out)[2] <- arg
  return(out)
}

#' LR statistics on one (re)sample
#'
#' @param data Data frame with columns `partial`, `whole` and, optionally,
#'   `inbreeding`.
#' @param indices Row indices of the (re)sample.
#' @param var_a Optional additive genetic variance.
#' @param average_inbreeding Optional average inbreeding, used only when `data`
#'   has no `inbreeding` column.
#' @return Named numeric vector.
#' @noRd
.lr_stats <- function(
  data,
  indices,
  var_a = NULL,
  average_inbreeding = NULL
) {
  # EBVs of the (re)sampled animals
  p <- data$partial[indices]
  w <- data$whole[indices]
  cov_pw <- stats::cov(p, w)

  # level bias, also expressed in genetic standard deviations when var_a is provided
  out <- c(n = length(p), level_bias = mean(p) - mean(w))
  if (!is.null(var_a)) {
    out["level_bias_in_GSD"] <- out[["level_bias"]] / sqrt(var_a)
  }
  # dispersion: slope of the regression of whole on partial
  out["dispersion_bias"] <- cov_pw / stats::var(p)

  # average inbreeding of the (resampled) animals when it is a column
  avg_inbreeding <- if ("inbreeding" %in% names(data)) {
    mean(data$inbreeding[indices])
  } else {
    average_inbreeding
  }
  if (!is.null(var_a) && !is.null(avg_inbreeding)) {
    ratio <- cov_pw / ((1 - avg_inbreeding) * var_a)
    # set ratio to NaN for edge cases (negative covariance or average inbreeding >= 1)
    # avoid sqrt()'s warning, which would otherwise be triggered on every resample
    out["accuracy_partial"] <- if (is.finite(ratio) && ratio >= 0) {
      sqrt(ratio)
    } else {
      NaN
    }
    out["average_inbreeding"] <- avg_inbreeding
    out["var_a"] <- var_a
  }

  # correlation between partial and whole, and its inverse
  out["rho"] <- stats::cor(p, w)
  out["inc_acc"] <- 1 / out[["rho"]]
  return(out)
}

#' Scatter plot of the whole on the partial EBVs
#'
#' Grey line: slope 1 reference. Blue line: regression of whole on partial,
#' whose slope is the dispersion bias (the level bias is not the intercept).
#'
#' @param data Data frame with columns `partial` and `whole`.
#' @param stats_df Output of the statistics step.
#' @param in_gsd Logical. If `TRUE`, both axes are divided by `sqrt(var_a)`
#'   (genetic standard deviations) and the caption gives the level bias in them.
#' @param var_a Additive genetic variance; used only when `in_gsd = TRUE`.
#' @param verbose Add the number of animals and the main statistics.
#' @param group Optional group labels, aligned with the rows of `data`.
#' @return A ggplot object.
#' @noRd
.plot_lr <- function(
  data,
  stats_df,
  in_gsd = FALSE,
  var_a = NULL,
  verbose = FALSE,
  group = NULL
) {
  # axes in EBV units, or divided by the genetic standard deviation
  axis_scale <- ifelse(isTRUE(in_gsd), sqrt(var_a), 1)
  unit <- ifelse(isTRUE(in_gsd), " (GSD)", "")
  plot_data <- data.frame(
    partial = data$partial / axis_scale,
    whole = data$whole / axis_scale
  )

  # regression line on the original EBV scale, then the intercept is divided
  # by the axis scale (the slope does not depend on the scale)
  slope <- stats_df["dispersion_bias", "value"]
  intercept <- (mean(data$whole) - slope * mean(data$partial)) / axis_scale

  # colour the points by group when labels are provided
  if (is.null(group)) {
    mapping <- ggplot2::aes(x = .data$partial, y = .data$whole)
  } else {
    plot_data$group <- as.factor(group)
    mapping <- ggplot2::aes(
      x = .data$partial,
      y = .data$whole,
      colour = .data$group
    )
  }

  # points, reference line and regression line
  p <- ggplot2::ggplot(plot_data, mapping) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(
      slope = slope,
      intercept = intercept,
      colour = "blue"
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(
      x = paste0("EBV partial", unit),
      y = paste0("EBV whole", unit)
    ) +
    ggplot2::theme_bw()

  # the colour label only exists when there is a colour aesthetic
  if (!is.null(group)) {
    p <- p + ggplot2::labs(colour = "Group")
  }

  if (isTRUE(verbose)) {
    # caption: level bias (in GSD if requested), dispersion and rho
    lb <- if (isTRUE(in_gsd)) {
      paste0(
        "level bias (GSD): ",
        round(stats_df["level_bias_in_GSD", "value"], 3)
      )
    } else {
      paste0("level bias: ", round(stats_df["level_bias", "value"], 3))
    }
    p <- p +
      ggplot2::labs(
        subtitle = paste0("N. animals: ", nrow(plot_data)),
        caption = paste0(
          lb,
          "; dispersion: ",
          round(stats_df["dispersion_bias", "value"], 3),
          "; rho: ",
          round(stats_df["rho", "value"], 3)
        )
      )
  }
  return(p)
}
