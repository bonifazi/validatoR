# R/validate_prediction.R
# validate_prediction(): validation of predictions against a target (e.g.
# pre-corrected phenotypes), plus its internal helpers.

#' Validate predictions against a target
#'
#' Compares predictions, for example (genomic) EBVs, with the
#' values they are meant to predict (the `target`), such as pre-corrected
#' phenotypes, raw phenotypes, true breeding values, or (genomic) EBVs from a more complete/other evaluation,
#' for a set of validation individuals. Returns the correlation, the regression statistics
#' of the target on the prediction (slope and intercept) and the mean difference.
#' When `h2` is supplied, the correlation is also divided by `sqrt(h2)` to
#' give an accuracy, which is needed when validating for pre-corrected phenotypes.
#' Optionally, standard errors are estimated by bootstrapping over individuals with replacement.
#' Optionally, a scatter plot of the target on the predictions is returned.
#'
#' @param prediction Numeric vector of predictions, plotted on the X axis
#'   (e.g. EBVs from a (genomic) evaluation).
#' @param target Numeric vector of the values the predictions are meant to
#'   predict, plotted on the Y axis. For example pre-corrected phenotypes or true breeding values.
#'   Same length and order as `prediction`.
#' @param h2 Optional heritability, a single number in (0, 1]. When given,
#'   `accuracy = correlation / sqrt(h2)` is also returned. Only meaningful
#'   when `target` is a pre-corrected phenotype and individuals are validated
#'   on their own records; leave as `NULL` (default) otherwise.
#' @param bootstrap Logical. If `TRUE`, standard errors are estimated by
#'   bootstrapping over individuals with replacement. default is `FALSE`.
#' @param n_boot Integer. Number of bootstrap resamples. default is `10000L`.
#' @param ncpus Integer. CPUs used for the bootstrap; `1` runs serially (default).
#' @param plot Logical. If `TRUE`, also return a ggplot2 scatter plot of `target`
#'   against `prediction`. Axes labels are generic and can be relabelled by adding ggplot2 layers, e.g.
#'   `+ ggplot2::labs(x = ..., y = ...)`.
#'
#' @details
#' The statistics regress `target` on `prediction`, so a
#' slope of 1 means no dispersion bias. With `x` the predictions and `y` the
#' target:
#' - `n`: number of pairs used (it has no standard error)
#' - `correlation`: `cor(x, y)`
#' - `slope`: `cov(x, y) / var(x)`, the regression of `y` on `x`
#' - `intercept`: `mean(y) - slope * mean(x)`
#' - `mean_diff`: `mean(x) - mean(y)`
#' - `accuracy`: `correlation / sqrt(h2)`, only when `h2` is given
#'
#' Missing values (`NA`) are not allowed in either vector, because dropping
#' them could silently change which animals are compared; the function stops
#' with an error. Remove the affected animals from both vectors first. A
#' `prediction` or `target` with no variation (all values equal), or fewer than
#' 3 pairs, also gives an error, because the correlation and the slope are not
#' defined.
#'
#' @return A list with `stats`, a data frame with column `value` (and `SE`
#'   when `bootstrap = TRUE`) and one row per statistic, and `plot`, a ggplot
#'   object or `NULL`.
#'
#' @export
#' @examples
#' # `pheno` is a pre-corrected phenotype simulated with h2 = 0.3, so passing
#' # `h2` gives the accuracy of the partial EBVs (about 0.5 for this toy data)
#' res <- validate_prediction(toy_validation$partial, toy_validation$pheno, h2 = 0.3)
#' res$stats
#' # get SE via bootstrapping
#' res_boot <- validate_prediction(toy_validation$partial, toy_validation$pheno,
#'                                 bootstrap = TRUE, n_boot = 200, h2 = 0.3)
#' res_boot$stats
#' # without `h2` (e.g. when comparing against another EBV such as `whole`) no
#' # accuracy is returned
#' validate_prediction(toy_validation$partial, toy_validation$whole)$stats
#' # scatter plot of the target on the prediction (grey line: slope 1,
#' # blue line: fitted regression)
#' res_plot <- validate_prediction(toy_validation$partial, toy_validation$pheno,
#'                                 h2 = 0.3, plot = TRUE)
#' res_plot$plot
validate_prediction <- function(
  prediction,
  target,
  h2 = NULL,
  bootstrap = FALSE,
  n_boot = 10000L,
  ncpus = 1L,
  plot = FALSE
) {
  # 1. input checks
  if (!is.numeric(prediction) || !is.numeric(target)) {
    stop("`prediction` and `target` must be numeric vectors.", call. = FALSE)
  }
  if (length(prediction) != length(target)) {
    stop("`prediction` and `target` must have the same length.", call. = FALSE)
  }
  if (
    !is.null(h2) &&
      !(is.numeric(h2) && length(h2) == 1L && h2 > 0 && h2 <= 1)
  ) {
    stop("`h2` must be a single number in (0, 1].", call. = FALSE)
  }
  if (
    !(is.logical(bootstrap) && length(bootstrap) == 1L && !is.na(bootstrap))
  ) {
    stop("`bootstrap` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!(is.logical(plot) && length(plot) == 1L && !is.na(plot))) {
    stop("`plot` must be TRUE or FALSE.", call. = FALSE)
  }

  # 2. no missing values allowed, then assemble data
  n_na_prediction <- sum(is.na(prediction))
  n_na_target <- sum(is.na(target))
  if (n_na_prediction > 0L || n_na_target > 0L) {
    stop(
      "Missing values are not allowed: `prediction` has ",
      n_na_prediction,
      " and `target` has ",
      n_na_target,
      ". Remove the affected animals ",
      "from both vectors first.",
      call. = FALSE
    )
  }
  data <- data.frame(prediction = prediction, target = target)
  if (nrow(data) < 3L) {
    stop("At least 3 pairs are needed.", call. = FALSE)
  }
  if (length(unique(prediction)) < 2L) {
    stop(
      "`prediction` has no variation (all values are equal), so the ",
      "slope and the correlation are not defined.",
      call. = FALSE
    )
  }
  if (length(unique(target)) < 2L) {
    stop(
      "`target` has no variation (all values are equal), so the ",
      "correlation is not defined.",
      call. = FALSE
    )
  }

  # 3. get the statistics (with and without bootstrapping SEs)
  stats_df <- if (isTRUE(bootstrap)) {
    .run_bootstrap(
      data,
      .general_stats,
      n_boot = n_boot,
      ncpus = ncpus,
      h2 = h2
    )
  } else {
    data.frame(
      value = .general_stats(
        data,
        seq_len(nrow(data)),
        h2 = h2
      )
    )
  }

  # n is fixed, so it has no sampling error
  if (isTRUE(bootstrap)) {
    stats_df["n", "SE"] <- NA_real_
  }

  # 4. (optional) plot
  p <- if (isTRUE(plot)) {
    .plot_general(data, stats_df)
  } else {
    NULL
  }

  return(list(stats = stats_df, plot = p))
}

#' Validation statistics on one (re)sample
#'
#' @param data Data frame with columns `prediction` and `target`.
#' @param indices Row indices of the (re)sample.
#' @param h2 Optional heritability; adds `accuracy` when not `NULL`.
#' @return Named numeric vector.
#' @noRd
.general_stats <- function(data, indices, h2 = NULL) {
  x <- data$prediction[indices]
  y <- data$target[indices]

  slope <- stats::cov(x, y) / stats::var(x)
  out <- c(
    n = length(x),
    correlation = stats::cor(x, y),
    slope = slope,
    intercept = mean(y) - slope * mean(x),
    mean_diff = mean(x) - mean(y)
  )
  if (!is.null(h2)) {
    out["accuracy"] <- out[["correlation"]] / sqrt(h2)
  }
  return(out)
}

#' Scatter plot of the target on the predictions
#'
#' Grey line: slope 1 reference. Blue line: fitted regression of `target` on
#' `prediction`.
#'
#' @param data Data frame with columns `prediction` and `target`.
#' @param stats_df Output of the statistics step, with rows `slope` and
#'   `intercept`.
#' @return A ggplot object.
#' @importFrom ggplot2 .data
#' @noRd
.plot_general <- function(data, stats_df) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$prediction, y = .data$target)
  ) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50") +
    ggplot2::geom_abline(
      slope = stats_df["slope", "value"],
      intercept = stats_df["intercept", "value"],
      colour = "blue"
    ) +
    ggplot2::labs(x = "Prediction", y = "Target") +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1)
  return(p)
}
