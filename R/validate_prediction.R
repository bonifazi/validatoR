# R/validate_prediction.R
# validate_prediction(): validation of predictions against a target (e.g.
# pre-corrected phenotypes), plus its internal helpers.

#' Validate predictions against a target
#'
#' Compares predictions, for example (genomic) EBVs, with the values they are
#' meant to predict (the `target`), for a set of validation animals. The target
#' can be pre-corrected or raw phenotypes, true breeding values, or (genomic)
#' EBVs from a more complete or different evaluation. Returns the correlation,
#' the regression of the target on the prediction (slope and intercept), the
#' mean difference, and the mean squared error (MSE) and its square root (RMSE).
#' Optionally, standard errors are estimated by bootstrapping over animals, and
#' a scatter plot of the target on the predictions is returned.
#'
#' @param prediction A numeric vector of predictions, for example EBVs from a
#'   (genomic) evaluation. It is plotted on the X axis.
#' @param target A numeric vector of the values the predictions are meant to
#'   predict, for example pre-corrected phenotypes or true breeding values. It
#'   has the same length and order as `prediction`, and is plotted on the Y
#'   axis.
#' @param h2 Optional single number in (0, 1]: the heritability. When provided,
#'   `accuracy = correlation / sqrt(h2)` is also returned. It is only
#'   meaningful when `target` is a pre-corrected phenotype and the animals are
#'   validated on their own records. Defaults to `NULL`.
#' @param var_a Optional single positive number: the additive genetic variance.
#'   When provided, the root mean squared error in genetic standard deviations,
#'   `rmse_in_GSD`, is also returned. Defaults to `NULL`.
#' @param plot Logical. If `TRUE`, also return a ggplot2 scatter plot of
#'   `target` on `prediction`, with a grey line of slope 1 and a blue line of
#'   the fitted regression. The axis labels are generic; relabel them by adding
#'   `+ ggplot2::labs(x = ..., y = ...)`. Defaults to `FALSE`.
#' @inheritParams validate_lr
#'
#' @details
#' # Regression of the target on the prediction
#' The slope and the intercept come from regressing `target` on `prediction`,
#' so a slope of 1 means no dispersion bias. `mean_diff` is the level bias: the
#' mean of the predictions minus the mean of the target.
#'
#' # Mean squared error
#' `mse` and `rmse` measure the distance between each prediction and its
#' target, so they include the level bias and the dispersion: `mse` equals
#' `mean_diff^2` plus the variance of `prediction - target` (with `n` as the
#' divisor).
#' Note: They are only meaningful when both vectors are on the same scale,
#' for example a partial and a whole EBV, or EBVs and true breeding values, and
#' are misleading for EBVs against a pre-corrected phenotype.
#'
#' `rmse` is the error of the predictions as they are, `prediction - target`.
#' Note: it is not the residual error around the fitted regression line (the
#' textbook regression formula, with the fitted value in place of the
#' prediction).
#'
#' # Accuracy
#' `accuracy` divides the correlation by `sqrt(h2)`. It is valid for individual
#' validation with a pre-corrected phenotype as `target`.
#'
#' # Missing values and errors
#' Missing values (`NA`) are not allowed in either vector, because dropping
#' them could silently change which animals are compared; the function stops
#' with an error. Remove the affected animals from both vectors first. A
#' `prediction` or `target` with no variation (all values equal), or fewer than
#' 3 pairs, also gives an error, because the correlation and the slope are not
#' defined.
#'
#' @returns
#' A list with two elements, `stats` and `plot`.
#'
#' `stats` is a data frame with one row per statistic and a column `value`.
#' With `bootstrap = TRUE` it also has a column `SE`, the bootstrap standard
#' error (`NA` for `n`). The rows are, with `x` being the predictions and
#' `y` being the target:
#'
#' * `n`: the number of pairs used.
#' * `correlation`: `cor(x, y)`.
#' * `slope`: `cov(x, y) / var(x)`, the regression of `y` on `x`. 1 means no
#'   dispersion bias.
#' * `intercept`: `mean(y) - slope * mean(x)`.
#' * `mean_diff`: `mean(x) - mean(y)`.
#' * `mse`: `mean((x - y)^2)`.
#' * `rmse`: `sqrt(mse)`.
#' * `rmse_in_GSD`: `rmse / sqrt(var_a)`. Only when `var_a` is provided.
#' * `accuracy`: `correlation / sqrt(h2)`. Only when `h2` is provided.
#'
#' `plot` is a ggplot object when `plot = TRUE`, and `NULL` otherwise.
#'
#' @seealso [validate_lr()] to compare a partial with a whole evaluation using
#'   the LR method.
#'
#' @export
#' @examples
#' # `pheno` is a pre-corrected phenotype simulated with h2 = 0.3, so passing
#' # `h2` gives the accuracy of the partial EBVs (about 0.5 for this toy data)
#' res <- validate_prediction(
#'   toy_validation$partial,
#'   toy_validation$pheno,
#'   h2 = 0.3
#' )
#' res$stats
#'
#' # without `h2` (e.g. when comparing against another EBV such as `whole`) no
#' # accuracy is returned; `var_a` adds the RMSE in genetic standard deviations
#' validate_prediction(
#'   toy_validation$partial,
#'   toy_validation$whole,
#'   var_a = 300
#' )$stats
#'
#' # bootstrap standard errors (few resamples, to keep the example fast)
#' validate_prediction(
#'   toy_validation$partial,
#'   toy_validation$pheno,
#'   h2 = 0.3,
#'   bootstrap = TRUE,
#'   n_boot = 200
#' )$stats
#'
#' # scatter plot of the target on the prediction (grey line: slope 1,
#' # blue line: fitted regression)
#' res_plot <- validate_prediction(
#'   toy_validation$partial,
#'   toy_validation$pheno,
#'   h2 = 0.3,
#'   plot = TRUE
#' )
#' res_plot$plot
validate_prediction <- function(
  prediction,
  target,
  h2 = NULL,
  var_a = NULL,
  bootstrap = FALSE,
  n_boot = 10000L,
  ncpus = 1L,
  plot = FALSE
) {
  # 1. input checks
  if (!is.numeric(prediction) || !is.numeric(target)) {
    stop("`prediction` and `target` must be numeric vectors.", call. = FALSE)
  }
  # check that the two vectors are paired
  if (length(prediction) != length(target)) {
    stop("`prediction` and `target` must have the same length.", call. = FALSE)
  }
  # check for h2 being a single number in (0, 1]
  if (
    !is.null(h2) &&
      !(is.numeric(h2) && length(h2) == 1L && h2 > 0 && h2 <= 1)
  ) {
    stop("`h2` must be a single number in (0, 1].", call. = FALSE)
  }
  # check for var_a being a single positive number
  if (!is.null(var_a)) {
    .check_positive_number(var_a, "var_a")
  }
  # check that flags are a single TRUE or FALSE
  .check_flag(bootstrap, "bootstrap")
  .check_flag(plot, "plot")
  # check the bootstrap settings
  .check_n_boot(n_boot)
  .check_ncpus(ncpus)

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
  # check that there are enough pairs
  if (nrow(data) < 3L) {
    stop("At least 3 pairs are needed.", call. = FALSE)
  }
  # check that both vectors have variation, otherwise slope and correlation are undefined
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
      h2 = h2,
      var_a = var_a
    )
  } else {
    data.frame(
      value = .general_stats(
        data,
        seq_len(nrow(data)),
        h2 = h2,
        var_a = var_a
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

  return(list(stats = .new_stats(stats_df), plot = p))
}

#' Validation statistics on one (re)sample
#'
#' @param data Data frame with columns `prediction` and `target`.
#' @param indices Row indices of the (re)sample.
#' @param h2 Optional heritability; adds `accuracy` when not `NULL`.
#' @param var_a Optional additive genetic variance; adds `rmse_in_GSD` when not
#'   `NULL`.
#' @return Named numeric vector.
#' @noRd
.general_stats <- function(data, indices, h2 = NULL, var_a = NULL) {
  # values of the (re)sampled pairs
  x <- data$prediction[indices]
  y <- data$target[indices]

  # regression of target on prediction
  slope <- stats::cov(x, y) / stats::var(x)
  # mean squared error of the prediction against the target
  mse <- mean((x - y)^2)
  out <- c(
    n = length(x),
    correlation = stats::cor(x, y),
    slope = slope,
    intercept = mean(y) - slope * mean(x),
    mean_diff = mean(x) - mean(y),
    mse = mse,
    rmse = sqrt(mse)
  )
  # rmse in genetic standard deviations only when var_a is provided
  if (!is.null(var_a)) {
    out["rmse_in_GSD"] <- out[["rmse"]] / sqrt(var_a)
  }
  # compute accuracy only when h2 is provided
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
