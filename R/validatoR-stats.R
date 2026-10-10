# R/validatoR-stats.R
# The `validatoR_stats` class: the `stats` table returned by the validation
# functions. The values keep their full precision; only printing is formatted.

#' Mark a table of statistics as a validatoR_stats table
#'
#' Adds the class `validatoR_stats` to a data frame, in front of
#' `data.frame`, so that it prints with [print.validatoR_stats()]. The data
#' are not changed.
#'
#' @param df A data frame with one row per statistic.
#' @return `df`, with the class `c("validatoR_stats", "data.frame")`.
#' @noRd
.new_stats <- function(df) {
  class(df) <- c("validatoR_stats", "data.frame")
  return(df)
}

#' Print a table of validation statistics
#'
#' Prints the `stats` table returned by the validation functions, such as
#' [validate_lr()] and [validate_prediction()], with a fixed number of
#' significant digits and without scientific notation, for example `0.8609` and
#' `0.00000004922`. The count `n` is always shown in full.
#' Only the display changes: the values in the table keep their full
#' precision.
#'
#' @details
#' # Getting the exact values
#' The table is an ordinary data frame (`is.data.frame()` is `TRUE`), so it
#' is read in the usual ways: `res$stats["rho", "value"]` for one value,
#' `res$stats$value` for a column, `as.data.frame(res$stats)` for a plain data
#' frame, and `write.csv(res$stats, "stats.csv")` writes the full precision.
#' To see more digits on the screen, use `print(res$stats, digits = 7)`.
#'
#' @param x A table of statistics: the `stats` element of the result of
#'   [validate_lr()] or [validate_prediction()].
#' @param digits A single whole number of at least 1: the number of
#'   significant digits to show. Defaults to `4`.
#' @param ... Further arguments passed to [print()].
#' @returns `x`, invisibly.
#' @examples
#' res <- validate_lr(
#'   toy_validation[, c("id", "partial")],
#'   toy_validation[, c("id", "whole")],
#'   var_a = 300
#' )
#' res$stats
#'
#' # more digits on the screen; the stored values do not change
#' print(res$stats, digits = 7)
#' res$stats["rho", "value"]
#' @export
print.validatoR_stats <- function(x, digits = 4, ...) {
  # check that digits is a single whole number of at least 1
  if (
    !(is.numeric(digits) &&
      length(digits) == 1L &&
      is.finite(digits) &&
      digits >= 1 &&
      digits == round(digits))
  ) {
    stop("`digits` must be a single whole number of at least 1.", call. = FALSE)
  }
  # format the values for printing, keeping the stored values unchanged
  shown <- as.data.frame(x)
  # find the row that is a count, which is shown with all its digits
  is_count <- rownames(shown) == "n"
  shown[] <- lapply(shown, function(column) {
    # round the statistics to the significant digits, but not the count
    rounded <- ifelse(is_count, column, signif(column, digits))
    return(format(rounded, scientific = FALSE, drop0trailing = TRUE))
  })
  print(shown, ...)
  return(invisible(x))
}
