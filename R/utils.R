# R/utils.R
# Small helpers shared by the exported functions: input checks and ID handling.

#' Stop unless `x` is a single TRUE or FALSE
#'
#' @param x Value to check.
#' @param arg Name of the argument, used in the error message.
#' @return `x`, invisibly.
#' @noRd
.check_flag <- function(x, arg) {
  if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  return(invisible(x))
}

#' Stop unless `x` is a single whole number of at least 2
#'
#' Used for the number of bootstrap resamples. `.run_bootstrap()` checks it too,
#' but with R's default `stopifnot()` message.
#'
#' @param x Value to check.
#' @return `x`, invisibly.
#' @noRd
.check_n_boot <- function(x) {
  if (
    !(is.numeric(x) &&
      length(x) == 1L &&
      is.finite(x) &&
      x >= 2 &&
      x == round(x))
  ) {
    stop("`n_boot` must be a single whole number of at least 2.", call. = FALSE)
  }
  return(invisible(x))
}

#' Stop unless `x` is a single whole number of at least 1
#'
#' Used for the number of CPUs of the bootstrap.
#'
#' @param x Value to check.
#' @return `x`, invisibly.
#' @noRd
.check_ncpus <- function(x) {
  if (
    !(is.numeric(x) &&
      length(x) == 1L &&
      is.finite(x) &&
      x >= 1 &&
      x == round(x))
  ) {
    stop("`ncpus` must be a single whole number of at least 1.", call. = FALSE)
  }
  return(invisible(x))
}

#' First few elements of a vector, pasted, for error messages
#'
#' @param x Vector.
#' @param k Number of elements to show.
#' @return A single string.
#' @noRd
.show_some <- function(x, k = 3L) {
  out <- paste(x[seq_len(min(k, length(x)))], collapse = ", ")
  return(out)
}

#' Animal IDs as text, so that numeric and text IDs match
#'
#' `as.character(100000)` is `"1e+05"`, so the same animal would get two
#' spellings when one input has numeric IDs and the other has text IDs. Numeric
#' IDs are written in full instead (`"100000"`); text IDs are left as they are.
#' Missing IDs stay `NA`.
#'
#' @param x Vector of IDs (numeric, character or factor).
#' @return Character vector of the same length as `x`.
#' @noRd
.as_id <- function(x) {
  ids <- if (is.numeric(x)) {
    format(x, scientific = FALSE, trim = TRUE)
  } else {
    as.character(x)
  }
  ids[is.na(x)] <- NA_character_
  return(ids)
}
