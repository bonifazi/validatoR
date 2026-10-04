#' Toy validation data
#'
#' Simulated EBVs from a "partial" and a "whole" evaluation,
#' built so that the LR statistics have known expected values.
#'
#' @format A data frame with 2000 rows and 5 columns:
#' \describe{
#'   \item{id}{Animal ID.}
#'   \item{partial}{EBV from the partial evaluation.}
#'   \item{whole}{EBV from the whole evaluation.}
#'   \item{group}{Factor with 4 cohorts.}
#'   \item{inbreeding}{Inbreeding coefficient.}
#' }
#'
#' @details `whole` is simulated from `partial`, so the expected LR
#'   statistics are: level bias 0.75, dispersion 0.90 and rho 0.85. Sampling
#'   noise moves the realised values slightly. Use `VAR_A = 300` when the
#'   accuracy of the partial EBV is needed.
#'
#' @source Simulated, see `data-raw/toy_validation.R`.
"toy_validation"
