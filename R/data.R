#' Toy validation data
#'
#' Simulated EBVs from a "partial" and a "whole" evaluation, and a
#' pre-corrected phenotype, built so that the statistics have known expected
#' values.
#'
#' @format A data frame with 2000 rows and 6 columns:
#' \describe{
#'   \item{id}{Animal ID.}
#'   \item{partial}{EBV from the partial evaluation.}
#'   \item{whole}{EBV from the whole evaluation.}
#'   \item{pheno}{Pre-corrected phenotype, simulated with a heritability of
#'     0.3.}
#'   \item{group}{Factor with 4 cohorts.}
#'   \item{inbreeding}{Inbreeding coefficient.}
#' }
#'
#' @details `whole` is simulated from `partial`, so the expected LR
#'   statistics are: level bias 0.75, dispersion 0.90 and rho 0.85. Sampling
#'   noise moves the realised values slightly. Use `var_a = 300` when the
#'   accuracy of the partial EBV is needed.
#'
#'   `pheno` is a true breeding value (`whole` plus an independent part, with
#'   variance `var_a`) plus noise, so that its heritability is 0.3. Because
#'   of that, `validate_prediction(partial, pheno, h2 = 0.3)` returns an
#'   accuracy of about 0.52 (the correlation between `partial` and the true
#'   breeding value), with a sampling error of about 0.04.
#'
#' @source Simulated, see `data-raw/toy_validation.R`.
"toy_validation"
