#' Toy validation data
#'
#' Simulated EBVs from a "partial" and a "whole" evaluation, and a
#' pre-corrected phenotype, built so that the LR statistics have known values.
#'
#' @format ## `toy_validation`
#' A data frame with 2,000 rows and 6 columns:
#' \describe{
#'   \item{id}{Animal ID.}
#'   \item{partial}{EBV from the partial evaluation, in trait units.}
#'   \item{whole}{EBV from the whole evaluation, in trait units.}
#'   \item{pheno}{Pre-corrected phenotype, in trait units, simulated with a
#'     heritability of 0.3.}
#'   \item{group}{Factor with 4 cohorts.}
#'   \item{inbreeding}{Inbreeding coefficient, as a coefficient and not a
#'     percentage.}
#' }
#'
#' @details `whole` is simulated from `partial`, and the sample moments of
#'   `partial` and of the noise of `whole` are set exactly, so the LR statistics
#'   have exactly these values: level bias 0.75, dispersion 0.90 and rho 0.85
#'   (`inc_acc` is `1 / 0.85`). With `var_a = 300` and the `inbreeding` column,
#'   `accuracy_partial` is 0.562.
#'
#'   `pheno` is a true breeding value (`whole` plus an independent part, with
#'   variance `var_a`) plus noise, so that its heritability is 0.3. Its random
#'   parts are also set exactly, so `validate_prediction(partial, pheno, h2 =
#'   0.3)` returns a correlation of 0.285 and an accuracy of 0.520 (the
#'   correlation between `partial` and the true breeding value), and a slope of
#'   0.90 and a mean difference of 0.75, as for `whole`. The accuracy is lower
#'   than `accuracy_partial` because `partial` is over-dispersed by construction
#'   (dispersion 0.90), while the LR accuracy assumes no over-dispersion.
#'
#' @source Simulated, see `data-raw/toy_validation.R`.
"toy_validation"
