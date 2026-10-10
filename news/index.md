# Changelog

## validatoR (development version)

### Breaking changes

- renamed `average_F` to `average_inbreeding` in
  [`validate_lr()`](https://bonifazi.github.io/validatoR/reference/validate_lr.md),
  also in its `stats` table

### New features

- added
  [`validate_lr_by_group()`](https://bonifazi.github.io/validatoR/reference/validate_lr_by_group.md)
  to validate several groups of animals in one call
- added `group` and `plot_subgroups` to
  [`validate_prediction()`](https://bonifazi.github.io/validatoR/reference/validate_prediction.md)

### Improvements

- standardised the error messages: argument name first, then “must”
- added dotted lines at the means of the original and the rebased EBVs
  to the plots of
  [`rebase_ebv()`](https://bonifazi.github.io/validatoR/reference/rebase_ebv.md)

### Bug fixes

- fixed
  [`validate_prediction()`](https://bonifazi.github.io/validatoR/reference/validate_prediction.md)
  so that it gives its own error message when `h2` is `NA`
- stopped rounding the count `n` when a `stats` table is printed

### Documentation

- documented `inc_acc` as in Bonifazi et al. (2022), with the percentage
  conversion
- explained why validatoR and how to install a specific release in the
  README
- listed
  [`rebase_ebv()`](https://bonifazi.github.io/validatoR/reference/rebase_ebv.md)
  first in the README and the vignette
- documented validating groups of animals in the README and the vignette

### Internal

- added tests for error messages and `ncpus`
- added tests for the by-group functions
- added GitHub issue templates

## validatoR 0.0.9

First beta version. Functions, arguments and results can change between
versions.

### New features

- added
  [`rebase_ebv()`](https://bonifazi.github.io/validatoR/reference/rebase_ebv.md),
  [`validate_lr()`](https://bonifazi.github.io/validatoR/reference/validate_lr.md),
  [`validate_prediction()`](https://bonifazi.github.io/validatoR/reference/validate_prediction.md)
  and `toy_validation`
- added the “Getting started” vignette
