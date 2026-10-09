# validatoR (development version)

## New features
- added `group` and `plot_subgroups` to `validate_prediction()`

## Improvements
- error messages follow one pattern: argument name first, then "must"

## Bug fixes
- `validate_prediction()` gives its own error message when `h2` is `NA`

## Documentation
- documented `inc_acc` as in Bonifazi et al. (2022), with the percentage conversion
- explained why validatoR and how to install a specific release in the README
- listed `rebase_ebv()` first in the README and the vignette

## Internal
- added tests for error messages and `ncpus`
- added GitHub issue templates

# validatoR 0.0.9

First beta version. Functions, arguments and results can change between
versions.

## New features
- added `rebase_ebv()`, `validate_lr()`, `validate_prediction()` and `toy_validation`
- added the "Getting started" vignette
