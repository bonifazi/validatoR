# Print a table of validation statistics

Prints the `stats` table returned by the validation functions, such as
[`validate_lr()`](https://bonifazi.github.io/validatoR/reference/validate_lr.md)
and
[`validate_prediction()`](https://bonifazi.github.io/validatoR/reference/validate_prediction.md),
with a fixed number of significant digits and without scientific
notation, for example `0.8609` and `0.00000004922`. The count `n` is
always shown in full. Only the display changes: the values in the table
keep their full precision.

## Usage

``` r
# S3 method for class 'validatoR_stats'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A table of statistics: the `stats` element of the result of
  [`validate_lr()`](https://bonifazi.github.io/validatoR/reference/validate_lr.md)
  or
  [`validate_prediction()`](https://bonifazi.github.io/validatoR/reference/validate_prediction.md).

- digits:

  A single whole number of at least 1: the number of significant digits
  to show. Defaults to `4`.

- ...:

  Further arguments passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

`x`, invisibly.

## Getting the exact values

The table is an ordinary data frame
([`is.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) is
`TRUE`), so it is read in the usual ways: `res$stats["rho", "value"]`
for one value, `res$stats$value` for a column,
`as.data.frame(res$stats)` for a plain data frame, and
`write.csv(res$stats, "stats.csv")` writes the full precision. To see
more digits on the screen, use `print(res$stats, digits = 7)`.

## Examples

``` r
res <- validate_lr(
  toy_validation[, c("id", "partial")],
  toy_validation[, c("id", "whole")],
  var_a = 300
)
res$stats
#>                       value
#> n                      2000
#> level_bias             0.75
#> level_bias_in_GSD    0.0433
#> dispersion_bias         0.9
#> rho                    0.85
#> inc_acc               1.176

# more digits on the screen; the stored values do not change
print(res$stats, digits = 7)
#>                           value
#> n                          2000
#> level_bias                 0.75
#> level_bias_in_GSD    0.04330127
#> dispersion_bias             0.9
#> rho                        0.85
#> inc_acc                1.176471
res$stats["rho", "value"]
#> [1] 0.85
```
