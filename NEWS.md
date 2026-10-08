# validatoR 0.0.9

First beta version. Functions, arguments and results can change between
versions.

* `validate_lr()` validates a partial against a whole evaluation with the LR
  method: level bias, dispersion bias, rho, ratio of accuracies, and the
  accuracy of the partial EBVs. It optionally returns bootstrap standard errors
  and a scatter plot.
* `validate_prediction()` validates predictions against a target (pre-corrected
  phenotypes, true breeding values or other EBVs): correlation, regression of
  the target on the prediction, mean difference, MSE and RMSE, and the accuracy
  when `h2` is provided. It optionally returns bootstrap standard errors and a
  scatter plot.
* `rebase_ebv()` puts EBVs on a common base, from a base population or from
  provided constants.
* `toy_validation` is a simulated data set with known answers, and the
  "Getting started" vignette walks through the three functions.
