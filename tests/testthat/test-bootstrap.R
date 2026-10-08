# Tests for .run_bootstrap(), the bootstrap engine shared by the validation
# functions. The tests are grouped in blocks, each with a short description of
# what it tests.

# statistic function for the tests: mean of the partial EBV in toy_validation
mean_stat <- function(data, i) {
  return(c(mean_p = mean(data$partial[i])))
}

# ----------------------------------------------------------------------------
# Block 1. The estimate and its standard error
# Tests: The shape of the result, the estimate on the original data, and the
# standard error of a mean against the analytical value.
# ----------------------------------------------------------------------------

test_that(".run_bootstrap returns a data.frame with value and SE", {
  set.seed(1)
  res <- .run_bootstrap(toy_validation, mean_stat, n_boot = 200)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("value", "SE"))
  expect_identical(rownames(res), "mean_p")
})

test_that(".run_bootstrap value is the estimate on the original data", {
  set.seed(1)
  res <- .run_bootstrap(toy_validation, mean_stat, n_boot = 200)

  expect_equal(res["mean_p", "value"], mean(toy_validation$partial))
})

test_that(".run_bootstrap SE of a mean is close to the analytical SE", {
  set.seed(1)
  res <- .run_bootstrap(toy_validation, mean_stat, n_boot = 200)
  analytical_se <- sd(toy_validation$partial) / sqrt(nrow(toy_validation))

  expect_equal(res["mean_p", "SE"], analytical_se, tolerance = 0.2)
})

# ----------------------------------------------------------------------------
# Block 2. Arguments and inputs
# Tests: Extra arguments passed to the statistic function, and the checks on
# data, the statistic function and the bootstrap settings.
# ----------------------------------------------------------------------------

test_that(".run_bootstrap passes extra arguments to the statistic function", {
  scaled_mean <- function(d, i, k) {
    return(c(m = k * mean(d$partial[i])))
  }
  res <- .run_bootstrap(toy_validation, scaled_mean, n_boot = 20, k = 3)

  expect_equal(res["m", "value"], 3 * mean(toy_validation$partial))
})

test_that(".run_bootstrap requires `data` to be a data.frame", {
  expect_error(
    .run_bootstrap(list(partial = 1:10), mean_stat, n_boot = 20),
    "is.data.frame"
  )
  expect_error(
    .run_bootstrap(
      as.matrix(toy_validation[, c("partial", "whole")]),
      mean_stat,
      n_boot = 20
    ),
    "is.data.frame"
  )
})

test_that(".run_bootstrap rejects bad inputs", {
  expect_error(.run_bootstrap(toy_validation, "mean", n_boot = 20))
  expect_error(.run_bootstrap(toy_validation, mean_stat, n_boot = 1))
  expect_error(
    .run_bootstrap(toy_validation, mean_stat, n_boot = 20, ncpus = 0),
    "ncpus"
  )
})

test_that(".run_bootstrap rejects more CPUs than the cores available", {
  # pretend that the machine has 2 cores
  local_mocked_bindings(detectCores = function(...) 2L, .package = "parallel")

  expect_error(
    .run_bootstrap(toy_validation, mean_stat, n_boot = 20, ncpus = 3),
    "`ncpus` must not be larger than the number of cores available (2), but it is 3.",
    fixed = TRUE
  )
})

# ----------------------------------------------------------------------------
# Block 3. Parallel clusters
# Tests: On Windows, the snow cluster is closed after a normal run and after a
# run in which the statistic function fails.
# ----------------------------------------------------------------------------

test_that(".run_bootstrap closes its snow cluster, also when the statistic errors", {
  skip_on_cran()
  skip_if_not(.Platform$OS.type == "windows") # snow is only used on Windows
  skip_if(parallel::detectCores() < 2)

  # statistic functions that live in the global env, so the workers do not
  # need validatoR installed (they would under devtools::load_all())
  ok_stat <- function(data, i) {
    return(c(mean_p = mean(data$partial[i])))
  }
  bad_stat <- function(data, i) {
    stop("boom")
    return(NULL) # never reached, keeps the helper in the package style
  }
  environment(ok_stat) <- globalenv()
  environment(bad_stat) <- globalenv()

  # record every cluster that .run_bootstrap() creates
  made <- list()
  make_cluster <- parallel::makeCluster
  local_mocked_bindings(
    makeCluster = function(...) {
      cl <- make_cluster(...)
      made[[length(made) + 1L]] <<- cl
      return(cl)
    },
    .package = "parallel"
  )

  # normal run: one cluster created, and afterwards it can no longer be used
  .run_bootstrap(toy_validation, ok_stat, n_boot = 20, ncpus = 2)
  expect_length(made, 1L)
  expect_error(parallel::clusterEvalQ(made[[1]], 1))

  # failing run: the error reaches the caller and the cluster is still closed
  expect_error(
    .run_bootstrap(toy_validation, bad_stat, n_boot = 20, ncpus = 2),
    "boom"
  )
  expect_length(made, 2L)
  expect_error(parallel::clusterEvalQ(made[[2]], 1))
})
