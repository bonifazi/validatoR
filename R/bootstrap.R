# R/bootstrap.R
# Shared bootstrap engine used by validation functions such as validate_lr() and validate_general().
# Method-agnostic: it knows nothing about the actual statistics, it only resamples
# rows and summarises whatever `stat_fun` returns.

#' Run a nonparametric bootstrap and summarise it
#'
#' Resamples the rows of `data` with replacement `n_boot` times, applies
#' `stat_fun` to each resample via [boot::boot()], and returns the point
#' estimates with their bootstrap standard errors.
#'
#' The parallel backend is chosen from the operating system: `"snow"` on
#' Windows and `"multicore"` (forking) on Linux and macOS. With `ncpus = 1`
#' the bootstrap runs serially. For `"snow"` the cluster is created here and
#' always closed through [on.exit()], so workers never outlive the call, even
#' when a resample errors or the run is interrupted.
#'
#' @param data data.frame passed to `stat_fun` on every resample.
#' @param stat_fun Function with signature `function(data, indices, ...)`
#'   returning a named numeric vector of statistics computed on
#'   `data[indices, ]`. All inputs must arrive as arguments; it must not
#'   rely on variables from an enclosing function.
#' @param n_boot Integer. Number of bootstrap resamples.
#' @param ncpus Integer. Number of CPUs; `1` (default) runs serially.
#' @param ... Extra named arguments passed unchanged to `stat_fun` on every
#'   resample and every worker (e.g. `VAR_A`, `h2`).
#'
#' @return A data.frame with one row per statistic (row names = statistic
#'   names) and columns `value` (estimate on the original data) and `SE`
#'   (standard deviation of the bootstrap replicates).
#'
#' @note On Windows with `ncpus > 1`, workers are fresh R sessions that load
#'   validatoR to find `stat_fun`, so the package must be installed
#'   (`devtools::install()`); this does not work under `devtools::load_all()`.
#'
#' @noRd
.run_bootstrap <- function(data, stat_fun, n_boot, ncpus = 1L, ...) {
  stopifnot(
    is.data.frame(data),
    is.function(stat_fun),
    is.numeric(n_boot), length(n_boot) == 1L, n_boot >= 2,
    is.numeric(ncpus),  length(ncpus)  == 1L, ncpus  >= 1
  )
  n_boot <- as.integer(n_boot)
  ncpus  <- as.integer(ncpus)

  # pick the parallel backend from the OS
  backend <- if (ncpus == 1L) {
    "no"
  } else {
    switch(
      .Platform$OS.type,
      windows = "snow",      # snow on Windows
      unix    = "multicore", # multicore on Linux/macOS
      stop("Cannot choose a parallel backend for OS type '", .Platform$OS.type,
           "'. Use `ncpus = 1`.", call. = FALSE)
    )
  }

  # check that requested n. CPUs is not larger than the number of cores available
  # (skipped when serial; detectCores() can also return NA, hence isTRUE)
  if (ncpus > 1L && isTRUE(ncpus > parallel::detectCores())) {
    stop("`ncpus` (", ncpus, ") is larger than the number of cores available.", call. = FALSE)
  }

  # for snow, own the cluster so its shutdown is guaranteed and stop on exit
  cl <- NULL
  if (backend == "snow") {
    cl <- parallel::makeCluster(ncpus)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  # run the bootstrap
  b <- boot::boot(
    data      = data,
    statistic = stat_fun, # called as stat_fun(data, indices, ...) on each resample
    R         = n_boot,
    parallel  = backend,
    ncpus     = ncpus,
    cl        = cl,
    ...
  )

  # b$t0: statistics on the original data (named vector)
  # b$t : n_boot x n_statistics matrix of replicates
  data.frame(
    value     = b$t0,
    SE        = apply(b$t, 2, stats::sd),
    row.names = names(b$t0)
  )
}