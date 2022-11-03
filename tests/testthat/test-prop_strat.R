test_that("propensity score strata created", {
    set.seed(28)
    samp_rows <- sample(1:nrow(rhc_X), 100)
    X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 5)]
    z <- rhc_X[samp_rows, "z"]
    nstrata <- 2
    ps <- prop_strat(X = X, z = z, nstrata = nstrata)
    expect_vector(ps$base_strata)
})

test_that("correct amount of propensity score strata created", {
  set.seed(30)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  nstrata <- 4
  ps <- prop_strat(X = X, z = z, nstrata = nstrata)
  expect_equal(length(unique(ps$base_strata)), nstrata)
})


test_that("error quantiles of propensity score aren't unique", {
  set.seed(2)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  X <- cbind(X, z)
  nstrata <- 8
  expect_error(prop_strat(X = X, z = z, nstrata = nstrata),
               "quantiles of the propensity score are not unique")
})


test_that("propensity score strata have correct sizes", {
  set.seed(42)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z)
  expect_true(all(table(ps$base_strata) %in% c(floor(length(z)/5), ceiling(length(z)/5))))
})

test_that("all units are assigned to propensity score strata", {
  set.seed(52)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z)
  expect_equal(sum(!is.na(ps$base_strata)), length(z))
})

test_that("warning if no treated or control units", {
  set.seed(25)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  expect_warning(prop_strat(X = X, z = z, nstrata = 20), regexp = "no treated and/or control units")
})
