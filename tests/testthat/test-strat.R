test_that("warning message if no variation in covariate", {
  set.seed(35)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  expect_warning(strat(z = z , X = X), "no variation")
})


test_that("can supply different argument formats to strat()", {
  set.seed(21)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  # Try supplying data frame
  X <- as.data.frame(rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)])
  # Try supplying without column names
  colnames(X) <- NULL
  z <- rhc_X[samp_rows, "z"]
  # Try supplying strata not as factor
  strata <- rep(0:1, each = 100)
  expect_equal(colnames(strat(z = z , X = X, base_strata = strata)$X), as.character(1:10))
})


test_that("error if length of z doesn't match X", {
  set.seed(12)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- c(rhc_X[samp_rows, "z"], rep(0, 20))
  expect_error(strat(z = z , X = X), "length of `z` must match")
})

test_that("error if length of z and strata don't match", {
  set.seed(12)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  expect_error(strat(z = z , X = X, base_strata = rep(1, 110)),
               "length of `base_strata` must match")
  expect_error(strat(z = z , X = X, refined_strata = rep(1, 110)),
               "length of `refined_strata` must match")
})


test_that("error if z isn't binary", {
  set.seed(12)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rep(1:4, each = 25)
  expect_error(strat(z = z , X = X),
               "`z` must only contain")
})

