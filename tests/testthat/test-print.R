test_that("Printing doesn't give error", {
  set.seed(147)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 5)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(z = z, X = X)
  expect_invisible(print(ref))
  ps <- prop_strat(z = z, X = X)
  expect_invisible(print(ps))
  expect_invisible(print(strat(z = z, X = X)))
})


