test_that("calc_smds returns expected list components", {
  set.seed(50)
  samp_rows <- sample(1:nrow(rhc_X), 400)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z)
  smds <- calc_smds(ref)
  expect_true(all(names(smds) %in% c("base", "refined")))
  expect_false(is.null(smds$base))
  expect_false(is.null(smds$refined))

  smds <- calc_smds(z = ref$z, X = ref$X, refined_strata = ref$refined_strata)
  expect_true(all(names(smds) %in% c("base", "refined")))
  expect_true(is.null(smds$base))
  expect_false(is.null(smds$refined))

  smds <- calc_smds(z = ref$z, X = ref$X, base_strata = ref$base_strata)
  expect_true(all(names(smds) %in% c("base", "refined")))
  expect_false(is.null(smds$base))
  expect_true(is.null(smds$refined))
})


test_that("calc_smds calculates smd correctly", {
  set.seed(33)
  samp_rows <- sample(1:nrow(rhc_X), 400)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 8)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z)
  expect_equal(abs(mean(ref$details$X_std[z == 1 & ref$refined_strata == "3:2", 5]) -
         mean(ref$details$X_std[z == 0 & ref$refined_strata == "3:2", 5])),
         calc_smds(ref)$refined["3:2", 5])
  expect_equal(mean(ref$details$X_std[z == 1 & ref$refined_strata == "4:1", 3]) -
                     mean(ref$details$X_std[z == 0 & ref$refined_strata == "4:1", 3]),
               calc_smds(ref, abs = FALSE)$refined["4:1", 3])
})


test_that("entering object vs arguments gives same smds", {
  set.seed(47)
  samp_rows <- sample(1:nrow(rhc_X), 400)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 8)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z)
  expect_equal(calc_smds(ref), calc_smds(z = ref$z, X = ref$X,
                                         base_strata = ref$base_strata, refined_strata = ref$refined_strata))
})
