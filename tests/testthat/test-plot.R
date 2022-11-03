test_that("Can plot just no stratification", {
  set.seed(10)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  expect_warning(plot(strat(z = z, X = X)), "No initial stratification")
  expect_s3_class(plot(strat(z = z, X = X), incl_base = FALSE), "ggplot")
})

test_that("Can plot just base stratification, by stratum and not", {
  set.seed(10)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  p <- plot(prop_strat(z = z, X = X), incl_none = FALSE, by_strata = TRUE)
  expect_length(p, 5)
  expect_s3_class(p[[3]], "ggplot")
})


test_that("Correct smd for base stratification", {
  set.seed(12)
  samp_rows <- sample(1:nrow(rhc_X), 102)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = 3)
  wgt <- plot(ps, weighted_avg = TRUE)
  unwgt <- plot(ps, weighted_avg = FALSE)
  smds <- calc_smds(ps)
  expect_equal(as.numeric(colMeans(smds$base)["pafi1"]), unwgt$data$abs_stand_diff[unwgt$data$covariates == "pafi1" & unwgt$data$type == "Base strata"])
  # Base strata are equal in size so weighting shouldn't change anything
  expect_equal(wgt$data, unwgt$data)
  })

test_that("Correct smd for refined stratification", {
  set.seed(18)
  samp_rows <- sample(1:nrow(rhc_X), 500)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(z = z, X = X)
  wgt <- plot(ref, weighted_avg = TRUE)
  unwgt <- plot(ref, weighted_avg = FALSE)
  smds <- calc_smds(ref)
  expect_equal(as.numeric(colMeans(smds$refined, na.rm = TRUE)["hrt1"]), unwgt$data$abs_stand_diff[unwgt$data$covariates == "hrt1" & unwgt$data$type == "Refined strata"])
  expect_equal(sum(table(ref$refined_strata) * smds$refined[, "hrt1"], na.rm = TRUE) / length(z), wgt$data$abs_stand_diff[wgt$data$covariates == "hrt1" & wgt$data$type == "Refined strata"])
})


test_that("Plotting by strata with both base and refined works", {
  set.seed(128)
  samp_rows <- sample(1:nrow(rhc_X), 500)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(z = z, X = X)
  expect_s3_class(plot(ref, by_strata = TRUE, incl_base = TRUE, incl_none = FALSE)[[2]], "ggplot")
  expect_s3_class(plot(ref, by_strata = TRUE, incl_base = TRUE,
                       weighted_avg = TRUE, incl_none = FALSE)[[2]], "ggplot")
  })


test_that("Plotting just refined works", {
  set.seed(14)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(z = z, X = X)
  expect_s3_class(plot(ref, incl_none = FALSE, incl_base = FALSE), "ggplot")
  expect_s3_class(plot(ref, by_strata = TRUE, incl_none = FALSE, incl_base = FALSE)[[3]], "ggplot")

})



test_that("Warnings and errors for incompatible arguments", {
  set.seed(1235)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:(ncol(rhc_X)-2), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = 2)
  expect_error(plot(ps, legend = c("one", "two")), "Length of `legend`")
  expect_error(plot(ps, incl_base = FALSE, incl_none = FALSE), "nothing to plot")
  expect_warning(plot(ps, by_strata = TRUE, incl_base = FALSE), "`by_strata` has thus been switched")
  expect_warning(plot(ps, by_strata = TRUE, incl_none = TRUE), "Cannot plot by stratum")

})

