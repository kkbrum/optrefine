test_that("List returned with all expected components", {
  set.seed(11)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  pvals <- rand_pvals(ps, options = list(nrand = 100))
  expect_setequal(names(pvals), c("pvals", "obs_details", "rand_details"))
  expect_setequal(names(pvals$pvals$base),
                  c("obj", "max", "avg", "smds"))
  expect_setequal(names(pvals$obs_details$base),
                  c("obj", "max", "smds"))
  expect_setequal(names(pvals$rand_details$base),
                  c("obj", "max", "smds"))
  expect_null(pvals$pvals$refined)
  expect_null(pvals$obs_details$refined)
  expect_null(pvals$rand_details$refined)
  expect_equal(length(pvals$pvals$base$smds), 11)
  expect_equal(length(pvals$obs_details$base$smds), 11)
  expect_equal(dim(pvals$rand_details$base$smds), c(100, 11))
})


test_that("Criterion argument works as expected", {
  set.seed(13)
  samp_rows <- sample(1:nrow(rhc_X), 150)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  ref <- refine(ps)
  pvals <- rand_pvals(ref, options = list(criterion = "sum", nrand = 10))
  expect_equal(pvals$rand_details$refined$obj, rowMeans(pvals$rand_details$refined$smds))
})


test_that("incl_base arguments work as expected", {
  set.seed(17)
  samp_rows <- sample(1:nrow(rhc_X), 300)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 3)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z)
  pvals <- rand_pvals(z = ref$z, X = ref$X, base_strata = ref$base_strata,
                      refined_strata = ref$refined_strata, options = list(incl_base = FALSE, nrand = 10))
  expect_null(pvals$pvals$base)
})


test_that("Pvals calculated properly", {
  set.seed(23)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  ref <- refine(ps)
  pvals <- rand_pvals(ref, options = list(nrand = 100))
  expect_equal(pvals$pvals$base$max,
               mean(pvals$rand_details$base$max > pvals$obs_details$base$max))
  expect_equal(pvals$pvals$base$avg,
               mean(rowMeans(pvals$rand_details$base$smds) > mean(pvals$obs_details$base$smds)))
  expect_equal(as.numeric(pvals$pvals$base$smds[5]),
               mean(pvals$rand_details$base$smds[, 5] > pvals$obs_details$base$smds[5]))
  expect_equal(pvals$pvals$refined$max,
               mean(pvals$rand_details$refined$max > pvals$obs_details$refined$max))
  expect_equal(pvals$pvals$refined$avg,
               mean(rowMeans(pvals$rand_details$refined$smds) > mean(pvals$obs_details$refined$smds)))
  expect_equal(as.numeric(pvals$pvals$refined$smds[3]),
               mean(pvals$rand_details$refined$smds[, 3] > pvals$obs_details$refined$smds[3]))
})


test_that("Sum obj for pvals calculated properly", {
  set.seed(27)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  ref <- refine(ps, options = list(criterion = "sum"))
  pvals <- rand_pvals(ref, options = list(nrand = 100, criterion = "sum"))
  expect_equal(pvals$obs_details$refined$obj,
               mean(pvals$obs_details$refined$smds))
  expect_equal(pvals$obs_details$refined$obj,
               ref$details$valueIP)
  expect_equal(pvals$rand_details$refined$obj,
               rowMeans(pvals$rand_details$refined$smds))
})


test_that("Max obj for pvals calculated properly", {
  set.seed(29)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(z = z, X = X, strata = rep(1, length(z)), options = list(criterion = "max"))
  pvals <- rand_pvals(ref, options = list(nrand = 100, criterion = "max"))
  expect_equal(pvals$obs_details$refined$obj,
               pvals$obs_details$refined$max)
  expect_equal(pvals$obs_details$refined$obj,
               ref$details$valueIP)
  expect_equal(pvals$rand_details$refined$obj,
               pvals$rand_details$refined$max)
})


test_that("Combo obj for pvals calculated properly", {
  set.seed(31)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(z = z, X = X, strata = rep(1, length(z)), options = list(criterion = "combo", wMax = 3))
  pvals <- rand_pvals(ref, options = list(nrand = 100, criterion = "combo", wMax = 3))
  expect_equal(pvals$obs_details$refined$obj,
               (sum(pvals$obs_details$refined$smds * 2) + 3 * pvals$obs_details$refined$max) / 13)
  expect_equal(pvals$obs_details$refined$obj,
               ref$details$valueIP)
  expect_equal(pvals$rand_details$refined$obj,
               (rowSums(pvals$rand_details$refined$smds * 2) + 3 * pvals$rand_details$refined$max) / 13)

})


test_that("Table has correct dimensions and column and row names", {
  set.seed(27)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  ref <- refine(ps)
  pvals <- rand_pvals(ref, options = list(nrand = 100))
  tab1 <- table_rand_pvals(ref, options = list(rand_pvals = pvals))
  expect_equal(dim(tab1), c(9, 8))
  expect_false(is.null(colnames(tab1)))
  expect_false(is.null(rownames(tab1)))
  expect_true(all(is.na(tab1[1, 1:4])))
})


test_that("Incl_base works properly for table", {
  set.seed(22)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  ref <- refine(ps)
  pvals <- rand_pvals(ref, options = list(nrand = 100, criterion = "max"))
  tab1 <- table_rand_pvals(ref, options = list(rand_pvals = pvals))
  tab2 <- table_rand_pvals(ref, options = list(incl_base = FALSE, rand_pvals = pvals))
  expect_equal(dim(tab2), c(9, 4))
  expect_equal(tab1[, 5:8], tab2)
})


test_that("Criterion and wMax options work for table", {
  set.seed(22)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  tab1 <- table_rand_pvals(z = ps$z, X = ps$X, base_strata = ps$base_strata,
                           options = list(criterion = "combo", wMax = 10, nrand = 10))
  expect_equal(dim(tab1), c(9, 4))
  expect_equal(tab1[4, 2], 0.1527478, tolerance = 10e-5)
})



test_that("Incl_base works properly for table", {
  set.seed(22)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(X = X, z = z, nstrata = 2)
  tab1 <- table_rand_pvals(z = ps$z, X = ps$X, base_strata = ps$base_strata,
                           options = list(criterion = "combo", wMax = 10, nrand = 10))
  expect_equal(dim(tab1), c(9, 4))
  expect_equal(tab1[4, 2], 0.1527478, tolerance = 10e-5)
})
