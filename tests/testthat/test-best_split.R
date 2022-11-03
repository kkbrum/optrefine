test_that("list returned with all expected components", {
  set.seed(12)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  best <- best_split(z, X, strata = factor(rep(1:4, each = 50)), ist = 2,
                           nc_list = list(c(10, 24), c(14, 20)),
                           nt_list = list(c(6, 10), c(8, 8), c(9, 7)),
                           min_split = 6)
  expect_setequal(names(best),
                  c("valuesLP", "valuesIP", "besti", "bestj", "n_smds", "n_fracs", "rand_c_prop",
                    "rand_t_prop", "pr", "selection"))
})


test_that("correct number of solutions returned", {
  set.seed(12)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  best <- best_split(z, X, strata = factor(rep(1:4, each = 50)), ist = 2,
                     nc_list = list(c(10, 24), c(14, 20)),
                     nt_list = list(c(6, 10), c(8, 8), c(9, 7)),
                     min_split = 6)
  expect_equal(dim(best$valuesIP), c(2, 3))
})


test_that("min_split enforced", {
  set.seed(15)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  best <- best_split(z, X, strata = factor(rep(1:4, each = 50)), ist = 2,
                     nc_list = list(c(10, 20), c(14, 16)),
                     nt_list = list(c(8, 12), c(10, 10), c(9, 11)),
                     min_split = 10)
  expect_equal(is.na(best$valuesIP),
               matrix(c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
                      nrow = 2, dimnames = list(nc_option = 1:2, nt_option = 1:3)))
  expect_error(best_split(z, X, strata = factor(rep(1:4, each = 50)), ist = 2,
                            nc_list = list(c(10, 20), c(14, 16)),
                            nt_list = list(c(8, 12), c(10, 10), c(9, 11)),
                            min_split = 15), "satisfied the min_split requirement")
})


test_that("error if sample sizes dont add up", {
  set.seed(50)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  expect_error(best_split(z = z, X = X, strata = factor(rep(1:4, each = 50)), ist = 2,
                          nc_list = list(c(10, 20), c(14, 16)),
                          nt_list = list(c(8, 12), c(10, 10), c(9, 11)),
                          min_split = 5),
               regexp = "sample size options sum to the number")
  expect_error(best_split(z = z, X = X, strata = factor(rep(1:4, each = 50)), ist = 2,
                          nc_list = list(c(12, 20), c(16, 16)),
                          nt_list = list(c(8, 12), c(10, 10), c(9, 11)),
                          min_split = 5),
               regexp = "sample size options sum to the number")
})


test_that("correct best solution chosen", {
  set.seed(70)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  strata <- factor(rep(1:4, each = 50))
  nc_list <- list(c(10, 21), c(16, 15))
  nt_list <- list(c(9, 10), c(11, 8), c(13, 6))
  best <- best_split(z = z, X = X, strata = strata, ist = 1,
                     nc_list = nc_list,
                     nt_list = nt_list,
                     min_split = 6)
  expect_equal(best$valuesIP[best$besti, best$bestj], min(best$valuesIP))
  expect_equal(best$valuesIP[best$besti, best$bestj], 3.2968304)
  expect_equal(best$besti, 1)
  expect_equal(best$bestj, 3)
})
