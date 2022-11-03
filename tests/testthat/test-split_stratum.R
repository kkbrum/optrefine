test_that("correct number of units selected", {
  set.seed(60)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  nc <- c(10, 22)
  nt <- c(8, 10)
  strata <- factor(rep(1:4, each = 50))
  ref_st <- split_stratum(z = z, X = X, strata = strata, ist = 2,
                     nc = nc,
                     nt = nt)
  expect_equal(c(t(table(z[strata == 2], ref_st$selection))),
               c(nc, nt))
})

test_that("correct number of fractions given", {
  set.seed(75)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  strata <- factor(rep(1:4, each = 50))
  ref_st <- split_stratum(z = z, X = X, strata = strata, ist = 4,
                     nc = c(10, 20),
                     nt = c(12, 8))
  expect_equal(sum(colSums(round(ref_st$pr, 5) > 0) == 2), ref_st$n_fracs)
})

test_that("integer programming returns integer solution", {
  set.seed(85)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  strata <- factor(rep(1:4, each = 50))
  ref_st <- split_stratum(z = z, X = X, strata = strata, ist = 4,
                          nc = c(11, 20),
                          nt = c(12, 7),
                          integer = TRUE)
  expect_equal(sum(colSums(round(ref_st$pr, 5) > 0) == 2), 0)
})


test_that("X can be dataframe", {
  set.seed(85)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- as.data.frame(rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)])
  z <- rhc_X[samp_rows, "z"]
  strata <- factor(rep(1:4, each = 50))
  ref_st <- split_stratum(z = z, X = X, strata = strata, ist = 4,
                          nc = c(11, 20),
                          nt = c(12, 7),
                          integer = TRUE)
  expect_equal(sum(colSums(round(ref_st$pr, 5) > 0) == 2), 0)
})

test_that("split_stratum can split into >2 strata", {
  set.seed(85)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  strata <- factor(rep(1:4, each = 50))
  ref_st <- split_stratum(z = z, X = X, strata = strata, ist = 4,
                          nc = c(11, 10, 10),
                          nt = c(5, 6, 8))
  expect_equal(length(unique(ref_st$selection)), 3)
  expect_equal(c(table(z[strata == 4], ref_st$selection)), c(12, 6, 9, 6, 10, 7))
})
