test_that("strat S3 object returned with all expected components", {
  set.seed(10)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z)
  expect_s3_class(ref, "strat")
  expect_setequal(names(ref), c("z", "X", "base_strata", "refined_strata", "details"))
  expect_setequal(names(ref$details),
                  c("valueLP", "valueIP", "n_fracs", "rand_c_prop",
                    "rand_t_prop", "pr", "wMax", "criterion", "X_std"))
})


test_that("warning if no treated or control units in base strata", {
  set.seed(25)
  samp_rows <- sample(1:nrow(rhc_X), 100)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  strata <- rep(1, length(z))
  strata[sample(which(z == 1), 20)] <- 2
  expect_warning(refine(X = X, z = z, strata = strata), regexp = "no treated and/or control units")
})


test_that("base and refined strata created", {
  set.seed(40)
  samp_rows <- sample(1:nrow(rhc_X), 300)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z)
  expect_vector(ref$base_strata)
  expect_vector(ref$refined_strata)
  expect_equal(length(ref$base_strata), length(z))
  expect_equal(length(ref$refined_strata), length(z))
  expect_equal(sum(is.na(ref$base_strata)), 0)
  expect_equal(sum(is.na(ref$refined_strata)), 0)
})


test_that("Rglpk works", {
  set.seed(50)
  samp_rows <- sample(1:nrow(rhc_X), 400)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  expect_false(is.null(refine(X = X, z = z, options = list(solver = "Rglpk"))$details$valueLP))
})


test_that("Arguments solver, standardize, and criterion work as expected", {
  set.seed(50)
  samp_rows <- sample(1:nrow(rhc_X), 400)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 10)]
  z <- rhc_X[samp_rows, "z"]
  ref <- refine(X = X, z = z, options = list(solver = "Rglpk",
                                             standardize = FALSE,
                                             criterion = "max"))
  expect_false(is.null(ref$details$valueLP))
  ref <- refine(X = X, z = z,
                options = list(criterion = "sum", solver = "Rglpk"))
  expect_false(is.null(ref$details$valueLP))
})


test_that("Arguments integer, wMax, and ist work as expected", {
  set.seed(58)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), 5)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = 4)
  ref <- refine(ps,
                options = list(integer = TRUE,
                               criterion = "combo",
                               wMax = 10,
                               ist = 2))
  expect_false(is.null(ref$details$valueLP))
  expect_equal(ref$details$valueLP, ref$details$valueIP, tolerance = 10e-6)
  expect_equal(ref$details$valueIP, 0.249396391, tolerance = 10e-6)
})


test_that("IP combo obj value calculated correctly", {
  set.seed(59)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  ncov <- 5
  nstrata <- 4
  wMax <- 10
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "combo", wMax = wMax))
  n_st_used <- sum(table(ref$refined_strata) != 0)
  smds <- calc_smds(z = ref$z, X = ref$X, refined_strata = ref$refined_strata, abs = TRUE)
  obj <- (sum(smds$refined, na.rm = TRUE) +
            wMax * sum(sapply(1:4, function(x) {max(smds$refined[(2*x-1):(2*x), ], na.rm = TRUE)}))) /
    ((ncov+1)*n_st_used + nstrata * wMax)
  expect_equal(ref$details$valueIP, obj)
})


test_that("IP combo obj value calculated correctly when only two strata split", {
  set.seed(59)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  ncov <- 5
  nstrata <- 4
  wMax <- 10
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "combo", wMax = wMax, ist = c(2, 3)))
  n_st_used <- sum(table(ref$refined_strata[ps$base_strata %in% c(2,3)]) != 0)
  smds <- calc_smds(z = ref$z, X = ref$X, refined_strata = ref$refined_strata, abs = TRUE)
  obj <- (sum(smds$refined[3:6, ], na.rm = TRUE) +
            wMax * sum(sapply(2:3, function(x) {max(smds$refined[(2*x-1):(2*x), ], na.rm = TRUE)}))) /
    ((ncov+1)*n_st_used + 2 * wMax)
  expect_equal(ref$details$valueIP, obj)
})


test_that("IP sum obj value calculated correctly", {
  set.seed(63)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  ncov <- 5
  nstrata <- 4
  wMax <- 10
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "sum"))
  n_st_used <- sum(table(ref$refined_strata) != 0)
  smds <- calc_smds(z = ref$z, X = ref$X, refined_strata = ref$refined_strata, abs = TRUE)
  obj <- (sum(smds$refined, na.rm = TRUE)) /
    ((ncov+1)*n_st_used)
  expect_equal(ref$details$valueIP, obj)
})


test_that("IP max obj value calculated correctly", {
  set.seed(69)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  ncov <- 5
  nstrata <- 4
  wMax <- 10
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "max"))
  n_st_used <- sum(table(ref$refined_strata) != 0)
  smds <- calc_smds(z = ref$z, X = ref$X, refined_strata = ref$refined_strata, abs = TRUE)
  obj <- (wMax * sum(sapply(1:4, function(x) {max(smds$refined[(2*x-1):(2*x), ], na.rm = TRUE)}))) /
    (nstrata * wMax)
  expect_equal(ref$details$valueIP, obj)
})


test_that("LP max obj value calculated correctly", {
  set.seed(72)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  ncov <- 5
  nstrata <- 3
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "max"))
  m <- 0
  n_m <- 0
  for (st in levels(ps$base_strata)) {
    m <- m + max(abs(ref$details$pr[, z == 1 & ps$base_strata == st] %*%
      ref$details$X_std[z == 1 & ps$base_strata == st, ] /
      rowSums(ref$details$pr[, z == 1 & ps$base_strata == st]) -
      ref$details$pr[, z == 0 & ps$base_strata == st] %*%
      ref$details$X_std[z == 0 & ps$base_strata == st, ] /
      rowSums(ref$details$pr[, z == 0 & ps$base_strata == st])), na.rm = TRUE)
    n_m <- n_m + 1
  }
  obj <- m / n_m
  expect_equal(ref$details$valueLP, obj)
})


test_that("LP combo obj value calculated correctly", {
  set.seed(75)
  samp_rows <- sample(1:nrow(rhc_X), 200)
  ncov <- 5
  nstrata <- 2
  wMax <- 4
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "combo", wMax = wMax))
  m <- 0
  n_m <- 0
  for (st in levels(ps$base_strata)) {
    smds <- abs(ref$details$pr[, z == 1 & ps$base_strata == st] %*%
                  ref$details$X_std[z == 1 & ps$base_strata == st, ] /
                  rowSums(ref$details$pr[, z == 1 & ps$base_strata == st]) -
                  ref$details$pr[, z == 0 & ps$base_strata == st] %*%
                  ref$details$X_std[z == 0 & ps$base_strata == st, ] /
                  rowSums(ref$details$pr[, z == 0 & ps$base_strata == st]))
    m <- m + sum(smds, na.rm = TRUE) + wMax * max(smds, na.rm = TRUE)
    n_m <- n_m + wMax + sum(!is.na(smds))
  }
  obj <- m / n_m
  expect_equal(ref$details$valueLP, obj)
})


test_that("LP sum obj value calculated correctly", {
  set.seed(78)
  samp_rows <- sample(1:nrow(rhc_X), 250)
  ncov <- 5
  nstrata <- 2
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "sum"))
  m <- 0
  n_m <- 0
  for (st in levels(ps$base_strata)) {
    smds <- abs(ref$details$pr[, z == 1 & ps$base_strata == st] %*%
                  ref$details$X_std[z == 1 & ps$base_strata == st, ] /
                  rowSums(ref$details$pr[, z == 1 & ps$base_strata == st]) -
                  ref$details$pr[, z == 0 & ps$base_strata == st] %*%
                  ref$details$X_std[z == 0 & ps$base_strata == st, ] /
                  rowSums(ref$details$pr[, z == 0 & ps$base_strata == st]))
    m <- m + sum(smds, na.rm = TRUE)
    n_m <- n_m + sum(!is.na(smds))
  }
  obj <- m / n_m
  expect_equal(ref$details$valueLP, obj)
})


test_that("LP combo obj value calculated correctly when only one stratum split", {
  set.seed(78)
  samp_rows <- sample(1:nrow(rhc_X), 250)
  ncov <- 5
  nstrata <- 2
  X <- rhc_X[samp_rows, sample(1:ncol(rhc_X), ncov)]
  z <- rhc_X[samp_rows, "z"]
  ps <- prop_strat(z = z, X = X, nstrata = nstrata)
  ref <- refine(ps,
                options = list(criterion = "sum", ist = 2))
  m <- 0
  n_m <- 0
  for (st in 2) {
    smds <- abs(ref$details$pr[, z == 1 & ps$base_strata == st] %*%
                  ref$details$X_std[z == 1 & ps$base_strata == st, ] /
                  rowSums(ref$details$pr[, z == 1 & ps$base_strata == st]) -
                  ref$details$pr[, z == 0 & ps$base_strata == st] %*%
                  ref$details$X_std[z == 0 & ps$base_strata == st, ] /
                  rowSums(ref$details$pr[, z == 0 & ps$base_strata == st]))
    m <- m + sum(smds, na.rm = TRUE)
    n_m <- n_m + sum(!is.na(smds))
  }
  obj <- m / n_m
  expect_equal(ref$details$valueLP, obj)
})
