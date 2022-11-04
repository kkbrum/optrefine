#' Generate P-values using empirical randomization null distribution
#'
#' Randomize the treatment assignment within strata to generate the randomization distribution of covariate balance
#' given the strata and observed covariate values. Compare the observed covariate balance to this null distribution
#' to calculate P-values.
#'
#' @inheritParams table_rand_pvals
#'
#' @details
#'
#' The literature on multivariate matching has recently developed a new way of evaluating covariate imbalances,
#' comparing the imbalances found in an observational matched sample to
#' the imbalances that would have been produced in the same data by randomization
#' (Pimentel et al. 2015, Yu 2021).
#' We modify that approach for use with strata, randomizing patients within strata.
#' For a given stratification, we create a large number of stratified randomized experiments,
#' taking the actual patients in their actual strata, and randomizing them to
#' treatment or control with fixed within-stratum sample sizes.
#'
#' To investigate how the actual observational imbalance in covariates compares to
#' covariate imbalance in the randomized experiments built from the same strata, patients and covariates,
#' we look at 4 metrics-- the scaled objective value, which is a weighted combination
#' of the maximum and the sum of all SMDs, depending on the `criterion` argument,
#' the maximum and average SMDs across covariates and strata,
#' and the average SMD across strata for each covariate individually.
#' For each of these metrics, we record the observational value,
#' the median and minimum of the randomized values,
#' and the proportion of randomized values more imbalanced than the observational value (the P-value).
#'
#' The `options` list argument can contain any of the following elements:
#' \itemize{
#'     \item{nrand: }{how many times to randomize the treatment assignment when forming the null distribution.
#'     Default is 10000}
#'     \item{criterion: }{which optimization criterion to use when calculating the objective value.
#' Options are "max", "sum", or "combo", referring to whether to include the
#' maximum standardized mean difference (SMD), the sum of all SMDs,
#' or a combination of the maximum and the sum. The default is "combo"}
#'     \item{wMax: }{how much to weight the maximum standardized mean difference compared to the sum.
#' Only used if criterion is set to "combo". Default is 5}
#'     \item{incl_base: }{whether to include columns for the initial stratification in the table.
#'     Default is `TRUE` if a base stratification is provided}
#' }
#' @return List with three components:
#' \itemize{
#'    \item{pvals: }{list containing `base` and `refined` elements, each of which is a list with randomization P-values
#'    for the objective value (`NULL` for the base stratification),
#'    the maximum standardized mean difference (SMD),
#'    the average SMD across covariates and strata,
#'    and for the average SMD across strata for each covariate (this element is a vector)}
#'    \item{obs_details: }{list containing `base` and `refined` elements, each of which is a list with
#'    the observed values for
#'    the objective value (`NULL` for the base stratification),
#'    the maximum standardized mean difference (SMD),
#'    and for the average SMD across strata for each covariate (this element is a vector)}
#'     \item{rand_details: }{list containing `base` and `refined` elements, each of which is a list with
#'    a vector of `nrand` randomized values for
#'    the objective value (`NULL` for the base stratification),
#'    the maximum standardized mean difference (SMD),
#'    and for the average SMD across strata for each covariate
#'    (this element is a matrix with `nrand` rows and a column for each covariate)}
#' }
#' @export
#' @import stats
#'
#' @examples
#' # Choose 500 patients and 5 covariates to work with for the example
#' set.seed(15)
#' samp <- sample(1:nrow(rhc_X), 500)
#' cov_samp <- sample(1:26, 5)
#'
#' # Let it create propensity score strata for you and then refine them
#' ref <- refine(X = rhc_X[samp, cov_samp], z = rhc_X[samp, "z"])
#'
#' # Calculate info for covariate balance randomization distribution
#' rpvals <- rand_pvals(object = ref, options = list(nrand = 100))
#'
#' # Look at pvals before and after
#' rpvals$pvals

rand_pvals <- function(object = NULL, z = NULL, X = NULL,
                       base_strata = NULL, refined_strata = NULL, options = list()) {

  if (is.null(object)) {
    object <- strat(z = z, X = X, base_strata = base_strata, refined_strata = refined_strata)
  }

  if (!is.null(options$criterion)) {criterion <- options$criterion} else {criterion <- "combo"}
  if (!is.null(options$nrand)) {nrand <- options$nrand} else {nrand <- 10000}
  if (!is.null(options$wMax)) {wMax <- options$wMax} else {wMax <- 5}
  if (!is.null(options$incl_base)) {incl_base <- options$incl_base} else {incl_base <- TRUE}

  smds <- calc_smds(object = object)
  n <- length(object$z)
  pvals <- list(base = NULL, refined = NULL)
  obs_details <- list(base = NULL, refined = NULL)
  rand_details <- list(base = NULL, refined = NULL)

  if (incl_base & !is.null(object$base_strata)) {
    smds_base_rand <- matrix(NA, nrow = nrand, ncol = ncol(object$X),
                             dimnames = list(NULL, colnames(object$X)))
    max_base_rand <- rep(NA, nrand)
    obj_base_rand <- NA
    n_st <- table(object$z, object$base_strata)
    for (irand in 1:nrand) {
      # Randomize the treated vs control units
      rand_z <- rep(0, n)
      for (lev in levels(object$base_strata)) {
        rand_z[object$base_strata == lev][
          sample(sum(n_st[, colnames(n_st) == lev]), n_st[2, colnames(n_st) == lev])] <- 1
      }

      # Calculate average imbalance across strata (not weighted)
      #    for each covariate using this random z vector
      smds_base_rand_temp <- calc_smds(object = new_strat(z = rand_z, X = object$X,
                                                          base_strata = object$base_strata,
                                                          details = list("X_std" = object$details$X_std)))$base
      smds_base_rand[irand, ] <- colMeans(smds_base_rand_temp, na.rm = TRUE)
      # Calculate max imbalance across cov and strata using random z
      max_base_rand[irand] <- max(smds_base_rand_temp, na.rm = TRUE)
    }

    pvals$base <- list("obj" = NA,
                       "max" = sum(max_base_rand > max(smds$base, na.rm = TRUE)) / nrand,
                       "avg" = sum(rowMeans(smds_base_rand) > mean(smds$base)) / nrand,
                       "smds" = rowSums(t(smds_base_rand) > colMeans(smds$base, na.rm = TRUE)) / nrand)
    obs_details$base <- list("obj" = NA, "max" = max(smds$base, na.rm = TRUE), "smds" = colMeans(smds$base, na.rm = TRUE))
    rand_details$base <- list("obj" = NA, "max" = max_base_rand, "smds" = smds_base_rand)
  }
  if (!is.null(object$refined_strata)) {

    # Prepare the weights for the objective calculation
    if (criterion == "sum") {
      wMax <- 0
      wEach <- 1
    } else if (criterion == "max") {
      wMax <- 1
      wEach <- 0
    } else {
      wEach <- 1
    }

    smds_ref_rand <- matrix(NA, nrow = nrand, ncol = ncol(object$X),
                            dimnames = list(NULL, colnames(object$X)))
    max_ref_rand <- rep(NA, nrand)
    obj_ref_rand <- rep(NA, nrand)
    n_st <- table(object$z, object$refined_strata)
    for (irand in 1:nrand) {
      # Randomize the treated vs control units
      rand_z <- rep(0, n)
      for (lev in levels(object$refined_strata)) {
        rand_z[object$refined_strata == lev][
          sample(sum(n_st[, colnames(n_st) == lev]), n_st[2, colnames(n_st) == lev])] <- 1
      }

      # Calculate average imbalance across strata (not weighted)
      #    for each covariate using this random z vector
      smds_ref_rand_temp <- calc_smds(object = new_strat(rand_z, object$X, refined_strata = object$refined_strata,
                                                         details = list("X_std" = object$details$X_std)))$refined
      smds_ref_rand[irand, ] <- colMeans(smds_ref_rand_temp, na.rm = TRUE)
      # Calculate max imbalance across cov and strata using random z
      max_ref_rand[irand] <- max(smds_ref_rand_temp, na.rm = TRUE)
      # Calculate overall objective using this random z vector
      obj_ref_rand[irand] <- (wEach * sum(smds_ref_rand_temp, na.rm = TRUE) +
                                wMax * sum(sapply(1:(length(levels(object$refined_strata))/2),
                                                  function(x) {max(smds_ref_rand_temp[(2*x-1):(2*x), ], na.rm = TRUE)}))) /
        (wEach * sum(!is.na(smds_ref_rand_temp)) + wMax * length(levels(object$refined_strata))/2)
    }

    obj_ref <- (wEach * sum(smds$refined, na.rm = TRUE) +
                  wMax * sum(sapply(1:(length(levels(object$refined_strata))/2),
                                    function(x) {max(smds$refined[(2*x-1):(2*x), ], na.rm = TRUE)}))) /
      (wEach * sum(!is.na(smds$refined)) + wMax * length(levels(object$refined_strata))/2)

    pvals$refined <- list("obj" = sum(obj_ref_rand > obj_ref) / nrand,
                          "max" = sum(max_ref_rand > max(smds$refined, na.rm = TRUE)) / nrand,
                          "avg" = sum(rowMeans(smds_ref_rand) > mean(smds$refined, na.rm = TRUE)) / nrand,
                          "smds" = rowSums(t(smds_ref_rand) > colMeans(smds$refined, na.rm = TRUE)) / nrand)
    obs_details$refined <- list("obj" = obj_ref, "max" = max(smds$refined, na.rm = TRUE), "smds" = colMeans(smds$refined, na.rm = TRUE))
    rand_details$refined <- list("obj" = obj_ref_rand, "max" = max_ref_rand, "smds" = smds_ref_rand)
  }

  return(list(pvals = pvals, obs_details = obs_details, rand_details = rand_details))
}

#' Generate a covariate balance table from the empirical randomization null distribution
#'
#' Generate a table using the information collected in \code{\link{rand_pvals}()}.
#' See \code{\link{rand_pvals}()} for more details about the methods used.
#'
#' @inheritParams refine
#' @param object an optional object of class `strat`,
#' typically created using \code{\link{strat}()}
#' or as a result of a call to \code{\link{prop_strat}()} or \code{\link{refine}()}.
#' If not provided, `z` and `X` must be specified
#' @param base_strata optional initial stratification for which to calculate
#' the empirical randomization null distribution;
#' only used if `object` is not supplied
#' @param refined_strata optional refined stratification for which to calculate
#' the empirical randomization null distribution;
#' only used if `object` is not supplied
#' @param options list of additional options, listed in the `details` below
#'
#'
#' @details The `options` list argument can contain any of the following elements:
#' \itemize{
#'     \item{nrand: }{how many times to randomize the treatment assignment when forming the null distribution.
#'     Default is 10000}
#'     \item{criterion: }{which optimization criterion to use when calculating the objective value.
#' Options are "max", "sum", or "combo", referring to whether to include the
#' maximum standardized mean difference (SMD), the sum of all SMDs,
#' or a combination of the maximum and the sum. The default is "combo"}
#'     \item{wMax: }{how much to weight the maximum standardized mean difference compared to the sum.
#' Only used if criterion is set to "combo". Default is 5}
#'     \item{incl_base: }{whether to include columns for the initial stratification in the table.
#'     Default is `TRUE` if a base stratification is provided}
#'     \item{rand_pvals: if already calculated, the returned list of information from \code{\link{rand_pvals}()}.
#' If `NULL`, this will be calculated}
#' }
#' @return Matrix with 4 or 8 columns, depending whether one or both of base and
#' refined strata are provided and the `incl_base` option.
#' The columns give the observed standardized mean difference or objective value,
#' the median and maximum across `nrand` null simulations, and the P-value which is the
#' proportion of the null simulations that have worse covariate balance than the observed value.
#' The top three rows give the scaled objective value and the average and maximum standardized mean differences across
#' all strata and covariates. The following rows, one for each covariate, give the standardized mean difference
#' for that covariate, averaged across strata. The first row for the scaled objective value is `NULL` for the base
#' stratification, if included, as the base stratification does not generally minimize a mathematical objective function.
#'
#' @export
table_rand_pvals <- function(object = NULL, z = NULL, X = NULL,
                             base_strata = NULL, refined_strata = NULL, options = list()) {

  if (is.null(object)) {
    object <- strat(z = z, X = X, base_strata = base_strata, refined_strata = refined_strata)
  }

  if (!is.null(options$criterion)) {criterion <- options$criterion} else {criterion <- "combo"}
  if (!is.null(options$nrand)) {nrand <- options$nrand} else {nrand <- 10000}
  if (!is.null(options$wMax)) {wMax <- options$wMax} else {wMax <- 5}
  if (!is.null(options$incl_base)) {incl_base <- options$incl_base} else {incl_base <- TRUE}
  if (!is.null(options$rand_pvals)) {rand_pvals <- options$rand_pvals} else {rand_pvals <- NULL}
  if (is.null(rand_pvals)) {
    rand_pvals <- rand_pvals(object = object, options = list(nrand = nrand, criterion = criterion,
                                                             wMax = wMax, incl_base = incl_base))
  }

  rand_pval_tab <- NULL
  if (incl_base & !is.null(object$base_strata)) {
    rand_pval_tab <- rbind(c(NA, NA, NA, NA),
                           c(rand_pvals$obs_details$base$max, median(rand_pvals$rand_details$base$max),
                             min(rand_pvals$rand_details$base$max), rand_pvals$pvals$base$max),
                           c(mean(rand_pvals$obs_details$base$smds), median(rowMeans(rand_pvals$rand_details$base$smds)),
                             min(rowMeans(rand_pvals$rand_details$base$smds)), rand_pvals$pvals$base$avg),
                           cbind((rand_pvals$obs_details$base$smds), apply(rand_pvals$rand_details$base$smds, 2, median),
                                 apply(rand_pvals$rand_details$base$smds, 2, min), rand_pvals$pvals$base$smds))
  }

  if (!is.null(object$refined_strata)) {
    rand_pval_tab <- cbind(rand_pval_tab,
                           rbind(c(rand_pvals$obs_details$refined$obj, median(rand_pvals$rand_details$refined$obj),
                                   min(rand_pvals$rand_details$refined$obj), rand_pvals$pvals$refined$obj),
                                 c(rand_pvals$obs_details$refined$max, median(rand_pvals$rand_details$refined$max),
                                   min(rand_pvals$rand_details$refined$max), rand_pvals$pvals$refined$max),
                                 c(mean(rand_pvals$obs_details$refined$smds), median(rowMeans(rand_pvals$rand_details$refined$smds)),
                                   min(rowMeans(rand_pvals$rand_details$refined$smds)), rand_pvals$pvals$refined$avg),
                                 cbind((rand_pvals$obs_details$refined$smds), apply(rand_pvals$rand_details$refined$smds, 2, median),
                                       apply(rand_pvals$rand_details$refined$smds, 2, min), rand_pvals$pvals$refined$smds)))
  }

  colnames(rand_pval_tab) <- rep(c("Obs", "Med",
                                   "Min", "P-val"), ncol(rand_pval_tab)/4)
  rownames(rand_pval_tab) <- c("Objective", "Max SMD", "Avg SMD", colnames(object$X))

  return(rand_pval_tab)
}
