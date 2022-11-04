#' Calculate standardized mean differences for initial and refined strata
#'
#' Summarizes initial and/or refined strata in terms of standardized mean differences (SMDs).
#'
#' @inheritParams refine
#' @param base_strata optional initial stratification for which to calculate SMDs;
#' only used if `object` is not supplied
#' @param refined_strata optional refined stratification for which to calculate SMDs;
#' only used if `object` is not supplied
#' @param abs boolean whether to return absolute standardized mean differences or raw values.
#' Default is TRUE for absolute values
#'
#' @return List with two elements, "base" and "refined", each containing
#' a matrix of standardized mean differences for each stratum (row) and covariate (column).
#'
#' @export
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
#' # Look at covariate balance for propensity score and refined strata
#' calc_smds(object = ref)
#'

calc_smds <- function(object = NULL, z = NULL, X = NULL,
                            base_strata = NULL, refined_strata = NULL, abs = TRUE) {

  if (is.null(object)) {
    object <- strat(z = z, X = X, base_strata = base_strata, refined_strata = refined_strata)
  }

  if(!is.null(object$base_strata)) {
    st <- object$base_strata
    if(!is.factor(st)) {
      st <- as.factor(st)
    }
    ust <- levels(st)
    S <- length(ust)
    base_smds <- matrix(NA, nrow = S, ncol = ncol(object$details$X_std),
                        dimnames = list(ust, colnames(object$details$X_std)))
    for (i in 1:S) {
      if (sum(st == ust[i]) > 0) {
        ist <- ust[i]
        mean_t <- colMeans(object$details$X_std[st == ist & object$z == 1, , drop = FALSE])
        mean_c <- colMeans(object$details$X_std[st == ist & object$z == 0, , drop = FALSE])
        base_smds[i, ] <- mean_t-mean_c
      } else {
        base_smds[i, ] <- NA
      }
    }
  } else {
    base_smds <- NULL
  }

  if (!is.null(object$refined_strata)) {
    st <- object$refined_strata
    if(!is.factor(st)) {
      st <- as.factor(st)
    }
    ust <- levels(st)
    S <- length(ust)
    refined_smds <- matrix(NA, nrow = S, ncol = ncol(object$details$X_std),
                           dimnames = list(ust, colnames(object$details$X_std)))
    for (i in 1:S) {
      if (sum(st == ust[i], na.rm = TRUE) > 0) {
        ist <- ust[i]
        mean_t <- colMeans(object$details$X_std[st == ist & object$z == 1, , drop = FALSE])
        mean_c <- colMeans(object$details$X_std[st == ist & object$z == 0, , drop = FALSE])
        refined_smds[i, ] <- mean_t-mean_c
      } else {
        refined_smds[i, ] <- NA
      }
    }
  } else {
    refined_smds <- NULL
  }

  if (abs) {
    if (!is.null(base_smds)) {
      base_smds <- abs(base_smds)
    }
    if (!is.null(refined_smds)) {
      refined_smds <- abs(refined_smds)
    }
  }

  return(list("base" = base_smds, "refined" = refined_smds))
}
