
#' Form propensity score strata
#'
#' Form initial propensity score strata
#' to be improved upon by \code{\link{refine}()}.
#'
#' @param z Vector of treatment assignment
#' @param X Covariate matrix or data.frame
#' @param nstrata The number of strata to form
#'
#' @return Object of class "strat", which is a list containing `z`,
#' `X` with the propensity score as an additional column, `base_strata`
#' (a factor of the resulting propensity score strata), and `details`,
#' (a list containing `X_std`, which is the standardized version of the new `X`)
#'
#' @export
#' @import stats
#'
#' @examples
#'  ps <- prop_strat(z = rhc_X[, "z"],
#'                   X = rhc_X[,  !(colnames(rhc_X) %in% c("pr", "z"))])
#' table(rhc_X[, "z"], ps$base_strata)
prop_strat <- function(z, X, nstrata = 5) {
  pr <- glm(z ~ X, family = binomial)$fitted.values
  qts <- quantile(pr, c(0, (1:(nstrata - 1))/nstrata, 1))
  if (length(qts) != length(unique(qts))) {
    stop("The quantiles of the propensity score are not unique. Need to lower the `nstrata` accordingly.",
         call. = FALSE)
  }
  X <- cbind(X, pr) # add propensity score to X
  st <- cut(pr, qts, labels = FALSE, include.lowest = TRUE) # propensity score strata

  # If any strata don't have both treated and control units, give warning message
  bad_st <- NULL
  for (lev in unique(st)) {
    if (length(unique(z[st == lev])) != 2) {
      bad_st <- c(bad_st, lev)
    }
  }
  if (!is.null(bad_st)) {
    warning(paste0("The following propensity score strata have no treated and/or control units: ", paste0(bad_st, collapse = ", ")))
  }

  return(strat(z = z, X = X, base_strata = st))
}

