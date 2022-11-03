#' Helper for object of class "strat"
#'
#' Creates an object of S3 class "strat"
#' @inheritParams prop_strat
#' @param base_strata Original strata, if they exist
#' @param refined_strata Refined strata, if they exist
#' @param details A list of details from the linear program.
#' Include X_std if calculated
#' @return Object of class `strat` if valid
#'
#' @export
#' @import stats
#'
#' @examples
#' # Don't need to include any stratification
#' strat_object <- strat(z = rhc_X[, "z"], X = rhc_X[, !(colnames(rhc_X) %in% "z")])
#'
#' # Can include base and/or refined stratification if desired
#' strat_object <- strat(z = rhc_X[, "z"], X = rhc_X[, !(colnames(rhc_X) %in% "z")],
#'                       base_strata = rep(1, nrow(rhc_X)),
#'                       refined_strata = NULL)

strat <- function(z, X, base_strata = NULL, refined_strata = NULL,
                  details = NULL) {

  if (is.data.frame(X)) X<-as.matrix(X)
  if (!is.null(base_strata) && !is.factor(base_strata)) {
    base_strata <- as.factor(base_strata)
  }
  if (!is.null(refined_strata) && !is.factor(refined_strata)) {
    refined_strata <- as.factor(refined_strata)
  }
  if (is.null(colnames(X))) {
    colnames(X) <- 1:ncol(X)
  }
  bad_covs <- colnames(X)[apply(X, 2, function(x) {length(unique(x))}) == 1]
  if (length(bad_covs) > 0) {
    warning(paste0("The following covariates will be removed due to having no variation: ", paste0(bad_covs, collapse = ", ")))
    X <- X[, !colnames(X) %in% bad_covs]
  }

  if (is.null(details) || is.null(details$X_std)) {
    X_std <- apply(X, 2, function(x) (x - mean(x)) / sd(x))
    X_std <- matrix(X_std, nrow = nrow(X), ncol = ncol(X))
    colnames(X_std) <- colnames(X)
    details[["X_std"]] <- X_std
  }

  return(validate_strat(new_strat(z, X, base_strata = base_strata,
                                  refined_strata = refined_strata, details = details)))
}


#' Validator for object of class "strat"
#'
#' Checks validity of an object of S3 class "strat"
#' @param object An object of class `strat`
#' @return Error or object of class `strat` if valid
#'
validate_strat <- function(object) {
  if(!all((object$z==1|(object$z==0)))) {
    stop("`z` must only contain 0s for control units and 1s for treated units")
  }
  stopifnot(is.matrix(object$X))
  if (length(object$z)!=(dim(object$X)[1])) {
    stop("length of `z` must match number of rows in `X`", call. = FALSE)
  }
  if (!is.null(object$base_strata)) {
    if(length(object$base_strata)!=length(object$z)) {
      stop("length of `base_strata` must match length of `z`", call. = FALSE)
    }  }
  if (!is.null(object$refined_strata)) {
    if(length(object$refined_strata)!=length(object$z)) {
      stop("length of `refined_strata` must match length of `z`", call. = FALSE)
    }
  }
  return(object)
}

#' Constructor for object of class "strat"
#'
#' Creates an object of S3 class "strat"
#' @inheritParams strat
#' @param details A list of details from the linear program.
#' Also includes `X_std` if calculated
#'
new_strat <- function(z, X, base_strata = NULL, refined_strata = NULL,
                      details = NULL) {

  structure(list("z" = z, "X" = X,
                 "base_strata" = base_strata, "refined_strata" = refined_strata,
                 "details" = details),
            class = "strat")
}

