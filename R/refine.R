#' Refine initial stratification
#'
#' Refine an initial stratification by splitting each stratum or specified subset of strata
#' into two refined strata. If no initial
#' stratification is provided, one is first generated
#' using \code{\link{prop_strat}()}.
#'
#' @param object an optional object of class `strat`,
#' typically created using \code{\link{strat}()}
#' or as a result of a call to \code{\link{prop_strat}()}.
#' If not provided, `z` and `X` must be specified
#' @param z vector of treatment assignment; only used if `object` is not supplied
#' @param X covariate matrix/data.frame; only used if `object` is not supplied
#' @param strata vector of initial strata assignments; only used if `object` is not supplied.
#' Can be `NULL`, in which case an initial stratification using the quintiles of the
#' propensity score is generated using \code{\link{prop_strat}()} and the generated
#' propensity score is also added to the X matrix as an extra covariate
#' @param options list containing various options described in the `Details` below
#'
#'
#' @details
#' The `options` argument can contain any of the following elements:
#' \itemize{
#'     \item{solver: }{character specifying the optimization software to use.
#'     Options are "Rglpk" or "gurobi". The default is "Rglpk" unless a gurobi installation is detected,
#'     in which case it is set to "gurobi". It is recommended to use "gurobi" if available.}
#'     \item{standardize: }{boolean whether or not to standardize the covariates in X. Default is `TRUE`}
#'     \item{criterion: }{which optimization criterion to use.
#' Options are "max", "sum", or "combo", referring to whether to optimize the
#' maximum standardized mean difference (SMD), the sum of all SMDs,
#' or a combination of the maximum and the sum. The default is "combo"}
#'     \item{integer: }{boolean whether to use integer programming as opposed to
#' randomized rounding of linear programs. Note that setting this to `TRUE`
#' may cause this function to never finish depending on the size of the data
#' and is not recommended except for tiny data sets}
#'     \item{wMax: }{how much to weight the maximum standardized mean difference compared to the sum.
#' Only used if criterion is set to "combo". Default is 5}
#'     \item{ist: }{which strata to split. Should be a level from the specified `strata` or
#' a vector of multiple levels. Default is to split all strata}
#'     \item{minsplit: }{The minimum number of treated and control units to allow in a refined stratum.
#' Default is 10}
#'     \item{threads: }{How many threads you'd like the optimization to use if using the "gurobi" solver. Uses all available threads by default}
#' }
#'
#' Note that setting a seed before using this function will ensure that the results are reproducible
#' on the same machine, but results may vary across machines due to how the optimization solvers work.
#'
#'
#' @return Object of class "strat", which is a list object with the following components:
#'  \itemize{
#'     \item{z: }{treatment vector}
#'     \item{X: }{covariate matrix}
#'     \item{base_strata: }{initial stratification}
#'     \item{refined_strata: }{refined_stratification}
#'     \item{details: }{various details about the optimization that can be ignored in practice, but may be interesting:
#'     \itemize{
#'         \item{valueIP, valueLP: }{integer (determined via randomized rounding, unless `integer` option set to true)
#'         and linear programming scaled objective values}
#'         \item{n_fracs: }{number of units with fractional LP solutions}
#'         \item{rand_c_prop, rand_t_prop: }{proportions of the control and treated units in each
#'         stratum that were selected with randomness}
#'         \item{pr: }{linear programming solution,
#'         with rows corresponding to the strata and columns to the units}
#'         \item{criterion: }{criterion used in the optimization (see the `details` about the `options` for the optimization)}
#'         \item{wMax: }{weight placed on the maximum standardized mean difference in the optimization
#'         (see the `details` about the `options` for the optimization)}
#'         \item{X_std: }{standardized version of `X`}
#'         }
#'     }
#' }
#'
#'
#' @export
#'
#' @examples
#' # Choose 400 patients and 4 covariates to work with for the example
#' set.seed(15)
#' samp <- sample(1:nrow(rhc_X), 400)
#' cov_samp <- sample(1:26, 4)
#'
#' # Let it create propensity score strata for you and then refine them
#' ref <- refine(X = rhc_X[samp, cov_samp], z = rhc_X[samp, "z"])
#'
#' # Or, specify your own initial strata
#' ps <- prop_strat(z = rhc_X[samp, "z"],
#'                  X = rhc_X[samp, cov_samp], nstrata = 3)
#' ref <- refine(X = ps$X, z = ps$z, strata = ps$base_strata)
#'
#' # Can just input the output of prop_strat() directly
#' ref <- refine(object = ps)
#'
#' @import MASS

refine <- function(object = NULL, z = NULL, X = NULL, strata = NULL,
                   options = list()) {

  stopifnot(is.list(options))
  stopifnot(all(names(options) %in% c("solver", "standardize", "criterion",
                                      "integer", "wMax", "ist", "threads", "minsplit")))
  if (is.null(options$solver)) {
    if(requireNamespace("gurobi", quietly = TRUE)) {
      solver <- "gurobi"
    } else {
      solver <- "Rglpk"
    }
  } else {
    solver <- options$solver
  }
  if (is.null(options$standardize)) {standardize <- TRUE} else {standardize <- options$standardize}
  if (is.null(options$criterion)) {criterion <- "combo"} else {criterion <- options$criterion}
  if (is.null(options$integer)) {integer <- FALSE} else {integer <- options$integer}
  if (is.null(options$wMax)) {wMax <- 5} else {wMax <- options$wMax}
  if (is.null(options$ist)) {ist <- NULL} else {ist <- options$ist}
  if (is.null(options$threads)) {threads <- NULL} else {threads <- options$threads}
  if (is.null(options$minsplit)) {min_split <- 10} else {min_split <- options$minsplit}

  stopifnot(solver %in% c("Rglpk", "gurobi"))
  stopifnot(is.logical(standardize))
  stopifnot(is.logical(integer))
  stopifnot(criterion %in% c("combo", "max", "sum"))
  stopifnot(!is.null(object) || (!is.null(z) && !is.null(X)))

  if (is.null(object)) {
    object <- strat(z = z, X = X, base_strata = strata)
  }
  if (is.null(object$base_strata)) {
    object <- prop_strat(object$z, object$X, 5)
  }
  if (standardize){
    X <- object$details$X_std
  } else {
    X <- object$X
  }
  stopifnot(is.null(ist) || all(ist %in% levels(object$base_strata)))

  # Only split the strata indicated
  if (is.null(ist)) {
    u <- (levels(object$base_strata))
  } else {
    u <- ist
  }
  # If any strata don't have both treated and control units, don't split them
  bad_st <- NULL
  for (lev in u) {
    if (length(unique(object$z[object$base_strata == lev])) != 2) {
      bad_st <- c(bad_st, lev)
    }
  }
  u <- u[!u %in% bad_st]
  if (!is.null(bad_st)) {
    warning(paste0("The following initial strata have no treated and/or control units so will not be split further: ", paste0(bad_st, collapse = ", ")))
  }


  s <- rep(NA,length(object$z))

  if (criterion == "sum") {
    wMax <- 0
    wEach <- 1
  } else if (criterion == "max") {
    wMax <- 1
    wEach <- 0
  } else {
    # wMax is already an argument, defaulted to 5
    wEach <- 1
  }

  pr <- matrix(NA, nrow = 2, ncol = length(object$z))
  objLP <- 0
  objIP <- 0
  n_obj <- 0
  n_fracs <- 0
  rand_c_prop <- c()
  rand_t_prop <- c()

  for (i in 1:length(u)){
    who<-object$base_strata==u[i]
    Nc <- sum(1-object$z[who])
    Nt <- sum(object$z[who])
    # Only need to go up to 1/2 for either control or treated since it would be redundant
    # to just swap the roles of the strata in both lists
    nc_list <- lapply(list(0,1/5,1/4,1/3,1/2), function(x) {
      c(floor(round(x * Nc, 10)), ceiling(round((1-x) * Nc, 10)))
    })
    # Don't need to include 1 because the 0 controls can only be paired with 0 treated
    # so don't need to reverse the roles of strata
    nt_list <- lapply(list(0,1/5,1/4,1/3,1/2,2/3,3/4,4/5), function(x) {
      c(floor(round(x * Nt, 10)), ceiling(round((1-x) * Nt, 10)))
    })

    best<-best_split(object$z, X = X, strata = object$base_strata, ist = u[i],
                     nc_list = nc_list, nt_list = nt_list,
                     wMax = wMax, wEach = wEach, solver = solver,
                     integer = integer, min_split = min_split, threads = threads)
    si <- best$selection
    s[who]<-si
    objLP <- (n_obj * objLP  + best$n_smds * best$valuesLP[best$besti, best$bestj]) / (n_obj + best$n_smds)
    objIP <- (n_obj *objIP + best$n_smds * best$valuesIP[best$besti, best$bestj]) / (n_obj + best$n_smds)
    n_obj <- n_obj + best$n_smds
    n_fracs <- n_fracs + best$n_fracs
    rand_c_prop <- c(rand_c_prop, best$rand_c_prop)
    rand_t_prop <- c(rand_t_prop, best$rand_t_prop)
    pr[, who] <- best$pr
  }

  refined_strata <- factor(paste0(object$base_strata, ":1"), levels = unlist(lapply(levels(object$base_strata), function(x) return(paste0(x, c(":1", ":2"))))))
  refined_strata[object$base_strata %in% u] <- object$base_strata[object$base_strata %in% u]:factor(s[object$base_strata %in% u], levels= c(1, 2))

  return(new_strat(z = object$z, X = object$X,
                   base_strata = object$base_strata, refined_strata = refined_strata,
                   details = list(valueLP = objLP, valueIP = objIP, n_fracs = n_fracs,
                                  rand_c_prop = rand_c_prop, rand_t_prop = rand_t_prop,
                                  pr = pr, wMax = wMax, criterion = criterion,
                                  X_std = object$details$X_std)))
}
