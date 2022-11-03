#' Find the best split for a stratum
#'
#' Runs \code{\link{split_stratum}()} many times and selects the best result.
#'
#' @inheritParams split_stratum
#' @param nc_list a list of choices for the `nc` parameter in \code{\link{split_stratum}()}.
#' Each element is a vector with entries corresponding to the number of control
#' units that should be placed in each new stratum
#' @param nt_list a list of choices for the `nt` parameter in \code{\link{split_stratum}()}.
#' Each element is a vector with entries corresponding to the number of treated
#' units that should be placed in each new stratum
#' @param min_split a numeric specifying the minimum number of each control and treated units
#'  to be tolerated in a stratum. Any combination of elements
#' from `nc_list` and `nt_list` that violate this are skipped
#'
#' @return A list containing the following elements:
#' \itemize{
#'     \item{valuesIP, valuesLP: }{matrices containing integer and linear programming
#' scaled objective values for each sample size tried, with rows corresponding to the
#' elements of `nc_list` and columns corresponding to the elements of `nt_list`}
#'     \item{besti, bestj: }{indices of the best sample sizes in `nc_list` and in `nt_list`, respectively}
#'     \item{n_smds: }{number of standardized mean differences contributing to the objective values
#' (multiply the scaled objective values by this number to get the true objective values)}
#'     \item{n_fracs: }{number of units with fractional LP solutions in the best split}
#'     \item{rand_c_prop, rand_t_prop: }{proportions of the control and treated units in each
#'     stratum that were selected with randomness for the best split}
#'     \item{pr: }{linear programming solution for the best split,
#'     with rows corresponding to the strata and columns to the units}
#'     \item{selection: }{vector of selected strata for each unit
#'     in the initial stratum to be split for the best split}
#' }
#'
#' @export
#' @examples
#'
#' # Generate a small data set
#' set.seed(25)
#' samp <- sample(1:nrow(rhc_X), 1000)
#' cov_samp <- sample(1:26, 10)
#'
#' # Create some strata
#' ps <- prop_strat(z = rhc_X[samp, "z"],
#'                  X = rhc_X[samp, cov_samp], nstrata = 5)
#'
#' # Save the sample sizes
#' tab <- table(ps$z, ps$base_strata)
#'
#' # Choose the best sample sizes among the options provided
#' best_split(z = ps$z, X = ps$X, strata = ps$base_strata, ist = 1,
#'            nc_list = list(c(floor(tab[1, 1] * 0.25), ceiling(tab[1, 1] * 0.75)),
#'                           c(floor(tab[1, 1] * 0.4), ceiling(tab[1, 1] * 0.6))),
#'            nt_list = list(c(floor(tab[2, 1] * 0.3), ceiling(tab[2, 1] * 0.7))),
#'            min_split = 5)


best_split <- function(z, X, strata, ist, nc_list, nt_list,
                       wMax = 5, wEach = 1, solver = "Rglpk",
                       integer = FALSE, min_split = 10, threads = threads){

  stopifnot(length(ist) == 1)
  if (!all(lapply(nc_list, sum) == sum(1-z[strata == ist]))) {
    stop("Not all control sample size options sum to the number of controls", call. = FALSE)
  }
  if (!all(lapply(nt_list, sum) == sum(z[strata == ist]))) {
    stop("Not all treated sample size options sum to the number of treated units", call. = FALSE)
  }
  o <- vector(mode="list", length = length(nc_list) * length(nt_list))
  vIP <- matrix(NA, length(nc_list), length(nt_list),
                dimnames = list("nc_option" = 1:length(nc_list), "nt_option" = 1:length(nt_list)))
  vLP <- vIP
  bestv <- Inf
  besti <- NA
  bestj <- NA
  k <- 0
  allBAD <- TRUE

  for (i in 1:length(nc_list)) {
    for (j in 1:length(nt_list)) {
      nc <- nc_list[[i]]
      nt <- nt_list[[j]]
      k <- k+1
      empty_st <- any((nc == 0) & (nt == 0))
      if (is.null(min_split) || empty_st || (min(nc) >= min_split & min(nt) >= min_split)) {
        allBAD <- FALSE # At least one split was possible
        ok <- split_stratum(z = z, X = X, strata = strata,
                            ist = ist, nc = nc, nt = nt,
                            wMax = wMax, wEach = wEach,
                            solver = solver, integer = integer,
                            threads = threads)
        o[[k]] <- ok
        vIP[i,j] <- ok$valueIP
        vLP[i,j] <- ok$valueLP
        if (ok$valueIP < bestv) {
          bestv <- ok$valueIP
          besti <- i
          bestj <- j
          bestk <- k
        }
      }
    }
  }

  if (allBAD){
    stop("Error in call to best_split(). No pair of elements from nc_list and nt_list satisfied the min_split requirement.", call. = FALSE)
  }
  k <- bestk

  return(list(valuesIP=vIP, valuesLP=vLP, besti=besti, bestj=bestj,
              n_smds = o[[k]]$n_smds, n_fracs = o[[k]]$n_fracs,
              rand_c_prop = o[[k]]$rand_c_prop, rand_t_prop = o[[k]]$rand_t_prop,
              pr = o[[k]]$pr, selection=o[[k]]$selection))
}

