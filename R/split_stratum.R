#' Split one stratum into multiple strata
#'
#' Split one stratum into multiple with specified sample sizes.
#'
#' @inheritParams prop_strat
#' @inheritParams refine
#' @param ist the stratum to be split
#' @param nc a vector stating how many control units to place in
#' each of the new split strata. The sum must be the total number of controls
#' in the stratum to be split
#' @param nt a vector stating how many treated units to place in
#' each of the new split strata. The sum must be the total number of treated units
#' in the stratum to be split
#' @param wMax the weight the objective places on the maximum epsilon
#' @param wEach the weight the objective places on each epsilon
#' @param solver character specifying the optimization software to use.
#'     Options are "Rglpk" or "gurobi". The default is "gurobi"
#' @param integer boolean whether to use integer programming instead of randomized rounding.
#'     Default is `FALSE`. It is not recommended to set this to `TRUE` as the problem may never finish
#' @param threads how many threads to use in the optimization if using "gurobi" as the solver. Default will use all available threads
#'
#' @return A list containing the following elements:
#' \itemize{
#'     \item{valueIP, valueLP: }{integer and linear programming scaled objective values}
#'     \item{n_smds: }{number of standardized mean differences contributing to the objective values
#' (multiply the scaled objective values by this number to get the true objective values)}
#'     \item{n_fracs: }{the number of units with fractional linear programming solutions}
#'     \item{rand_c_prop, rand_t_prop: }{proportions of the control and treated units in each stratum
#'     that were selected with randomness}
#'     \item{pr: }{linear programming solution, with rows corresponding to the strata and columns to the units}
#'     \item{selection: }{vector of selected strata for each unit in the initial stratum to be split}
#' }
#'
#' @examples
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
#' split_stratum(z = ps$z, X = ps$X, strata = ps$base_strata, ist = 1,
#'            nc = c(floor(tab[1, 1] * 0.25), ceiling(tab[1, 1] * 0.75)),
#'            nt = c(floor(tab[2, 1] * 0.3), ceiling(tab[2, 1] * 0.7)))
#'
#' @import Rglpk
#' @import sampling
#' @import stats
#' @export

split_stratum <- function(z, X, strata, ist, nc, nt,
                          wMax = 5, wEach = 1, solver = "Rglpk",
                          integer = FALSE, threads = NULL) {
  stopifnot(ist %in% unique(strata))
  if (is.data.frame(X)) X <- as.matrix(X)
  stopifnot(is.matrix(X))
  stopifnot(is.vector(z) & (all((z == 0) | (z == 1))))
  stopifnot(length(z) == (dim(X)[1]))
  stopifnot(is.vector(wMax) & (length(wMax) == 1) & (wMax >= 0))
  stopifnot(is.vector(wEach) & (length(wEach) == 1) & (wEach >= 0))
  stopifnot((wMax + wEach) > 0)

  X_full <- X
  X <- X[strata == ist, ]
  z_full <- z
  z <- z[strata == ist]

  stopifnot(is.vector(nc) & is.vector(nt) &
              (sum(nc) == sum(1-z)) & (sum(nt) == sum(z)) &
              (length(nc) == length(nt)))

  # S is the number of strata to be split into
  # This is 2 for now, but coded such that it would be easy to split into more
  S <- length(nc)

  I<-dim(X)[1]
  J<-dim(X)[2]
  # SI decision vars (whether or not to include in each new stratum),
  # 2SJ epsilons (pos and neg for each new stratum and cov),
  # and one var equal to max epsilon
  nvar <- S*I + (2*S*J) + 1

  # Constrain a and b to add to 1
  A <- matrix(0, nrow = I + S * J + 2 * S + S * J, ncol = S * I + 2 * S * J + 1)
  for (irow in 1:I) {
    A[irow, (0:(S-1)) * I + irow] <- 1
  }
  rhs1 <- rep(1, I)
  sense1 <- rep("=", I)
  sense1_rglpk <- rep("==", I)

  # Constrain a, b, eps to be nonnegative
  lb <- rep(0, nvar)
  ub <- c(rep(1, S*I), rep(Inf, 2*S*J + 1))

  # Constraints that define the eps
  X1 <- outer(z, rep(1, J),"*") * X  # treated X
  X0 <- outer(1-z, rep(1, J),"*") * X  # control X
  id <- diag(1, J, J)
  idpm <- cbind(id, -id)
  Sadj <- S
  for (s in 1:S) {
    if (nt[s] > 0 & nc[s] > 0) {
      A[(I + (s-1) * J + 1) : (I + s * J), ((s-1) * I + 1):(s*I)] <- t(X1)/nt[s] - t(X0)/nc[s]
    } else {
      Sadj <- Sadj - 1
    }
    A[(I + (s-1) * J + 1) : (I + s * J), (S*I + 2*(s-1)*J + 1):(S*I + 2*s*J)] <-  idpm
  }
  sense3 <- rep("=", S*J)
  sense3_rglpk <- rep("==", S*J)
  rhs3 <- rep(0, S*J)

  # Sample size constraints
  rhs4 <- NULL
  for (s in 1:S) {
    A[(I + S * J + 2 * s - 1): (I + S * J + 2 * s), ((s-1) * I + 1): (s*I)] <- rbind(z, 1-z)
    rhs4 <- c(rhs4, nt[s], nc[s])
  }
  sense4 <- rep("=", 2*S)
  sense4_rglpk <- rep("==", 2*S)

  # Define maximum constraints
  m1J <- rep(-1,J)
  idid <- cbind(id, id)
  for (s in 1:S) {
    A[(I+S*J+2*S + (s-1)*J + 1): (I+S*J+2*S + s*J), (S*I + 2*(s-1)*J + 1):(S*I + 2*s*J)] <- idid
    A[(I+S*J+2*S + (s-1)*J + 1): (I+S*J+2*S + s*J), S*I+2*S*J+1] <- m1J
  }
  sense5 <- rep("<", S*J)
  sense5_rglpk <- rep("<=", S*J)
  rhs5 <- rep(0, S*J)

  # Obj is the avg imbalance across all nonempty strata/covs
  obj <- c(rep(0, S*I), rep(wEach, 2*S*J), wMax)
  obj <- obj / (wEach * Sadj * J + wMax)

  # Set up model to pass to gurobi or rglpk
  model <- list(A = A, obj = obj, modelsense = "min",
                rhs = c(rhs1, rhs3, rhs4, rhs5))
  if (integer) model$vtype <- c(rep("B", S*I), rep("C", 2*S*J + 1))

  if (solver == "gurobi") {
    model$sense <- c(sense1, sense3, sense4, sense5)
    gurobi_seed <- sample(1:100000, 1)
    if (is.null(threads)) {
      result <- gurobi::gurobi(model, list(OutputFlag = 0, seed = gurobi_seed, method = 4))
    } else {
      result <- gurobi::gurobi(model, list(OutputFlag = 0, seed = gurobi_seed, method = 4, threads = threads))
    }
    pr <- matrix(result$x[1:(S*I)], nrow = S, byrow = TRUE)
    v <- result$objval
  } else {
    model$sense <- c(sense1_rglpk, sense3_rglpk, sense4_rglpk, sense5_rglpk)
    result <- Rglpk::Rglpk_solve_LP(obj = model$obj,
                                    mat = model$A,
                                    dir = model$sense,
                                    rhs = model$rhs,
                                    bounds = list(lower = list(ind = 1:nvar, val = lb),
                                                  upper = list(ind = 1:nvar, val = ub)),
                                    types = model$vtype)
    pr <- matrix(result$solution[1:(S*I)], nrow = S, byrow = TRUE)
    v <- result$optimum
  }

  best_valueIP <- Inf
  best_selection <- NULL
  st_mat <- round(pr, 5)
  # How many columns have any non 0 or 1 entries?
  n_fracs <- sum(colSums(! (st_mat == 1 | st_mat == 0)) > 0)
  # What proportion of controls and treated were randomly selected into each stratum?
  rand_c_prop <- sapply(1:S, function(x) {(sum(st_mat[x, z == 0]) - sum(st_mat[x, z == 0] == 1)) / sum(st_mat[x, z == 0])})
  rand_t_prop <- sapply(1:S, function(x) {(sum(st_mat[x, z == 1]) - sum(st_mat[x, z == 1] == 1)) / sum(st_mat[x, z == 1])})

  # Randomized rounding
  n_rr <- 10
  for (rr_i in 1:n_rr) {
    st_mat <- round(pr, 5)
    if(!integer) {
      if (S > 2) {
        # (sample sizes only correct in expectation since we draw independently for each unit)
        for (i in 1:I) {
          if (!all(st_mat[, i] %in% c(0,1))) {
            st_mat[, i] <- rmultinom(1, 1, st_mat[, i])
          }
        }
      }
      else {
        # sample sizes correct but only works for 2 strata. dependent sampling
        if(!all(st_mat[1, ] %in% c(0,1))) {
          drawn_lp <- which(st_mat[1, ] == 1)
          frac_t <- (st_mat[1, ] < 1) & (st_mat[1, ] > 0) & z == 1
          needed_t <- round(sum(st_mat[1, frac_t]), 3)
          if (needed_t == 0) {
            draw_t <- NULL
          } else if(needed_t == 1) {
            draw_t <- which(frac_t)[sample(1:sum(frac_t), size = 1, prob = pr[1, frac_t])]
          } else {
            draw_t <- which(frac_t)[sampling::UPmidzuno(pr[1, frac_t]) == 1]
          }

          frac_c <- (st_mat[1, ] < 1) & (st_mat[1, ] > 0) & z == 0
          needed_c <- round(sum(st_mat[1, frac_c]), 3)
          if (needed_c == 0) {
            draw_c <- NULL
          } else if(needed_c == 1) {
            draw_c <- which(frac_c)[sample(1:sum(frac_c), size = 1, prob = pr[1, frac_c])]
          } else {
            draw_c <- which(frac_c)[sampling::UPmidzuno(pr[1, frac_c]) == 1]
          }

          st_mat[1, ] <- 0
          st_mat[1, c(drawn_lp, draw_t, draw_c)] <- 1
          st_mat[2, ] <- 1 - st_mat[1, ]
        }
      }
    }
    st <- which(st_mat == 1, arr.ind = TRUE)[, 1]
    Sadj <- length(unique(st))

    # Evaluate covariate balance of best integer solution
    eval <- NULL
    for (s in 1:S) {
      if (sum(z == 1 & st == s) > 0) {
        tmean <- colMeans(X[z == 1 & st == s, , drop = FALSE])
      } else {
        tmean <- rep(NA, J)
      }
      if (sum(z == 0 & st == s) > 0) {
        cmean <- colMeans(X[z == 0 & st == s, , drop = FALSE])
      } else {
        cmean <- rep(NA, J)
      }
      eval <- rbind(eval, tmean, cmean)
    }
    rownames(eval)<-c(paste0(c("T", "C"), rep(1:S, each = 2)))

    epsIP <- matrix(NA,2*S+1,J)
    for (s in 1:S) {
      epsIP[(2*(s-1)+1), ] <- pmax(0, eval[2*s, ] - eval[2*s-1, ])
      epsIP[2*s, ] <- pmax(0, eval[2*s-1, ] - eval[2*s, ])
    }
    epsIP[(2*S + 1),]<-apply(epsIP[1:(2*S),, drop = FALSE], 2, max, na.rm = TRUE)
    rownames(epsIP)<-c(paste(c("Pos eps","Neg eps"), rep(1:S, each = 2)), "max")
    if (!is.null(colnames(X))) colnames(epsIP)<-colnames(X)
    epsIP_nona <- epsIP
    epsIP_nona[is.na(epsIP)] <- 0

    valueIP <- (wEach * sum(epsIP_nona[1:(2*S),]) + wMax * max(epsIP_nona[2*S + 1, ], na.rm = TRUE)) / (wEach * J * Sadj + wMax)
    if (valueIP < best_valueIP) {
      best_valueIP <- valueIP
      best_selection <- st
    }
  }

  return(list(valueIP = best_valueIP, valueLP = v, n_smds = wEach * J * Sadj + wMax, n_fracs = n_fracs,
              rand_c_prop = rand_c_prop, rand_t_prop = rand_t_prop, pr = pr, selection = best_selection))
}

