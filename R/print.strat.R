

#' Print stratification object
#'
#' Print method for class "strat".
#' Prints tables of numbers of control and treated units without strata and in initial and/or improved strata.
#' Also displays average and maximum standardized mean difference for each stratification.
#'
#' @param x object of S3 class 'strat'
#' @param ... further arguments passed to or from other methods
#'
#' @return Returns `x` back invisibly and prints tables and statistics to the console
#'
#' @export
#'
#' @examples
#' # Choose 750 patients and 5 covariates to work with for the example
#' set.seed(21)
#' samp <- sample(1:nrow(rhc_X), 750)
#' cov_samp <- sample(1:26, 5)
#' ref <- refine(X = rhc_X[samp, cov_samp], z = rhc_X[samp, "z"])
#' print(ref)

print.strat <- function(x, ...) {
  cat(paste0(dim(x$X)[1], " units and ", dim(x$X)[2], " covariates\n"))
  smds_no_st <- calc_smds(z = x$z, X = x$X, base_strata = rep(1, length(x$z)))
  smds <- calc_smds(x)

  cat("\nNo stratification")
  cat("\n---------------------- \n")
  avg_no_st <- mean(smds_no_st$base, na.rm = TRUE)
  max_no_st <- max(smds_no_st$base, na.rm = TRUE)
  max_no_st_inds <- which(smds_no_st$base == max_no_st, arr.ind = TRUE)
  print(table(x$z, dnn = list("Treatment")))
  cat(paste0("Average standardized mean difference across covariates and strata: ", round(avg_no_st, 3)))
  cat(paste0("\nMaximum standardized mean difference across covariates and strata: ",
             round(max_no_st, 3), " (", colnames(smds_no_st$base)[max_no_st_inds[1, 2]], ")\n"))

  cat("\nOriginal stratification")
  cat("\n---------------------- \n")
  if(!is.null(x$base_strata)) {
    avg_base <- mean(smds$base, na.rm = TRUE)
    max_base <- max(smds$base, na.rm = TRUE)
    max_base_inds <- which(smds$base == max_base, arr.ind = TRUE)
    print(table(x$z, x$base_strata, dnn = list("Treatment", "Base stratum")))
    cat(paste0("Average standardized mean difference across covariates and strata: ", round(avg_base, 3)))
    cat(paste0("\nMaximum standardized mean difference across covariates and strata: ",
               round(max_base, 3), " (", colnames(smds$base)[max_base_inds[1, 2]], " for stratum ",
               rownames(smds$base)[max_base_inds[1, 1]], ")\n"))
  } else {
    cat("None\n")
  }

  cat("\nRefined stratification")
  cat("\n---------------------- \n")
  if (!is.null(x$refined_strata)) {
    avg_refined <- mean(smds$refined, na.rm = TRUE)
    max_refined <- max(smds$refined, na.rm = TRUE)
    max_refined_inds <- which(smds$refined == max_refined, arr.ind = TRUE)
    print(table(x$z, x$refined_strata, dnn = list("Treatment", "Refined stratum")))
    cat(paste0("Average standardized mean difference across covariates and strata: ", round(avg_refined, 3)))
    cat(paste0("\nMaximum standardized mean difference across covariates and strata: ",
               round(max_refined, 3), " (", colnames(smds$refined)[max_refined_inds[1, 2]], " for stratum ",
               rownames(smds$refined)[max_refined_inds[1, 1]], ")\n"))
  } else {
    cat("None\n")
  }

  return(invisible(x))
}
