
#' Plot diagnostics for a "strat" object
#'
#'  Plots the standardized mean differences for `strat` objects in the format of Love (2002).
#'
#' @inheritParams print.strat
#' @param incl_none whether to plot imbalances before any stratification
#' @param incl_base whether to plot imbalances for the base stratification (if one exists)
#' @param by_strata whether to generate a list of plots, one for each base stratum if
#' `incl_base` is `TRUE`, or one for each refined stratum if `incl_base` is `FALSE`.
#' Not used if `incl_none` is `TRUE`
#' @param weighted_avg whether to take the weighted average instead of the straight average
#' when collapsing standardized mean differences across strata. Default is `FALSE`
#' @param legend a vector of labels to use for the three stratifications on the plot.
#' The corresponding label for any stratification that is not to be plotted must
#' still be provided but will be ignored and
#' can be set to `NA`
#'
#' @return Either a ggplot object for the Love plot of standardized mean differences or
#' a list of such ggplot objects if `by_strata` is `TRUE`
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' # Choose 800 patients and 5 covariates to work with for the example
#' set.seed(28)
#' samp <- sample(1:nrow(rhc_X), 800)
#' cov_samp <- sample(1:26, 5)
#' ref <- refine(X = rhc_X[samp, cov_samp], z = rhc_X[samp, "z"])
#' plot(ref)
#'
#' @references Love, T. E. (2002), "Displaying covariate balance after adjustment for selection bias",
#' Joint Statistical Meetings, yumpu.com/en/document/read/41664623.

plot.strat <- function(x, incl_none = TRUE, incl_base = TRUE,
                       by_strata = FALSE, weighted_avg = FALSE,
                       legend = c("No strata", "Base strata", "Refined strata"),
                       ...) {

  if (length(legend) != 3) {
    stop("Length of `legend` must be 3.")
  }

  if (incl_base & is.null(x$base_strata)) {
    warning("No initial stratification was given in the `strat` object `x`. `incl_base` has thus been switched to `FALSE`.")
    incl_base <- FALSE
  }
  if (!incl_base) {
    x$base_strata <- NULL
  }
  if (!incl_none & !incl_base & is.null(x$refined_strata)) {
    stop("`incl_none` and `incl_base` are both set to `FALSE` while the `strat` object `x` has no refined strata. There is thus nothing to plot.")
  }
  if (by_strata & !incl_base & is.null(x$refined_strata)) {
    warning("`incl_base` is set to `FALSE` while the `strat` object `x` has no refined strata. `by_strata` has thus been switched to `FALSE`.")
    by_strata <- FALSE
  }
  if (by_strata & incl_none) {
    warning("Cannot plot by stratum if `incl_none` is `TRUE`. `by_strata` has thus been switched to `FALSE`.")
    by_strata <- FALSE
  }

  smds <- calc_smds(object = x)
  if (incl_base) {
    if(weighted_avg) {
      smds$base_nona <- smds$base
      smds$base_nona[is.na(smds$base)] <- 0
      smds_base_avg <- (table(x$base_strata) %*% smds$base_nona) / length(x$z)
    } else {
      smds_base_avg <- colMeans(smds$base, na.rm = TRUE)
    }
  }

  if (!is.null(x$refined_strata)) {
    if(weighted_avg) {
      smds$refined_nona <- smds$refined
      smds$refined_nona[is.na(smds$refined)] <- 0
      smds_refined_avg <- (table(x$refined_strata) %*% smds$refined_nona) / length(x$z)
    } else {
      smds_refined_avg <- colMeans(smds$refined, na.rm = TRUE)
    }
  }

  if (incl_none) {
    smds_no_st <- calc_smds(z = x$z, X = x$X, base_strata = rep(1, length(x$z)))
    ordering <- order(smds_no_st$base)
    smds_no_st$base <- smds_no_st$base[1, ordering]
  } else if (incl_base) {
    ordering <- order(smds_base_avg)
  } else {
    ordering <- order(smds_refined_avg)
  }

  if (incl_base) {
    smds$base <- smds$base[, ordering]
    smds_base_avg <- smds_base_avg[ordering]
  }
  if (!is.null(x$refined_strata)) {
    smds$refined <- smds$refined[, ordering]
    smds_refined_avg <- smds_refined_avg[ordering]
  }
  covariates_plot <- factor(colnames(x$X)[ordering], levels = colnames(x$X)[ordering])

  if (by_strata & incl_base) {

    legend_new <- c(legend[2])
    num_incl <- 1
    abs_stand_diff <- c(t(smds$base))

    if (!is.null(x$refined_strata)) {

      legend_new <- c(legend_new, legend[3])
      num_incl <- 2
      if (!weighted_avg) {
        smds_refined_collapsed <- t(sapply(1:nrow(smds$base), function(s) {
          colMeans(smds$refined[(2*s-1):(2*s), ])
        }))
      } else {

        smds_refined_collapsed <- t(sapply(1:nrow(smds$base), function(s) {
          (table(x$refined_strata)[(2*s-1):(2*s)] %*% smds$refined_nona[(2*s-1):(2*s), ]) / table(x$base_strata)[s]
        }))
      }
      abs_stand_diff <- c(abs_stand_diff, c(t(smds_refined_collapsed)))
    }

    plot_dataframe <- data.frame(abs_stand_diff = abs_stand_diff,
                                 covariates = rep(covariates_plot, nrow(smds$base) * num_incl),
                                 type = factor(rep(legend_new, each = length(covariates_plot) * nrow(smds$base)),
                                               levels = legend_new),
                                 stratum = rep(rep(row.names(smds$base), each = length(covariates_plot)), num_incl))

    p <- apply(as.array(levels(x$base_strata)), 1, function(s) {
      ggplot(plot_dataframe[plot_dataframe$stratum == s,],
             aes(x = .data$abs_stand_diff, y = .data$covariates)) +
        geom_point(size = 5, aes(shape = .data$type)) +
        scale_shape_manual(values = c(1, 4)) +
        geom_vline(xintercept = c(.1,.2), lty = 2) +
        xlab("Absolute standardized difference") +
        ylab("Covariate") +
        xlim(c(0, max(plot_dataframe$abs_stand_diff + 0.1))) +
        labs(shape = "") +
        ggtitle(s) +
        theme(text = element_text(size=10), strip.text.x = element_text(size = 12))
    })
    names(p) <- levels(x$base_strata)

  } else if(by_strata & !incl_base) {

    plot_dataframe <- data.frame(abs_stand_diff = c(t(smds$refined)),
                                 covariates = rep(covariates_plot, nrow(smds$refined)),
                                 type = factor(rep(legend[3], length(smds$refined))),
                                 stratum = rep(row.names(smds$refined), each = length(covariates_plot)))

    p <- apply(as.array(levels(x$refined_strata)), 1, function(s) {
      ggplot(plot_dataframe[plot_dataframe$stratum == s,],
             aes(x = .data$abs_stand_diff, y = .data$covariates)) +
        geom_point(size = 5, aes(shape = .data$type)) +
        scale_shape_manual(values = c(4)) +
        geom_vline(xintercept = c(.1,.2), lty = 2) +
        xlab("Absolute standardized difference") +
        xlim(c(0, max(plot_dataframe$abs_stand_diff + 0.1))) +
        ylab("Covariate") +
        labs(shape = "") +
        ggtitle(s) +
        theme(text = element_text(size=10), strip.text.x = element_text(size = 12))
    })
    names(p) <- levels(x$refined_strata)

  } else {

    abs_stand_diff <- NULL
    num_incl <- 0
    legend_new <- c()
    shapes <- c()
    if (incl_none) {
      abs_stand_diff <- smds_no_st$base
      num_incl <- num_incl + 1
      legend_new <- c(legend[1])
      shapes <- c(0)
    }
    if (incl_base) {
      abs_stand_diff <- c(abs_stand_diff, smds_base_avg)
      num_incl <- num_incl + 1
      legend_new <- c(legend_new, legend[2])
      shapes <- c(shapes, 1)
    }
    if (!is.null(x$refined_strata)) {
      abs_stand_diff <- c(abs_stand_diff, smds_refined_avg)
      num_incl <- num_incl + 1
      legend_new <- c(legend_new, legend[3])
      shapes <- c(shapes, 4)
    }

    plot_dataframe <- data.frame(abs_stand_diff = abs_stand_diff,
                                 covariates = rep(covariates_plot, num_incl),
                                 type = factor(rep(legend_new, each = length(covariates_plot)), levels = legend_new))

    p <- ggplot(plot_dataframe, aes(x = .data$abs_stand_diff, y = .data$covariates)) +
      geom_point(size = 5, aes(shape = .data$type)) +
      scale_shape_manual(values = shapes) +
      geom_vline(xintercept = c(.1,.2), lty = 2) +
      xlab("Absolute standardized difference") +
      xlim(c(0, max(plot_dataframe$abs_stand_diff + 0.1))) +
      ylab("Covariate") +
      labs(shape = "") +
      theme(text = element_text(size=10), strip.text.x = element_text(size = 12))
  }
  return(p)
}
