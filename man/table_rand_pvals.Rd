% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rand_pvals.R
\name{table_rand_pvals}
\alias{table_rand_pvals}
\title{Generate a covariate balance table from the empirical randomization null distribution}
\usage{
table_rand_pvals(
  object = NULL,
  z = NULL,
  X = NULL,
  base_strata = NULL,
  refined_strata = NULL,
  options = list()
)
}
\arguments{
\item{object}{an optional object of class \code{strat},
typically created using \code{\link{strat}()}
or as a result of a call to \code{\link{prop_strat}()} or \code{\link{refine}()}.
If not provided, \code{z} and \code{X} must be specified}

\item{z}{vector of treatment assignment; only used if \code{object} is not supplied}

\item{X}{covariate matrix/data.frame; only used if \code{object} is not supplied}

\item{base_strata}{optional initial stratification for which to calculate
the empirical randomization null distribution;
only used if \code{object} is not supplied}

\item{refined_strata}{optional refined stratification for which to calculate
the empirical randomization null distribution;
only used if \code{object} is not supplied}

\item{options}{list of additional options, listed in the \code{details} below}
}
\value{
Matrix with 4 or 8 columns, depending whether one or both of base and
refined strata are provided and the \code{incl_base} option.
The columns give the observed standardized mean difference or objective value,
the median and maximum across \code{nrand} null simulations, and the P-value which is the
proportion of the null simulations that have worse covariate balance than the observed value.
The top three rows give the scaled objective value and the average and maximum standardized mean differences across
all strata and covariates. The following rows, one for each covariate, give the standardized mean difference
for that covariate, averaged across strata. The first row for the scaled objective value is \code{NULL} for the base
stratification, if included, as the base stratification does not generally minimize a mathematical objective function.
}
\description{
Generate a table using the information collected in \code{\link{rand_pvals}()}.
See \code{\link{rand_pvals}()} for more details about the methods used.
}
\details{
The \code{options} list argument can contain any of the following elements:
\itemize{
\item{nrand: }{how many times to randomize the treatment assignment when forming the null distribution.
Default is 10000}
\item{criterion: }{which optimization criterion to use when calculating the objective value.
Options are "max", "sum", or "combo", referring to whether to include the
maximum standardized mean difference (SMD), the sum of all SMDs,
or a combination of the maximum and the sum. The default is "combo"}
\item{wMax: }{how much to weight the maximum standardized mean difference compared to the sum.
Only used if criterion is set to "combo". Default is 5}
\item{incl_base: }{whether to include columns for the initial stratification in the table.
Default is \code{TRUE} if a base stratification is provided}
\item{rand_pvals: if already calculated, the returned list of information from \code{\link{rand_pvals}()}.
If \code{NULL}, this will be calculated}
}
}
