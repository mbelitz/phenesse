#' Calculating the CIs of a percentile estimate of a seasonal abundance distribution
#' using the non-parametric bootstrapping.
#'
#'\code{weib_percentile_ci} uses non-parametric bootstrapping from the boot package
#' to estimate 95% CIs
#'
#' @param observations is a vector of dates/time of observations given as numeric values
#'
#' @param percentile is the percentile of the cumulative distribution function of interest
#'
#' @param iterations is the number of iterations you want to run to create empirical
#' bootstrapping to estimate bias of original CDF. The bias is used to calculate
#' a bias corrected estimate of the percentile bound.
#'
#'@param bootstraps is the number of bootstraps you want to run to create the CIs
#'
#'@examples
#'
#' # Gather last 10 sightings of dodo
#' observations <- c(1662, 1638, 1631, 1628, 1628, 1611, 1607, 1602, 1601, 1598)
#'
#' # Estimate when 90% of dodo were left with 95% CI
#' weib_percentile_ci(observations, iterations = 100, percentile = 0.9, bootstraps = 100)

weib_percentile_ci <- function(observations, iterations, percentile, bootstraps){

  weibfun <- function(data, i){
    d <- data[i]
    return(phenesse::weib_percentile(d, iterations = iterations, percentile = percentile))
  }

  estimate_ci <- function(observations){
    bootstrap <- boot::boot(observations, weibfun, R = bootstraps)
    boot_ci <- boot::boot.ci(bootstrap, conf = 0.95, type = "bca")
    low_ci <- boot_ci$bca[4]
    high_ci <- boot_ci$bca[5]
    ci_df <- data.frame(estimate = bootstrap$t0, low_ci, high_ci)
    return(ci_df)
  }
  estimate <- estimate_ci(observations)
  return(estimate)
}
