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
#' @param bootstraps is the number of bootstraps you want to run to create the CIs
#'
#' @keywords phenology, weibull, percentile
#'
#' @export
#' @importFrom boot boot boot.ci
#'
#' @examples
#'
#' Gather 10 sightings of an individuals - numbers represent day of year observed
#' testobs <- c(150,160,162,164,168,170,172,176,178,188)
#'
#' Estimate when 90% of individuals are still in the phenological state (e.g.,
#' when 90% of plants are still in flower or when 90% of butterflies are still on wing)
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
