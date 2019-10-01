#' Calculating the CIs of a naive (non-parameterized) estimate of a percentile of
#' a vector of observations using non-parametric bootstrapping
#'
#' \code{naive_percentile} Estimates CI around a naive percentile estimate using
#' non-parameteric bootstrapping from the boot package
#'
#' @param observations A vector of observations given as numeric values
#'
#' @param percentile The naive percentile of interest
#'
#' @param bootstraps The number of bootstraps you want to run to create the CIs,
#' defaults to 100000
#'
#' @param conf The confidence level wanted. Defaults to 95% CI.
#'
#' @param type A vector of character strings represenging the type of intervals
#' required to calculate the CI. Defaults to "bca". See ??boot.ci for more information.
#'
#' @keywords phenology, quantile, percentile
#'
#' @export
#' @importFrom boot boot boot.ci
#'
#' @examples
#'
#' #' # Gather last 10 sightings of dodo
#' observations <- c(1662, 1638, 1631, 1628, 1628, 1611, 1607, 1602, 1601, 1598)
#'
#' # Estimate Naive percentile when 90% of dodo were left with 95% CI given last observations
#' weib_percentile_ci(observations, iterations = 100, percentile = 0.9, bootstraps = 100)

quantile_ci <- function(observations, percentile, bootstraps = 100000,
                        conf = 0.95, type = "bca"){

  quantilefun <- function(data, i){
    d <- data[i]
    return(quantile(d, probs = c(percentile)))
  }

  estimate_ci <- function(observations){
    bootstrap <- boot::boot(observations, quantilefun, R = bootstraps)
    boot_ci <- boot::boot.ci(bootstrap, conf = 0.95, type = type)
    low_ci <- boot_ci$bca[4]
    high_ci <- boot_ci$bca[5]
    ci_df <- data.frame(estimate = bootstrap$t0, low_ci, high_ci)
    return(ci_df)
  }
  estimate <- estimate_ci(observations)
  return(estimate)
}
