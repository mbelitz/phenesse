#' Calculating the confidence intervals (CIs) of a quantile or mean estimate of a
#' a vector of observations using non-parametric bootstrapping.
#'
#' \code{quantile_ci} Estimates CIs around a quantile percentile estimate using
#' non-parameteric bootstrapping from the boot package
#'
#' \code{mean_ci} Estimates CIs around a mean estimate using non-parametric bootstrapping
#' from the boot package
#'
#' @param observations A vector of observations given as numeric values
#'
#' @param percentile The percentile of interest
#'
#' @param bootstraps The number of bootstraps you want to run to create the CIs,
#' defaults to 100000
#'
#' @param conf The confidence level wanted. Defaults to 95% CI.
#'
#' @param type A vector of character strings represenging the type of intervals
#' required to calculate the CI. Defaults to "bca". See ??boot.ci for more information.
#'
#' @keywords phenology, quantile, percentile, mean
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

quantile_ci <- function(observations, percentile, bootstraps = 100000,
                        conf = 0.95, type = 'bca'){

  quantilefun <- function(data, i){
    d <- data[i]
    return(quantile(d, probs = c(percentile)))
  }

  estimate_ci <- function(observations){
    bootstrap <- boot::boot(observations, quantilefun, R = bootstraps)
    boot_ci <- tryCatch(boot::boot.ci(bootstrap, conf = 0.95, type = type), error = function(e) NA)
    if(type == "bca"){
      low_ci <- tryCatch(boot_ci$bca[4], error = function(e) NA)
      high_ci <- tryCatch(boot_ci$bca[5], error = function(e) NA)} else{
        low_ci <- tryCatch(boot_ci$percent[4], error = function(e) NA)
        high_ci <- tryCatch(boot_ci$percent[5], error = function(e) NA)
      }
    ci_df <- data.frame(estimate = bootstrap$t0, low_ci, high_ci)
    return(ci_df)
  }
  estimate <- estimate_ci(observations)
  return(estimate)
}

#' mean_ci function - calculates the mean and uses non-parametric bootstrapping
#' to calculate the confidence intervals

mean_ci <- function(observations, bootstraps = 100000,
                        conf = 0.95, type = 'bca'){

  meanfun <- function(data, i){
    d <- data[i]
    return(mean(d))
  }

  estimate_ci <- function(observations){
    bootstrap <- boot::boot(observations, meanfun, R = bootstraps)
    boot_ci <- tryCatch(boot::boot.ci(bootstrap, conf = 0.95, type = type), error = function(e) NA)
    if(type == "bca"){
      low_ci <- tryCatch(boot_ci$bca[4], error = function(e) NA)
      high_ci <- tryCatch(boot_ci$bca[5], error = function(e) NA)} else{
        low_ci <- tryCatch(boot_ci$percent[4], error = function(e) NA)
        high_ci <- tryCatch(boot_ci$percent[5], error = function(e) NA)
      }
    ci_df <- data.frame(estimate = bootstrap$t0, low_ci, high_ci)
    return(ci_df)
  }
  estimate <- estimate_ci(observations)
  return(estimate)
}
