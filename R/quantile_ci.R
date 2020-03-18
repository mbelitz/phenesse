#' Calculating the confidence intervals (CIs) of a quantile estimate of a
#' a vector of observations using non-parametric bootstrapping.
#'
#' @description
#'
#' \code{quantile_ci}Estimates CIs around a quantile percentile estimate using
#' non-parametric bootstrapping from the boot package
#'
#' @param observations A vector of observations given as numeric values
#'
#' @param percentile The percentile of interest
#'
#' @param bootstraps The number of bootstraps you want to run to create the CIs,
#' defaults to 100000
#'
#' @param conf The confidence level wanted. Defaults to 95\% CI.
#'
#' @param type A vector of character strings representing the type of intervals
#' required to calculate the CI. Defaults to "bca".
#' See ??boot.ci for more information.
#'
#' @return The quantile estimate and confidence intervals.
#'
#' @keywords phenology quantile percentile
#' @export
#' @importFrom boot boot boot.ci
#'
#' @examples
#'
#' # Gather sightings of iNaturalist observations for four species:
#' # Danaus plexippus, Speyeria cybele, Rudbeckia hirta, and Asclepias syriaca
#'
#' # Estimate when the first 10 percent of individuals of the butterfly species
#' # Speyeria cybele are in flight.
#'
#' data(inat_examples)
#' s_cybele <- subset(inat_examples, scientific_name == "Speyeria cybele")
#' quantile_ci(observations = s_cybele$doy, percentile = 0.1, bootstraps = 100)
#'

quantile_ci <- function(observations, percentile, bootstraps = 100000,
                        conf = 0.95, type = 'bca'){

  quantilefun <- function(data, i){
    d <- data[i]
    return(stats::quantile(d, probs = c(percentile)))
  }

  estimate_ci(observations, .f = quantilefun, n_boots = bootstraps,
              conf = conf, type = type)
}
