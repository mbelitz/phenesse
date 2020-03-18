#' Calculating the CIs of a percentile estimate of a seasonal abundance
#' distribution using the non-parametric bootstrapping.
#'
#' @description
#' \code{weib_percentile_ci} uses non-parametric bootstrapping
#' from the boot package to estimate 95% CIs around a weib_percentile estimate
#'
#' @inheritParams estimate_ci
#'
#' @param iterations is the number of iterations you want to run to create
#' empirical bootstrapping to estimate bias of original CDF. The bias is used to
#' calculate a bias corrected estimate of the percentile bound.
#' @param percentile is the percentile of the cumulative distribution function
#' of interest
#' @param bootstraps is the number of bootstraps you want to run to create the
#' CIs
#'
#' @return The Weibull-corrected estimate of the percentile of interest and CIs.
#'
#' @keywords phenology weibull percentile
#'
#' @importFrom boot boot boot.ci
#'
#' @examples
#'
#' # Gather sightings of iNaturalist observations for four species:
#' # Danaus plexippus, Speyeria cybele, Rudbeckia hirta, and Asclepias syriaca
#'
#' # Estimate when the first 50 percent of individuals of the butterfly species
#' # Speyeria cybele are in flight.
#'
#'\donttest{
#' data(inat_examples)
#' s_cybele <- subset(inat_examples, scientific_name == "Speyeria cybele")
#' weib_percentile_ci(observations = s_cybele$doy, iterations = 10,
#'                    percentile = 0.5, bootstraps = 10)
#' }
#'
#'@export
#'
weib_percentile_ci <- function(observations, iterations, percentile, bootstraps,
                              type = "bca", conf = 0.95, parallelize = "no",
                              ncpus = getOption("boot.ncpus", 1L), cl = NULL){

  weibfun <- function(data, i){
    d <- data[i]
    return(weib_percentile(d, iterations = iterations, percentile = percentile))
  }

  estimate_ci(observations, .f = weibfun, n_boots = bootstraps,
              conf = conf, type = type, parallelize = parallelize,
              ncpus = ncpus, cl = cl)
}
