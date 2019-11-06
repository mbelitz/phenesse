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
#' @param type A vector of character strings represenging the type of intervals
#' required to calculate the CI. Defaults to "bca". See ??boot.ci for more information.
#'
#' @param conf The confidence level wanted. Defaults to 95% CI.
#'
#' @param parallel The type of parallel operation to be used (if any). If missing,
#' the default is that no parallelization will occur. Parallelization options are
#' "multicore" and "snow"
#'
#' @param ncpus An integer that represents the number of processes to be used in parellel
#' operation. One could chose this to be the number of available CPUs.
#'
#' @param cl An optional parallel or snow cluster for use if parallel = "snow". If not supplied,
#' a cluster on the local machine is created for the duration of the boot call.
#'
#' @keywords phenology weibull percentile
#'
#' @export
#' @importFrom boot boot boot.ci
#'
#' @examples
#'
#' Gather sightings of iNaturalist observations for four species:
#' Danaus plexippus, Speyeria cybele, Rudbeckia hirta, and Asclepias syriaca
#'
#' Estimate when the first 10% of individuals of the butterfly species
#' Speyeria cybele are in flight.
#'
#' s_cybele <- subset(inat_examples, scientific_name == "Speyeria cybele")
#' weib_percentile_ci(observations = s_cybele$doy, iterations = 100, percentile = 0.1, bootstraps = 100)

weib_percentile_ci <- function(observations, iterations, percentile, bootstraps,
                              type = "bca", conf = 0.95, parallelize = "no",
                              ncpus = getOption("boot.ncpus", 1L), cl = NULL){

  weibfun <- function(data, i){
    d <- data[i]
    return(phenesse::weib_percentile(d, iterations = iterations, percentile = percentile))
  }

  estimate_ci <- function(observations){
    bootstrap <- boot::boot(observations, weibfun, R = bootstraps, parallel = parallelize,
                            ncpus = ncpus, cl = cl)
    boot_ci <- tryCatch(boot::boot.ci(bootstrap, conf = conf, type = type), error = function(e) NA)
    if(type == "bca"){
      low_ci <- boot_ci$bca[4]
      high_ci <- boot_ci$bca[5]
    } else if(type == "perc"){
      low_ci <-boot_ci$percent[4]
      high_ci <- boot_ci$percent[5]
    } else if(type == "norm"){
      low_ci <- boot_ci$normal[4]
      high_ci <- boot_ci$normal[5]
    } else if(type == "basic"){
      low_ci <- boot_ci$basic[4]
      high_ci <- boot_ci$basic[5]
    } else{
      low_ci <- "Bootstrap type NA"
      high_ci <- "Bootstrap type NA"
    }
    ci_df <- data.frame(estimate = bootstrap$t0, low_ci, high_ci)
    return(ci_df)
  }
  estimate <- estimate_ci(observations)
  return(estimate)
}
