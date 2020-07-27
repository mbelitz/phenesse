#' Calculating the confidence intervals (CIs) of an arithmetic mean.
#'
#' @description
#'
#' \code{mean_ci}Function estimates CIs using nonparametric bootstrapping around a
#' mean estimate.
#'
#' @param observations A vector of observations given as numeric values
#'
#' @param bootstraps The number of bootstraps you want to run to create the CIs,
#' defaults to 100000
#'
#' @param conf The confidence level wanted. Defaults to 95\% CI.
#'
#' @param type A vector of character strings representing the type of intervals
#' required to calculate the CI. Defaults to "bca". See ??boot.ci
#' for more information.
#'
#' @return The estimated CIs around a mean estimate.
#'
#' @keywords phenology estimates mean
#' @importFrom boot boot boot.ci
#'
#' @examples
#' # Estimate when the mean observation of Rudbeckia hirta for the year 2019 up
#' # to October
#' data(inat_examples)
#' r_hirta <- subset(inat_examples, scientific_name == "Rudbeckia hirta")
#' mean_ci(observations = r_hirta$doy , bootstraps = 100)
#'
#' # note low number of bootstraps for quick processing speed
#'
#' @describeIn mean_ci Estimates CIs around a mean percentile estimate using
#' non-parametric bootstrapping from the boot package
#' @export
mean_ci <- function(observations, bootstraps = 100000,
                    conf = 0.95, type = 'perc'){

  meanfun <- function(data, i){
    d <- data[i]
    return(mean(d))
  }

  phenesse::estimate_ci(observations, .f = meanfun, n_boots = bootstraps,
              conf = conf, type = type)
}
