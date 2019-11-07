#' Calculating the confidence intervals (CIs) of a mean estimate of a
#' a vector of observations using non-parametric bootstrapping.
#'
#' @description
#'
#' Functions that estimate CIs using nonparametric bootstrapping around a mean estimate.
#'
#' #' \code{mean_ci} Estimates CIs around a mean estimate using non-parametric bootstrapping
#' from the boot package
#'
#' @inheritParams boot::boot.ci
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
#' @param type A vector of character strings represenging the type of intervals
#' required to calculate the CI. Defaults to "bca". See ??boot.ci for more information.
#'
#' @keywords phenology estimates mean
#' @importFrom boot boot boot.ci
#'
#' @examples
#'\dontrun{
#' # Estimate when the mean observation of Rudbeckia hirta for the year 2019 up to October
#' r_hirta <- subset(inat_examples, scientific_name == "Rudbeckia hirta")
#' mean_ci(observations = r_hirta$doy)
#' }
#'
#' @describeIn mean_ci Estimates CIs around a mean percentile estimate using
#' non-parameteric bootstrapping from the boot package
#' @export

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
