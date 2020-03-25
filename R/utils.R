#' Calculate confidence intervals using bootsrap of any statistical function
#' of interest.
#'
#' @param observations is a vector of dates/time of observations given as
#' numeric values
#' @param .f function to use
#' @param n_boots is the number of bootstraps you want to run to create the
#' CIs
#' @param type A vector of character strings representing the type of intervals
#' required to calculate the CI. Defaults to "bca". See ??boot.ci for more
#' information.
#' @param conf The confidence level wanted. Defaults to 95\% CI.
#' @param parallelize The type of parallel operation to be used (if any). If
#' missing, the default is that no parallelization will occur. Parallelization
#' options are "multicore" and "snow"
#' @param ncpus An integer that represents the number of processes to be
#' used in parallel operation.
#' @param cl An optional parallel or snow cluster for use if parallel = "snow".
#' If not supplied, a cluster on the local machine is created for
#' the duration of the boot call.
#' @return A data frame with estimate, and the lower and upper points of its confidence interval
#' @export
estimate_ci <- function(observations, .f, n_boots,
                        parallelize = "no",
                        ncpus = getOption("boot.ncpus", 1L),
                        cl = NULL,
                        type = "bca", conf = 0.95){
  bootstrap <- boot::boot(observations, .f, R = n_boots,
                          parallel = parallelize,
                          ncpus = ncpus, cl = cl)
  boot_ci <- tryCatch(boot::boot.ci(bootstrap, conf = conf, type = type),
                      error = function(e) NA)
  if(is.na(boot_ci))
    return(data.frame(estimate = bootstrap$t0, low_ci = NA, high_ci = NA))

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

  data.frame(estimate = bootstrap$t0, low_ci, high_ci)
}
