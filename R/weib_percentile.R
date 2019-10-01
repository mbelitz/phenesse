#' Calculating a percentile estimate of a seasonal abundance distribution from
#' incidental observations.
#'
#' \code{weib_percentile} uses empirical bootstrapping to estimate a percentile
#' of the Weibull distribution, given random variables.
#'
#' @param observations is a vector of dates/time of observations given as integers
#'
#' @param percentile is the percentile of the cumulative distribution function of interest
#'
#' @param iterations is the number of iterations you want to run to create empirical
#' bootstrapping to estimate bias of original CDF. The bias is used to calculate
#' a bias corrected estimate of the percentile bound.
#'
#' @keywords phenology, weibull, percentile
#'
#' @export
#'
#' @examples
#'
#' # Gather last 10 sightings of dodo
#' observations <- c(1662, 1638, 1631, 1628, 1628, 1611, 1607, 1602, 1601, 1598)
#'
#' # Estimate when 90% of dodo were left
#' weib_percentile(observations, percentile = 0.9)
#'
#' Estimate when 50% of dodo were left with 50 iterations for quicker processing
#' weib_percentile(observations, percentile = 0.5, iterations = 50)

weib_percentile <- function(observations, percentile = 0.9, iterations = 100){

  cdf_orig <- function(observations){
    weib <- fitdistrplus::fitdist(observations, distr = "weibull", method = "mle")
    cdf <- 1 - exp(-(observations/weib$estimate['scale'])^weib$estimate['shape'])
    return(cdf)
  }

  cdf_added <- function(observations){
    weib_orig <- cdf_orig(observations)
    weib <- fitdistrplus::fitdist(observations, distr = "weibull", method = "mle")

    added_vec <- sort(append(observations, values = c(min(observations - 15),
                                                      max(observations + 15))), decreasing = FALSE)
    cdfadded <- 1 - exp(-(added_vec/weib$estimate['scale'])^weib$estimate['shape'])
    return(cdfadded)
  }

  create_predict_df <- function(observations){

    cdf <- cdf_orig(observations)

    cdf_addedvec <- cdf_added(observations)

    added_vec <- sort(append(observations, values = c(min(observations - 15),
                                                      max(observations + 15))), decreasing = FALSE)

    cdf_df <- data.frame(x = added_vec, y = cdf_addedvec)
    ends <- data.frame(x = c(min(added_vec - 1), max(added_vec+ 1)), y = c(-0.001,1.001))
    cdf_df <- rbind(cdf_df, ends)
    cdf_df <- cdf_df[order(cdf_df$x, decreasing = FALSE),]

    return(cdf_df)

  }

  get_theta_hat_i <- function(observations, percentile = percentile){

    emptyvec <- vector(mode = "numeric", length = length(observations))

    for(i in 1:length(observations)){
      df1 <- create_predict_df(observations)
      sim_vector <- runif(n = length(observations),min = 0, max = 1)
      df2 <- data.frame(x = observations, y = sim_vector[i])
      emptyvec[i] <-  reconPlots::curve_intersect(df1, df2)$x
    }

    new_vector <- sort(emptyvec, decreasing = FALSE)

    new_df1 <- create_predict_df(new_vector)

    new_df2 <- data.frame(x = observations, y = percentile)

    theta_hat_i <- reconPlots::curve_intersect(new_df1, new_df2)$x

    return(theta_hat_i)
  }


  theta_hat_df <- data.frame(x = observations, y = percentile)

  theta_hat <- reconPlots::curve_intersect(create_predict_df(observations), theta_hat_df)[['x']]

  bias <- mean(replicate(n = iterations,
                         expr = get_theta_hat_i(observations = observations,
                                                percentile = percentile)))

  2 * theta_hat - bias
}
