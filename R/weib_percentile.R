#' Calculating a percentile estimate of a seasonal abundance distribution from
#' incidental observations.
#'
#' @description
#' \code{weib_percentile} uses empirical bootstrapping to estimate a percentile
#' of the Weibull distribution, given random variables.
#'
#' @param observations is a vector of dates/time of observations
#' given as integers
#'
#' @param percentile is the percentile of the cumulative distribution function
#' of interest
#'
#' @param iterations is the number of iterations you want to use to bootstrap
#' an estimate of bias of the original CDF. The bias is used to calculate
#' a Weibull-corrected estimate of the percentile bound.
#'
#' @keywords phenology weibull percentile
#'
#' @importFrom fitdistrplus fitdist
#'
#' @examples
#'\dontrun{
#' # Gather sightings of iNaturalist observations for four species:
#' # Danaus plexippus, Speyeria cybele, Rudbeckia hirta, and Asclepias syriaca
#'
#' # Estimate when the first 50\% of individuals of the milkweed species
#' # Asclepias syriaca have been observed.
#'
#' data(inat_examples)
#' a_syriaca <- subset(inat_examples, scientific_name == "Asclepias syriaca")
#' weib_percentile(a_syriaca$doy, percentile = 0.5, iterations = 10)
#'
#' # Estimate when 90\% of individuals of the milkweed species A. syriaca have
#' been observed,
#' # using only 100 iterations for quicker processing
#' weib_percentile(a_syriaca$doy, percentile = 0.5, iterations = 10)
#' }
#' @export
weib_percentile <- function(observations, percentile = 0.9, iterations = 500){

  curve_intersect <- function(curve1, curve2, empirical=TRUE, domain=NULL) {
    if (!empirical & missing(domain)) {
      stop("'domain' must be provided with non-empirical curves")
    }

    if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
      stop("'domain' must be a two-value numeric vector, like c(0, 10)")
    }

    if (empirical) {
      # Approximate the functional form of both curves
      curve1_f <- stats::approxfun(curve1$x, curve1$y, rule = 2)
      curve2_f <- stats::approxfun(curve2$x, curve2$y, rule = 2)

      # Calculate the intersection of curve 1 and curve 2 along the x-axis
      point_x <- stats::uniroot(function(x) curve1_f(x) - curve2_f(x),
                         c(min(curve1$x), max(curve1$x)))$root

      # Find where point_x is in curve 2
      point_y <- curve2_f(point_x)
    } else {
      # Calculate the intersection of curve 1 and curve 2 along the x-axis
      # within the given domain
      point_x <- stats::uniroot(function(x) curve1(x) - curve2(x), domain)$root

      # Find where point_x is in curve 2
      point_y <- curve2(point_x)
    }

    return(list(x = point_x, y = point_y))
  }

  create_cdf_ends <- function(observations){
    weib <- fitdistrplus::fitdist(observations, distr = "weibull",
                                  method = "mle")
    cdf0 <- as.numeric(weib$estimate['scale']*
                         (-log(1-0.01))^(1/weib$estimate['shape']))
    cdf100 <- as.numeric(weib$estimate['scale']*
                           (-log(1-0.99))^(1/weib$estimate['shape']))

    added_vec <- sort(append(observations, values = c(cdf0, cdf100)),
                      decreasing = FALSE)
    cdfadded <- 1 - exp(-(added_vec/weib$estimate['scale'])
                        ^weib$estimate['shape'])
    return(added_vec)
  }

  create_predict_df <- function(observations){

    added_vec <- create_cdf_ends(observations)
    vec_start <- min(added_vec)
    vec_end <- max(added_vec)
    new_vec <- seq(from = vec_start, to = vec_end, by = 0.5)

    weib <- fitdistrplus::fitdist(observations, distr = "weibull",
                                  method = "mle")
    cdfadded <- 1 - exp(-(new_vec/weib$estimate['scale'])
                        ^weib$estimate['shape'])

    cdf_df <- data.frame(x = new_vec, y = cdfadded)
    ends <- data.frame(x = c(min(added_vec - 1),
                             max(added_vec+ 1)), y = c(-0.001,1.001))
    cdf_df <- rbind(cdf_df, ends)
    cdf_df <- cdf_df[order(cdf_df$x, decreasing = FALSE),]

    return(cdf_df)

  }

  get_theta_hat_i <- function(observations, percentile = percentile){

    emptyvec <- vector(mode = "numeric", length = length(observations))

    for(i in seq_along(observations)){
      df1 <- create_predict_df(observations)
      sim_vector <- stats::runif(n = length(observations),min = 0, max = 1)
      df2 <- data.frame(x = observations, y = sim_vector[i])
      emptyvec[i] <- curve_intersect(df1, df2)$x
    }

    new_vector <- sort(emptyvec, decreasing = FALSE)

    new_df1 <- create_predict_df(new_vector)

    new_df2 <- data.frame(x = observations, y = percentile)

    theta_hat_i <- curve_intersect(new_df1, new_df2)$x

    return(theta_hat_i)
  }

  theta_hat_df <- data.frame(x = observations, y = percentile)

  theta_hat <- curve_intersect(create_predict_df(observations),
                               theta_hat_df)[['x']]

  bias <- mean(replicate(n = iterations,
                         expr = get_theta_hat_i(observations = observations,
                                                percentile = percentile)))

  2 * theta_hat - bias
}
