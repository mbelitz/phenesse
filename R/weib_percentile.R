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
#' @return The Weibull-corrected estimate of the percentile of interest.
#'
#' @keywords phenology weibull percentile
#'
#' @importFrom fitdistrplus fitdist
#'
#' @export
#'
#' @examples
#'\donttest{
#' # Gather sightings of iNaturalist observations for four species:
#' # Danaus plexippus, Speyeria cybele, Rudbeckia hirta, and Asclepias syriaca
#'
#' # Estimate when the first 50 percent of individuals of the milkweed species
#' # Asclepias syriaca have been observed.
#'
#' data(inat_examples)
#' a_syriaca <- subset(inat_examples, scientific_name == "Asclepias syriaca")
#' weib_percentile(a_syriaca$doy, percentile = 0.5, iterations = 10)
#'
#' # Estimate when 90 percent of individuals of the milkweed species A. syriaca
#' # have been observed, using only 10 iterations for quicker processing. To get
#' # more stable result, more observations should be used.
#'
#' weib_percentile(a_syriaca$doy, percentile = 0.5, iterations = 10)
#' }
#'
weib_percentile <- function(observations, percentile, iterations = 500){

  # curve_intersect determines where two lines intersect
  # parameters needed are two dataframes with two columns, x and y, which could
  # be plotted.

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
      curve2_f <- suppressWarnings(stats::approxfun(curve2$x, curve2$y, rule = 2))

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

  # use a data frame to plot a smooth CDF from -0.001 to 1.001 given
  # our original observations

  create_predict_df <- function(observations){
    # previous create_cdf_ends()
    weib <- fitdistrplus::fitdist(observations, distr = "weibull",
                                  method = "mle")
    cdf0 <- as.numeric(weib$estimate['scale']*
                         (-log(1-0.01))^(1/weib$estimate['shape']))
    cdf100 <- as.numeric(weib$estimate['scale']*
                           (-log(1-0.99))^(1/weib$estimate['shape']))
    added_vec <- sort(append(observations, values = c(cdf0, cdf100)),
                      decreasing = FALSE)

    new_vec <- seq(from = min(added_vec), to = max(added_vec), by = 0.5)

    cdfadded <- 1 - exp(-(new_vec/weib$estimate['scale'])^weib$estimate['shape'])

    cdf_df <- data.frame(x = new_vec, y = cdfadded)
    ends <- data.frame(x = c(min(added_vec - 1), max(added_vec + 1)),
                       y = c(-0.001,1.001))
    cdf_df <- rbind(cdf_df, ends)
    cdf_df <- cdf_df[order(cdf_df$x, decreasing = FALSE),]

    return(cdf_df)
  }

  # calculates the theta hat value for each iteration, which
  # when averaged is used to calculate the bias value

  get_theta_hat_i <- function(observations, percentile){

    emptyvec <- vector(mode = "numeric", length = length(observations))
    df1 <- create_predict_df(observations) # move out of loop since constant

    for(i in seq_along(observations)){
      sim_vector <- stats::runif(n = 1, min = 0, max = 1)
      df2 <- data.frame(x = observations, y = sim_vector)
      emptyvec[i] <- curve_intersect(df1, df2)$x
    }

    new_vector <- sort(emptyvec, decreasing = FALSE)

    new_df1 <- create_predict_df(new_vector)

    new_df2 <- data.frame(x = observations, y = percentile)

    theta_hat_i <- curve_intersect(new_df1, new_df2)$x

    return(theta_hat_i)
  }

  theta_hat_df <- data.frame(x = observations, y = percentile)

  # calculate theta hat original value
  theta_hat <- curve_intersect(curve1 = create_predict_df(observations),
                               curve2 = theta_hat_df)[['x']]
  # calculate bias value based off of the mean of many theta hat i calculations
  bias <- mean(replicate(n = iterations,
                         expr = get_theta_hat_i(observations = observations,
                                                percentile = percentile)))
  # final calculation, which gives you theta bar!
  2 * theta_hat - bias
}
