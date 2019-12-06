#' Example iNaturalist-sourced data
#'
#' @description Example data downloaded from iNaturalist.org for the Washington,
#' DC area using the bounding box bounds = c(38, -77, 39, -76). Data was
#' downloaded on 10/30/2019 for four species: Speyeria cybele, Danaus
#' plexippus, Rudbeckia hirta, and Asclepias syriaca. These data were not scored
#' to mark phenology, so all life stages/reproductive stages are included in the
#' download.The download only includes 2019 observations and the doy
#' (day of year)column was added post data download by MW Belitz using the
#' lubridate package.
#'
#' @docType data
#'
#' @usage data(inat_examples)
#'
#' @format A data frame with 252 rows and 6 variables:
#' \describe{
#'    \item{scientific_name}{binomial of species}
#'    \item{latitude}{latitude where observations occurred}
#'    \item{longitude}{longitude where observations occurred}
#'    \item{common_name}{common name related to species}
#'    \item{observed_on}{original date listed of observation}
#'    \item{doy}{day of year when the observation occurred,
#'    variable created by MW Belitz using the package lubridate}
#'    }
#'
#' @keywords datasets
#'
#' @references \url{https://inaturalist.org}
#'
#' @examples
#' data(inat_examples)
#' \dontrun{
#' View(inat_examples)
#' }

"inat_examples"
