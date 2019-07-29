#' Calculate the weighted centroid
#'
#' Calculate the coordinate of the weighted centroid from a Spatial Points Data
#' Frame object
#'
#' @param  spdt Spatial Points data frame with weighted information in the
#'  slot \code{data}
#'
#' @return coordinates x,y of a point
#'
#' @importFrom sp coordinates
#' @export
#' @examples
#' library(sf)
#'
#' # SpatialPoints
#' stations <- as(imhen::stations, "Spatial")
#' stations@data <- stations@data[, "latitude", drop = FALSE]
#' weighted_centroid(stations)
#'
weighted_centroid <- function(spdt) {
  weights <- slot(spdt, "data")
  weights <- weights / sum(weights)
  spdt <- coordinates(spdt) # step 2
  spdt <- as.data.frame(spdt)
  spdt <- lapply(spdt, function(x) sum(x * weights))
  spdt <- unlist(spdt)
  spdt
}
