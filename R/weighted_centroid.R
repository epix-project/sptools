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
#' @importFrom magrittr %>%
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
  weights <- spdt %>% # step 1
    slot("data") %>% {. / sum(.)}
  spdt %>%            # step 2
    coordinates() %>%
    as.data.frame() %>%
    lapply(. %>% `*`(weights) %>% sum()) %>%
    unlist()
}
