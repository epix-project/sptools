#' Transform SpatialPointsDataFrame into SpatialPoints
#'
#' @param spdf A SpatialPointsDataFrame object.
#'
#' @return A SpatialPoints object.
#'
#' @importFrom sp SpatialPoints CRS proj4string
#'
#' @export
#'
#' @examples
#' library(sf)
#' # devtools::install_github("epix-project/imhen")
#' stations <- as(imhen::stations, "Spatial")
#' stations
#' remove_data_spatialpoints(stations)
#'
remove_data_spatialpoints <- function(spdf) {
  SpatialPoints(spdf, CRS(proj4string(spdf)))
}

# ------------------------------------------------------------------------------

#' Transform SpatialPolygonsDataFrame into SpatialPolygons
#'
#' @param spdf A SpatialPolygonDataFrame object.
#'
#' @return A SpatialPolygon object.
#'
#' @importFrom sp SpatialPolygons CRS proj4string
#'
#' @export
#'
#' @examples
#' vn <- gadm("vietnam", "sp", 1)
#' vn
#' remove_data_spatialpolygons(vn)
#'
remove_data_spatialpolygons <- function(spdf) {
  SpatialPolygons(spdf@polygons, spdf@plotOrder, CRS(proj4string(spdf)))
}
