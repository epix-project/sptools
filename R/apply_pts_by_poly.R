#' Apply functions over points by polygons
#'
#' This function is a wrapper aroud the `sptools::points_in_polygon` and applies
#' a function `f` to the variable `var` of the `spatialPointsDataFrame` `points`
#' object, by the polygons of `spatialPolygonsDataFrame` `polygons` object.
#'
#' @param points a `SpatialPointsDataFrame` object.
#' @param polygons a `SpatialPolygonsDataFrame` object.
#' @param var the name of the variable of `points` on which to apply the
#' function `f`.
#' @param f the function to be applied.
#' @param ... arguments to passed in `sptools::points_in_polygon`
#'
#' @author Marc Choisy
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' # SpatialPolygonsDataFrame
#' vn <- sf::as_Spatial(gadmVN::gadm(level = "province"))
#' # SpatialPoints
#' stations <- as(imhen::stations, "Spatial")
#' names(stations)
#'
#' # to calculate the mean elevation of the station for each polygon
#' apply_pts_by_poly(stations, vn, "elevation", mean)
apply_pts_by_poly <- function(points, polygons, var, f, ...) {
  sapply(seq_along(polygons),
         function(x) f(points_in_polygon(points, polygons[x, ])[[var]], ...))
}
