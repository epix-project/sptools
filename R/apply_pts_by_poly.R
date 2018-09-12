#' Apply functions over points by polygons
#'
#' This function is a wrapper aroud the `sptools::points_in_polygon` and applies
#' a function `f` to the variable `var` of the `spatialPointsDataFrame` `points`
#' object, by the polygons of `spatialPolygonsDataFrame` `polygons` object.
#'
#' @param points a `SpatialPointsDataFrame` object.
#' @param polygons a `SpatialPolygonsDataFrame` object.
#' @var the name of the variable of `points` on which to apply the function `f`.
#' @f the function to be applied.
#'
#' @author Marc Choisy
#'
#' @importFrom magrittr %>%
#' @export
#'
apply_pts_by_poly <- function(points, polygons, var, f, ...) {
  sapply(seq_along(polygons),
         function(x) f(points_in_polygon(points, polygons[x, ])[[var]], ...))
}
