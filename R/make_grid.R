#' Sample Point Locations in a SpatialPolygon*
#'
#' This is a wrapper around the \code{sp::makegrid} function, only for
#' \code{SpatialPolygon*} objects.
#'
#' @param sppoly \code{SpatialPolygon*} object as defined in the \code{sp} package.
#' @param n approximate sample size.
#' @param ... optional arguments passed to \code{sp::makegrid}
#'
#' @return A \code{SpatialPoints$} object. The length of the object is
#' approximately equal to \code{n}.
#'
#' @importFrom sp makegrid SpatialPointsDataFrame
#' @importFrom rgeos gArea
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' library(sf)
#' vn <- gadmVN::gadm(level = "country")
#' vn <- sf::as_Spatial(vn)
#' sp::plot(vn)
#' grid <- make_grid(vn, 100)
#' length(grid)
#' sp::plot(grid, add = TRUE)
#' grid2 <- make_grid(vn, cellsize = .5)
#' length(grid2)
#' sp::plot(grid2, add = TRUE, col = "blue")
#'
make_grid <- function(sppoly, n, ...) {
  if (!missing(n)) {
    n <- n * do.call(`*`, as.list(apply(sppoly@bbox, 1, diff))) /
      suppressWarnings(gArea(sppoly))
  }
  makegrid(sppoly, n, ...) %>%
    SpatialPoints(crs(sppoly)) %>%
    points_in_polygon(sppoly)
}
