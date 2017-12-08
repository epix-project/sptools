#' Sample Point Locations in a SpatialPolygon*
#'
#' This is a wrapper around the \code{sp::makegrid} function, only for
#' \code{SpatialPolygon*} objects.
#'
#' @param sppoly \code{SpatialPolygon*} object as defined in the \code{sp} package.
#' @param n approximate sample size.
#' @param ... optional arguments passed to \code{sp::makegrid}
#'
#' @return A \code{SpatialPoins$} object. The length of the object is
#' approximately equal to \code{n}.
#'
#' @importFrom sp makegrid SpatialPointsDataFrame
#'
#' @export
#'
#' @examples
#' vn <- gadmVN::gadm(level = "country")
#' sp::plot(vn)
#' grid <- make_grid(vn, 100)
#' length(grid)
#' sp::plot(grid, add = TRUE)
#' grid2 <- make_grid(vn, cellsize = .5)
#' length(grid2)
#' sp::plot(grid2, add = TRUE, col = "blue")
#'
make_grid <- function(sppoly, n, ...) {
  if(!missing(n)) {
    n <- n * do.call(`*`, as.list(apply(sppoly@bbox, 1, diff))) /
      suppressWarnings(gArea(sppoly))
  }
  tmp <- setNames(makegrid(sppoly, n, ...), c("longitude", "latitude"))
  tmp <- SpatialPointsDataFrame(tmp, tmp, proj4string = crs(sppoly))
  points_in_polygon(tmp, sppoly)
}
