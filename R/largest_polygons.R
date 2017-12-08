#' Filter the Largest Polygons of each polygon slot of a SpatialPolygon* object
#'
#' @param sppoly \code{SpatialPolygon*} object as defined in package \code{sp}.
#'
#' @return An object of the same class as \code{sppoly} where each slot of the
#' \code{polygons} element is made on one single \code{Polygon} object that is
#' the largest \code{Polygon} is the input \code{sppoly}.
#'
#' @importFrom rgeos gEnvelope
#' @importFrom maptools checkPolygonsHoles
#'
#' @export
#'
#' @examples
#' vn_prov <- gadmVN::gadm()
#' vn_prov2 <- largest_polygons(vn_prov)
#' sp::plot(vn_prov)
#' sp::plot(vn_prov2)
#' # same with the country level:
#' vn <- gadmVN::gadm(level = "country")
#' vn2 <- largest_polygons(vn)
#' rgeos::gEnvelope(vn)
#'
largest_polygons <- function(sppoly) {
  surfaces <- areas(sppoly)
  sppoly@polygons <- Map(function(x, y, z) {
                           x@Polygons <- x@Polygons[y]
                           x@plotOrder <- 1L
                           x@area <- z
                           checkPolygonsHoles(x)},
                         sppoly@polygons,
                         sapply(surfaces, which.max),
                         sapply(surfaces, max))
  sppoly@bbox <- gEnvelope(sppoly)@bbox
  sppoly
}
