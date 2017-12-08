#' Areas of the Polygons of a SpatialPolygon* Object
#'
#' Returns a list of the areas of all the \code{Polygon} objects of the polygons
#' slot of a \code{SpatialPolygon*} object.
#'
#' @return A list of same length as the input \code{SpatialPolygon*}
#' object. Each slot of the list is a numeric vector, the lenght of which is
#' equal to the number of \code{Polygon} objects constituing the polygon entity.
#' Each of these values are the areas of the of the \code{Polygon} objects,
#' expressed in the unit of the CRS.
#'
#' @param sppoly \code{SpatialPolygon*} object as defined in package \code{sp}.
#'
#' @export
#'
#' @examples
#' # working on the provinces polygons of Vietnam:
#' vn_prov <- gadmVN::gadm()
#' poly_areas <- areas(vn_prov)
#' # we can see that some of the polygons are made of several polygons:
#' sapply(poly_areas, length)
#' # we can verify that it sums well for the whole country
#' rgeos::gArea(vn_prov)
#' sum(sapply(poly_areas, sum))
#' # as well as for each province:
#' cbind(rgeos::gArea(vn_prov, TRUE), sapply(poly_areas, sum))
#'
areas <- function(sppoly) {
  lapply(sppoly@polygons, function(x) sapply(x@Polygons, function(x) x@area))
}
