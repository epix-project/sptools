#' CRS of a Spatial* Object
#'
#' Retrieve the CRS of a Spatial* Object
#'
#' @param spobject an object of class Spatial
#'
#' @return A CRS object as defined in the \code{sp} package.
#'
#' @export
#'
#' @importFrom sp proj4string CRS
#'
#' @examples
#' library(sf)
#' # working on the provinces polygons of Vietnam:
#' vn_prov <- gadmVN::gadm()
#' vn_prov <- sf::as_Spatial(vn_prov)
#' crs(vn_prov)
#' vn_prov2 <- sp::spTransform(vn_prov, projVN)
#' crs(vn_prov2)
#' vn_prov3 <- sp::spTransform(vn_prov2, crs(vn_prov))
#' crs(vn_prov)
#'
crs <- function(spobject) {
  sp::CRS(sp::proj4string(spobject))
}
