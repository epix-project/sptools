#' CRS of a Spatial* Object
#'
#' Retrieve the CRS of a Spatial* Object
#'
#' @return A CRS object as defined in the \code{sp} package.
#'
#' @export
#'
#' @importFrom sp proj4string CRS
#'
#' @examples
#' vn_prov <- gadmVN::gadm()
#' crs(vn_prov)
#' vn_prov2 <- sp::spTransform(vn_prov, projVN)
#' crs(vn_prov2)
#' vn_prov3 <- sp::spTransform(vn_prov2, crs(vn_prov))
#' crs(vn_prov)
#'
crs <- function(spobject) {
  sp::CRS(sp::proj4string(spobject))
}
