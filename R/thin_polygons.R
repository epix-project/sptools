# ------------------------------------------------------------------------------
#' Thinning (simplification)
#'
#' The function performs \code{\link[maptools]{thinnedSpatialPoly}} on a
#' \code{sf} object of polygons.
#'
#' @param sf_obj an objet of class "sf".
#' @param tolerance the tolerance value in the metric of the input object (cf.
#'  function `thinnedSpatialPoly`).
#'
#' @importFrom sf as_Spatial st_as_sf
#' @importFrom maptools thinnedSpatialPoly
#'
#' @examples
#' vn <- gadm("vietnam", "sf", 0)
#' vn2 <- thin_polygons(vn, 0.1)
#'
#' sp::plot(vn)
#' sp::plot(vn2)
#' @export
thin_polygons <- function(sf_obj, tolerance) {
  sf_obj <- as_Spatial(sf_obj)
  sf_obj <- thinnedSpatialPoly(sf_obj, tolerance)
  sf_obj <- st_as_sf(sf_obj)
  sf_obj
}
