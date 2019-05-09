# ------------------------------------------------------------------------------
#' Defines new boundaries box and projections of a sf object
#'
#' The function defines the attributes \code{bbox} and \code{crs} of a sf
#' object.
#'
#' @param sf_obj an objet of class "sf".
#' @param boundbox character, bounding box.
#' @param crs character, coordinate reference system.
#' @examples
#' library(sf)
#' vn <- gadm("Vietnam", "sf", 0)
#' vn
#' bb <- st_bbox(vn)
#' crs <- st_crs(vn)
#'
#' vn2 <- gadmVN::gadm()
#' vn2 <- define_bbox_proj(vn2, bb, crs)
#' vn2
#'
#' @export
define_bbox_proj <- function(sf_obj, boundbox, crs) {
  attr(sf_obj[["geometry"]], "bbox") <- boundbox
  attr(sf_obj[["geometry"]], "crs") <- crs
  sf_obj
}
