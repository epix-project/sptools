#' Splits a SpatialPolygons* into a list of SpatialPolygons*
#'
#' @param spdf a SpatialPolygons* object.
#'
#' @return a list of SpatialPolygons*
#'
#' @author Marc Choisy
#'
#' @examples
#' library(sf)
#'
#' # SpatialPolygonsDataFrame
#' vn <- sf::as_Spatial(gadmVN::gadm(level = "country"))
#' vn_lst <- split_spdf(vn)
#' vn_lst
#'
#' @export
#'
split_spdf <- function(spdf) {
  lapply(seq_along(spdf), function(x) spdf[x, ])
}
