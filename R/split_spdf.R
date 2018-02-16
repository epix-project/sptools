#' Splits a SpatialPolygons* into a list of SpatialPolygons*
#'
#' @param spdf a SpatialPolygons* object.
#'
#' @return a list of SpatialPolygons*
#'
#' @author Marc Choisy
#'
#' @export
#'
split_spdf <- function(spdf) {
  lapply(seq_along(spdf), function(x) spdf[x, ])
}
