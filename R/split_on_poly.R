#' Splits a raster on a spatial polygon
#'
#' This function combines the above functions \code{as_list} and
#' \code{crop_on_poly} to split a \code{RasterLayer} object on the features of a
#' \code{SpatialPpolygons*} object. Of course, the \code{RasterLayer} and the
#' \code{SpatialPpolygons*} objects need to superpose.
#'
#' @param rstr A RasterLayer
#' @param plgns A SpatialPolygon* object
#'
#' @return A list of \code{RasterLayer} objects.
#'
#' @export
#'
#' @examples
#' # provinces <- sptools::gadm("vietnam", "sp", 1)
#' # ppp2010 <- worldpopVN::getpop(2010)
#' # class(ppp2010)
#' # ppp2010_split <- split_on_poly(ppp2010, provinces[1:3, ])
#' # class(ppp2010_split)
#' # table(sapply(ppp2010_split, class))
split_on_poly <- function(rstr, plgns) {
  plgns %>%
    as_list() %>%
    lapply(., function(x) crop_on_poly(rstr, x))
}
