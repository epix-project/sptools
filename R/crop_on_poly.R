#' Crops a raster file according to a polygon
#'
#' @param rstr raster
#'
#' @param plgn spatial polygon sp
#'
#' @examples
#' # library(sf)
#' # library(sptools)
#' # library(dplyr)
#' # provinces <- provinces <- "vietnam" %>%
#' #   sptools::gadm("sf", 1) %>%
#' #   transmute(province = VARNAME_1) %>%
#' #   as("Spatial")
#' # ppp2010 <- worldpopVN::getpop(2010)
#' # hanoi <- crop_on_poly(subset(provinces, province == "Ha Noi"), ppp2010)
#'
#' @importFrom raster crop
#' @importFrom raster rasterize
#' @importFrom raster mask
#'
#' @export
#'
crop_on_poly <- function(rstr, plgn) {
#  require(raster) # crop, rasterize, mask
  crpd_rstr <- crop(rstr, plgn)
  themask <- rasterize(plgn, crpd_rstr)
  mask(crpd_rstr, themask)
}
