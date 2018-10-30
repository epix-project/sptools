#' Crops a raster file according to a polygon
#'
#' @param rstr raster
#' @param plgn spatial polygon sp
#'
#' @examples
#'
#' library(sf)
#' library(dplyr)
#'
#' provinces <- provinces <- "vietnam" %>%
#'   sptools::gadm("sf", 1) %>%
#'   transmute(province = VARNAME_1) %>%
#'   as("Spatial")
#' srtm <- srtmVN::getsrtm()
#' hanoi <- crop_on_poly(srtm, subset(provinces, province == "Ha Noi"))
#'
#' @importFrom raster crop
#' @importFrom raster rasterize
#' @importFrom raster mask
#' @importFrom sp proj4string
#' @importFrom sp CRS
#' @importFrom sp spTransform
#'
#' @export
#'
crop_on_poly <- function(rstr, plgn) {
  plgn <- spTransform(plgn, CRS(proj4string(rstr)))
  crpd_rstr <- crop(rstr, plgn)
  themask <- rasterize(plgn, crpd_rstr)
  mask(crpd_rstr, themask)
}
