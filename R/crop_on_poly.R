#' Crops a raster file according to a polygon
#'
#' @param rstr raster
#' @param plgn spatial polygon sp
#'
#' @examples
#'
#' library(sf)
#' # download vietnam admin1 administrative map in the internal library and in
#' # the working direction
#' vn <- sptools::gadm("vietnam", "sf", 1, intlib = TRUE, save = TRUE)
#'
#' provinces <- as(vn["VARNAME_1"], "Spatial")
#' srtm <- srtmVN::getsrtm()
#' hanoi <- crop_on_poly(srtm, subset(provinces, VARNAME_1 == "Ha Noi"))
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
