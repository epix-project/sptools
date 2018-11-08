#' Rescales the values of a raster as weights
#'
#' This function rescales the values of a raster between 0 and 1 so that they
#' sum to 1. These values can then be used as weights.
#'
#' @param rstr a RasterLayer object
#'
#' @return A RasterLayer object with values between 0 and 1 and summing to 1.
#'
#' @importFrom raster values values<-
#'
#' @export
#'
#' @examples
#' # provinces <- sptools::gadm("vietnam", "sp", 1)
#' # ppp2010 <- worldpopVN::getpop(2010)
#' # hanoi <- sptools::crop_on_poly(subset(provinces, VARNAME_1 == "Ha Noi"), ppp2010)
#' # # before rescaling:
#' # sum(values(hanoi), na.rm = TRUE)
#' # # after rescaling:
#' # sum(values(rescale(hanoi)), na.rm = TRUE)
rescale_raster <- function(rstr) {
  val <- values(rstr)
  values(rstr) <- val / sum(val, na.rm = TRUE)
  rstr
}
