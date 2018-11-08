#' Resample a Raster object from a Grid
#'
#' This function is a wrapper around the \code{\link[raster]{resample}}
#' function. Instead of resampling a Raster object from another Raster object as
#' in \code{\link[raster]{resample}}, this function resamples a Raster object
#' from a \code{SpatialPoints*} grid object. In case of a \code{RasterBrick} or
#' a \code{RasterStack} object, it resamples only the first layer and returns it
#' as a \code{RasterLayer} object.
#'
#' @param rstr A \code{Raster*} object (i.e. a \code{RasterLayer} or a
#' \code{RasterBrick} object).
#' @param grd A \code{SpatialPoint} object with points regularly spaced.
#'
#' @return A \code{RasterLayer} object.
#'
#' @importFrom magrittr %>%
#' @importFrom raster rasterFromXYZ raster resample
#' @importFrom sp spTransform proj4string
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sp)
#' library(raster)
#' library(wordlpopVN)
#'
#'
#' # The polygon of a country:
#' country <- gadm("vietnam", "sp", 0)
#' # The raster of population
#' ppp2010 <- worldpopVN::getpop(2010)
#'
#' # A grid of 100 points over the country:
#' proj <- proj4string(country)
#' grid100 <- country %>%
#'   makegrid(100) %>%
#'   SpatialPoints(CRS(proj))
#'
#' # Let's resample:
#' ppp2010rspld <- resample_from_grid(ppp2010, grid100)
#'
#' # Let's compare:
#' ppp2010
#' ppp2010rspld
#'
#' plot(ppp2010rspld)
#' plot(grid100, add = TRUE)
#' plot(country, add = TRUE)
#' }
resample_from_grid <- function(rstr, grd) {
  crs <- proj4string(rstr)
  # this function manages RasterLayer or Bricks (or Stack)
  layer_or_brick <- function(x) {
    if (class(x) == "RasterLayer") return(x)
    raster(x, 1)
  }
  grd %>%
    spTransform(crs) %>%
    as.data.frame() %>%
    rasterFromXYZ(crs = crs) %>%
    layer_or_brick() %>%
    resample(rstr, .)
}
