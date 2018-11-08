#' Adds values of a RasterLayer object to a SpatialPointsDataFrame object
#'
#' Based on the \code{\link[raster]{extract}} function.
#'
#' @param sptsdf A \code{SpatialPointsDataFrame} object.
#' @param rstr A \code{RasterLayer} object.
#' @param varname The name of the new variable. Defaults to "new_data".
#'
#' @return A \code{SpatialPointsDataFrame} object.
#'
#' @importFrom magrittr %<>%
#' @importFrom sp spTransform proj4string
#' @importFrom raster extract
#'
#' @export
#'
#' @examples
#' library(sp)
#' sptools::gadm("vietnam", "sp", 0) %>%
#'   spsample(100, "random") %>%
#'   SpatialPointsDataFrame(data.frame(variable = 1:100)) %>%
#'   add_from_raster(srtmVN::getsrtm(), "elevation") %>%
#'   slot("data") %>%
#'   head()

add_from_raster <- function(sptsdf, rstr, varname = "new_data") {
# Note that we chose to project the SpatialPointsDataFrame instead of the
# RasterLayer because it is much quicker this way.
  proj0 <- proj4string(sptsdf)
  sptsdf %<>% spTransform(proj4string(rstr))
  sptsdf[[varname]] <- raster::extract(rstr, sptsdf)
  sptsdf %<>% spTransform(proj0)
  sptsdf
}
