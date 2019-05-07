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
#' @importFrom sp spTransform proj4string SpatialPointsDataFrame
#' @importFrom raster extract
#'
#' @export
#'
#' @examples
#' library(sp)
#'
#' # download vietnam country administrative map in the internal library and in
#' # the working direction
#' vn <- sptools::gadm("vietnam", "sp", 0, intlib = TRUE, save = TRUE)
#'
#' # With a SpatialPointsDataFrame
#' value <- spsample(vn, 100, "random")
#' value <- SpatialPointsDataFrame(value, data.frame(variable = 1:100))
#' value <- add_from_raster(value, srtmVN::getsrtm(), "elevation")
#'
#' head(value@data)
#'
#' # With a SpatialPoints
#' value <- spsample(vn, 100, "random")
#' value <- SpatialPoints(value, proj4string = vn@proj4string, bbox = vn@bbox)
#' value <- add_from_raster(value, srtmVN::getsrtm(), "elevation")
#'
#' head(value@data)
add_from_raster <- function(sptsdf, rstr, varname = "new_data") {
# Note that we chose to project the SpatialPointsDataFrame instead of the
# RasterLayer because it is much quicker this way.
  proj0 <- proj4string(sptsdf)
  sptsdf <- spTransform(sptsdf, proj4string(rstr))
  if (class(sptsdf) == "SpatialPoints") {
    df <- data.frame(varname = raster::extract(rstr, sptsdf))
    names(df)[which(names(df) == "varname")] <- varname
    sptsdf <- SpatialPointsDataFrame(sptsdf@coords,
                                     df,
                                     proj4string = sptsdf@proj4string)
  } else {
    sptsdf[[varname]] <- raster::extract(rstr, sptsdf)
  }
  sptsdf <- spTransform(sptsdf, proj0)
  sptsdf
}
