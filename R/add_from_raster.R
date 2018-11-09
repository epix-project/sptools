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
#' @importFrom sp spTransform proj4string SpatialPointsDataFrame
#' @importFrom raster extract
#' @importFrom dplyr data_frame
#'
#' @export
#'
#' @examples
#' library(sp)
#' library(magrittr) # for " %>% "
#'
#' vn <- sptools::gadm("vietnam", "sp", 0)
#'
#' # With a SpatialPointsDataFrame
#' value <- vn %>%
#'   spsample(100, "random") %>%
#'   SpatialPointsDataFrame(data.frame(variable = 1:100)) %>%
#'   add_from_raster(srtmVN::getsrtm(), "elevation")
#'
#' value %>%
#'   slot("data") %>%
#'   head()
#'
#' # With a SpatialPoints
#' value <- vn %>%
#'   spsample(100, "random") %>%
#'   SpatialPoints(proj4string = vn@proj4string, bbox = vn@bbox) %>%
#'   add_from_raster(srtmVN::getsrtm(), "elevation")
#'
#' value %>%
#'   slot("data") %>%
#'   head()
add_from_raster <- function(sptsdf, rstr, varname = "new_data") {
# Note that we chose to project the SpatialPointsDataFrame instead of the
# RasterLayer because it is much quicker this way.
  proj0 <- proj4string(sptsdf)
  sptsdf %<>% spTransform(proj4string(rstr))
  if (class(sptsdf) == "SpatialPoints") {
    sptsdf <- SpatialPointsDataFrame(sptsdf@coords,
                                     data_frame(!!varname :=
                                                raster::extract(rstr, sptsdf)),
                                     proj4string = rstr@crs)
  } else {
    sptsdf[[varname]] <- raster::extract(rstr, sptsdf)
  }
  sptsdf %<>% spTransform(proj0)
  sptsdf
}
