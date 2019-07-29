#' Convert an SpatialPoints* grid into a Raster*
#'
#' @param grid A SpatialPointsDataFrame grid (i.e. where points are regularly
#'             spaced).
#'
#' @return A Raster* object.
#'
#' @importFrom raster as.data.frame rasterFromXYZ
#' @importFrom sp proj4string
#'
#' @export
#'
#' @examples
#' library(sf)
#' vn <- gadmVN::gadm(level = "country")
#' vn <- sf::as_Spatial(vn)
#' grid <- make_grid(vn, 100)
#' grid
#' grid2raster(grid)
#'
#' # An example with data:
#' grid2 <- add_variable_spts(grid, 1:length(grid))
#' grid2raster(grid2)
#'
#'
grid2raster <- function(grid) {
  grid2 <- as.data.frame(grid)
  grid2 <- move_xy(grid2)
  grid2 <- rasterFromXYZ(grid2, crs = proj4string(grid))
  grid2
}
