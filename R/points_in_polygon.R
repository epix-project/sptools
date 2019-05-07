#' Filtering SpatialPoints* inside a SpatialPolygon*
#'
#' Filtering the points of a SpatialPoints* object that are inside a
#' SpatialPolygon* object.
#'
#' @param points \code{SpatialPoints*} object as defined in the package \code{sp}.
#' @param polygon \code{SpatialPolygon*} object as defined in the package \code{sp}.
#'
#' @return A \code{SpatialPoints*} object of which all the points are inside the
#' inputed \code{SpatialPolygon*} object.
#'
#' @importFrom sp identicalCRS over spTransform SpatialPolygons
#' @importFrom stats complete.cases
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' # SpatialPolygonsDataFrame
#' vn <- sf::as_Spatial(gadmVN::gadm(level = "country"))
#' # SpatialPoints
#' stations <- as(imhen::stations, "Spatial")
#' length(stations)
#'
#'
#' stations2 <- points_in_polygon(stations, vn)
#' length(stations2)
#' sp::plot(vn)
#' sp::plot(stations, add = TRUE, col = "red")
#' sp::plot(stations2, add = TRUE, col = "blue")
#' # The 2 stations that are excluded should obviously not be excluded.
#'
#' # Let's add a 5-km buffer zone around the coast:
#' vnp <- sp::spTransform(vn, projVN)
#' vnp2 <- rgeos::gBuffer(vnp, width = 5000)
#' vn2 <- sp::spTransform(vnp2, proj0)
#' sp::plot(vn2, border = "green", add = TRUE)
#' # Let's now see how many stations are discarded:
#' stations3 <- points_in_polygon(stations, vn2)
#' length(stations3)
#'
#'
#' # Let's now see with a simplified version of the polygon:
#' vn3 <- largest_polygons(vn)
#' stations4 <- points_in_polygon(stations, vn3)
#' length(stations4)
#' sp::plot(vn3)
#' sp::plot(stations, add = TRUE, col = "red")
#' sp::plot(stations4, add = TRUE, col = "blue")
#'
#' # With the 5-km buffer zone:
#' vn4 <- sp::spTransform(vn3, projVN)
#' vn4 <- rgeos::gBuffer(vn4, width = 5000)
#' vn4 <- sp::spTransform(vn4, crs(vn))
#'
#' sp::plot(vn4, border = "green", add = TRUE)
#' stations5 <- points_in_polygon(stations, vn4)
#' length(stations5)
#' sp::plot(vn4)
#' sp::plot(stations, add = TRUE, col = "red")
#' sp::plot(stations5, add = TRUE, col = "blue")
points_in_polygon <- function(points, polygon) {
  if (class(polygon) == "SpatialPolygonsDataFrame")
    polygon <- SpatialPolygons(polygon@polygons,
                               proj4string = polygon@proj4string)
  if (!identicalCRS(points, polygon))
    polygon <- spTransform(polygon, crs(points))
  points[complete.cases(over(points, polygon)), ]
}
