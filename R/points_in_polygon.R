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
#' @importFrom sp identicalCRS over spTransform
#' @importFrom rgeos gIntersection
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' vn <- gadmVN::gadm(level = "country")
#' data(stations, package = "imhen")
#' length(stations)
#' stations2 <- points_in_polygon(stations, vn)
#' length(stations2)
#' plot(vn)
#' plot(stations, add = TRUE, col = "red")
#' plot(stations2, add = TRUE, col = "blue")
#' # The 3 stations that are excluded should obviously not be excluded.
#' # Let's add a 5-km buffer zone around the coast:
#' vnp <- sp::spTransform(vn, projVN)
#' vnp2 <- rgeos::gBuffer(vnp, width = 5000)
#' vn2 <- sp::spTransform(vnp2, proj0)
#' plot(vn2, border = "green", add = TRUE)
#' # Let's now see how many stations are discarded:
#' stations3 <- points_in_polygon(stations, vn2)
#' length(stations3)
#' # Let's now see with a simplified version of the polygon:
#' vn3 <- largest_polygons(vn)
#' stations4 <- points_in_polygon(stations, vn3)
#' length(stations4)
#' plot(vn3)
#' plot(stations, add = TRUE, col = "red")
#' plot(stations4, add = TRUE, col = "blue")
#' # With the 5-km buffer zone:
#' vn4 <- vn3 %>%
#'   sp::spTransform(projVN) %>%
#'   rgeos::gBuffer(width = 5000) %>%
#'   sp::spTransform(crs(vn))
#' plot(vn4, border = "green", add = TRUE)
#' stations5 <- points_in_polygon(stations, vn4)
#' length(stations5)
#' plot(vn4)
#' plot(stations, add = TRUE, col = "red")
#' plot(stations5, add = TRUE, col = "blue")
points_in_polygon <- function(points, polygon) {
  if(!identicalCRS(points, polygon))
    polygon <- spTransform(polygon, crs(points))
  suppressWarnings(gIntersection(points, polygon)) # suppressWarning could be suppressed with a version of sp > 1.2-5.
}
