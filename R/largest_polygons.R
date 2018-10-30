# internal function: it works on a whole SpatialPolygon* object, not subset possible
#' @importFrom maptools checkPolygonsHoles
.largest_polygons <- function(sppoly) {
  surfaces <- areas(sppoly)
  sppoly@polygons <- Map(function(x, y, z) {
                           x@Polygons <- x@Polygons[y]
                           x@plotOrder <- 1L
                           x@area <- z
                           checkPolygonsHoles(x)
                           },
                         sppoly@polygons,
                         sapply(surfaces, which.max),
                         sapply(surfaces, max))
  sppoly
}




#' Filter the Largest Polygon of each polygon slot of a SpatialPolygon* object
#'
#' @param sppoly \code{SpatialPolygon*} object as defined in package \code{sp}.
#' @param subset a subsetting condition
#'
#' @return An object of the same class as \code{sppoly} where each slot of the
#' \code{polygons} element is made on one single \code{Polygon} object that is
#' the largest \code{Polygon} is the input \code{sppoly}.
#'
#' @seealso \code{\link[sptools]{rm_largest_polygons}}
#'
#' @importFrom rgeos gEnvelope
#'
#' @export
#'
#' @examples
#' library(sf)
#' # working on the provinces polygons of Vietnam:
#' vn_prov <- gadmVN::gadm()
#' vn_prov <- sf::as_Spatial(vn_prov)
#' vn_prov2 <- largest_polygons(vn_prov)
#' sp::plot(vn_prov)
#' sp::plot(vn_prov2)
#' # same with the country level:
#' vn <- gadmVN::gadm(level = "country")
#' vn <- sf::as_Spatial(vn)
#' vn2 <- largest_polygons(vn)
#' rgeos::gEnvelope(vn)
#' # extracting the largest polygon only for Da Nang:
#' vn_prov3 <- largest_polygons(vn_prov, province == "Da Nang")
#' sp::plot(vn_prov3)
#' # extracting the largest polygon only for Da Nang and Khanh Hoa:
#' vn_prov4 <- largest_polygons(vn_prov, province %in% c("Da Nang", "Khanh Hoa"))
#' sp::plot(vn_prov4)
#'
largest_polygons <- function(sppoly, subset) {
  if (missing(subset)) sppoly <- .largest_polygons(sppoly)
  else {
    condition <- substitute(subset)
    condition_c <- as.character(condition)
    variable <- condition_c[2]
    set <- condition_c[3]
    if (condition_c[1] == "==") {
      sppoly@polygons[[which(sppoly@data[[variable]] == set)]] <-
       .largest_polygons(sppoly[sppoly@data[[variable]] == set, ])@polygons[[1]]
    } else {
      set <- gsub("^c\\(\\\"", "", set)
      set <- gsub("\\\"\\)", "", set)
      set <- strsplit(set, "\\\", \\\"")[[1]] # because strsplit returns a list
      for (i in set) sppoly@polygons[[which(sppoly@data[[variable]] == i)]] <-
        .largest_polygons(sppoly[sppoly@data[[variable]] == i, ])@polygons[[1]]
    }
  }
  sppoly@bbox <- gEnvelope(sppoly)@bbox
  sppoly
}


