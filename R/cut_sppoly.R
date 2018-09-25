#' Transform a 2-variable dataframe into a list of single-point SpatialPoints.
#' @param df a 2-variable dataframe
#' @importFrom sp SpatialPoints
#' @return a list of single-point SpatialPoints
df2splist <- function(df) {
  lapply(split(df, 1:nrow(df)), SpatialPoints)
}


#-------------------------------------------------------------------------------
#' Distance from a point to all the points of a data frame of coordinates.
#' @param pt a SpatialPoints* object
#' @param poly_coord_df a data frame of coordinates with first column being the
#' longitude and the second column being the latitude.
#' @importFrom rgeos gDistance
dist_pt_poly <- function(pt, poly_coord_df) {
  sapply(df2splist(poly_coord_df), gDistance, pt)
}


#' Cuts a Polygon object into 2 lines of which it returns the coordinates
#'
#' The polygon is cut between the points of the polygon that are closest to pt1
#' and pt2
#' @param poly A Polygon object as defined the sp package.
#' @param pt1 A single-point SpatialPoint object as defined in the sp package.
#' @param pt2 A single-point SpatialPoint object as defined in the sp package.
#' @return a list of 2 2-variable data frames of coordinates. In each data frame
#' the first column is the longitude and the second column is the latitude.
cut_poly <- function(poly, pt1, pt2) {
  poly_coord_df <- as.data.frame(poly@coords)
# finds the points of the polygon that are closest to pt1 and pt2:
  points <- sort(sapply(lapply(list(pt1, pt2),
                               dist_pt_poly, poly_coord_df), which.min))
# cuts the polygon between these two points are return output in a list:
  list(poly_coord_df[points[1]:points[2], ],
       rbind(poly_coord_df[points[2]:nrow(poly_coord_df), ],
             poly_coord_df[1:points[1], ]))
}


#x <- vn_prov@polygons[[1]]@Polygons[[1]]

#' @importFrom sp Line Lines SpatialLines
#' @importFrom methods slot
cut_sppoly <- function(sppoly, pt1, pt2) {
#  vn_prov@polygons[[1]]@Polygons
  f1 <- function(x, pt1, pt2) lapply(x, cut_poly, pt1, pt2)

#  vn_prov@polygons
  f2 <- function(x, pt1, pt2) lapply(lapply(x, slot, "Polygons"), f1, pt1, pt2)

  third_level <- function(ind, x) lapply(x, lapply, `[[`, ind)

  out <- lapply(1:2, third_level, f2(sppoly@polygons, pt1, pt2))
  out <- lapply(out, lapply, lapply, Line)
  out <- lapply(out, function(x) Map(Lines, x, sapply(sppoly@polygons, slot, "ID")))
  lapply(out, SpatialLines, crs(sppoly))
}



