#' Exclude features with NA attributes
#'
#' This function removes the features of a \code{SpatialPointsDataFrame} for
#' which there is NA is the attributes.
#'
#' @param spdf A \code{SpatialPointsDataFrame} object.
#'
#' @return A \code{SpatialPointsDataFrame} object.
#'
#' @export
#'
#' @examples
#' # stations <- as(imhen::stations, "Spatial")
#' # stations@data$elevation[seq(2, 12, 2)] <- NA
#' # head(stations@data)
#' # head(na_exclude(stations)@data)
na_exclude <- function(spdf) {
  require(sp) # for the SpatialPointsDataFrame class
  sel <- which(rowSums(is.na(spdf@data)) > 0)
  if (length(sel) > 0) return(spdf[-sel, ])
  spdf
}
