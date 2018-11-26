#' Transforms an sf points into a dataframe
#'
#' @param sfpts An \code{sf} points object.
#'
#' @return A data frame that contains the same fields as \code{sfpts} plus the
#' longitude and latitude in 2 separate columns.
#'
#' @importFrom sf st_geometry<- st_coordinates
#'
#' @export
#'
#' @examples
#' sfpts2df(worldpopVN::popcenters2009)
#'
sfpts2df <- function(sfpts) {
  data <- sfpts
  st_geometry(data) <- NULL
  data.frame(data, setNames(data.frame(st_coordinates(sfpts)), c("longitude", "latitude")))
}
