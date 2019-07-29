#' Removes variable(s) from a Spatial*DataFrame
#'
#' @param spdf A Spatial*DataFrame object
#' @param var A vector of variable names or indexes to remove.
#'
#' @return An object of the same class as \code{spdf}.
#'
#' @export
#'
#' @examples
#' # An example with a SpatialPointsDataFrame object:
#' # devtools::install_github("marc/imhen")
#' library(sf)
#' stations <- as(imhen::stations, "Spatial")
#' stations
#' remove_var(stations, "elevation")
#' remove_var(stations, 2)
#' remove_var(stations)
#'
#' # An example with a SpatialPoints object:
#'# download vietnam admin1 administrative map in the internal library and in
#' # the working direction
#' vn <- sptools::gadm("vietnam", "sp", 1, intlib = TRUE, save = TRUE)
#' vn
#' remove_var(vn, c(6, 9))
remove_var <- function(spdf, var = NULL) {
  if (is.null(var)) var <- seq_len(ncol(spdf@data))
  spdf@data[, var] <- NULL
  spdf
}
