#' Resolution of a grid
#'
#' Equivalent to the `raster::res` function but for a grid `SpatialPoints`
#' object as returned by `sp::makegrid` or `sptools::make_grid`.
#'
#' @param grdsppts a grid \code{SpatialPoints} object as returned by
#'                 \code{sp::makegrid} or \code{sptools::make_grid}.
#'
#' @author Marc Choisy
#'
#' @importFrom sp coordinates
#' @importFrom utils head
#'
#' @export
#' @examples
#' library(sf)
#' vn <- gadmVN::gadm(level = "country")
#' vn <- sf::as_Spatial(vn)
#' sp::plot(vn)
#' grid <- make_grid(vn, 100)
#' res_grid(grid)
res_grid <- function(grdsppts) {
  grdsppts <- coordinates(grdsppts)
  grdsppts <- data.frame(grdsppts)
  grdsppts <- lapply(grdsppts, diff)
  grdsppts <- lapply(grdsppts, function(x) x[x > 0])
  grdsppts <- unlist(grdsppts)
  grdsppts <- round(grdsppts, 2)
  grdsppts <- table(grdsppts)
  grdsppts <- sort(grdsppts, decreasing = TRUE)
  grdsppts <- head(grdsppts, 1)
  grdsppts <- names(grdsppts)
  grdsppts <- as.numeric(grdsppts)
  grdsppts
}
