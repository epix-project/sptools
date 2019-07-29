#' Convert a Spatial polygon into a list of Spatial polygons
#'
#' @param plgn a SpatialPolygon*
#'
#' @examples
#' library(sf)
#' # download vietnam admin1 administrative map in the internal library and in
#' # the working direction
#' vn <- sptools::gadm("vietnam", "sf", 1, intlib = TRUE, save = TRUE)
#'
#' provinces <- as(vn["VARNAME_1"], "Spatial")
#' class(provinces)
#' list_provinces <- as_list(provinces)
#' class(list_provinces)
#' table(sapply(list_provinces, class))
#'
#' @export
#'
as_list <- function(plgn) {
  lapply(seq_along(plgn), function(x) plgn[x, ])
}
