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
#' @importFrom magrittr %>%
#' @importFrom sp coordinates
#'
#' @export
#'
res_grid <- function(grdsppts) {
  grdsppts %>%
    coordinates() %>%
    data.frame() %>%
    lapply(diff) %>%
    lapply(function(x) x[x > 0]) %>%
    unlist() %>%
    round(2) %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    head(1) %>%
    names() %>%
    as.numeric()
}
