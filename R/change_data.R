#' Change the data of a Spatial*DataFrame object.
#'
#' The aim of this function is to propose an alternative to the bug reported
#' \href{https://github.com/edzer/sp/issues/56}{here}.
#'
#' @param spdf A Spatial*DataFrame object.
#' @param df A data frame.
#'
#' @return An object of the same class as \code{spdf}.
#'
#' @export
#'
#' @examples
#  # download vietnam admin1 administrative map in the internal library and in
#' # the working direction
#' vn <- sptools::gadm("vietnam", "sp", 1, intlib = TRUE, save = TRUE)
#' vn
#' change_data(vn, data.frame(gaussian = rnorm(63), uniform = runif(63)))
#'
change_data <- function(spdf, df) {
  spdf@data <- df
  spdf
}
