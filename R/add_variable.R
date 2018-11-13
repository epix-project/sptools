#' Adds variable(s) to a Spatial*DataFrame
#'
#' @param sp A Spatial*DataFrame object
#' @param df A vector of a data frame
#'
#' @return An object of the same class than \code{sp}.
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' vn <- gadm("vietnam", "sp", 1)
#' vn
#' vn %<>% add_variable_spdf(rnorm(63))
#' vn
#' # The name of the new variable will be "df". To prevent that we could choose
#' # the data frame format:
#' vn %<>% add_variable_spdf(data.frame(normal = rnorm(63)))
#' vn
#' # The data frame format allows the additional possibility to add several
#' # additional variables all at once:
#' vn %<>% add_variable_spdf(data.frame(uniform = runif(63), exponential = rexp(63)))
#' vn
add_variable_spdf <- function(spdf, df) {
  spdf@data <- cbind(spdf@data, df)
  spdf
}


add_variable_sp <- function(sp, df) {

}
