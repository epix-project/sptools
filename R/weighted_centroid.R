#' Calculate the weighted centroid
#'
#' Calculate the coordinate of the weighted centroid from a Spatial Points Data
#' Frame object
#'
#' @param  spdt Spatial Points data frame with weighted information in the
#'  slot \code{data}
#'
#' @return coordinates x,y of a point
#'
#' @importFrom sp coordinates
#' @importFrom magrittr %>%
#' @export
weighted_centroid <- function(spdt) {
  weights <- spdt %>% # step 1
    slot("data") %>% {. / sum(.)}
  spdt %>%            # step 2
    coordinates() %>%
    as.data.frame() %>%
    lapply(. %>% `*`(weights) %>% sum()) %>%
    unlist()
}