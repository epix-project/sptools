#' Projections
#'
#' Pre-defined CRS
#'
#' @format CRS objects as defined in the \code{sp} package:
#' \describe{
#'   \item{proj0}{unprojected coordinates on the GWS84 datum}
#'   \item{projVN}{EPSG:3405 aka VN-2000 aka UTM48N}
#' }
#' @source \url{http://spatialreference.org}
#' @name projections
#' @examples
#' #' vn_prov <- gadmVN::gadm()
#' crs(vn_prov)
#' vn_prov2 <- sp::spTransform(vn_prov, projVN)
#' crs(vn_prov2)
#' vn_prov3 <- sp::spTransform(vn_prov2, crs(vn_prov))
#' crs(vn_prov)
NULL

#' @rdname projections
"proj0"

#' @rdname projections
"projVN"
