#' Get GADM data
#'
#' Download sp or sf file format from GADM
#'
#' @param country character string, name of the country to download
#' @param format character string, format to downlaod, either sp or sf
#' @param level integer between 0 and 3 or 4, level of the administrative borders,
#'              0 being country borders.
#'
#' @author Marc Choisy
#'
#' @importFrom countrycode countrycode
#' @importFrom utils download.file
#' @export
gadm <- function(country, format, level) {
  country <- countrycode(country, "country.name", "iso3c")
  file <- paste0("gadm36_", country, "_", level, "_", format, ".rds")
  if (!file.exists(file))
    download.file(paste0("https://biogeo.ucdavis.edu/data/gadm3.6/R", format,
                         "/", file), file)
  data <- readRDS(file)
  file.remove(file)
  data
}
