#' Get GADM data
#'
#' Download sp or sf file format from GADM
#'
#' @param country character string, name of the country to download
#' @param format character string, format to downlaod, either sp or sf
#' @param level integer between 0 and 3 or 4, level of the administrative
#'   borders, 0 being country borders.
#' @param file_rm boolean, if TRUE, remove the dowmloaded file.
#'   By default, TRUE.
#' @param path character string, name where the dowloaded file is saved
#'
#' @author Marc Choisy, Lucie Contamin
#'
#' @importFrom countrycode countrycode
#' @importFrom utils download.file
#' @export
gadm <- function(country, format, level, file_rm = TRUE, path = NULL) {
  country <- countrycode(country, "country.name", "iso3c")
  file <- paste0("gadm36_", country, "_", level, "_", format, ".rds")
  if (is.null(path) == FALSE) {
    pfile <- paste0(path, "/", file)
  } else {
    pfile <- file
  }
  if (!file.exists(pfile))
    download.file(paste0("https://biogeo.ucdavis.edu/data/gadm3.6/R", format,
                         "/", file), pfile, mode = "wb")
  data <- readRDS(pfile)
  if (isTRUE(file_rm)) file.remove(pfile)
  data
}
