#' Get GADM data
#'
#' Download sp or sf file format from GADM in the internal library of the
#' package and/or a specified path and/or a temporary path.
#'
#' If the `path` and/or `intlib` arguments are `NULL`, it will be asked
#' interactively to the user to decide where to store the file. The function
#' will return a message is the file is already present in the internal library
#' or in the specify path.
#'
#' @param country character string, name of the country to download
#' @param format character string, format to downlaod, either sp or sf
#' @param level integer between 0 and 3 or 4, level of the administrative
#'   borders, 0 being country borders.
#' @param path character string, name where the download file is also saved. By
#'   default NULL.
#' @param intlib boolean, if the file is saved in the internal library. By
#'   default NULL.
#'
#' @author Marc Choisy, Lucie Contamin
#'
#' @importFrom countrycode countrycode
#' @importFrom utils download.file installed.packages
#' @examples
#'
#' # to download file only in a specific path
#' kh <- gadm("Cambodia", "sf", 0, path = getwd(), intlib = FALSE)
#'
#' @export
gadm <- function(country, format, level, path = NULL, intlib = NULL) {

  # prerequisite
  country <- countrycode(country, "country.name", "iso3c")
  file <- paste0("gadm36_", country, "_", level, "_", format, ".rds")
  dirname <- paste0(installed.packages()["sptools", "LibPath"],
                     "/sptools/extdata/")
  if (!dir.exists(dirname)) dir.create(dirname)
  pfile <- paste0(dirname, file)

  # download file
  if (!file.exists(pfile)) {
    if(isFALSE(intlib) & file.exists(paste0(path, file)) & !is.null(path)) {
      message(cat(
        paste0("The file '", file, "' is already present in ", path)))
    } else {
      # download in internal library
      download.file(paste0("https://biogeo.ucdavis.edu/data/gadm3.6/R", format,
                           "/", file), pfile, mode = "wb")
    }

    if (is.null(intlib)) {
      # download in internal library
      message(cat(
        paste0("\n Do you want to download it in your internal library ?",
               "  (yes/ no) ? \n")))
      ans <- readline("Selection: ")
      if (ans == "no") {
        tmp <- paste0(tempdir(), "/")
        dir.create(tmp, showWarnings = FALSE)
        file.rename(pfile, paste0(tmp, file))
        pfile <- paste0(tmp, file)
        intlib <- FALSE
      }
      if (ans == "yes") {
        intlib <- TRUE
      }
    } else if (intlib == FALSE & !file.exists(paste0(path, file)) &
               isFALSE(path)) {
      tmp <- paste0(tempdir(), "/")
      dir.create(tmp, showWarnings = FALSE)
      file.copy(pfile, paste0(tmp, file))
      file.remove(pfile)
      pfile <- paste0(tmp, file)
    }
  } else {
    message(cat(
      paste0("The file '", file, "' is already present in the library")))
  }

  if(is.null(path) == FALSE & isFALSE(path) == FALSE) {
    if(!file.exists(paste0(path, file))){
      file.copy(pfile, paste0(path, "/", file), overwrite = TRUE)
      if (isFALSE(intlib)) {
        file.remove(pfile)
        pfile <- paste0(path, "/", file)
      }
    } else {
      pfile <- paste0(path, "/", file)
    }
  } else if (isFALSE(path) == FALSE) {
    message(cat(
      paste0("\n Do you want to save the map in another location",
             " (yes/ no (default)) \n")))
    ans <- readline("Selection: ")
    if (ans == "yes") {
      message(cat(paste0("\n Can you provides the path to the location ? \n",
                         "By default, working direction")))
      ans <- readline("Path: ")
      if (ans == "") {
        ans <- getwd()
      }
      if (file.exists(paste0(ans, "/", file))) {
        message(cat(
          paste0("The file '", file, "' is already present in ", ans)))
      }
      file.copy(pfile, paste0(ans, "/", file), copy.mode = TRUE)
      if(isFALSE(intlib)) {
        file.remove(pfile)
        pfile <- paste0(ans, "/", file)
      }
    }
    if (ans %in% c("no", "")) {
      tmp <- paste0(tempdir(), "/")
      dir.create(tmp, showWarnings = FALSE)
      file.copy(pfile, paste0(tmp, file))
      if (isFALSE(intlib) & paste0(dirname(pfile), "/") != tmp) {
        file.remove(pfile)
      }
      pfile <- paste0(tmp, file)
    }
  }
  data <- readRDS(pfile)

  if(exists("tmp")) {
    if(file.exists(paste0(tmp, file))) file.remove(paste0(tmp, file))
  }
  data
}
