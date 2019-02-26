# ------------------------------------------------------------------------------
#' Download corresponding files
#'
#' From a vector of one or multiple string identify which files need to be
#' downloaded
#'
#' @param file file to downloaded (absolute path)
#' @param alt_path alternative path to the internal library that may contains
#' the download files
#' @param format character string, format to downlaod, either sp or sf
#' @noRd
download_file <- function(file, alt_path, format) {
  alt_file <- paste0(alt_path, "/", basename(file))
  if (file.exists(alt_file)) {
    ndfile <- alt_file[file.exists(alt_file)] # files not to download
    file.copy(ndfile, paste0(dirname(file), "/", basename(ndfile)))
  }
  if (any(!file.exists(file))) {
    file <- file[!file.exists(file)]  # file to download
    download.file(paste0("https://biogeo.ucdavis.edu/data/gadm3.6/R", format,
                         "/", basename(file)), file, mode = "wb")
  }
}

#-------------------------------------------------------------------------------
#' Get GADM data
#'
#' Download sp or sf file format from GADM in the internal library of the
#' package and/or a specified path and/or a temporary path.
#'
#' If the \code{save} and/or \code{intlib} arguments are \code{NULL}, it will
#' be asked interactively to the user to decide where to store the file. The
#' function will return a message if the file is already present in the internal
#' library or in the specify path. \cr\cr
#' The table will show you the interaction between these two arguments
#' to download sp or sf file format from GADM:
#'
#'
#'
#' @param country character string, name of the country to download
#' @param format character string, format to downlaod, either sp or sf
#' @param level integer between 0 and 3 or 4, level of the administrative
#'   borders, 0 being country borders.
#' @param intlib boolean, specifies whether the downloaded file should be saved
#' in the library of packages. If \code{NULL}, it will be asked interactively.
#' By default \code{TRUE}, see `Details` for more information.
#' @param save boolean, specifies whether the downloaded file should be saved
#' in a specific path or not. If \code{NULL}, it will be asked interactively.
#' By default \code{FALSE}, see `Details` for more information.
#' @param path character string, path to save the downloaded file. If
#' \code{NULL}, the file will be saved in the working directory. By default
#' \code{NULL}, see `Details` for more information.
#'
#' @author Marc Choisy, Lucie Contamin
#'
#' @importFrom countrycode countrycode
#' @importFrom utils download.file installed.packages
#' @examples
#'
#' # to download Cambodia country administrative boundaries
#' kh <- gadm("Cambodia", "sf", 0)
#'
#' #' # to download Vietnam only in the working directory
#' vn <- gadm("Vietnam", "sf", 0, intlib = FALSE, save = TRUE)
#'
#' @export
gadm <- function(country, format, level, intlib = TRUE, save = FALSE,
                 path = NULL) {

  # prerequisite
  country <- countrycode(country, "country.name", "iso3c")
  file <- paste0("gadm36_", country, "_", level, "_", format, ".rds")
  dirname <- paste0(installed.packages()["sptools", "LibPath"],
                     "/sptools/extdata/")
  if (!dir.exists(dirname)) dir.create(dirname)
  pfile <- paste0(dirname, file)

  # path preparation
  if (isTRUE(save) & is.null(path)) path <- paste0(getwd(), "/")
  if (isFALSE(save)) {
    tmp <-  paste0(tempdir(), "/")
    dir.create(tmp, showWarnings = FALSE)
    path <- tmp
  }
  if (is.null(save)) {
    message(cat("\n Do you want to save the map in another location",
                " (yes/ no (default)) \n"))
    ans <- readline("Selection: ")
    if (ans == "yes") {
      message(cat("\n Can you provides the path to the location ? \n",
                  "By default, working direction"))
      ans2 <- readline("Path: ")
      if (ans2 == "") {
        path <- getwd()
      } else {
        path <- ans2
        if (!dir.exists(path)) dir.create(path, showWarnings = FALSE)
      }
    }
    if (ans %in% c("no", "")) {
      tmp <- paste0(tempdir(), "/")
      dir.create(tmp, showWarnings = FALSE)
      path <- tmp
    }
  }
  path <- paste0(path, "/", file)

  # download file
  if (any(!file.exists(pfile))) {
    if (isFALSE(intlib) & file.exists(path)) {
        message(cat(
          "The file '", file, "' is already present in ",
          unique(dirname(path)), sep = ""))
    }
    download_file(pfile, dirname(path), format = format)

  } else if (file.exists(pfile)){
      message(cat(
        "The file '", file, "' is already present in the internal library.",
        sep = ""))
  }

  # Save the file in the correct path
  file.copy(pfile, path)
  if (is.null(intlib)) {
    message(cat("\n Do you want to download it in your internal library ?",
                "  (yes (default) / no) ? \n"))
    ans <- readline("Selection: ")
    if (ans == "no") {
      intlib <- FALSE
    }
  }

  if (isFALSE(intlib)) {
    file.remove(pfile)
    pfile <- path
  }
  data <- readRDS(pfile)

  # remove temporary direction
  if (exists("tmp")) {
    if (file.exists(paste0(tmp, file))) file.remove(paste0(tmp, file))
  }
  data
}
