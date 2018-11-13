EPSG <- rgdal::make_EPSG()
EPSG <- EPSG[! is.na(EPSG$code), ]


revcol <- function(x) {
  purrr::safely(mcutils::reverse, matrix(c(NA, "+proj"), 1))(x, 2)$result
}


reformat <- function(x) {
  if (is.null(dim(x))) return(as.list(x))
  else return(x)
}


epsg <- epsg$prj4 %>%
  strsplit(" ") %>%
  lapply(grep, pattern = "proj|ellps|units", value = TRUE) %>%
  lapply(. %>%
           strsplit("=") %>%
           as.data.frame() %>%
           t() %>%
           revcol() %>%
           reformat() %>%
           as.data.frame(stringsAsFactors = FALSE) %>%
           unname() %>%
           do.call(setNames, .)) %>%
  do.call(dplyr::bind_rows, .) %>%
  cbind(code = paste0("+init=", epsg$code), ., note = sub("# ", "", epsg$note), prj4 = epsg$prj4)


