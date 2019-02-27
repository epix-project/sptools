#' Filters and order list by a time range
#'
#' Filters a list to keep only the data corresponding to a certain time
#' range in year (between \code{to} and \code{from} (exclude)) and return list
#' ordered from the most recent to the oldest.
#'
#' @param hist_lst A list containing at least the variable \code{year}.
#' @param from Initial date of the time range, \code{character}, \code{numeric}
#' or of class \code{Date}. Select year after `from`.
#' @param to Final date of the data, \code{character}, \code{numeric} or of
#' class \code{Date}.
#' @return A list with the same variables as \code{}.hist_lst
#' @keywords internal
#' @noRd
select_events <- function(hist_lst, from, to) {
  sel0 <- purrr::map(hist_lst, "year") %>% unlist %>% as.Date()
  sel0 <- sel0 > as.Date(paste0(from, "-01-01")) &
    sel0 <= as.Date(paste0(to, "-12-31"))
  event_lst <- hist_lst[sel0]
  event_lst[order(sapply(event_lst, "[[", "year"), decreasing = TRUE)]
}

################################################################################
#' Aggregates sf object
#'
#' Aggregates data in a select columns accordingly to a time range and by the
#' variavbles concerned by a split/combined event and return a sf object for the
#' time range imputed.
#'
#'  For each `split event`, the geometry of the variables contained  in the slot
#' `after` is combined and rename by the variable in the slot `before`.
#'  For each `rename event`, the variable contained in the slot `after` is
#'  rename by the variable in the slot `before`.
#'
#' @param df A sf data frame containing at least the variables \code{province},
#' \code{geometry}.
#' @param event_lst A list containing a list of event, each code with a slot
#' \code{after}, a slot \code{before}, a slot{event} (split/merge/rename).
#' @param col_name The name of the column containing the element to aggregates.
#' @param col_name2 The name of the 2nd column containing the element to
#' aggregates for complexe event, by default `NULL`.
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range) are merged.
#' @keywords internal
#' @noRd
aggregate_sf <- function(df, event_lst, col_name, col_name2 = NULL) {

  for (i in seq_along(event_lst)) {
    # select one event
    event <- event_lst[[i]]

    # For the complexe merge event
    if (event$event == "complexe merge") {
      suppressWarnings(tmp <-  split(df, f = df[, col_name] %>% unlist %in%
                                       event$after %>% unlist))
      # calculate the new geometry and update the new spatial definition (name
      # and geometry) in the data frame selected
      tmp$`TRUE` %<>%
        select(- !! col_name) %>%
        left_join(event$d.before %>% tidyr::unnest(), by = col_name2) %>%
        select(!! col_name, !! col_name2, geometry) %>%
        sf::st_as_sf() %>%
        group_by(.dots = col_name) %>%
        st_union(by_feature = TRUE) %>%
        ungroup
      # Update the new information in the general data frame
      df <- rbind(tmp$`TRUE`, tmp$`FALSE`) %>%
        arrange(!! sym(eval(col_name))) %>%
        st_cast("MULTIPOLYGON")
    }

    # For the split event
    if (event$event == "split" | event$event == "complexe split") {
      # Split the data frame to select the province that we need to merge
      # together
      if (event$event == "split") {
        suppressWarnings(tmp <-  split(df, f = df[, col_name] %>% unlist %in%
                                         event$after %>% unlist))
      } else {
        suppressWarnings(tmp <-  split(df, f = is.element(df[, col_name2] %>%
                                                 unlist,
                                                 event$d.after$district %>%
                                                   unlist %>% na.omit)))
      }

      # calculate the new geometry
      geom <- st_union(tmp$`TRUE`)
      # Update the new spatial definition (name and geometry) in the data frame
      # selected
      tmp$`TRUE` %<>% dplyr::mutate(new_var = event$before %>% unlist,
                             geometry = geom) %>%
        select(- !! col_name) %>%
        distinct %>%
        rename(!! col_name := new_var)
      # Update the new information in the general data frame
      df <- rbind(tmp$`TRUE`, tmp$`FALSE`) %>%
        st_cast("MULTIPOLYGON")
    }

    # Event rename
    if (event$event == "rename") {
      df %<>% mutate(!! col_name := gsub(event$after, event$before,
                                         !! sym(eval(col_name))))
    }
  }
  df %>% arrange(!! sym(eval(col_name)))
}

################################################################################
#' Aggregates sf from a list of event
#'
#' Tidy the data and merges data accordingly to a time range and by the
#' values concerned by a split/merge/rename event and return a sf data frame
#' for the time range imputed.
#'
#' For each `split event`, the geometry of the variables contained  in the slot
#' `after` is combined and rename by the variable in the slot `before`.
#'  For each `rename event`, the variable contained in the slot `after` is
#'  renamed.
#'  In the new rows, <NA> will be added in the other column than `sel` and
#'  `geometry`
#'
#' @param df_sf A sf data frame containing at least the variables
#' \code{province}, \code{geometry} and \code{district} if `history_lst`
#' contains complexe event
#' @param history_lst A list containing a list of event, each code with a slot
#' \code{after}, a slot \code{before}, a slot{event} (split/merge/rename/
#' complexe merge/complexe split) and a slot \code{year}.
#' @param from Initial date of the time range selected for the province
#' definition, of the class \code{Date}, \code{character} or \code{numeric}.
#' @param to Final date of the time range selected for the province
#' definition, of the class \code{Date}, \code{character} or \code{numeric}, by
#' default  \code{"2018-12-31"}
#'
#' @return A object of the same class as \code{df_sf} in which all the variables
#' and geometry that needed to be aggregated or renamed (according to the time
#' range) are changed.
#'
#' @importFrom dplyr select arrange left_join mutate rename distinct contains
#' sym group_by summarise ungroup
#' @importFrom sf st_union st_cast st_join
#' @importFrom magrittr %>% %<>%
#' @importFrom stats na.omit
#' @importFrom rlang := !!
#'
#' @examples
#'
#'  # to have the list of split/merge/rename event for Vietnam
#'  vn_history <- dictionary::vn_history
#'
#'  vn_prov04 <- gadmVN::gadm(date = "2004-01-01")
#'  vn_prov70 <- sf_aggregate_lst(vn_prov04, vn_history, from = "1970",
#'                                to = "2004")
#'
#' @export
sf_aggregate_lst <- function(df_sf, history_lst, from, to = "2018-12-31") {

  if (select_events(history_lst, from, to) %>% purrr::map("event") %>%
      grepl("complexe", .) %>% any) {
   sel <- c("province", "district")
   col_name <- "province"
   col_name2 <- "district"
  } else {
    sel <- "province"
    col_name <- "province"
  }

  # Prepare the data frame
  df <- select(df_sf, !! sel, geometry)
  # Select event(s)
  event_lst <- select_events(history_lst, from = from, to = to)
  # Merges back or renames variable(s) together (combine geometry)
  suppressWarnings(df_agg <- aggregate_sf(df, event_lst, col_name,
                                          col_name2 = col_name2) %>%
                     group_by(.dots = col_name) %>%
                     summarise)
  class(df_agg) <- c("sf", "data.frame")
  df_agg %<>% st_cast("MULTIPOLYGON")
}

## quiets concerns of R CMD check for the values that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("geometry", "new_var",
                                                         "."))
