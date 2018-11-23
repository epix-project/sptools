library(magrittr)  # for "%>%"
library(dictionary) # for "match_pattern", "XX_history", "XX_province",
# "XX_district"

context("`sf_aggregate_lst`")

test_that("`sf_aggregate_lst` returns the correct output", {

  sf1 <- sptools::gadm("Cambodia", "sf", 1, file_rm = TRUE) %>%
    mutate(province = NAME_1 %>% stringi::stri_escape_unicode() %>%
             kh_province[.]) %>%
    select(province, geometry)

  test1a <- sf_aggregate_lst(sf1, kh_history, from = "1998-01-01")
  expect_equal(match_pattern(test1a %>% as.data.frame, "province",
                             kh_province_year),
               "1997-2013")

  test1b <- sf_aggregate_lst(sf1, kh_history, from = "1995-01-01")
  expect_equal(match_pattern(test1b %>% as.data.frame, "province",
                             kh_province_year),
               "1994-1997")

  sf2 <- sptools::gadm("Laos", "sf" , 2, file_rm = TRUE) %>%
    mutate(province = NAME_1 %>% stringi::stri_escape_unicode() %>%
             la_province[.],
           district = NAME_2 %>% stringi::stri_escape_unicode() %>%
             la_district[.]) %>%
    select(province, district, geometry)

  test2a <- sf_aggregate_lst(sf2, la_history, from = "2008-01-01")
  expect_equal(match_pattern(test2a %>% as.data.frame, "province",
                             la_province_year),
               "2006-2013")

  test2b <- sf_aggregate_lst(sf2, la_history, from = "1998-01-01")
  expect_equal(match_pattern(test2b %>% as.data.frame, "province",
                             la_province_year),
               "1997-2006")

  sf3 <- sptools::gadm("Thailand", "sf" , 1, file_rm = TRUE) %>%
    mutate(province = NAME_1 %>% stringi::stri_escape_unicode() %>%
             th_province[.]) %>%
    select(province, geometry)

  test3 <- sf_aggregate_lst(sf3, th_history, from = "1980-01-01")
  expect_equal(match_pattern(test3 %>% as.data.frame, "province",
                             th_province_year),
               "1977-1981")

})
