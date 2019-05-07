library(dictionary) # for "match_pattern", "XX_history", "XX_province",
# "XX_district"

context("`sf_aggregate_lst`")

test_that("`sf_aggregate_lst` returns the correct output", {

  tmp <- file.path(tempdir())
  dir.create(tmp)
  sf1 <- sptools::gadm("Cambodia", "sf", 1, intlib = FALSE, save = TRUE)
  sf1 <- transform(sf1, province =
                     kh_province[stringi::stri_escape_unicode(sf1$NAME_1)])
  sf1 <- sf1[, c("province", "geometry")]
  sf1 <- sf::st_as_sf(sf1)
  unlink(tmp, recursive = TRUE)

  test1a <- sf_aggregate_lst(sf1, kh_history, from = "1998-01-01")
  expect_equal(match_pattern(test1a %>% as.data.frame, "province",
                             kh_province_year),
               "1997-2013")

  test1b <- sf_aggregate_lst(sf1, kh_history, from = "1995-01-01")
  expect_equal(match_pattern(test1b %>% as.data.frame, "province",
                             kh_province_year),
               "1994-1997")

  tmp <- file.path(tempdir())
  dir.create(tmp)
  sf2 <- sptools::gadm("Laos", "sf", 2, intlib = FALSE, save = TRUE)
  sf2 <- transform(sf2, province =
                     la_province[stringi::stri_escape_unicode(sf2$NAME_1)],
                   district =
                     la_district[stringi::stri_escape_unicode(sf2$NAME_2)])
  sf2 <- sf2[, c("province", "district", "geometry")]
  sf2 <- sf::st_as_sf(sf2)
  unlink(tmp, recursive = TRUE)

  test2a <- sf_aggregate_lst(sf2, la_history, from = "2008-01-01")
  expect_equal(match_pattern(test2a %>% as.data.frame, "province",
                             la_province_year),
               "2006-2013")

  test2b <- sf_aggregate_lst(sf2, la_history, from = "1998-01-01")
  expect_equal(match_pattern(test2b %>% as.data.frame, "province",
                             la_province_year),
               "1997-2006")

  tmp <- file.path(tempdir())
  dir.create(tmp)
  sf3 <- sptools::gadm("Thailand", "sf", 1, intlib = FALSE, save = TRUE)
  sf3 <- transform(sf3, province =
                     th_province[stringi::stri_escape_unicode(sf3$NAME_1)])
  sf3 <- sf3[, c("province", "geometry")]
  sf3 <- sf::st_as_sf(sf3)
  unlink(tmp, recursive = TRUE)

  test3 <- sf_aggregate_lst(sf3, th_history, from = "1980-01-01")
  expect_equal(match_pattern(test3 %>% as.data.frame, "province",
                             th_province_year),
               "1977-1981")
})
