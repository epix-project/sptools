library(dictionary) # for "match_pattern", "XX_history", "XX_admin1",
# "XX_admin2", "translate"

context("`sf_aggregate_lst`")

test_that("`sf_aggregate_lst` returns the correct output", {

  tmp <- file.path(tempdir())
  dir.create(tmp)
  sf1 <- sptools::gadm("Cambodia", "sf", 1, intlib = FALSE, save = FALSE)
  sf1 <- transform(sf1, admin1 = translate(sf1$NAME_1 , kh_admin1))
  sf1 <- sf1[, c("admin1", "geometry")]
  sf1 <- sf::st_as_sf(sf1)
  unlink(tmp, recursive = TRUE)

  test1a <- sf_aggregate_lst(sf1, kh_history, from = "1998-01-01")
  testthat::expect_equal(match_pattern(test1a %>% as.data.frame, "admin1",
                             kh_admin1_year), "1997-2013")

  test1b <- sf_aggregate_lst(sf1, kh_history, from = "1995-01-01")
  testthat::expect_equal(match_pattern(test1b %>% as.data.frame, "admin1",
                             kh_admin1_year), "1994-1997")


  sf2 <- sptools::gadm("Laos", "sf", 2, intlib = FALSE, save = FALSE)
  sf2 <- transform(sf2, admin1 = translate(sf2$NAME_1 , la_admin1),
                   admin2 = translate(NAME_2 , la_admin2))
  sf2 <- sf2[, c("admin1", "admin2", "geometry")]
  sf2 <- sf::st_as_sf(sf2)
  test2a <- sf_aggregate_lst(sf2, la_history, from = "2008-01-01")
  testthat::expect_equal(match_pattern(test2a %>% as.data.frame, "admin1",
                             la_admin1_year), "1997-2006")

  test2b <- sf_aggregate_lst(sf2, la_history, from = "1998-01-01")
  testthat::expect_equal(match_pattern(test2b %>% as.data.frame, "admin1",
                             la_admin1_year), "1997-2006")


  sf3 <- sptools::gadm("Thailand", "sf", 1, intlib = FALSE, save = FALSE)
  sf3 <- transform(sf3, admin1 = translate(NAME_1 , th_admin1))
  sf3 <- sf3[, c("province", "geometry")]
  sf3 <- sf::st_as_sf(sf3)
  test3 <- sf_aggregate_lst(sf3, th_history, from = "1980-01-01")
  testthat::expect_equal(match_pattern(test3 %>% as.data.frame, "admin1",
                             th_admin1_year), "1977-1981")
})
