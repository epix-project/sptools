library(dictionary) # for "match_pattern", "**_history", "**_admin1",
# "**_admin2", "translate"

context("`sf_aggregate_lst`")

test_that("`sf_aggregate_lst` returns the correct output", {

  sf1 <- sptools::gadm("Cambodia", "sf", 1, intlib = FALSE, save = FALSE)
  sf1 <- transform(sf1, admin1 = translate(sf1$NAME_1, kh_admin1))
  sf1 <- sf1[, c("admin1", "geometry")]
  sf1 <- sf::st_as_sf(sf1)

  test1a <- sf_aggregate_lst(sf1, kh_history, from = "1998-01-01")
  testthat::expect_equal(match_pattern(as.data.frame(test1a), "admin1",
                                       kh_admin1_year), "1997-2013")

  test1b <- sf_aggregate_lst(sf1, kh_history, from = "1995-01-01")
  testthat::expect_equal(match_pattern(as.data.frame(test1b), "admin1",
                                       kh_admin1_year), "1994-1997")


  sf2 <- sptools::gadm("Laos", "sf", 2, intlib = FALSE, save = FALSE)
  sf2 <- transform(sf2, admin1 = translate(sf2$NAME_1, la_admin1),
                   admin2 = translate(NAME_2, la_admin2))
  sf2 <- sf2[, c("admin1", "admin2", "geometry")]
  sf2 <- sf::st_as_sf(sf2)

  test2a <- sf_aggregate_lst(sf2, la_history, from = "2008-01-01")
  testthat::expect_equal(match_pattern(as.data.frame(test2a), "admin1",
                                       la_admin1_year), "2006-2013")

  test2b <- sf_aggregate_lst(sf2, la_history, from = "1998-01-01")
  testthat::expect_equal(match_pattern(as.data.frame(test2b), "admin1",
                                       la_admin1_year), "1997-2006")

  sf3 <- sptools::gadm("Thailand", "sf", 1, intlib = FALSE, save = FALSE)
  sf3 <- transform(sf3, admin1 = translate(NAME_1, th_admin1))
  sf3 <- sf3[, c("admin1", "geometry")]
  sf3 <- sf::st_as_sf(sf3)
  test3 <- sf_aggregate_lst(sf3, th_history, from = "1980-01-01")
  testthat::expect_equal(match_pattern(as.data.frame(test3), "admin1",
                                       th_admin1_year), "1977-1981")

  testthat::expect_equal(sptools:::select_events(dictionary::kh_history, 2020,
                                                 2020), list())

})
