library(sf) # for "as_Spatial"
library(imhen) # for "stations"

context("`na_exclude`")

testthat::test_that("`na_exclude` has the correct behaviour", {

  stations <- as(imhen::stations, "Spatial")

  test1 <- na_exclude(stations)
  expect_equal(class(test1), class(stations))
})
