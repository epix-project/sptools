library(sf)
library(sp)

context("add_from_raster")

testthat::test_that("`add_from_raster` has the correct behaviour", {

  # SpatialPolygons
  fr <- as_Spatial(sptools::gadm("France", "sf", 0, intlib = TRUE))

  test1 <- fr %>%
    spsample(100, "random") %>%
    SpatialPointsDataFrame(data.frame(variable = 1:100))

  expect_error(add_from_raster(test1, srtmVN::getsrtm(), "elevation"))

})
