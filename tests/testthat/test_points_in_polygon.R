library(raster) # for "crs"
library(sp) # for "CRS"
library(sf) # for "as_Spatial"
library(imhen) # for "stations"
library(gadmVN) # for "gadm"

context("`points_in_polygon`")

testthat::test_that("`points_in_polygon` has the correct behaviour", {

  vn <- sf::as_Spatial(gadmVN::gadm(level = "country"))
  crs(vn) <- sp::CRS("+init=EPSG:4326")

  stations <- as(imhen::stations, "Spatial")

  test1 <- points_in_polygon(stations, vn)
  testthat::expect_equal(class(test1)[1], "SpatialPointsDataFrame")
})
