library(sf)
library(sp)

context("resample_from_grid")

testthat::test_that("`resample_from_grid` has the correct behaviour", {

  # Raster
  ppp2010 <- worldpopVN::getpop(2010)
  # SpatialPolygons
  fr <- as_Spatial(sptools::gadm("France", "sf", 0, intlib = TRUE))

  # SpatialPoints
  gridfr <- fr %>% makegrid(100)%>% SpatialPoints(CRS(proj4string(ppp2010)))

  expect_error(resample_from_grid(ppp2010, gridfr))

})
