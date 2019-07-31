library(sf)
library(sp)

context("resample_from_grid")

test_that("`resample_from_grid` has the correct behaviour", {

  # Raster
  ppp2010 <- worldpopVN::getpop(2010)
  # SpatialPolygons
  fr <- as_Spatial(sptools::gadm("France", "sf", 0, intlib = TRUE))

  # SpatialPoints
  gridfr <- makegrid(fr, 100)
  gridfr <- SpatialPoints(gridfr, CRS(proj4string(ppp2010)))

  expect_error(resample_from_grid(ppp2010, gridfr))

})
