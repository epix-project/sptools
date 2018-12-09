library(dictionary) # for "XX_history", "XX_province"
library(magrittr)   # for " %>% ", "is_in

context("`gadm`")

testthat::test_that("`gadm` has the correct behaviour", {

  tmp <- file.path(tempdir())
  dir.create(tmp)
  test1 <- gadm("Cambodia", "sf", 0, path = tmp, intlib = FALSE)

  testthat::expect_equal(dir(tmp) %>% is_in("gadm36_KHM_0_sf.rds", .), TRUE)
  unlink(tmp, recursive = TRUE)
})
