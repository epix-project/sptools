library(dictionary) # for "XX_history", "XX_province"
library(magrittr)   # for " %>% ", "is_in

context("`gadm`")

testthat::test_that("`gadm` has the correct behaviour", {

  tmp <- getwd()
  test1 <- gadm("Cambodia", "sf", 0, path = NULL, intlib = FALSE, save = TRUE)

  testthat::expect_equal(dir(tmp) %>% is_in("gadm36_KHM_0_sf.rds", .), TRUE)
  file.remove("gadm36_KHM_0_sf.rds")

  test2 <- gadm("Cambodia", "sf", 0, save = FALSE, intlib = FALSE)
  testthat::expect_is(test2, c("sf", "data.frame"))


})
