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

  test3 <- gadm("Cambodia", "sf", 0, save = FALSE, intlib = TRUE)
  test4 <- capture.output(gadm("Cambodia", "sf", 0))
  testthat::expect_equal(test4[1], paste0("The file 'gadm36_KHM_0_sf.rds' is ",
     "already present in the internal library.Simple feature collection with 1",
     " feature and 2 fields"))
  test5 <- capture.output(gadm("Cambodia", "sf", 0, force = TRUE))
  testthat::expect_equal(test5[1],
                      "Simple feature collection with 1 feature and 2 fields")

  test6 <- gadm("Cambodia", "sf", 0, intlib = NULL, save = NULL)
  testthat::expect_is(test6, c("sf", "data.frame"))

})
