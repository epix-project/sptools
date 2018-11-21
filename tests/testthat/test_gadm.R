library(dictionary) # for "XX_history", "XX_province"

context("`gadm`")

testthat::test_that("`gadm` has the correct behaviour", {

  tmp <- file.path(tempdir(), "pkgtest")
  dir.create(tmp)
  gadm("Cambodia", "sf", 0, file_rm = FALSE, path = tmp)

  test1 <- dir(tmp)

  testthat::expect_equal(test1, "gadm36_KHM_0_sf.rds")
  unlink(tmp, recursive = TRUE)
})
