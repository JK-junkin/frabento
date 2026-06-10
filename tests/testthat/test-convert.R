testthat::test_that("space-sensitive parsing works correctly", {
  expect_equal(conv_dm2dd("35.3"), 35.5, tolerance = 1e-10)
  expect_equal(conv_dm2dd("35. 3"), 35.05, tolerance = 1e-10)
  expect_equal(conv_dm2dd("35.    3"), 35.05, tolerance = 1e-10)
})
# -----------------------------------------------------------------------------
testthat::test_that("degree-minute-second style formats work correctly", {
  expect_equal(conv_dm2dd("35°30′"), 35.5, tolerance = 1e-10)
  expect_equal(conv_dm2dd("30°30′750″"), 30 + 30.750 / 60, tolerance = 1e-10)
  expect_equal(conv_dm2dd("30°30.750′"), 30 + 30.750 / 60, tolerance = 1e-10
  )
})
# -----------------------------------------------------------------------------
testthat::test_that("negative coordinates with separators work correctly", {
  expect_equal(conv_dm2dd("-35-30"), -35.5, tolerance = 1e-10)
  expect_equal(conv_dm2dd("-30-  3"), -30.05, tolerance = 1e-10)
  expect_equal(conv_dm2dd("-135°30′"), -135.5, tolerance = 1e-10)
})
# -----------------------------------------------------------------------------
testthat::test_that("vectorized negative input works correctly", {
  out <- conv_dm2dd(c("-35-30", "-30- 3"))
  expect_equal(out, c(-35.5, -30.05), tolerance = 1e-10)
})
# -----------------------------------------------------------------------------
testthat::test_that("numeric values are treated as DD by default", {
  dmnum <- c(34.30, 34.3, 34.03, 34, -120.5, -30.8)
  expect_equal(conv_dm2dd(dmnum), dmnum, tolerance = 1e-10)
})
# -----------------------------------------------------------------------------
testthat::test_that("numeric values can be forcibly interpreted as DM", {
  expect_equal(conv_dm2dd(34.30, num_as_dm = TRUE), 34.5, tolerance = 1e-10)
  expect_equal(conv_dm2dd(34.03, num_as_dm = TRUE), 34.05, tolerance = 1e-10)
  expect_equal(conv_dm2dd(-30.8, num_as_dm = TRUE), NA_real_, tolerance = 1e-10)
})
# -----------------------------------------------------------------------------
testthat::test_that("original_value option works correctly", {
  out <- conv_dm2dd(c("35.30", "35.03"), original_value = TRUE)
  expect_named(out, c("35.30", "35.03"))
  out2 <- conv_dm2dd(c("35.30", "35.03"), original_value = FALSE)
  expect_null(names(out2))
})
# -----------------------------------------------------------------------------
testthat::test_that("invalid minute values produce warnings", {
  expect_warning(conv_dm2dd("34.75"))
  expect_warning(conv_dm2dd("130.99"))
  expect_warning(conv_dm2dd(34.75, num_as_dm = TRUE))
})
# -----------------------------------------------------------------------------
testthat::test_that("multiple malformed separators return NA", {
  expect_true(is.na(conv_dm2dd("35...30")))
  expect_true(is.na(conv_dm2dd("--35--30")))
})
