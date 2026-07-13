test_that("deprecated arguments throw error", {
  expect_error(wmap_sf(lgb = seq(140, 240, 20)), "no longer supported")
  expect_error(wmap_sf(ltb = seq(40, 70, 10)), "no longer supported")
})

test_that("plot can be built", {
  p <- wmap_sf(lgl = c(135, 245), ltl = c(35, 70))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot can be built with Greenwich centered map", {
  p <- wmap_sf(pacific_centered = FALSE, lgl = c(-45, 45), ltl = c(-45, 45))
  expect_no_error(ggplot2::ggplot_build(p))
})
