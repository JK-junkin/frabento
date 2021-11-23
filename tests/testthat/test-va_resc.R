library(frabento)

test_that("va_resc", {
    vl <- 1:5
    vr <- seq(20, 100, by = 20)

    expect_error(va_resc(vl, -10, 100))
    expect_error(va_resc(vl, 10, -100))
    expect_warning(va_resc(vl, 0, 100))
    expect_warning(va_resc(vl, c(-10, -5), c(0, -100)))

    expect_equal(va_resc(vr, 10, 100), vr/(100/10))
    expect_equal(va_resc(vl, 10, 100, "axis"), vl/(10/100))

#     expect_equal(va_resc(vv, c(-10, 0), 100, "var"), vv/(100))
})
