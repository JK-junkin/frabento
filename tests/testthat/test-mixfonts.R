library(frabento)

test_that("when input vector is character vector", {
    expect_equal(length(mixfonts(letters[1:3])), 3L)
})
