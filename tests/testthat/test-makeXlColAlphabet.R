library(frabento)

test_that("makeXlColAlphabet()", {
    expect_error(makeXlColAlphabet(nchar = -1))
    expect_error(makeXlColAlphabet(nchar = 0))
    expect_error(makeXlColAlphabet(nchar = 5))
    expect_error(makeXlColAlphabet(start = 0))
    expect_error(makeXlColAlphabet(start = -1))
    expect_error(makeXlColAlphabet(start = 1000))
    expect_error(makeXlColAlphabet(start = "AAA"))
    expect_error(makeXlColAlphabet(len = -1))
    expect_error(makeXlColAlphabet(len = 0))

    unc <- function(x) unique(nchar(x))
    expect_equal(unc(makeXlColAlphabet(nchar = 1)), 1L)
    expect_equal(unc(makeXlColAlphabet(nchar = 2)), 1:2)
    expect_equal(unc(makeXlColAlphabet(nchar = 3)), c(1, 3))
    expect_equal(unc(makeXlColAlphabet(nchar = 3, print_all = TRUE)), 1:3)

    expect_equal(makeXlColAlphabet(),                      c("A", "ZZ"), ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(nchar = 1),              c("A", "Z"), ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(len = 1),                        "A", ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(len = 1, start = 3),             "C", ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(len = 10),               c("A", "J"), ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(len = 10, start = 2),    c("B", "K"), ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(len = 10, start = "B"),  c("B", "K"), ignore_attr = TRUE)
    expect_equal(makeXlColAlphabet(len = 10, start = "b"),  c("B", "K"), ignore_attr = TRUE)
})
