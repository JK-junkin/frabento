library(frabento)

test_that("makeXlColAlphabet()", {
    unc <- function(x) unique(nchar(x))
    expect_equal(unc(makeXlColAlphabet(nchar = 1)), 1L)
    expect_equal(unc(makeXlColAlphabet(nchar = 2)), 1:2, ignore_attr = TRUE)
    expect_equal(unc(makeXlColAlphabet(nchar = 3)), 1:3, ignore_attr = TRUE)

    expect_error(makeXlColAlphabet(nchar = -1))
    expect_error(makeXlColAlphabet(nchar = 0))
    expect_error(makeXlColAlphabet(nchar = 4))

    expect_message(makeXlColAlphabet(), "A, B, ..., ZY, ZZ are gonna return.")
    expect_message(makeXlColAlphabet(nchar = 1), "A, B, ..., Y, Z are gonna return.")
    expect_message(makeXlColAlphabet(len = 10), "A, B, ..., I, J are gonna return.")
    expect_message(makeXlColAlphabet(len = 10, start = 2), "B, C, ..., J, K are gonna return.")
})
