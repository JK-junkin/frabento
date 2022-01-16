library(frabento)

test_that("when input vector is character vector", {
    expect_equal(label_repl2blank()(letters[1:3]),                                          c("a", "", "c"))
    expect_equal(label_repl2blank(nskip = 2                                )(letters[1:3]), c("a", "", ""))
    expect_equal(label_repl2blank(first = TRUE                             )(letters[1:3]), c("", "", "c"))
    expect_equal(label_repl2blank(last  = TRUE                             )(letters[1:3]), c("a", "", ""))
    expect_equal(label_repl2blank(first = TRUE, last  = TRUE               )(letters[1:3]), c("", "", ""))
    expect_equal(label_repl2blank(inverse = TRUE                           )(letters[1:3]), c("", "b", ""))
    expect_equal(label_repl2blank(inverse = TRUE, first = TRUE             )(letters[1:3]), c("a", "b", ""))
    expect_equal(label_repl2blank(inverse = TRUE, last = TRUE              )(letters[1:3]), c("", "b", "c"))
    expect_equal(label_repl2blank(inverse = TRUE, first = TRUE, last = TRUE)(letters[1:3]), c("a", "b", "c"))

    expect_equal(label_repl2blank(                                          )(letters[1:4]), c("a", "", "c", ""))
    expect_equal(label_repl2blank(nskip = 2                                 )(letters[1:4]), c("a", "", "", "d"))
    expect_equal(label_repl2blank(first = TRUE                              )(letters[1:4]), c("", "", "c", ""))
    expect_equal(label_repl2blank(last  = TRUE                              )(letters[1:4]), c("a", "", "c", ""))
    expect_equal(label_repl2blank(first = TRUE, last  = TRUE                )(letters[1:4]), c("", "", "c", ""))
    expect_equal(label_repl2blank(inverse = TRUE                            )(letters[1:4]), c("", "b", "", "d"))
    expect_equal(label_repl2blank(inverse = TRUE, first = TRUE              )(letters[1:4]), c("a", "b", "", "d"))
    expect_equal(label_repl2blank(inverse = TRUE, last = TRUE               )(letters[1:4]), c("", "b", "", "d"))
    expect_equal(label_repl2blank(inverse = TRUE, first = TRUE, last = TRUE )(letters[1:4]), c("a", "b", "", "d"))
})

