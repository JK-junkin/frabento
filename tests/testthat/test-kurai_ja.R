# library(frabento)

test_that("'num' is a scaler.", {
    expect_equal(kurai_ja(1), "")
    expect_equal(kurai_ja(1, remove_ichi = FALSE), "一")
    expect_equal(kurai_ja(22), "十")
    expect_equal(kurai_ja(333), "百")
    expect_equal(kurai_ja(4444), "千")
    expect_equal(kurai_ja(55555), "万")
    expect_equal(kurai_ja(55555, remove_ichi = FALSE), "一万")
    expect_equal(kurai_ja(6.66666e8), "億")
    expect_equal(kurai_ja(77e12), "十兆")
    expect_equal(kurai_ja(8e54), "百恒河沙")
    expect_equal(kurai_ja(9e72), "桁数: 73")
    expect_equal(kurai_ja(-1234567), "百万")
    expect_equal(kurai_ja(0.987654321), "")
})

test_that("'num' is a numeric vector.", {
    expect_equal(kurai_ja(c(1, 2e5, 3e10, 4e15, 5e20)),
                 c("", "十万", "百億", "千兆", "垓"))
    expect_equal(kurai_ja(c(1, 2e5, 3e10, 4e15, 5e20), remove_ichi = FALSE),
                 c("一", "十万", "百億", "千兆", "一垓"))
})
