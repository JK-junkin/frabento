library(frabento)
# devtools::load_all(here::here())

test_that("va_resc", {
    rp <- c(5, 15)
    rpn <- c(-15, -5, 5, 15)
    rn <- c(-15, -5)

    alp <- c(0, 1)
    alpn <- c(-1, 1)
    aln <- c(-1, 0)

    arp <- c(0, 10)
    arpn <- c(-10, 10)
    arn <- c(-10, 0)

    # 右が正, 左が正
    expect_equal(va_resc(va_resc(rp, alp, arp, "var"), alp, arp, "axis"), rp)
    
    # 右が正負, 左が正
    expect_equal(va_resc(va_resc(rpn, alp, arpn, "var"), alp, arpn, "axis"), rpn)

    # 右が負, 左が正
    expect_equal(va_resc(va_resc(rn, alp, arn, "var"), alp, arn, "axis"), rn)
    
    # 右が正, 左が正負
    expect_equal(va_resc(va_resc(rp, alpn, arp, "var"), alpn, arp, "axis"), rp)
   
    # 右が正負, 左が正負
    expect_equal(va_resc(va_resc(rpn, alpn, arpn, "var"), alpn, arpn, "axis"), rpn)
    
    # 右が負, 左が正負
    expect_equal(va_resc(va_resc(rn, alpn, arn, "var"), alpn, arn, "axis"), rn)
    
    # 右が正, 左が負
    expect_equal(va_resc(va_resc(rp, aln, arp, "var"), aln, arp, "axis"), rp)
    
    # 右が正負, 左が負
    expect_equal(va_resc(va_resc(rpn, aln, arpn, "var"), aln, arpn, "axis"), rpn)
    
    # 右が負, 左が負
    expect_equal(va_resc(va_resc(rn, aln, arn, "var"), aln, arn, "axis"), rn)

    expect_error(va_resc(rp, 0, 100))
    expect_error(va_resc(rp, 1, 0))
#     expect_warning(va_resc(vl, 0, 100))
#     expect_warning(va_resc(vl, c(-10, -5), c(0, -100)))
})
