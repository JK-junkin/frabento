#' @title Japanese scale units by 'Man-shin-hou'
#' @description 入力された数値に関して万進法 (中数) での日本語単位を返す関数.
#' @param num A numeric value or a numeric vector.
#'   Smaller numbers after decimal point are rounded.
#' @param remove_ichi Not print '一' when TRUE (default).
#' @param scipen An integer. A penalty to be applied when deciding to print
#'   numeric values in fixed or exponential notation. Default: 108.
#' @return Japanese scale units by Kanji
#' @examples 
#' if(interactive()){
#'  kurai_ja(c(1, 24, 100, 1000, 11111, 6e18, 1e72, -123489)) 
#'  }
#' @seealso 
#'  \code{\link[purrr]{map}}
#'  \code{\link[stringr]{str_remove}}
#' @rdname kurai_ja
#' @export 
#' @importFrom purrr map_lgl
#' @importFrom stringr str_remove
kurai_ja <- function(num, remove_ichi = TRUE, scipen = 108) {
    junits <- c("", "\u4e07", "\u5104", "\u5146", "\u4eac", "\u5793", "\u79ed",
                "\u7a63", "\u6e9d", "\u6f97", "\u6b63", "\u8f09", "\u6975",
                "\u6052\u6cb3\u6c99", "\u963f\u50e7\u7947",
                "\u90a3\u7531\u591a", "\u4e0d\u53ef\u601d\u8b70",
                "\u7121\u91cf\u5927\u6570")
    senmade <- c("\u4e00", "\u5341", "\u767e", "\u5343")

    org_scipen <- getOption("scipen")
    options(scipen = scipen)
    nc <- nchar(round(num, digits = 0))
    nc[num < 0] <- nc[num < 0] - 1L

    kk <- (nc - 1) %/% 4 + 1L
    unit <- junits[kk] 
    unit[is.na(unit)] <- as.character(nc)[is.na(unit)]

    rs <- nc %% 4
    rs[purrr::map_lgl(rs, ~ .x == 0)] <- 4
    a <- senmade[rs]
    a[nc >= 73] <- "\u6841\u6570: "

    options(scipen = org_scipen)
    out <- paste0(a, unit)
    if (remove_ichi) {
        stringr::str_remove(out, "^\u4e00")
    } else {
        out
    }
}
