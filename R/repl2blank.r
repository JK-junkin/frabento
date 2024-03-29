#' @title Replace string elements to blank at a certain interval
#' @description ggplot作図の際に軸ラベルの一部を空白にすることで minor break の
#' ようにみせかける.
#' @param vec A character or an numeric vector. ラベルにする文字列/数値ベクトル
#' @param nskip 何個置きに空白文字にするか, Default: 1.
#' @param shift blankで置き換える位置を右にずらす. Default: 0.
#' @param first 1個目の値を空白にする, Default: FALSE. TRUEで強制的に空白にする
#' . inverse = TRUE の時は逆動作となる.
#' @param last 最後の値を空白にする, Default: FALSE. TRUEで強制的に空白にする.
#' inverse = TRUE の時は逆動作となる.
#' @param inverse 空白と非空白を逆転させる, Default: FALSE
#' @param comma 3桁ごとにコンマをつける, Default: FALSE
#' @return A character vector
#' @details See example and also vignette("repl2blank")
#' @examples
#' repl2blank(1:10)
#' repl2blank(1:10, nskip = 2)
#' repl2blank(1:10, nskip = 2, shift = 1)
#' @seealso 
#'  \code{\link[frabento]{label_repl2blank}}
#' @rdname repl2blank
#' @export
#' @importFrom stringr str_detect
#' @importFrom scales comma
repl2blank <- function(vec, nskip = 1, shift = 0, first = FALSE, last = FALSE, 
                       inverse = FALSE, comma = FALSE) {

    if (comma) vec <- scales::comma(vec)

    org <- vec
    index <- seq_along(vec) + (-shift)
    resid <- c(1, index[index %% (nskip + 1) == 0] + 1)
    
    vec[!(index %in% resid)] <- ""
    
    ifelse(first, vec[1] <- "", vec)
    ifelse(last,  vec[length(vec)] <- "", vec)

    if (inverse) {
        pos <- stringr::str_detect(vec, "\\b", negate = TRUE)
        vec[pos] <- org[pos]
        vec[!pos] <- ""
    }
    vec
}

#' @title Label character vector with blanks at a certain interval
#' @description ggplot作図の際に軸ラベルの一部を空白にすることで minor break の
#' ようにみせかける.
#' @param nskip 何個置きに空白文字にするか, Default: 1.
#' @param shift blankで置き換える位置を右にずらす. Default: 0.
#' @param first 1個目の値を空白にする, Default: FALSE. TRUEで強制的に空白にする
#' . inverse = TRUE の時は逆動作となる.
#' @param last 最後の値を空白にする, Default: FALSE. TRUEで強制的に空白にする.
#' inverse = TRUE の時は逆動作となる.
#' @param inverse 空白と非空白を逆転させる, Default: FALSE
#' @param comma 3桁ごとにコンマをつける, Default: FALSE
#' @param ptime Parse date-time type vector, Default: FALSE
#' @param ftime Format of date-time, Default: '%Y-%m-%d'
#' @return A function
#' @details See example and also vignette("repl2blank")
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' library(frabento)
#' 
#' data.frame(year =  -6000:1979, haba = treering) %>%
#'     ggplot(aes(x = year, y = haba)) +
#'     geom_line(size = 0.2) +
#'     scale_x_continuous(breaks = seq(-6000, 2000, by = 200),
#'                        labels = label_repl2blank(nskip = 4))
#' # label with comma
#' data.frame(year =  -6000:1979, haba = treering) %>%
#'     ggplot(aes(x = year, y = haba)) +
#'     geom_line(size = 0.2) +
#'     scale_x_continuous(breaks = seq(-6000, 2000, by = 200),
#'                        labels = label_repl2blank(nskip = 4, comma = TRUE))
#' @rdname label_repl2blank
#' @export
label_repl2blank <-
    function(nskip = 1, shift = 0, first = FALSE, last = FALSE, inverse = FALSE, 
             comma = FALSE, ptime = FALSE, ftime = "%Y-%m-%d") {
    force_all <- function(...) { list(...) }
    force_all(nskip, first, last, inverse, comma)
    if (ptime) {
        function(x) {
            a <- repl2blank(vec = x, nskip = nskip, shift = shift, first = first,
                            last = last, inverse = inverse, comma = comma)
            a <- strftime(a, ftime)
            a[is.na(a)] <- ""
            a
        }
    } else {
        function(x) repl2blank(vec = x, nskip = nskip, shift = shift, first = first,
                               last = last, inverse = inverse, comma = comma)
    }
}

