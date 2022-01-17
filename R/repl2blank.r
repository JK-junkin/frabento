#' @title Replace string elements to blank at a certain interval
#' @description ggplot作図の際に軸ラベルの一部を空白にすることで minor break のようにみせかける.
#' @param vec A character or an numeric vector. ラベルにする文字列/数値ベクトル
#' @param nskip 何個置きに空白文字にするか, Default: 1.
#' @param first 1個目の値を空白にする, Default: FALSE. TRUEで強制的に空白にする. inverse = TRUE の時はTRUEで強制的に値を与える.
#' @param last 最後の値を空白にする, Default: FALSE. TRUEで強制的に空白にする. inverse = TRUE の時はTRUEで強制的に値を与える.
#' @param inverse 空白と非空白を逆転させる, Default: FALSE
#' @return A character vector
#' @details 例をご覧ください.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  repl2blank(1:10)
#'  repl2blank(1:10, nskip = 2)
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}}
#' @rdname repl2blank
#' @export
#' @importFrom stringr str_detect
repl2blank <-
    function(vec, nskip = 1, first = FALSE, last = FALSE, inverse = FALSE) {
    org <- vec
    index <- seq_along(vec)
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
#' @description ggplot作図の際に軸ラベルの一部を空白にすることで minor break のようにみせかける.
#' @param nskip 何個置きに空白文字にするか, Default: 1.
#' @param first 1個目の値を空白にする, Default: FALSE. TRUEで強制的に空白にする. inverse = TRUE の時はTRUEで強制的に値を与える.
#' @param last 最後の値を空白にする, Default: FALSE. TRUEで強制的に空白にする. inverse = TRUE の時はTRUEで強制的に値を与える.
#' @param inverse 空白と非空白を逆転させる, Default: FALSE
#' @param ptime Parse date-time type vector, Default: FALSE
#' @param ftime Format of date-time, Default: '%Y-%m-%d'
#' @return A function
#' @details 例をご覧ください.
#' @examples
#' \dontrun{
#' if(interactive()){
#' library(dplyr)
#' library(tibble)
#' library(ggplot2)
#' 
#' tibble::tibble(year =  -6000:1979, haba = treering) %>%
#'     ggplot(aes(x = year, y = haba)) +
#'     geom_line(size = 0.2) +
#'     scale_x_continuous(breaks = seq(-6000, 2000, by = 200),
#'                        labels = label_repl2blank(nskip = 4))
#'  }
#' }
#' @rdname label_repl2blank
#' @export
label_repl2blank <-
    function(nskip = 1, first = FALSE, last = FALSE, inverse = FALSE,
             ptime = FALSE, ftime = "%Y-%m-%d") {
    force_all <- function(...) { list(...) }
    force_all(nskip, first, last, inverse)
    if (ptime) {
        function(x) {
            a <- repl2blank(vec = x, nskip = nskip, first = first,
                            last = last, inverse = inverse)
            a <- strftime(a, ftime)
            a[is.na(a)] <- ""
            a
        }
    } else {
        function(x) repl2blank(vec = x, nskip = nskip, first = first,
                               last = last, inverse = inverse)
    }
}

