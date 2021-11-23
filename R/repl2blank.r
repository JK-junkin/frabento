#' @title 軸ラベルを一定間隔でブランクに置き換える関数
#' @description 当初はggplot作図の際に軸ラベルの一部を空白にすることで minor break のようにみせかけるために作成.
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

#' @title 軸ラベルを一定間隔でブランクに置き換える関数を返す
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
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[scales]{character(0)}}
#' @rdname label_repl2blank
#' @keywords force_all
#' @export
label_repl2blank <-
    function(nskip = 1, first = FALSE, last = FALSE, inverse = FALSE,
             ptime = FALSE, ftime = "%Y-%m-%d") {
    scales:::force_all(nskip, first, last, inverse)
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

