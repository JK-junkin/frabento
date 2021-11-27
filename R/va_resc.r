#' @title Values and 2nd Axis Rescaler
#' @description 2軸グラフを描く時は左軸に値をスケーリングし, 右軸は目盛り数値を調整する.
#' @param v Values to plot. 描画したい数値のベクトル.
#' @param lim1 Limit(s) of 1st (left-hand) y-axis. 左軸に対してplotしたい変数の範囲 (要素数2) またはvector (要素数3以上).
#' vectorを指定した場合は range(lim1) となる. また, 正数であれば最大値のみの指定も可能 (最小値は0となる).
#' NULLの場合は range(v) となる. Default: NULL
#' @param lim2 Limit(s) of 2nd (right-hand) y-axis. 右軸に対してplotしたい変数の範囲 (要素数2) またはvector (要素数3以上).
#' vectorを指定した場合は range(lim2) となる. また, 正数であれば最大値のみの指定も可能 (最小値は0となる).
#' NULLの場合は range(v) となる. Default: NULL
#' @param scale_which A string. 右軸 (2nd y-axis) に対して描画したい変数を左軸にスケーリングする場合は`var`,
#' 右軸の目盛りを変換する場合は`axis`. Default: "var".
#' @param return_lims Logical. 変換後の数値の代わりに範囲を標準 (message) 出力する, Default: FALSE
#' @return 変換後の数値
#' @details See example.
#' @references
#' \url{https://blog.statsbeginner.net/entry/2020/09/08/181536}
#' \url{https://knknkn.hatenablog.com/entry/2019/02/27/204913"}
#' \url{https://notchained.hatenablog.com/entry/2016/10/02/011204"}
#' @examples
#' \dontrun{
#' if(interactive()){
#' #EXAMPLE1
#' lh <- range(faithful$waiting)
#' rh <- range(faithful$eruptions)
#' 
#' faithful %>%
#'     tidyr::pivot_longer(cols = dplyr::everything()) %>%
#'     dplyr::mutate(val = ifelse(name == "eruptions", 
#'                                va_resc(value, lh, rh, "var"), value)) %>%
#'     ggplot(aes(x = as.integer(rownames(.)), group = name)) +
#'     geom_line(aes(y = val, color = name), size = 0.3) +
#'     scale_y_continuous(name = "waiting",
#'                        sec.axis = sec_axis(~ va_resc(., lh, rh, "axis"),
#'                                            name = "eruptions"))
#'  }
#' }
#' @rdname va_resc
#' @export
va_resc <- function(v, lim1 = NULL, lim2 = NULL,
                    scale_which = "var", return_lims = FALSE) {
    if (is.null(lim1)) lim1 <- range(v, na.rm = TRUE)
    if (is.null(lim2)) lim2 <- range(v, na.rm = TRUE)

    if (all(length(lim1) == 1, lim1 < 0)) stop("If the maximum value is negative, 'lim1' must include no less than two values.")
    if (all(length(lim2) == 1, lim2 < 0)) stop("If the maximum value is negative, 'lim2' must include no less than two values.")

    if (all(length(lim1) == 1, lim1 == 0)) warning("'lim1' should be no less than ", lim1,
                                                   " when assigning a value. The return value will be all zeros.")
    if (all(length(lim2) == 1, lim2 == 0)) warning("'lim2' should be no less than ", lim2,
                                                   " when assigning a value. The return value will be all zeros.")

    if (all(length(lim1) == 1, lim1 >= 0)) lim1 <- c(0, lim1) 
    if (all(length(lim2) == 1, lim2 >= 0)) lim2 <- c(0, lim2) 

    if (length(lim1) > 2) lim1 <- range(lim1, na.rm = TRUE)
    if (length(lim2) > 2) lim2 <- range(lim2, na.rm = TRUE)

    if (lim1[1] >= lim1[2]) lim1 <- rev(lim1)
    if (lim2[1] >= lim2[2]) lim2 <- rev(lim2)

    if (any(min(v, na.rm = TRUE) < range(c(lim1, lim2))[1],
            max(v, na.rm = TRUE) > range(c(lim1, lim2))[2])) {
        warning("One or more values are out of bounds. Confirm lim1 a/o lim2.")
    }

    if (return_lims) return(rbind(lim1, lim2))

    y1_range  <- lim1[2] - lim1[1]
    y2_range  <- lim2[2] - lim2[1]

    if (scale_which == "var") { # 右軸にプロットする変数を想定
        to_zero   <- v - lim2[1]
        scaled    <- to_zero * (y1_range / y2_range)
        from_zero <- scaled + lim1[1]
    } else {                    # 左軸を右軸にスケーリング
        to_zero   <- v - lim1[1]
        scaled    <- to_zero * (y2_range / y1_range)
        from_zero <- scaled + lim2[1]
    }
    return(from_zero)
}

