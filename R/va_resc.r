#' @title Values and second Axis RESCaler
#' @description 2軸グラフを描く時は左軸に値をスケーリングし, 右軸は目盛り数値を調整する.
#' @param v Values to plot. 描画したい数値のベクトル.
#' @param lh_lim Limit(s) of 1st (left-hand) y-axis. 左軸の範囲 (要素数2) またはvector (要素数3以上).
#' vectorを指定した場合は range(lh_lim) となる. また, 正数であれば最大値, 負数であれば最小値のみの指定も可能 (もう一方の値は0となる).
#' NULLの場合は range(v, na.rm = TRUE) となる. Default: NULL
#' @param rh_lim Limit(s) of 2nd (right-hand) y-axis. 右軸の範囲 (要素数2) またはvector (要素数3以上).
#' vectorを指定した場合は range(rh_lim) となる. また, 正数であれば最大値のみの指定も可能 (最小値は0となる).
#' NULLの場合は range(v, na.rm = TRUE) となる. Default: NULL
#' @param scale_which One of the following: \describe{
#' \item{`"var"`}{右軸 (2nd y-axis) に対して描画したい変数を左軸にスケーリングする場合に使用.}
#' \item{`"axis"`}{右軸の目盛り値をスケーリングする場合に使用.}
#' }
#' @param return_lims Logical. 変換後の数値の代わりに範囲を出力する (matrix型), Default: FALSE
#' @return 変換後の数値
#' @details See example and also vignette("va_resc")
#' @references
#' \url{https://blog.statsbeginner.net/entry/2020/09/08/181536}
#' @examples
#' if(interactive()){
#'  library(dplyr)
#'  library(tidyr)
#'  library(ggplot2)
#'  library(patchwork)
#'  
#'  lh <- range(faithful$waiting)
#'  rh <- range(faithful$eruptions)
#'  
#'  g <- faithful %>%
#'      tidyr::pivot_longer(cols = dplyr::everything()) %>%
#'      dplyr::mutate(val = ifelse(name == "eruptions", 
#'                                 va_resc(value, lh, rh, "var"), value)) %>%
#'      ggplot(aes(x = as.integer(rownames(.)), group = name)) +
#'      geom_line(aes(y = val, color = name), size = 0.3)
#'  
#'  # basically
#'  g1 <- g +
#'      scale_y_continuous(name = "waiting", # title of left y-axis
#'                         sec.axis = sec_axis(~ va_resc(., lh, rh, "axis"),
#'                                             name = "eruptions")) +
#'      ggtitle("g1")
#'  
#'  # a function factory is useful
#'  create_right_axis_rescaler <- function(lh, rh) {
#'    function(x) va_resc(x, lh_lim = lh, rh_lim = rh, "axis") 
#'  }
#'  axis_for_eruptions <- create_right_axis_rescaler(lh = lh, rh = rh)
#'  g2 <- g + 
#'      scale_y_continuous(name = "waiting", # title of left y-axis
#'                         sec.axis = sec_axis(~ axis_for_eruptions(.),
#'                                             name = "eruptions")) +
#'      ggtitle("g2")
#'  
#'  # patchwork
#'  g1 / g2
#' }
#' @rdname va_resc
#' @export
va_resc <- function(v, lh_lim = NULL, rh_lim = NULL,
                    scale_which = c("var", "axis"), return_lims = FALSE) {

    scale_which <- match.arg(scale_which, c("var", "axis"))

    if (is.null(lh_lim)) lh_lim <- range(v, na.rm = TRUE)
    if (is.null(rh_lim)) rh_lim <- range(v, na.rm = TRUE)

    temp_paste <- function(x) {
        paste("If", x, "= 0 and length(", x, ") equals 1, the return is a",
              "single value (0 or Inf), so you must specify a non-zero value.")
    }

    if (all(length(lh_lim) == 1, lh_lim == 0)) stop(temp_paste(x = "`lh_lim`")) 
    if (all(length(rh_lim) == 1, rh_lim == 0)) stop(temp_paste(x = "`rh_lim`"))

    if (all(length(lh_lim) == 1, lh_lim != 0)) lh_lim <- c(0, lh_lim) 
    if (all(length(rh_lim) == 1, rh_lim != 0)) rh_lim <- c(0, rh_lim) 

    if (length(lh_lim) > 2) lh_lim <- range(lh_lim, na.rm = TRUE)
    if (length(rh_lim) > 2) rh_lim <- range(rh_lim, na.rm = TRUE)

    if (lh_lim[1] > lh_lim[2]) lh_lim <- rev(lh_lim)
    if (rh_lim[1] > rh_lim[2]) rh_lim <- rev(rh_lim)

#     if (any(min(v, na.rm = TRUE) < range(c(lh_lim, rh_lim))[1],
#             max(v, na.rm = TRUE) > range(c(lh_lim, rh_lim))[2])) {
#         warning("One or more values are out of bounds. Confirm lh_lim a/o rh_lim.")
#     }

    if (return_lims) return(rbind(lh_lim, rh_lim))

    y1_range  <- lh_lim[2] - lh_lim[1]
    y2_range  <- rh_lim[2] - rh_lim[1]

    if (scale_which == "var") { # assume the variable plotted to Right Hand axis
        to_zero   <- v - rh_lim[1]
        scaled    <- to_zero * (y1_range / y2_range)
        from_zero <- scaled + lh_lim[1]
    } else {                    # scale RH axis
        to_zero   <- v - lh_lim[1]
        scaled    <- to_zero * (y2_range / y1_range)
        from_zero <- scaled + rh_lim[1]
    }
    return(from_zero)
}

