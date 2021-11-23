#' @title 2軸グラフ用の数値調整関数
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

#' @title facet作図後にパネルごとに異なるcoordinateを適用する関数
#' @description 個々のパネルの座標を調整する.
#' @param panel_ranges Limits of axes or an axis.
#' @param expand Add margin to each axes or an axis, Default: TRUE
#' @param default PARAM_DESCRIPTION, Default: FALSE
#' @param clip PARAM_DESCRIPTION, Default: 'on'
#' @return ggproto object
#' @details DETAILS
#' @references
#' \url{https://andburch.github.io/ggplot_facets/}
#' \url{https://www.javaer101.com/article/3690503.html}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggproto}}
#' @rdname coord_panel_ranges
#' @export
#' @importFrom ggplot2 ggproto
coord_panel_ranges <-
    function(panel_ranges, expand = TRUE, default = FALSE, clip = "on") {
    UniquePanelCoords <- ggplot2::ggproto(
      "UniquePanelCoords", ggplot2::CoordCartesian,

      num_of_panels = 1,
      panel_counter = 1,
      panel_ranges = NULL,

      setup_layout = function(self, layout, params) {
        self$num_of_panels <- length(unique(layout$PANEL))
        self$panel_counter <- 1
        layout
      },

      setup_panel_params =  function(self, scale_x, scale_y, params = list()) {
        if (!is.null(self$panel_ranges) & length(self$panel_ranges) != self$num_of_panels)
          stop("Number of panel ranges does not equal the number supplied")

        train_cartesian <- function(scale, limits, name, given_range = NULL) {
          if (is.null(given_range)) {
#         range <- ggplot2:::scale_range(scale, limits, self$expand)
              expansion <- ggplot2:::default_expansion(scale, expand = self$expand)
              range <- ggplot2:::expand_limits_scale(scale, expansion,
                                                     coord_limits = self$limits[[name]])
          } else {
              range <- given_range
          }

#       out <- scale$break_info(range)
#       out$arrange <- scale$axis_order()
#       names(out) <- paste(name, names(out), sep = ".")
#       out
          out <- list(
            ggplot2:::view_scale_primary(scale, limits, range),
            sec = ggplot2:::view_scale_secondary(scale, limits, range),
            arrange = scale$axis_order(),
            range = range
        )
        names(out) <- c(name, paste0(name, ".", names(out)[-1]))
        out
        }

        cur_panel_ranges <- self$panel_ranges[[self$panel_counter]]
        if (self$panel_counter < self$num_of_panels)
          self$panel_counter <- self$panel_counter + 1
        else
          self$panel_counter <- 1

        c(train_cartesian(scale_x, self$limits$x, "x", cur_panel_ranges$x),
          train_cartesian(scale_y, self$limits$y, "y", cur_panel_ranges$y))
      }
    )
    ggplot2::ggproto(NULL, UniquePanelCoords, panel_ranges = panel_ranges,
                     expand = expand, default = default, clip = clip)
}
