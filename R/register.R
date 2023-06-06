#' @title Register all fonts
#' @description register all system fonts obtained from `systemfonts` package
#' by `windowsFonts` or `quartzFonts`/ `systemfonts` で取得したフォント名を全て
#' OS固有のデバイス (`windowsFonts`, `quartzFonts`) に登録する.
#' @details The original function was transferred from \url{https://github.com/Gedevan-Aleksizde/fontregisterer} (Version 0.3) accessed on 2023/06/02 (Fri.).
#' @importFrom systemfonts system_fonts
#' @importFrom stats family
#' @importFrom utils getFromNamespace
#' @import grDevices
#' @export
register_all_fonts <- function() {
  if(Sys.info()["sysname"] %in% c("Darwin", "Windows")){
    f <- stats::aggregate(. ~ family + name, data = system_fonts(), 
                          FUN = function(x) unique(x)[1])
    # TODO: why???
    f$path      <- as.character(f$path)
    f$index     <- as.integer(f$index)
    f$style     <- as.character(f$style)
    f$width     <- as.integer(f$width)
    f$weight    <- as.integer(f$weight)
    f$italic    <- as.logical(f$italic)
    f$monospace <- as.logical(f$monospace)
    f <- lapply(stats::setNames(as.list(unique(f$family)), unique(f$family)),
                function(x) get_styles(subset(f, family == x)))

  }
  if(Sys.info()["sysname"] == "Windows"){
    do.call(windowsFonts, stats::setNames(lapply(names(f), windowsFont), names(f)))
  } else if(Sys.info()["sysname"] == "Darwin"){
    do.call(quartzFonts, stats::setNames(lapply(f, quartzFont), names(f)))
  } else {
    suppressPackageStartupMessages(gettext("This function does not register any font because your operating system is neither Mac nor Windows"))
  }
}

#' @title Get font names corresponding to four style names for a font family
#' @description `systemfonts::system_fonts()` などの結果から4書体のフォントを取
#' 得する. ただしうまくいくとは限らない.
#' @param data `data.frame` alike, output of font list from `systemfonts::
#' system_fonts()` or its subset, which includes six columns: `name`, `family`,
#' `style`, `weight`, `width`, and `italic`/ `data.frame` 相当のオブジェクトで,
#' `systemfonts::system_fonts()` のから特定のファミリだけ取り出したもので,
#' `name`, `family`, `style`, `weight`, `width`, `italic` 列を持つもの.
#' @return  character vector of length 4: It's elements are names of plain,
#' bold, italic, and bold italic font./長さ4の文字列ベクトル. 要素は順に標準,
#' ボールド, イタリック, ボールドイタリック体のpostscript名.
#' @details The original function was transferred from \url{https://github.com/Gedevan-Aleksizde/fontregisterer} (Version 0.3) accessed on 2023/06/02 (Fri.).
#' @export
get_styles <- function(data) {
    width <- weight <- style <- NULL
    styles <- c("plain", "normal", "regular", "medium", "bold", "italic",
              "bolditalic", "w3")
    styles_plain <- c("plain", "normal", "regular", "medium", "w3")
    data$style <- with(data, factor(tolower(style), styles))
    plain <- subset(data, width == "normal" | weight == "normal")

    if(NROW(plain) > 0){
        plain <- plain
    } else {
        plain <- data
    }
    if(NROW(subset(plain, style %in% styles_plain)) > 0){
        plain <- plain[order(plain$style), ]$name[1]
    } else {
        plain <- plain$name[1]
    }
    bold <- subset(data, style == "bold")
    if(NROW(bold) > 0){
        bold <- bold$name[1]
    } else {
        bold <- plain
    }
    italic <- subset(data, style == "italic" | italic)
    if(NROW(italic) > 0){
        italic <- italic$name[1]
    } else {
        italic <- plain
    }
    bi <- subset(data, style == "bold italic")
    if(NROW(bi) > 0){
        bi <- bi$name[1]
    } else {
        bi <- italic
    }
    return(c(plain = plain, bold = bold, italic = italic, bolditalic = bi))
}
