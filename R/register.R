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
