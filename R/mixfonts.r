#' @title Unify ascii and multibyte fonts into a string
#' @description Put ASCII font family character(s) and multibyte font family
#' character(s) into a single string for use in ggplot.
#' @param strs A vector of character to be converted
#' @param asciifont ASCII font family, Default: 'Arial'
#' @param mbytefont Multi byte font family, Default: 'MS Gothic'
#' @return Character vector of HTML _span_ tag.
#' @details This function is supposed to be used with `ggtext::element_markdown()
#' ` and `ggtext::geom_richtext()`. See vignette("mixfonts")
#' @examples 
#' if(interactive()){
#'  library(frabento)
#'  register_all_fonts()
#'  
#'  mixfonts(c("仔魚を含まない\n0歳魚", "1歳魚+"))
#' }
#' @seealso 
#'  \code{\link[frabento]{label_mixfonts}}
#'  \code{\link[ggtext]{element_markdown}}
#'  \code{\link[ggtext]{geom_richtext}}
#' @rdname mixfonts
#' @export 
#' @importFrom foreach %do%
mixfonts <- function(strs, asciifont = "Arial", mbytefont = "MS Gothic") {
    s <- a <- i <- j <- k <- l <- m <- NULL # for R CMD CHECK

    foreach::foreach(s = strs, .combine = "c") %do% {
        atoms <- stringr::str_split(s, pattern = "", simplify = FALSE) %>%
            unlist()
        chr_bytes <- foreach::foreach(a = atoms, .combine = "c") %do% {
            length(charToRaw(a))
        }
        idx <- cumsum(rle(chr_bytes)$length)
        val <- rle(chr_bytes)$values
        molecules <- foreach::foreach(i = seq_along(idx), j = idx,
                                      .combine = "c") %do% {
            if (i == 1) {
                paste0(atoms[1:j], collapse = "")
            } else {
                paste0(atoms[(idx[i-1] + 1):j], collapse = "")
            }
        }
        fonts <- foreach::foreach(k = val, .combine = "c") %do% {
            ifelse(k == 1, asciifont, mbytefont)
            } 
        spans <- foreach::foreach(l = molecules, m = fonts,
                                  .combine = "paste0") %do% {
            htmltools::span(l, style = paste0("font-family:\'", m, "\'"))
        }
        as.character(spans) %>%
            stringi::stri_trans_nfkc() %>%
            stringr::str_replace_all("\n", "<br>") %>%
            stringr::str_replace_all("(?<=\\>) ", "&emsp;") %>%
            stringr::str_replace_all(" (?=\\<)", "&emsp;") %>%
            stringr::str_replace_all("_\\{", "<sub>") %>%
            stringr::str_replace_all("\\^\\{", "<sup>") %>%
            stringr::str_replace_all("(?<=<sub>[:graph:]{0,30})\\}", "</sub>") %>%
            stringr::str_replace_all("(?<=<sup>[:graph:]{0,30})\\}", "</sup>")
    }
}

#' @title Label mixed-fonts HTML strings
#' @param asciifont ASCII (Single byte) font family, Default: 'Arial'
#' @param mbytefont Multi byte font family, Default: 'MS Gothic'
#' @return Returns `mixfonts()`.
#' @details This function is supposed to be used with `ggtext::element_markdown()
#' `. See vignettees("mixfonts")
#' @seealso
#'  \code{\link[frabento]{mixfonts}}
#'  \code{\link[ggtext]{element_markdown}}
#'  \code{\link[ggtext]{geom_richtext}}
#' @examples 
#' if(interactive()){
#'  library(ggplot2)
#'  library(magrittr)
#'  library(ggtext)
#'  library(frabento)
#'  register_all_fonts()
#'
#'  set.seed(30)
#'  data.frame(cpue = c(rnorm(n = 30, mean = 300, sd = 25),
#'                      rnorm(n = 30, mean = 500, sd = 35)),
#'             year = rep(seq(1990, length.out = 30, by = 1), times = 2),
#'             age  = rep(c("0歳魚", "1歳魚+"), each = 30)) %>%
#'      ggplot(aes(x = year, y = cpue, group = age)) +
#'      geom_path(aes(color = age)) +
#'      scale_color_discrete(labels = label_mixfonts()) +
#'      theme(legend.text = element_markdown(color = "blue"),
#'            legend.position = "right")
#' }
#' @rdname label_mixfonts
#' @export 
label_mixfonts <- function(asciifont = "Arial", mbytefont = "MS Gothic") {
    function(x) mixfonts(x, asciifont, mbytefont)
}
