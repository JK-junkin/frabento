#' @title Unify ascii and multibyte fonts into a string
#' @description Put ASCII font family character(s) and multibyte font family
#' character(s) into a single string for use in ggplot.
#' @param strs A vector of character to be converted
#' @param asciifont ASCII font family, Default: 'Arial'
#' @param mbytefont Multi byte font family, Default: 'MS Gothic'
#' @return Character vector of HTML _span_ tag.
#' @details This function is supposed to be used with `element_markdown()` in `ggtext` package
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  mixfonts(c("全0歳魚", "仔魚を含まない\n0歳魚"))
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_split}},\code{\link[stringr]{str_replace}}
#'  \code{\link[foreach]{foreach}},\code{\link[foreach]{\%do\%}}
#'  \code{\link[htmltools]{builder}}
#' @rdname mixfonts
#' @export 
#' @importFrom stringr str_split str_replace_all
#' @importFrom foreach foreach %do%
#' @importFrom htmltools span
mixfonts <- function(strs, asciifont = "Arial", mbytefont = "MS Gothic") {
    s <- a <- i <- j <- k <- l <- m <- NULL # for R CMD CHECK
    foreach::foreach(s = strs, .combine = "c") %do% {
        atoms <- stringr::str_split(s, pattern = "", simplify = FALSE) %>%
            unlist()
        alen <- foreach::foreach(a = atoms, .combine = "c") %do% {
            length(charToRaw(a))
        }
        ids <- cumsum(rle(alen)$length)
        vls <- rle(alen)$values
        moles <- foreach::foreach(i = seq_along(ids), j = ids, .combine = "c") %do% {
            if (i == 1) {
                paste0(atoms[1:j], collapse = "")
            } else {
                paste0(atoms[(ids[i-1] + 1):j], collapse = "")
            }
        }
        fmly <- foreach::foreach(k = vls, .combine = "c") %do% {
            ifelse(k == 1, asciifont, mbytefont)
            } 
        spans <- foreach::foreach(l = moles, m = fmly, .combine = "paste0") %do% {
            htmltools::span(l, style = paste0("font-family:\'", m, "\'"))
        }
        as.character(stringr::str_replace_all(spans, "\n", "<br>"))
    }
}

#' @title Label mixed-fonts HTML strings
#' @param asciifont ASCII font family, Default: 'Arial'
#' @param mbytefont Multi byte font family, Default: 'MS Gothic'
#' @return `mixfonts` function
#' @details See vignettees("mixfonts")
#' @seealso
#'  \code{\link[ggtext]{element_markdown}}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(ggplot2)
#'  library(dplyr)
#'  library(tibble)
#'  library(ggtext)
#'  library(frabento)
#'
#'  tibble::tibble(cpue = c(rnorm(n = 30, mean = 300, sd = 25),
#'                          rnorm(n = 30, mean = 500, sd = 35)),
#'                 year = rep(seq(1990, length.out = 30, by = 1), times = 2),
#'                 age  = rep(c("0歳魚", "1歳魚+"), each = 30)) %>%
#'      ggplot(aes(x = year, y = cpue, group = age)) +
#'      geom_path(aes(color = age)) +
#'      scale_color_discrete(labels = label_mixfonts()) +
#'      theme(legend.text = element_markdown(color = "blue"))
#'  }
#' }
#' @rdname label_mixfonts
#' @export 
label_mixfonts <- function(asciifont = "Arial", mbytefont = "MS Gothic") {
    function(x) mixfonts(x, asciifont, mbytefont)
}
