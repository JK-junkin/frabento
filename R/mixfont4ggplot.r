#' @title To display two font families in a single character string on ggplot title and labels
#' @description This function has been made to cope with VERY VERY specific
#' demand in Japanese domestic Stock assessment.
#' @param strs A vector of character to be converted
#' @param asciifont ASCII font family, Default: 'Arial'
#' @param mbytefont Multi byte font family, Default: 'MS Gothic'
#' @return An HTML _span_ tag string.
#' @details This function is supposed to be used with `element_markdown()` in `ggtext` package
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(ggplot2)
#'  library(ggtext)
#'
#'  # EXAMPLE1
#'  mixfonts(c("シラス込み0歳魚", "\nシラスを除く\nカタクチイワシ0歳魚"))
#'
#'  # EXAMPLE2
#'  ggplot(women) +
#'      geom_point(aes(x = height, y = weight)) +
#'      labs(x = mixfonts("身長\n(inch)", asciifont = "Times New Roman")) +
#'      theme_linedraw(base_family = "MS Gothic") +
#'      theme(axis.title.x.bottom = element_markdown())
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

#' @export 
label_mixfonts <- function(asciifont = "Arial", mbytefont = "MS Gothic") {
    function(x) mixfonts(x, asciifont, mbytefont)
}
