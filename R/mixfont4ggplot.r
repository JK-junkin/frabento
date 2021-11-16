#' @title To display two font families in a single character string on ggplot title and labels
#' @description This function has been made to cope with VERY VERY specific
#' demand in Japanese domestic Stock assessment.
#' @param string A character string to be converted
#' @param asciifont ASCII font family, Default: 'Arial'
#' @param mbytefont Multi byte font family, Default: 'MS Gothic'
#' @return An HTML <span> tag string.
#' @details See also `ggtext` package
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # EXAMPLE1
#'  mixFonts("<br>シラスを除く<br>カタクチイワシ0歳魚")
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[foreach]{foreach}}
#'  \code{\link[htmltools]{builder}}
#' @rdname mixFonts
#' @export 
#' @importFrom stringr str_split
#' @importFrom foreach foreach
#' @importFrom htmltools span
mixFonts <- function(string, asciifont = "Arial", mbytefont = "MS Gothic") {
    atoms <- stringr::str_split(string, pattern = "", simplify = FALSE) %>%
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
    return(spans)
}

#' @export 
label_mixFonts <- function(asciifont = "Arial", mbytefont = "MS Gothic") {
    function(x) mixFonts(x, asciifont, mbytefont)
}
