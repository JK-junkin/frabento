#' @title Making Microsoft Excel's alphabet column names
#' @description Genarate a character vector such as "A" "B" ... "AA" "AB" ... "ZZZ".
#' @param nchar A number of combined characters, Default: 2
#' @param len Length (size) of the output vector, Default: 26L + nchar
#' @param start Index at start, Default: 1. For example, when 'start' = 1 (default) is "A"; 'start' = 2 is "B".
#' @return A character vector.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[foreach]{foreach}}
#' @rdname makeXlColAlphabet
#' @importFrom foreach foreach
#' @importFrom dplyr case_when
#' @export 
makeXlColAlphabet <- function(nchar = 2L, len = NULL, start = 1L) {
    if(nchar <= 0) stop("'nchar' should be greater than or equal to (GTE) 1.")
    if(nchar > 3) stop("'nchar' greater than (GT) 3 is not supported.")
    if(start < 1) stop("'start' should be greater than 0.")

    if(is.null(len)) {
        len <- dplyr::case_when(nchar == 1 ~ 26^1 - start + 1,
                                nchar == 2 ~ 26^1 + 26^2 - start + 1,
                                nchar == 3 ~ 26^1 + 26^2 + 26^3 - start + 1)
    }

    x <- foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }

    res <- x[start:(len + start - 1L)]
    message(paste(head(res, n = 2), collapse = ", "), ", ..., ",
            paste(tail(res, n = 2), collapse = ", "), " are gonna return.")
    return(res)
}

# sinew::makeOxygen(makeXlColAlphabet)
