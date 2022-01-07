#' @title Make Microsoft Excel's alphabet column names
#' @description Genarate a character vector such as "A" "B" ... "AA" "AB" ... "ZZZ".
#' @param nchar A number of combined characters, Default: 2
#' @param len Length (size) of the output vector, Default: NULL
#' @param start Index or character at start, Default: 1 (is 'A'). `start` larger than `len` will return NA.
#' @return A character vector.
#' @details No description.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' makeXlColAlphabet(nchar = 1)
#' makeXlColAlphabet(len = 10, start = 3)
#' makeXlColAlphabet(len = 1978:2020, start = "B")
#'  }
#' }
#' @seealso 
#'  \code{\link[foreach]{foreach}}
#'  \code{\link[dplyr]{case_when}}
#' @rdname makeXlColAlphabet
#' @importFrom foreach foreach
#' @importFrom dplyr case_when
#' @importFrom utils head tail
#' @export 
makeXlColAlphabet <- function(nchar = 2L, len = NULL, start = 1L) {

    i <- NULL # for R CMD CHECK: no visible binding for global variable
    x <- foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }

    if(nchar <= 0) stop("'nchar' should be greater than or equal to (GTE) 1.")
    if(nchar > 3) stop("'nchar' greater than (GT) 3 is not supported.")

    if(is.character(start)) start <- stringr::str_which(toupper(start), x) 
    if(start < 1) stop("'start' should be greater than 0.")

    if(is.null(len)) {
        len <- dplyr::case_when(nchar == 1 ~ 26^1 - start + 1,
                                nchar == 2 ~ 26^1 + 26^2 - start + 1,
                                nchar == 3 ~ 26^1 + 26^2 + 26^3 - start + 1)
    }

    res <- x[start:(len + start - 1L)]
    message(paste(head(res, n = 2), collapse = ", "), ", ..., ",
            paste(tail(res, n = 2), collapse = ", "), " are gonna return.")
    return(res)
}

# sinew::makeOxygen(makeXlColAlphabet)
