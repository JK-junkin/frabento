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
#' @export 
makeXlColAlphabet <- function(nchar = 2L, len = 26L + nchar, start = 1L) {
    if(nchar <= 0) stop("'nchar' should be greater than or equal to (GTE) 1.")
    if(nchar > 3) stop("'nchar' greater than (GT) 3 is not supported.")
    if(start < 1) stop("'start' should be greater than 0.")

    x <- foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }

    x[start:(len + start - 1L)]
}

# sinew::makeOxygen(makeXlColAlphabet)
