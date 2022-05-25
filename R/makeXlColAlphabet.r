#' @title Make Microsoft Excel's alphabet column names
#' @description Genarate a character vector such as "A" "B" ... "AA" "AB" ... "ZZZ".
#' @param start Index or character at start, Default: 1 (is 'A'). `start` larger than `len` will return NA.
#' @param len Length (size) of the output vector, Default: NULL
#' @param nchar A number of combined characters, Default: 2
#' @param print_all displays all characters to the console if TRUE. When FALSE (default), showing only the first and last characters with column numbers.
#' @return A character vector.
#' @details No description.
#' @examples
#' \dontrun{
#' if(interactive()){
#' makeXlColAlphabet(nchar = 1)
#' makeXlColAlphabet(len = 10, start = 3)
#' makeXlColAlphabet(len = length(1978:2020), start = "B", print_all = TRUE)
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
makeXlColAlphabet <- function(start = 1L, len = NULL, nchar = 2L, 
                              print_all = FALSE) {

    if (nchar <= 0) stop("'nchar' should be greater than or equal to (GTE) 1.")
    if (nchar > 3) stop("'nchar' greater than (GT) 3 is not supported.")
    if (is.character(start) & nchar < nchar(start)) {
        stop("Preset 'nchar = '", nchar, " is exceeded by `nchar(start)`.\n",
             "Please confirm the 'start' string.")
    }

    i <- NULL # for R CMD CHECK: no visible binding for global variable
    x <- foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }

    if (is.character(start)) {
        start <- stringr::str_which(x, paste0("^", toupper(start), "$"))
    }

    if (is.null(len)) {
        len <- dplyr::case_when(nchar == 1 ~ 26^1 - start + 1,
                                nchar == 2 ~ 26^1 + 26^2 - start + 1,
                                nchar == 3 ~ 26^1 + 26^2 + 26^3 - start + 1)
    }

    if (start < 1) {
        stop("'start' should be greater than 0.")
    } else if (len < 1) {
        stop("'start' should be less than ", len + start,
             " when 'nchar' is ", nchar, ".")
    }

    res <- x[start:(len + start - 1L)]
    if (!print_all) {
        firlas <- unique(res[c(1, length(res))])
        names(firlas) <- as.character(unique(c(start, start + len - 1)))
        firlas
    } else if (print_all) {
        res
    }
}

# sinew::makeOxygen(makeXlColAlphabet)
