#' @title Make Microsoft Excel's alphabet column names
#' @description Genarate a character vector such as "A" "B" ... "AA" "AB" ... "ZZZ".
#' @param start Index or character at start, Default: NULL (but 'A' is automatically
#' assigned to). `start` larger than `len` will return NA.
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
makeXlColAlphabet <- function(start = NULL, len = NULL, nchar = 2L, 
                              print_all = FALSE) {

    if (is.character(start)) start <- get_abcorder(abc = start, nchar = nchar)
    if (is.null(start))      start <- 1L
    if (is.null(len))        len <- sum(26^seq_len(nchar)) - start + 1

    if (any(start < 1)) {
        stop("'start' should be greater than 0.")
    } else if (any(len < 1)) {
        stop("'start' should be less than ", len + start,
             " when 'nchar' is ", nchar, ".")
    }

    x <- make_abcbase(nchar = nchar)
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


get_abcorder <- function(abc, nchar = NULL) {
    if (nchar < max(nchar(abc), na.rm = TRUE)) {
        stop(paste("`max(nchar(abc))` exceeds preset 'nchar'(", nchar, ").",
                   "Please confirm the balance of 'nchar' and 'abc' string."))
    }

    x <- make_abcbase(nchar = nchar)
    stringr::str_which(x, paste0("^", toupper(abc), "$", collapse = "|"))
}


make_abcbase <- function(nchar = NULL) {
    if (is.null(nchar)) { nchar <- 2; warning("2 was assigned to 'nchar'.") }
    if (nchar <= 0) stop("'nchar' should be greater than or equal to (GTE) 1.")
    if (nchar > 4)  stop(paste("'nchar' greater than (GT) 4 is NOT recommended",
                               "because it processes too much time."))

    i <- NULL # for R CMD CHECK: no visible binding for global variable
    foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }
}

shift_abc <- function(start, n = 0) {
    makeXlColAlphabet(start = start, len = 1, nchar = 3) %>%
        names() %>% as.integer() %>%
#     makeXlColAlphabet(start = . - 1, len = 2) # error
        { makeXlColAlphabet(start = . - n, len = 1, nchar = 3) }
}
