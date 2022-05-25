#' @title Make Microsoft Excel's alphabet column names
#' @description Genarate a character vector such as "A" "B" ... "AA" "AB" ... "ZZZ".
#' @param start Index or character at start, Default: NULL (to which 'A' is automatically
#' assigned). `start` larger than `len` will return NA.
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

    if (is.character(start)) start <- abc2index(abc = start, nchar = nchar)
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nchar PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[foreach]{foreach}}
#' @rdname make_abcbase
#' @export 
#' @importFrom foreach foreach
make_abcbase <- function(nchar = NULL) {
    if (is.null(nchar)) { nchar <- 2; warning("2 was assigned to 'nchar'.") }
    if (nchar <= 0 | nchar > 4) {
        stop(paste("'nchar' should be from 1 to 4 (integer).",
                   "'nchar' greater than 4 is NOT recommended",
                   "because it would take too much time."))
    }

    i <- NULL # for R CMD CHECK: no visible binding for global variable
    foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param abc PARAM_DESCRIPTION
#' @param nchar PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_subset}}
#' @rdname abc2index
#' @export 
#' @importFrom stringr str_which
abc2index <- function(abc, nchar = NULL) {
    x <- make_abcbase(nchar = nchar)

    nchar <- max(nchar(x), na.rm = TRUE)
    if (nchar < max(nchar(abc), na.rm = TRUE)) {
        stop(paste("`max(nchar(abc))` exceeds preset 'nchar'(", nchar, ").",
                   "Please adjust (increase) 'nchar'."))
    }

    stringr::str_which(x, paste0("^", toupper(abc), "$", collapse = "|"))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param num PARAM_DESCRIPTION
#' @param nchar PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname index2abc
#' @export 
index2abc <- function(num, nchar = NULL) {
    x <- make_abcbase(nchar = nchar)

    len <- length(x) # Never NA
    if (len < max(num, na.rm = TRUE)) {
        stop(paste("`max(num)` exceeds the length of alphabet base( N =",
                   len, ").", "Please adjust (increase) 'nchar'."))
    }
    
    if (any(num < 0)) num[num < 0] <- 0 
    x[num]
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param abc PARAM_DESCRIPTION
#' @param n_shift PARAM_DESCRIPTION, Default: 0
#' @param nchar PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname shift_abc
#' @export 
shift_abc <- function(abc, n_shift = 0L, nchar = NULL) {
    newindex <- abc2index(abc, nchar = nchar) %>% { . + n_shift }
    index2abc(num = newindex, nchar = nchar)
}

