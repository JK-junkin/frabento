#' @title Make Microsoft Excel's alphabetical column names
#' @description Genarate a character vector such as "A" "B" ... "AA" "AB" ... "ZZZ".
#' @param start Index or character at start, Default: NULL (to which 'A' is automatically
#' assigned). `start` larger than `len` will return NA.
#' @param len Length (size) of the output vector, Default: NULL
#' @param nchar A number of combined characters, Default: 2
#' @param print_all displays all characters to the console if TRUE. When FALSE (default), showing only the first and last characters with column numbers.
#' @return A character vector.
#' @details No description.
#' @examples
#' makeXlColAlphabet()
#' makeXlColAlphabet(nchar = 1)
#' makeXlColAlphabet(start = "B", len = length(1978:2020))
#' makeXlColAlphabet(start = 23, len = 10, print_all = TRUE)
#' @seealso 
#'  \code{\link[frabento]{shift_abc}}
#'  \code{\link[frabento]{make_abcbase}}
#'  \code{\link[frabento]{index2abc}}
#'  \code{\link[frabento]{abc2index}}
#' @rdname makeXlColAlphabet
#' @importFrom foreach foreach
#' @importFrom dplyr case_when
#' @importFrom utils head tail
#' @export 
makeXlColAlphabet <- function(start = NULL, len = NULL, nchar = 2L, 
                              print_all = FALSE) {

    if (is.character(start)) start <- abc2index(abc = start)
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

#' @title Create an alphabet vector (data source)
#' @description Create an alphabet data source according to the given `nchar`.
#' @param nchar an integer, Default: NULL
#' @return a character vector which length is `sum(26^(1:nchar))`.
#' @examples 
#' paste0(
#'   head(make_abcbase(nchar = 3)), tail(make_abcbase(nchar = 3)),
#'   sep = ", ..., "
#' )
#' sum(26^(1:5)) # length is 12356630
#' \dontrun{
#'  make_abcbase(nchar = 0) # Error
#'  make_abcbase(nchar = 5) # Error
#' }
#' @rdname make_abcbase
#' @export 
make_abcbase <- function(nchar = NULL) {
    if (is.null(nchar)) { nchar <- 2; warning("2 was assigned to 'nchar'.") }
    if (nchar <= 0 | nchar > 4) {
        stop(paste("'nchar' is currently constrained between 1L and 4L!\n",
                   "Because it is memory-intensive and time-consuming",
                   "when 'nchar' is greater than 4L, where the length of vector",
                   "returned would be 12,356,630 (`sum(26^(1:5))`)",
                   "if 'nchar = 5L'."))
    }

    i <- NULL # for R CMD CHECK: no visible binding for global variable
    foreach::foreach(i = seq_len(nchar), .combine = "c") %do% {
        do.call(paste0, expand.grid(rep(list(LETTERS), i))[rev(seq_len(i))])
    }
}

#' @title Get the alphabetical colnames corresponding to numbers (indices)
#' @description This function supports nchar up to 4 (ZZZZ), i.e. the upper
#' limit of number is 475,254.
#' @param num an integer vector which is converted to alphabets.
#' @param nchar an integer used for creating an alphabetical name base.
#' Default: 4L.
#' @return a character vector
#' @examples 
#' index2abc(num = 5)        # "E"
#' index2abc(num = 10^(1:5)) # "J" "CV" "ALL" "NTP" "EQXD"
#' \dontrun{
#'  index2abc(num = 500000)   # Error. due to exceeding sum(26^(1:4))
#' }
#' @rdname index2abc
#' @export 
index2abc <- function(num, nchar = 4L) {
    x <- make_abcbase(nchar = nchar)

    len <- length(x) # Never NA
    if (len < max(num, na.rm = TRUE)) {
        stop(paste("`max(num)` exceeds the upper limit of alphabet base",
                   "( N =", len, ")."))
    }
    
    if (any(num <= 0) || any(is.na(num))) num[num <= 0] <- NA_integer_
    x[num]
}

#' @title Get the numbers (indices) corresponding to alphabetical colnames
#' @description This function supports nchar up to 4, i.e. from "A" to "ZZZZ".
#' @param abc a character vector to be converted to numbers (indices).
#' @return an integer vector
#' @examples 
#' abc2index(letters)         # 1–26
#' abc2index(c("a", NA, "a")) # 1 NA 1
#' \dontrun{
#'  abc2index("zzzza")         # Error
#' }
#' @rdname abc2index
#' @importFrom foreach foreach
#' @importFrom stringr str_which
#' @export 
abc2index <- function(abc) {
    i <- NULL
    nchar_max <- max(nchar(abc), na.rm = TRUE)
    x <- make_abcbase(nchar = nchar_max)
    foreach::foreach(i = abc, .combine = "c") %do% {
        if (is.na(i)) {
            NA
        } else {
            stringr::str_which(x, paste0("^", toupper(i), "$"))
        }
    }
}

#' @title Shift n from given alphabetical colname
#' @description This function is useful for output with openxlsx package.
#' @param abc a character vector to be shifted.
#' @param n_shift numbers to shift, Default: 0
#' @return a character vector
#' @examples 
#' shift_abc("a", n_shift = 3)        # D
#' shift_abc(letters, n_shift = 3)    # D–AC
#' shift_abc(letters, n_shift = 1:26) # B–AZ. skipping ahead one by one.
#' @rdname shift_abc
#' @export 
shift_abc <- function(abc, n_shift = 0L) {
    newindex <- abc2index(abc) + n_shift
    index2abc(num = newindex)
}

