#' @title Convert numbers as expressions of exponential format
#' @description Modify input numbers to expression class of exponential format like `expression(1.35~'×'~10^{4})`.
#' @param x numeric vector.
#' @param digits a number of digits before `\\u00d7`. Default: 3
#' @param scale a number to multiply x. Default: 1.
#' @param prefix a character string to paste just before x. Default: ''.
#' @param suffix a character string to paste just after x. Default: ''.
#' @param decimal.mark a character string to use decimal mark, Default: '.'.
#' @param trim Logical, if ‘FALSE’, values are right-justified to a common
#' width (see ‘base::format()’). Default: TRUE.
#' @param ... extra arguments passed to `scientific()`.
#' @return An expression
#' @details DETAILS
#' @examples 
#' expfn(c(1.2e8, 1.03e8, -Inf))
#' expfn(-Inf)
#' @seealso 
#'  \code{\link[stringr]{str_extract}}
#'  \code{\link[scales]{scientific}}
#' @rdname expfn
#' @export 
#' @importFrom stringr str_extract
#' @importFrom scales scientific
expfn <- function(x, digits = 3, scale = 1, prefix = "", suffix = "",
                  decimal.mark = ".", trim = TRUE, ...) {
    xx <- scales::scientific(x, digits = digits, scale = scale,
                             prefix = prefix, suffix = suffix,
                             decimal.mark = decimal.mark, trim = trim, ...)
    x1 <- stringr::str_extract(xx, "^.*(?=e)")
    x2 <- stringr::str_extract(xx, "(?<=e).*$")
    cat(x1, "\n")
    if (isTRUE(all.equal(-Inf, as.double(xx)))) {
        outtext <- "0"
    } else if (all(as.double(x1) == 1)) {
        outtext <- paste0("10^{", as.integer(x2), "}")
    } else {
        outtext <- paste0(x1, "~'\u00d7'~10^{", as.integer(x2), "}")
    }
    parse(text = outtext)
}

#' @title Label exponential format
#' @description Return functions to display exponential format expression where
#' `expfn` works internally.
#' @inheritParams expfn
#' @return A function
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname label_expfn
#' @export 
label_expfn <- function(digits = 3, scale = 1, prefix = "", suffix = "",
                        decimal.mark = ".", trim = TRUE, ...) {
    force_all <- function(...) { list(...) }
    force_all(digits, scale, prefix, suffix, decimal.mark, trim, ...)
    function(x) expfn(x, digits = digits, scale = scale,
        prefix = prefix, suffix = suffix, decimal.mark = decimal.mark,
        trim = trim, ...)
}

