#' @title Unicode normalization of colnames and contents of data.frame with
#' compatibility equivalence
#' @description This is a dplyr-dependent preprocessing function
#' @param .data data.frame-like object (`tibble`, `data.table`, etc.)
#' @param remove_space_colnames If TRUE, remove all white spaces from all
#' column names. Default: FALSE.
#' @param remove_space_contents If TRUE, remove all white spaces from all
#' contents. Default: FALSE.
#' @param coln_spc Space character to be removed in colnames. Default: "\\[:blank:\\]"
#' @param cont_spc Space character to be removed in contents. Default: "\\[:blank:\\]"
#' @return object of the same class as .data
#' @details NULL
#' @examples 
#'  # data.frame
#'  df <- data.frame(`列１（トン）` = 1, `列2 (kg) ` = 2)
#'  nfkc_all(df)
#'
#'  # data.table
#'  require(data.table)
#'  dt <- data.table(`列１（トン）` = 1.1, `列2 (kg) ` = 2.1)
#'  nfkc_all(dt)
#'
#'  # tibble
#'  require(tibble)
#'  tbl <- tibble(`列１（トン）` = "1.2\r\nトン", `列2 (kg) ` = " 2.2\n kg")
#'  nfkc_all(tbl)
#'  # remove white-spaces (blanks) from all column names
#'  nfkc_all(tbl, remove_space_colnames = TRUE)
#'  # remove white-spaces (blanks) from all contents
#'  nfkc_all(tbl, remove_space_contents = TRUE)
#'  # remove all white-spaces (blanks) from .data
#'  nfkc_all(tbl, remove_space_colnames = TRUE, remove_space_contents = TRUE)
#'
#'  # remove all spaces (including new line characters) from all contents
#'  nfkc_all(tbl, remove_space_contents = TRUE, cont_spc = "[:space:]")
#' @rdname nfkc_all
#' @export 
#' @importFrom dplyr rename_all mutate_all
#' @importFrom stringi stri_trans_nfkc
nfkc_all <- function(.data, remove_space_colnames = FALSE,
                     remove_space_contents = FALSE,
                     coln_spc = "[:blank:]", cont_spc = "[:blank:]") {
    cls <- class(.data)

    out <-
        .data %>%
        dplyr::rename_all(stringi::stri_trans_nfkc) %>%
        dplyr::mutate_all(stringi::stri_trans_nfkc)

    if(remove_space_colnames) {
        out <- 
            dplyr::rename_all(out, ~ stringr::str_remove_all(.x, coln_spc))
    }

    if(remove_space_contents) {
        out <- 
            dplyr::mutate_all(out, ~ stringr::str_remove_all(.x, cont_spc))
    }

    class(out) <- cls
    return(out)
}




