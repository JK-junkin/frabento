#' @title Unicode normalization of colnames and contents of data.frame with
#' compatibility equivalence
#' @description This is a dplyr-dependent preprocessing function
#' @param .data data.frame-like object (tibble, data.table, etc.)
#' @return object of the same class as .data
#' @details ""
#' @examples 
#'  # data.frame
#'  df <- data.frame(`列１（トン）` = 1, `列2 (kg) ` = 2)
#'  nfkc_all(df)
#'  # tibble
#'  tbl <- tibble::tibble(`列１（トン）` = 1, `列2 (kg) ` = 2)
#'  nfkc_all(tbl)
#'  # data.table
#'  dt <- data.table::data.table(`列１（トン）` = 1, `列2 (kg) ` = 2)
#'  nfkc_all(dt)
#' @rdname nfkc_all
#' @export 
#' @importFrom dplyr rename_all mutate_all
#' @importFrom stringi stri_trans_nfkc
nfkc_all <- function(.data) {
    cls <- class(.data)
    out <-
        .data %>%
        dplyr::rename_all(stringi::stri_trans_nfkc) %>%
        dplyr::mutate_all(stringi::stri_trans_nfkc)
    class(out) <- cls
    out
}
