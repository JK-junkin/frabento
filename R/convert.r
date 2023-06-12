#' @title Convert DM coordinates values (sexagesimal) to DD (decimal degree)
#' @description This function is used to convert latitude and longitude from
#' degrees, minutes format to decimal degree format, which is useful for map
#' drawing.
#' @param dm60 sexagesimal numbers of character / numeric class.
#' @param sep_marks a separation character between D and M part. Default: c(".",
#' "-", "_", "u00b0")
#' @param out_original include original values to output. Default: FALSE.
#' @param out_original_D D (degrees) part on console to make sure the
#' conversion is correct. Default: FALSE.
#' @param out_original_M print M (minutes) part on console to make sure the
#' conversion is correct. Default: FALSE.
#' @return a vector of numeric class (default) or a data.frame if any output
#' options are TRUE.
#' @details See example
#' @examples 
#' if(interactive()){
#'  numx <- c(34.30, 34.3, 34.03)
#'  conv_dm2dd(numx) # 34.50 34.50 34.05
#'  conv_dm2dd(numx, out_original_D = TRUE, out_original_M = TRUE)
#'  
#'  x <- c("35.30", "35.3", "35.03", "35", "30.30", "30.30.750")
#'  conv_dm2dd(x) # Make sure the last value equals (30 + 0.30750 / 60 * 100)
#'  
#'  x2 <- c("35°30′", "30°30.750′", NA, "35-30", "30-30.750", NA, "30_03")
#'  conv_dm2dd(x2)
#'  
#'  conv_dm2dd(x2, out_original = TRUE, out_original_D = TRUE, out_original_M = TRUE)
#' }
#' @rdname conv_dm2dd
#' @export 
#' @importFrom stringr str_extract str_remove str_detect
#' @importFrom dplyr if_else
conv_dm2dd <- function(dm60, sep_marks = c(".", "-", "_", "u00b0"),
                       out_original = FALSE,
                       out_original_D = FALSE, out_original_M = FALSE) {

    assertthat::assert_that(
        any(class(dm60) %in% c("numeric", "character")),
        msg = paste("Class of `dm60` should be numeric or character.",
                    "See help(conv_dm2dd).")
    )

    if (is.numeric(dm60)) {
        # Separate dm60 into D and M part
        nx1 <- floor(dm60)
        nx2 <- (dm60 - nx1)
        out <- data.frame(value = nx1 + (nx2 / 60 * 100), org = dm60,
                          org_D = nx1, org_M = nx2)
    } else {
        # extract D part (until first occurrence of sep_marks)
        spm1 <- paste0(rep("^\\", length(sep_marks)), sep_marks, collapse = "")
        pat1 <- paste0("[", spm1, "]+")
        x1 <- stringr::str_extract(dm60, pat1)
        nx1 <- as.numeric(x1)

        # extract M part (everything after first occurence of sep_marks)
        spm2 <- paste0(rep("\\", length(sep_marks)), sep_marks, collapse = "")
        x1s <- paste0("^(", paste0(x1, collapse = "|"), ")")
        x20 <- stringr::str_remove(dm60, x1s) %>%
            stringr::str_remove(paste0("[", spm2, "]+")) %>%
            stringr::str_extract("\\d*\\.?\\d*")
        nx20 <- dplyr::if_else(x20 != "", as.numeric(x20), 0)

        ## identify the devisor unit (decadal)
        devi <- 10^as.integer(nchar(x20))
        has_dec <- stringr::str_detect(x20, "\\.") & !is.na(x20)
        devi[has_dec] <- 10^as.integer(nchar(floor(nx20[has_dec])))
        nx2 <- nx20 / devi

        out <- data.frame(value = nx1 + (nx2 / 60 * 100), org = dm60,
                          org_D = x1, org_M = x20)
    }
    return(out[, c(TRUE, out_original, out_original_D, out_original_M),
               drop = TRUE])
}
