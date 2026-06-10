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
#'  dmx <- c("35", "35.30", "35.3", "35.03", "35. 3", "35.    3", "30.30.750")
#'  conv_dm2dd(dmx)
#'  dmx2 <- c("35°30′", "30°30′750″", "30°30.750′", "-35-30", "-30-  3")
#'  conv_dm2dd(dmx2)
#'  conv_dm2dd(dmx2, original_value = FALSE)
#'
#'  dmnum <- c(34.30, 34.3, 34.03, 34, -120.5, -30.8)
#'  conv_dm2dd(dmnum)
#'  conv_dm2dd(dmnum, num_as_dm = TRUE)
#'  
#'  conv_dm2dd("-135°30′")
#' }
#' @rdname conv_dm2dd
#' @importFrom stringr str_extract str_remove str_detect
#' @importFrom dplyr if_else
#' @export 
conv_dm2dd <- function(dm60, original_value = TRUE, num_as_dm = FALSE) {
  assertthat::assert_that(is.numeric(dm60) || is.character(dm60))
  if (is.character(dm60)) {
    out <- vapply(
      dm60,
      FUN.VALUE = numeric(1),
      FUN = function(x) {
        if (is.na(x)) { return(NA_real_) }

        # preserve sign
        sgn <- ifelse(stringr::str_detect(x, "^\\s*-\\s*"), -1, 1)
        # remove sign character
        x <- stringr::str_remove(x, "^\\s*[+-]\\s*")

        # normalize consecutive spaces
        x <- stringr::str_squish(x)
        # detect explicit space after separator
        has_space <- stringr::str_detect(x, "[^0-9]\\s+")

        x <- stringr::str_replace_all(x, "[^0-9]+", ".")
        x <- stringr::str_remove_all(x, "\\.$")
        sp <- strsplit(x, "\\.")[[1]]

        if (length(sp) == 1) { return(sgn * as.numeric(sp[1])) }
        if (length(sp) == 2) {
          if (has_space) { # 35. 3 -> 35.03
            if (nchar(sp[2]) == 1) {
              sp[2] <- paste0("0", sp[2])
            }
          } else { # 35.3 -> 35.30
            if (nchar(sp[2]) == 1) {
              sp[2] <- paste0(sp[2], "0")
            }
          }
        }
        deg <- as.numeric(sp[1])
        minute <- as.numeric(paste(sp[-1], collapse = "."))
        minute <- validate_minute(minute = minute, original_x = x)
        sgn * (deg + minute / 60)
      }
    )
  } else { # numeric
    if (!num_as_dm) {
      out  <- dm60
    } else {
      deg <- trunc(dm60)
      minute <- abs(dm60 - deg) * 100
      minute <- validate_minute(minute = minute, original_x = dm60)
      out <- sign(dm60) * (abs(deg) + minute / 60)
    }
  }

  if (original_value) {
    names(out) <- as.character(dm60)
    return(out)
  } else {
    return(unname(out))
  }
}

#' @keywords inner function
validate_minute <- function(minute, original_x) {
  invalid <- minute >= 60
  invalid[is.na(invalid)] <- FALSE
  if (any(invalid)) {
    warning(
      paste0(
        "Invalid minute value detected in: ",
        paste(original_x[invalid], collapse = ", "),
        " (minute >= 60). Returning NA."
      ),
      call. = FALSE
    )
    minute[invalid] <- NA_real_
  }
  minute
}
