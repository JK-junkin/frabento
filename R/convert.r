#' @title Convert degree-minute coordinates to decimal degrees
#' @description
#' Convert latitude and longitude values from degree-minute (DM) format
#' to decimal degrees (DD). Both character and numeric inputs are supported.
#'
#' Character inputs may contain separators such as periods, spaces, hyphens,
#' degree symbols, minute symbols, and second symbols.
#'
#' Numeric inputs are treated as decimal degrees by default. Set
#' `num_as_dm = TRUE` to interpret numeric values as degree-minute
#' coordinates.
#'
#' @param dm60 A character or numeric vector containing coordinates in
#'   degree-minute format.
#' @param original_value Logical. If `TRUE`, the original input values are
#'   retained as names of the returned vector. Default: `FALSE`.
#' @param num_as_dm Logical. If `TRUE`, numeric inputs are interpreted as
#'   degree-minute values rather than decimal degrees. Default: `FALSE`.
#'
#' @return
#' A numeric vector of decimal degree values.
#' If `original_value = TRUE`, the original input values are attached as
#' names to the output vector.
#'
#' @details
#' Character inputs are automatically parsed from common degree-minute
#' notations. One or more consecutive non-numeric separator characters are
#' collapsed into a single delimiter.
#'
#' Supported examples include:
#'
#' * `"35.30"`
#' * `"35. 3"`
#' * `"35°30′"`
#' * `"30°30′750″"` (degree-minute-second style)
#' * `"30°30.750′"` (fractional minutes)
#' * `"-135°30′"`
#' * `"35...30"`
#'
#' Minute values greater than or equal to 60 are considered invalid and
#' converted to `NA` with a warning.
#' @examples
#' dmx <- c(
#'   "35",
#'   "35.30",
#'   "35.3",
#'   "35.03",
#'   "35. 3",
#'   "35°30′",
#'   "30°30′750″",
#'   "30°30.750′",
#'   "-135°30′",
#'   "35...---30",
#'   "35...   3"
#' )
#' conv_dm2dd(dmx)
#' conv_dm2dd(dmx, original_value = TRUE)
#'
#' dmnum <- c(34.30, 34.3, 34.03, 34, -120.5)
#' conv_dm2dd(dmnum)
#' conv_dm2dd(dmnum, num_as_dm = TRUE)
#'
#' @rdname conv_dm2dd
#' @importFrom stringr str_detect str_remove str_replace_all str_remove_all str_squish
#' @export
conv_dm2dd <- function(dm60, original_value = FALSE, num_as_dm = FALSE) {
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
