#' @title Transformation function compressing a specified interval
#' @description This function was transcribed from the post on Stackoverflow.
#' --> https:
#' //stackoverflow.com/questions/61010786/error-nas-are-not-allowed-in-subscripted-assignments-while-using-squash-axis-i
#' @param from a double value, left (bottom) end of the axis
#' @param to a double value, right (top) end of the axis
#' @param magnif_ratio the compression scale factor of the range between from and to.
#' @return A transformation called "squash_axis", which is capsulated by trans_new() function
#' @examples 
#' if(interactive()){
#'  library(ggplot2)
#'  library(frabento)
#'   
#'  set.seed(123)
#'  dat <- data.frame(group = rep(c('A', 'B', 'C', 'D'), each = 10), 
#'                    value = c(rnorm(10), rnorm(10) + 100))
#'  ggplot(dat,aes(x = group, y = value)) +
#'    geom_point() +
#'    coord_trans(y = squash_axis(5, 95, 10))
#'
#'  ggplot(dat,aes(x = group,y = value)) +
#'    geom_point() +
#'    scale_y_continuous(trans = squash_axis(5, 95, 10))
#' }
#' @seealso 
#'  \code{\link[scales]{trans_new}}
#' @rdname squash_axis
#' @export 
#' @importFrom scales trans_new
squash_axis <- function(from, to, magnif_ratio) { 
  trans <- function(x) {    
    # get indices for the relevant regions
    isq <- x > from & x < to & !is.na(x)
    ito <- x >= to & !is.na(x)

    # apply transformation
    x[isq] <- from + (x[isq] - from) / magnif_ratio
    x[ito] <- from + (to - from) / magnif_ratio + (x[ito] - to)

    return(x)
  }

  inv <- function(x) {

    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from) / magnif_ratio & !is.na(x)
    ito <- x >= from + (to - from) / magnif_ratio & !is.na(x)

    # apply transformation
    x[isq] <- from + (x[isq] - from) * magnif_ratio
    x[ito] <- to + (x[ito] - (from + (to - from) / magnif_ratio))

    return(x)
  }

  # return the transformation
  return(scales::trans_new("squash_axis", trans, inv, domain = c(from, to)))
}
