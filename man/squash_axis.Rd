% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/squash_axis.r
\name{squash_axis}
\alias{squash_axis}
\title{A transformation function that squashes the range of \link{from, to} by
magnif_ratio on a given axis}
\usage{
squash_axis(from, to, magnif_ratio)
}
\arguments{
\item{from}{a double value, left (bottom) end of the axis}

\item{to}{a double value, right (top) end of the axis}

\item{magnif_ratio}{the compression scale factor of the range \link{from, to}}
}
\value{
A transformation called "squash_axis", which is capsulated by trans_new() function
}
\description{
This function was transcribed from the post on Stackoverflow. --> https://stackoverflow.com/questions/61010786/error-nas-are-not-allowed-in-subscripted-assignments-while-using-squash-axis-i
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 library(ggplot2)
 
 dat <- data.frame(group = rep(c('A', 'B', 'C', 'D'), each = 10), 
                   value = c(rnorm(10), rnorm(10) + 100))
 ggplot(dat,aes(x = group, y = value)) +
   geom_point() +
   coord_trans(y = squash_axis(5, 95, 10))
 # Error
 ggplot(dat,aes(x = group,y = value)) +
   geom_point() +
   scale_y_continuous(trans = squash_axis(5, 95, 10))
  }
}
}
\seealso{
\code{\link[scales]{trans_new}}
}