% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repl2blank.r
\name{repl2blank}
\alias{repl2blank}
\title{軸ラベルを一定間隔でブランクに置き換える関数}
\usage{
repl2blank(vec, nskip = 1, first = FALSE, last = FALSE, inverse = FALSE)
}
\arguments{
\item{vec}{A character or an numeric vector. ラベルにする文字列/数値ベクトル}

\item{nskip}{何個置きに空白文字にするか, Default: 1.}

\item{first}{1個目の値を空白にする, Default: FALSE. TRUEで強制的に空白にする. inverse = TRUE の時はTRUEで強制的に値を与える.}

\item{last}{最後の値を空白にする, Default: FALSE. TRUEで強制的に空白にする. inverse = TRUE の時はTRUEで強制的に値を与える.}

\item{inverse}{空白と非空白を逆転させる, Default: FALSE}
}
\value{
A character vector
}
\description{
当初はggplot作図の際に軸ラベルの一部を空白にすることで minor break のようにみせかけるために作成.
}
\details{
例をご覧ください.
}
\examples{
\dontrun{
if(interactive()){
 repl2blank(1:10)
 repl2blank(1:10, nskip = 2)
 }
}
}
\seealso{
\code{\link[stringr]{str_detect}}
}