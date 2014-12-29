\name{Rgetword-package}
\alias{Rgetword-package}
\alias{Rgetword}
\docType{package}
\title{
Returning the words repeatly appeared in a text
}
\description{
This is an useful tool for text mining. The function r_getword returns the words repeatly appeared in a text.
}
\details{
\tabular{ll}{
Package: \tab Rgetword\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-29\cr
License: \tab GPL (>= 2)\cr
}
Usage: r_getword(text, min, max, exclude)

	text: character

	min: Int, minimum length of the words

	max: Int, maximum length of the words

	exclude: Boolean, whether exclude the words which included in other words
}
\author{
Kai Liu

Maintainer: Kai Liu <kai_liu@outlook.com>
}
\references{
http://en.wikipedia.org/wiki/Suffix_array
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\examples{
r_getword("asdf12asdf",2,4, FALSE)
[1] "asdf" "df"   "sdf" 

r_getword("阿斯蒂芬阿斯蒂芬",1,4,TRUE)
[1] "阿斯蒂芬"
}
