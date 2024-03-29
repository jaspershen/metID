% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated_functions.R
\name{filterIden}
\alias{filterIden}
\title{Filter identifications according to m/z error, RT error, MS similarity and total score}
\usage{
filterIden(
  object,
  ms1.match.ppm = 25,
  rt.match.tol = 30,
  ms2.match.tol = 0.5,
  total.score.tol = 0.5
)
}
\arguments{
\item{object}{A metIdentifyClass object.}

\item{ms1.match.ppm}{MS1 match ppm.}

\item{rt.match.tol}{RT match tolerance.}

\item{ms2.match.tol}{MS2 match (MS2 similarity) tolerance.}

\item{total.score.tol}{Total score tolerance.}
}
\value{
A new metIdentifyClass.
}
\description{
Filter identifications according to m/z error, RT error, MS similarity and total score.
\lifecycle{deprecated}
}
\examples{
data("annotate_result", package = "metID")
annotate_result2 <- filterIden(annotate_result, rt.match.tol = 5)
}
\seealso{
The example and demo data of this function can be found
https://jaspershen.github.io/metID/articles/metID.html
}
\author{
Xiaotao Shen
\email{shenxt1990@163.com}
}
