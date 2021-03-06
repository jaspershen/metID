% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function4metIdentifyClass.R
\name{get_ms2_spectrum_from_object}
\alias{get_ms2_spectrum_from_object}
\title{Get spectra of peaks from metIdentifyClass object}
\usage{
get_ms2_spectrum_from_object(object, peak.name)
}
\arguments{
\item{object}{metIdentifyClass.}

\item{peak.name}{Peak name.}
}
\value{
A MS2 spectrum.
}
\description{
Get spectra of peaks from metIdentifyClass object.
\lifecycle{maturing}
}
\examples{
data("annotate_result", package = "metID")
peak_name <- which_has_identification(annotate_result)
get_ms2_spectrum_from_object(annotate_result, peak_name[1,1])
}
\seealso{
The example and demo data of this function can be found
https://jaspershen.github.io/metID/articles/metID.html
}
\author{
Xiaotao Shen
\email{shenxt1990@163.com}
}
