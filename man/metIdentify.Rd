% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated_metIdentify.R
\name{metIdentify}
\alias{metIdentify}
\title{Identify metabolites based on MS/MS database.}
\usage{
metIdentify(
  ms1.data,
  ms2.data = NULL,
  ms1.ms2.match.mz.tol = 25,
  ms1.ms2.match.rt.tol = 10,
  ms1.match.ppm = 25,
  ms2.match.ppm = 30,
  mz.ppm.thr = 400,
  ms2.match.tol = 0.5,
  fraction.weight = 0.3,
  dp.forward.weight = 0.6,
  dp.reverse.weight = 0.1,
  rt.match.tol = 30,
  polarity = c("positive", "negative"),
  ce = "all",
  column = c("hilic", "rp"),
  ms1.match.weight = 0.25,
  rt.match.weight = 0.25,
  ms2.match.weight = 0.5,
  path = ".",
  total.score.tol = 0.5,
  candidate.num = 3,
  database,
  threads = 3,
  silence.deprecated = FALSE
)
}
\arguments{
\item{ms1.data}{The name of ms1 peak table (csv format). Column 1 is "name", Column 2 is
"mz" and column is "rt" (second).}

\item{ms2.data}{MS2 data, must be mgf, msp or mzXML format. For example, ms2.data = c("test.mgf", "test2.msp").}

\item{ms1.ms2.match.mz.tol}{MS1 peak and MS2 spectrum matching m/z tolerance. Default is 25 pm.}

\item{ms1.ms2.match.rt.tol}{MS1 peak and MS2 spectrum matching RT tolerance. Default is 10 s.}

\item{ms1.match.ppm}{Precursor match ppm tolerance.}

\item{ms2.match.ppm}{Fragment ion match ppm tolerance.}

\item{mz.ppm.thr}{Accurate mass tolerance for m/z error calculation.}

\item{ms2.match.tol}{MS2 match (MS2 similarity) tolerance.}

\item{fraction.weight}{The weight for matched fragments.}

\item{dp.forward.weight}{Forward dot product weight.}

\item{dp.reverse.weight}{Reverse dot product weight.}

\item{rt.match.tol}{RT match tolerance.}

\item{polarity}{The polarity of data, "positive"or "negative".}

\item{ce}{Collision energy. Please confirm the CE values in your database. Default is "all".}

\item{column}{"hilic" (HILIC column) or "rp" (reverse phase).}

\item{ms1.match.weight}{The weight of MS1 match for total score calculation.}

\item{rt.match.weight}{The weight of RT match for total score calculation.}

\item{ms2.match.weight}{The weight of MS2 match for total score calculation.}

\item{path}{Work directory.}

\item{total.score.tol}{Total score tolerance. The total score are refering to MS-DIAL.}

\item{candidate.num}{The number of candidate.}

\item{database}{MS2 database name or MS2 database.}

\item{threads}{Number of threads}

\item{silence.deprecated}{Silenc the deprecated information or not.}
}
\value{
A metIdentifyClass object.
}
\description{
Identify metabolites based on MS/MS database.
\lifecycle{deprecated}
}
\examples{
\dontrun{
##creat a folder nameed as example
path <- file.path(".", "example")
dir.create(path = path, showWarnings = FALSE)

##get MS1 peak table from metID
ms1_peak <- system.file("ms1_peak", package = "metID")
file.copy(from = file.path(ms1_peak, "ms1.peak.table.csv"), 
          to = path, overwrite = TRUE, recursive = TRUE)
ms2_data <- system.file("ms2_data", package = "metID")
file.copy(from = file.path(ms2_data, "QC1_MSMS_NCE25.mgf"), 
          to = path, overwrite = TRUE, recursive = TRUE)
database <- system.file("ms2_database", package = "metID")

file.copy(from = file.path(database, "msDatabase_rplc0.0.2"), 
          to = path, overwrite = TRUE, recursive = TRUE)

annotate_result3 <- 
metIdentify(ms1.data = "ms1.peak.table.csv", 
                     ms2.data = c("QC1_MSMS_NCE25.mgf"), 
                     ms2.match.tol = 0.5, 
                     ce = "all",
                     ms1.match.ppm = 15, 
                     rt.match.tol = 30, 
                     polarity = "positive", 
                     column = "rp", 
                     path = path, 
                     candidate.num = 3,
                     database = "msDatabase_rplc0.0.2", 
                     threads = 2)
annotate_result3
}
}
\seealso{
The example and demo data of this function can be found
https://jaspershen.github.io/metID/articles/metID.html
}
\author{
Xiaotao Shen
\email{shenxt1990@163.com}
}
