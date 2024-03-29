% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getHealthData.R
\name{readHealthData}
\alias{readHealthData}
\title{readHealthData}
\usage{
readHealthData(Female = 1, write_rda = TRUE)
}
\arguments{
\item{Female}{Either a value of 0 to read in Men or 1 to read in Women.}

\item{write_rda}{Default is to write the .Rda file.}
}
\value{
data.frame
}
\description{
Reads the Women (WGH) or Men (MGH) General Health Dataset.
}
\keyword{internal}
