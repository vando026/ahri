% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intCens.R
\name{getGImpute}
\alias{getGImpute}
\title{getGImpute}
\usage{
getGImpute(rtdat, gdat, i)
}
\arguments{
\item{rtdat}{An existing dataset generated from \code{\link{getRTData}}.}

\item{gdat}{The imputed dateset from \code{\link{gImpute}}.}

\item{i}{The ith imputed infection time in gdat.}
}
\value{
data.frame
}
\description{
Adds the ith column of imputed infection times from \code{\link{gImpute}} to an
existing dataset.
}
