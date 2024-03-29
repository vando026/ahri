% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setArgs.R
\name{setArgs}
\alias{setArgs}
\title{Set arguments and parameters to control functions and data
transformations.}
\usage{
setArgs(
  Years = c(2000:2025),
  Age = list(All = c(0, 100)),
  AgeCat = NULL,
  AgeBy = 5,
  nSim = 1,
  imputeMethod = imputeRandomPoint,
  aname = "filename",
  mcores = 1,
  setFun = identity,
  addVars = identity,
  MoreArgs = NULL
)
}
\arguments{
\item{Years}{numeric vector of years.}

\item{Age}{list of ages as in \code{list(Fem=c(15, 49), Mal=c(15, 54)}.}

\item{AgeCat}{numeric vector of age categories, e.g. \code{seq(15, 55, by=5)}.}

\item{AgeBy}{integer value for size age categories.}

\item{nSim}{number of simulations to perform.}

\item{imputeMethod}{select either \code{\link{imputeRandomPoint}} or
\code{\link{imputeMidPoint}}.}

\item{aname}{String to name an object or set a filename.}

\item{mcores}{number of cores to use for parallel package (used in
\code{\link{MIdata}}). This is not available for Windows users apparently.}

\item{setFun}{a function used by \code{\link{setData}} by which the data can be further subset.}

\item{addVars}{a function used by \code{\link{setData}} by which additional variables
can be added.}

\item{MoreArgs}{a list, which adds more arguments if needed.}
}
\value{
list
}
\description{
A function that collects and stores various arguments that can be used as
inputs to other functions. The \code{\link{setArgs}} function comes with default parameter
values for the arguments. The user is asked to check these values and understand how
these will affect the data.
}
\examples{
# Check default parameter values
Args = setArgs()
Args 
# Set specific age and year ranges 
Args <- setArgs(Years=c(2008:2018), Age=list(Mal=c(15, 54), Fem=c(15, 49)))
setHIV(Args)
# Keep all ages
ArgsEpi <- setArgs(Age=list(All=c(0, 100)), AgeCat=seq(0, 100, 20))
setEpisodes(ArgsEpi)
}
