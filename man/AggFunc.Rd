% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getIncidence.R
\name{AggFunc}
\alias{AggFunc}
\title{AggFunc}
\usage{
AggFunc(RHS)
}
\arguments{
\item{RHS}{right hand side of the formula, as in Sex or AgeCat or "Sex + Cat".}
}
\value{
data.frame
}
\description{
Function to create aggregates of sero events and pyears by right-hand side
formula.
}
\examples{
AggByYear <- AggFunc("Year")
AggByAge <- AggFunc("AgeCat")
# Show for one imputation 
Args <- setArgs(nSim=1, imputeMethod=imputeEndPoint)
hiv   <- getHIV(Args)
rtdat <- getRTData(hiv)
idat <- getBirthDate(Args$inFiles$epifile)
ydat <- getIncData(rtdat, idat, Args)
inc <- AggByYear(ydat)
}