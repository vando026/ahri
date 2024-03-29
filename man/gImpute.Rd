% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intCens.R
\name{gImpute}
\alias{gImpute}
\title{Uses the G-imputation method to impute the HIV sero date times.}
\usage{
gImpute(
  dat,
  Results,
  nSim = 1,
  start_date = NULL,
  tscale = 1,
  trans_back = TRUE,
  mcores = 1
)
}
\arguments{
\item{dat}{A dataset of the covaraiate values by individual and time period. \code{dat} must have an ID variable
called IIntID, a start date \code{obs_statr}, a latest-negative date \code{late_neg}
and earliest-positive date \code{early_pos}.}

\item{Results}{Results from \code{\link{readUniRegResults}}.}

\item{nSim}{The number of imputations to perform.}

\item{start_date}{If null, start_date is the first obs_start date of ID, else it is
same start_date for everyone. Must be a string in the following YYYY-MM-DD format: e.g. "2005-01-23".}

\item{tscale}{Scalar by which to scale time.  The default is 1, which means
that the time-scale is in days. The tscale used must correspond with the time scale of your Time
variable. So if Time is in days, \code{tscale=1} else if Time is in months \code{tscale
= 30.44}.}

\item{trans_back}{If \code{tscale != 1}, transform the infection times back into days.
Default is TRUE.}

\item{mcores}{The number of cores to use for parallel processing using \code{mclapply}.}
}
\value{
data.frame
}
\description{
Uses the G-imputation method to impute the infection times.
}
\examples{
# See full examples at https://github.com/vando026/ahri/wiki/6-G-Imputation
# install.packages(system.file("IntCens_0.2.tar.gz", package = "ahri"), repos = NULL, type = 'source')
library(IntCens)
data(gdat_demo)
home <- file.path('/home/alain/Seafile/AHRI_Data')
write.table(gdat_demo, file=file.path(home, "intcens/input_data.txt"),
  row.names=FALSE, quote=FALSE)
unireg(input = file.path(home, "intcens/input_data.txt"),
  output = file.path(home, "intcens/res_dat.txt"),
  model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
  subject_id = "IIntID", convergence_threshold=0.01, r = 1.0)
ureg_res <- readUniRegResults(
  File=file.path(home, "intcens/res_dat.txt"))
Args <- setArgs(
  Years=c(2005:2018), nSim=50,
  Age=list(Mal=c(15, 54)))
gdates <- gImpute(gdat_demo, ureg_res, nSim=Args$nSim)
}
