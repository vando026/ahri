#' @title calcIncidence
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param edat dataset from \code{\link{censorData()}}. 
#' 
#' @param wdat dataset from \code{\link{censorData()}}. 
#'
#' @param Args takes list from \code{\link{setArgs()}}
#'
#' @return data.frame, df_tbl
#'
#' @import dplyr

calcIncidence <- function(edat, wdat, 
  byVar=c("Year", "AgeCat"),
  Args=eval.parent(quote(Args))) {
  inc <- aggregate(as.formula(paste(
    "cbind(sero_event, pyears=Time/365.25) ~ ",
    paste(byVar[1], byVar[2], sep='+'))),
    data=edat, FUN=sum)
  # Add weights
  dat <- inner_join(inc, wdat, by=byVar)
  dat <- split(dat, dat[, byVar[1]])
  # Get crude/adj rate
  dat <- sapply(dat, function(x) ageadjust.direct(
    x[,"sero_event"],x[,"pyears"],stdpop=x[, "IIntID"]))
  dat <- data.frame(t(dat)*100)
  dat[byVar[1]] <- rownames(dat)
  dat <- merge(dat, crude, by=byVar[1])
  dat
}
