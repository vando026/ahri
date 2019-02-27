#' @title plotIncSex
#' 
#' @description  Plot HIV incidence.
#' 
#' @param dat dataset from \code{\link{getIncidence}}. 
#' 
#' @export

plotIncSex <- function(Mal, Fem, yLim=7,
  Colors=c("blue", "red"), gfun=png,
  Title="", fname="year_plot") {

  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.0, height=5.0, pointsize=9, 
      res=200, type="cairo")
  }
  par(mar=c(4.0,4.5,1.4,0.5))
  Fem <- data.frame(Fem,
    Grp=seq(nrow(Fem))+0.20,
    sex="Fem")
  Mal <- data.frame(Mal,
    Grp=seq(nrow(Mal))-0.20,
    sex="Mal")
  Sex <- rbind(Mal, Fem)
  Sex <- arrange(Sex, sex)

  labs <- rownames(Mal)
  len <- length(labs)
  scols <- c(rep(Colors[1], len),rep(Colors[2], len))

  with(Sex,
    plotrix::plotCI(Grp, y=rate, 
    ui=uci, li=lci, 
    ylab="Incidence rate per 100 person-years", 
    xlab="Year", font.lab=2,
    cex.axis=1.2, cex.lab=1.3,
    xaxt="n", bty="n", 
    ylim=c(0, yLim),
    pt.bg=scols, col=scols,
    lwd=2, cex=0.6, pch=21))
    axis(side=1, at = seq(length(labs)), 
      labels = labs, cex.axis=1.2, cex.lab=1.3)

  legend("top", 
    c("Men", "Women"),
    lwd=10, lty=1, col=Colors,
    ncol=2, bty="n", pt.lwd=8, xpd=TRUE,
    cex=1.2)

  if(!is.null(gfun)) dev.off()
}

#' @title plotIncAge
#' 
#' @description  Plot HIV incidence by Age.
#' 
#' @param dat dataset from \code{\link{getIncidence}}. 
#' 
#' @importFrom plotrix plotCI 
#' 
#' @export  


# You need to add integer var to plot for CI
plotIncAge <- function(
  age_mal, age_fem, 
  Colors=c("grey50", "grey80"),
  fname='age_plot', TIFF=TRUE) {

  if(TIFF==TRUE) {
    tiff(file.path(output, paste0(fname, ".tiff")),
      units="in", width=5.0, height=5, pointsize=8, 
      res=200, type="cairo")
  }
  par(mar=c(4.0,4.5,0.8,0.5))

  Fem <- data.frame(age_fem,
    Grp=seq(nrow(age_fem))+0.15,
    sex="Fem")
  Mal <- data.frame(age_mal,
    Grp=seq(nrow(age_mal))-0.15,
    sex="Mal")
  Sex <- rbind(Mal, Fem)
  Sex <- arrange(Sex, sex)

  labs <- rownames(Mal)
  len <- length(labs)
  scols <- c(rep(Colors[1], len),rep(Colors[2], len))

  with(Sex,
    plotrix::plotCI(Grp, y=rate, 
    ui=uci, li=lci, 
    ylab="Incidence rate per 100 person-years", 
    xlab="Age Group", font.lab=2,
    cex.axis=1.2, cex.lab=1.3,
    xaxt="n", bty="n", 
    ylim=c(0, max(uci)+0.7), 
    pt.bg=scols, col=scols,
    lwd=2, cex=0.6, pch=21))
    axis(side=1, at = seq(length(labs)), 
      labels = labs, cex.axis=1.2, cex.lab=1.3)

  legend("top", 
    c("Men", "Women"),
    lwd=10, lty=1, col=Colors,
    ncol=2, bty="n", pt.lwd=8, xpd=TRUE,
    cex=1.2)

  if (TIFF==TRUE) dev.off() 
}

#' @title plotIncPrev
#' 
#' @description Plots incidence and prevalence. 
#' 
#' @param inc Incidence data, must include CIs, from \code{\link{getIncidence}}. 
#' @param prev Prevalence data, only a vector. 
#' 
#' @export 
#' @examples
#' inc <- getIncidence(Args)$adj$Year
#' hiv <- setHIV(Args) 
#' prev <-  calcTrend(hiv, 
#'   Formula = "HIVResult ~ Year", fmt=FALSE)
#' prev <- prev$adj.rate*100
#' plotIncPrev(inc, prev) 

plotIncPrev <- function(inc, prev=NULL,
  Args=NULL, gfun=png, bwidth=2.5, fname="test") {
  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.0, height=4.0, pointsize=9, 
      res=200, type="cairo")
  }
  par(mar=c(4.0,4.5,3.8,4.5))
  x <- as.numeric(rownames(inc))
  y <- inc$rate
  plotCI(x, inc$rate, ui=inc$uci, li=inc$lci,
    ylim=c(0, 5), bty="u", sfrac=0, lwd=2, pch=19, col="blue",
    xlab="Year", ylab="HIV incidence per 100 person-years", 
    font.lab=2, cex.axis=1.2, cex.lab=1.2)
  # staxlab(1, at=(x), labels=x, cex=1.3, line.spacing=1)
  lines(x, y, lty=1, col="blue", lwd=2)
  abline(v=2011, lty=3)
  par(new = T)
  x <- as.numeric(rownames(inc))
  # ys <- ksmooth(x, prev, "normal", bandwidth=bwidth)
  # ymax = max(ys$y)*1.20
  plot(x, prev, axes=F, type="l", lwd=3,
    ylim=c(0, 50), xlab=NA, ylab=NA,  col="red")
  axis(side = 4, cex.axis=1.2)
  mtext(side = 4, line = 2, "HIV prevalence (%)", font=2, 
    cex=1.2)
  legend("top", inset=c(0, -0.12), xpd=TRUE,
    c("HIV Incidence", "HIV Prevalence"),
    lwd=2, pch=c(19, NA), lty=1, cex=1.3,
    bty="n", ncol=2, col=c("blue", "red"))
  if(!is.null(gfun)) dev.off()
}
