#' @title plotKSInc
#' 
#' @description  Plot HIV incidence.
#' 
#' @param dat dataset from \code{\link{getIncidence()}}. 

plotKSInc <- function(
  inc1, inc2=NULL, inc3=NULL, 
  Colors=c("grey60", "grey70", NULL),
  bwidth=c(2,2, NULL), ylim1=c(0, 7), 
  gcolor="grey50", TIFF=TRUE,
  Legend=c("Men", "Women", NULL),
  title="", fname="year_plot") {

  if(TIFF==TRUE) {
    tiff(file.path(output, paste0(fname, ".tiff")),
      units="in", width=5.0, height=5.0, pointsize=10, 
      res=200, type="cairo")
  }

  par(mar=c(4.0,4.5,0.8,0.5))
  x <- rownames(inc1)

  if(length(x)>9) {
    labs <- grep("^20", rownames(inc1), value=TRUE)
    labs[seq(1, length(inc1[, "rate"]), 2)] <- " "
  } else {
    labs <- x 
  }

  plot(x, inc1[, "rate"], type='n',
    pch=4, bty="n", xaxt='n',
    ylim=ylim1, main=title,
    cex.axis=1.1, cex.lab=1.3,
    xlab="Year", font.lab=2,
    ylab="Incidence Rate per 100 person-years")
  axis(side = 1, at=labs, labels=labs, cex.axis=1.1)

  renderInc <- function(dat, Colors, bwidth) {
    ub_ks <- ksmooth(x, dat[, "uci"], "normal", bandwidth = bwidth[1])
    lb_ks <- ksmooth(x, dat[, "lci"], "normal", bandwidth = bwidth[1])
    polygon(c(ub_ks$x, rev(ub_ks$x)), c(ub_ks$y, rev(lb_ks$y)), 
      col=Colors, border=Colors)
    points(x, dat[, "rate"], pch=4, col=gcolor, cex=0.5)
    lines(ksmooth(x, dat[, "rate"], "normal", bandwidth = bwidth), 
      lwd=1, lty=1, col=gcolor)
  }

  renderInc(inc1, Colors[1], bwidth[1])
  if(!is.null(inc2)) renderInc(inc2, Colors[2], bwidth[2])
  if(!is.null(inc3)) renderInc(inc3, Colors[3], bwidth[3])

  legend("top", Legend,
    lwd=12, lty=1, col=Colors,
    ncol=length(Legend), 
    bty="n", pt.lwd=6, xpd=TRUE,
    cex=1.3)

  if(TIFF==TRUE) dev.off()
}

#' @title plotIncAge
#' 
#' @description  Plot HIV incidence by Age.
#' 
#' @param dat dataset from \code{\link{getIncidence()}}. 
#' 
#' @importFrom plotrix plotCI 


# You need to add integer var to plot for CI
plotIncAge <- function(
  age_mal, age_fem, 
  Colors=c("grey50", "grey80"),
  fname='age_plot', TIFF=TRUE) {

  par(mar=c(4.0,4.5,0.8,0.5))
  if(TIFF==TRUE) {
    tiff(file.path(output, paste0(fname, ".tiff")),
      units="in", width=5.0, height=5, pointsize=8, 
      res=200, type="cairo")
  }

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
    cex.axis=1.2, cex.lab=1.5,
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
    cex=1.6)

  if (TIFF==TRUE) dev.off() 
}
