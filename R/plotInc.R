#' @title plotIncSex
#' 
#' @description  Plot HIV incidence with 95% CIs as whiskers.
#' 
#' @param Mal data.frame with rate, lci, and uci for men.
#' @param Fem data.frame with rate, lci, and uci for women.
#' 
#' @export

plotIncSex <- function(Mal, Fem, yLim=7,
  Colors=c("blue", "red"), gfun=png,
  Title="", fname="year_plot") {
  alainr::getColor()
  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.0, height=5.0, pointsize=9, 
      res=200, type="cairo")
  }
  par(mar=c(4.0,4.5,1.4,0.5))
  Fem1 <- data.frame(Fem,
    Grp=seq(nrow(Fem))+0.20,
    sex="Fem")
  Mal1 <- data.frame(Mal,
    Grp=seq(nrow(Mal))-0.20,
    sex="Mal")
  Sex <- rbind(Mal1, Fem1)
  Sex <- arrange(Sex, sex)

  # browser()
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
    pt.bg=par("bg"), col=scols,
    lwd=2, cex=0.6, pch=21))
    axis(side=1, at = seq(length(labs)), 
      labels = labs, cex.axis=1.2, cex.lab=1.3)
  kfem <- ksmooth(Sex$Grp[Sex$sex=="Fem"], Fem$rate, "normal", bandwidth=2.3)
  kmal <- ksmooth(Sex$Grp[Sex$sex=="Mal"], Mal$rate, "normal", bandwidth=2.9)
  lines(kfem, col=Reds[8], lwd=2)
  lines(kmal, col=Blues[8], lwd=2)
  legend("top", 
    c("Men", "Women"),
    lwd=10, lty=1, col=c(Colors[1], Colors[2]),
    ncol=2, bty="n", pt.lwd=8, xpd=TRUE,
    cex=1.2)

  if(!is.null(gfun)) dev.off()
}


#' @title plotIncSexArea
#' 
#' @description  Plot HIV incidence with 95% CIs as area.
#' 
#' @param Mal data.frame with rate, lci, and uci for men.
#' @param Fem data.frame with rate, lci, and uci for women.
#' 
#' @export
plotIncSexArea <- function(Mal, Fem, yLim=7,
  Colors=c("blue", "red"), gcolor="grey50", ipoints=TRUE,
  bwidth=list(mal=c(2.5, 2.5), fem=c(2.5, 2.5)),
  gfun=png, Title="", fname="year_plot", 
  output="~/Dropbox/R") {

  alainr::getColor()
  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.0, height=5.0, pointsize=9, 
      res=200, type="cairo")
  }
  x <- as.numeric(rownames(Mal))

  plot(x, Mal[, "rate"], type='n',
    pch=4, bty="l", xaxt='n',
    ylim=c(0, yLim), main=Title,
    cex.axis=1.1, cex.lab=1.3,
    xlab="Year", font.lab=2,
    ylab="Incidence Rate per 100 person-years")
  axis(side=1, at=x, cex.axis=1.1)
  # plotrix::staxlab(side = 1, at=x, labels=x, srt=45, cex.axis=1.1)

  renderInc <- function(dat, Colors, bwidth) {
    uci <- dat[, "uci"]; lci <- dat[, "lci"]
    lci[is.na(lci)] <- 0
    ub_ks <- ksmooth(x, uci, "normal", bandwidth = bwidth[2])
    lb_ks <- ksmooth(x, lci, "normal", bandwidth = bwidth[2])
    polygon(c(ub_ks$x, rev(ub_ks$x)), c(ub_ks$y, rev(lb_ks$y)), 
      col=adjustcolor(Colors, alpha.f=0.8), 
      border=adjustcolor(Colors, alpha.f=0.8))
    if (ipoints) points(x, dat[, "rate"], pch=4, col=gcolor, cex=0.5)
    lines(ksmooth(x, dat[, "rate"], "normal", bandwidth = bwidth[1]), 
      lwd=1, lty=1, col=gcolor)
  }

  renderInc(Mal, Colors[1], bwidth$mal)
  renderInc(Fem, Colors[2], bwidth$fem)

  legend("top", 
    c("Men", "Women"),
    lwd=10, lty=1, col=c(Colors[1], Colors[2]),
    ncol=2, bty="n", pt.lwd=8, xpd=TRUE,
    cex=1.4)
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

plotIncPrev <- function(
  inc, prev=NULL, x=NULL, yLim=8, Main="", prev_lty=1,
  Args=NULL, gfun=png, bottom=TRUE, left=TRUE, right=TRUE,
  bwidth=2.5, fname="test", inc_col="blue", prev_col="red") {

  ml <- ifelse(left, 4.5, 3)
  mr <- ifelse(right, 5, 3)
  mb <- ifelse(bottom, 4.5, 3)
  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.0, height=4.0, pointsize=9, 
      res=200, type="cairo")
  }
  par(mar=c(mb,ml,4.5,mr))
  if (is.null(x))
    x <- as.numeric(rownames(inc))
  plotrix::plotCI(x, inc$rate, ui=inc$uci, li=inc$lci, main=Main, 
    ylim=c(0, yLim), bty="u", sfrac=0, lwd=2, pch=19, col=inc_col,
    xlab="", ylab=ifelse(left, "HIV incidence / 100 p-years", ""),
    font.lab=2, cex.axis=1.4, cex.lab=1.6, cex.main=1.5, axes=FALSE)
  axis(side=2, cex.axis=1.4)
  box(bty="u")
  labs  <- x[as.logical(x %% 2)]
  axis(side=1, at=labs, labels=labs, cex.axis=1.4)
  # plotrix::staxlab(1, at=labs, srt=45, labels=labs, cex=1.3, line.spacing=1)
  # axis(side=2, labels=FALSE)
  lines(x, inc$rate, lty=1, col=inc_col, lwd=2)
  # abline(v=2011, lty=3)
  mtext(side = 1, line = 3.5, text=ifelse(bottom, "Year", ""),
    font=2, cex=1.2)
  par(mar=c(mb,ml,4.5,mr))
  par(new = T)
  # ys <- ksmooth(x, prev, "normal", bandwidth=bwidth)
  # ymax = max(ys$y)*1.20
  plot(x, prev, axes=F, type="l", lwd=3, lty=prev_lty,
    ylim=c(0, 60), xlab=NA, ylab=NA,  col=prev_col)
  axis(side = 4, cex.axis=1.2)
  mtext(side = 4, line = 3, 
    text=ifelse(right, "HIV prevalence (%)", ""),
    font=2, cex=1.1)
  if(!is.null(gfun)) dev.off()
}
