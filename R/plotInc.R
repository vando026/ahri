#' @title plotKSInc
#' 
#' @description  Plot HIV incidence.
#' 
#' @param dat dataset from \code{\link{getIncidence()}}. 

plotKSInc <- function(
  inc1, inc2=NULL, inc3=NULL, 
  ylim1=c(0, 7), bwidth=c(2,2),
  gcolor="grey50", TIFF=TRUE,
  Colors=c("grey60", "grey70", NULL),
  Legend=c("Males", "Females", NULL),
  title="", fname="year_plot") {


  if(TIFF==TRUE) {
    tiff(file.path(output, paste0(fname, ".tiff")),
      units="in", width=5.0, height=5.0, pointsize=10, 
      res=300, type="cairo")
  }

  par(mar=c(4.0,4.5,0.8,0.5))
  x <- rownames(inc1)

  if(length(x)>9) {
    labs <- grep("^20", rownames(inc1), value=TRUE)
    labs[seq(1, length(inc1[, "rate"]), 2)] <- " "
  } else {
    labs <- x 
  }

  y <-  inc1[, "rate"]
  ub <- inc1[, "lower"]
  lb <- inc1[, "upper"]

  plot(x, y, type='n',
    pch=4, bty="n",
    ylim=ylim1,
    main=title,
    cex.axis=1.0,
    cex.lab=1.0,
    xlab="Year", font.lab=2,
    ylab="Incidence Rate per 100 person-years",
    xaxt='n')
  axis(side = 1, at=labs, labels=labs, cex.axis=1.0)

  ub_ks <- ksmooth(x, ub, "normal", bandwidth = bwidth[1])
  lb_ks <- ksmooth(x, lb, "normal", bandwidth = bwidth[1])
  polygon(c(ub_ks$x, rev(ub_ks$x)), c(ub_ks$y, rev(lb_ks$y)), 
    col=Colors[1], border=Colors[1])
  points(x, y, pch=4, col=gcolor, cex=0.5)
  lines(ksmooth(x, y,
    "normal", bandwidth = bwidth[1]), 
    lwd=1, lty=1, col=gcolor)

  if(!is.null(inc2)) {
    y2 <-  inc2[, "rate"]
    ub2 <- inc2[, "lower"]
    lb2 <- inc2[, "upper"]
    ub_ks <- ksmooth(x, ub2, "normal", bandwidth = bwidth[2])
    lb_ks <- ksmooth(x, lb2, "normal", bandwidth = bwidth[2])
    polygon(c(ub_ks$x, rev(ub_ks$x)), c(ub_ks$y, rev(lb_ks$y)), 
          col=Colors[2], border=Colors[2])
    points(x, y2, pch=4, col=gcolor, cex=0.5)
    lines(ksmooth(x, y2,
        "normal", bandwidth = bwidth[2]), 
        lwd=1, lty=1, col=gcolor)
  }

  if (!is.null(inc3)) {
    y3 <-  inc3[, "rate"]
    ub3 <- inc3[, "lower"]
    lb3 <- inc3[, "upper"]
    ub_ks3 <- ksmooth(x, ub3, "normal", bandwidth = bwidth)
    lb_ks3 <- ksmooth(x, ub3, "normal", bandwidth = bwidth)
    polygon(c(ub_ks3$x, rev(ub_ks3$x)), c(ub_ks3$y, rev(lb_ks3$y)), 
          col=Colors[3], border=Colors[3])
    points(x, y3, pch=4, col=gcolor, cex=0.5)
    lines(ksmooth(x, inc3[, "rate"],
        "normal", bandwidth = bwidth), 
        lwd=1, lty=1, col=gcolor)
  }

  legend("top", Legend,
    lwd=12, lty=1, col=Colors,
    ncol=length(Legend), 
    bty="n", pt.lwd=6, xpd=TRUE,
    cex=1.0)

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
    ui=upper, li=lower, 
    ylab="Incidence rate per 100 person-years", 
    xlab="Age Group", font.lab=2,
    xaxt="n", bty="n", 
    ylim=c(0, ceiling(max(rate))), 
    pt.bg=scols, col=scols,
    lwd=2, cex=0.6, pch=21))
    axis(side=1, at = seq(length(labs)), 
      labels = labs)

  legend("top", 
    c("Males", "Females"),
    lwd=10, lty=1, col=Colors,
    ncol=2, bty="n", pt.lwd=8, xpd=TRUE,
    cex=1.0)

  if (TIFF==TRUE) dev.off() 
}
