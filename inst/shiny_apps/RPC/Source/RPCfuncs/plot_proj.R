
# MSEproj <- readRDS("C:/temp/MSEproj.rda")

# Called functions
proj_plot<-function(OBJs, type = c("SSB0", "SSBMSY", "F", "SPR", "Catch")) {
  type <- match.arg(type)
  MSEproj<-OBJs$MSEproj
  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1)
  layout(matrix(c(1,2),nrow=1),widths=c(1.5,0.5))
  par(mai=c(0.6,0.5,0.5,0.01))

  # Median plot (top)
  CurrentYr <- MSEproj@OM$CurrentYr[1]

  nyh<-MSEproj@nyears
  nyp<-MSEproj@proyears
  hy<-CurrentYr - nyh:1 + 1
  py<-CurrentYr + 1:nyp
  ay<-c(hy, py)

  hist <- switch(type,
                 "SSB0" = MSEproj@SSB_hist,
                 "SSBMSY" = MSEproj@SSB_hist,
                 "F" = MSEproj@FM_hist,
                 "SPR" = OBJs$MSEhist@TSdata$SPR$Equilibrium,
                 "Catch" = MSEproj@CB_hist) %>% apply(2, median)
  proj_med <- switch(type,
                     "SSB0" = MSEproj@SSB,
                     "SSBMSY" = MSEproj@SSB,
                     "F" = MSEproj@FM,
                     "SPR" = MSEproj@SPR$Equilibrium,
                     "Catch" = MSEproj@Catch) %>% apply(c(2, 3), median)
  max_y <- max(hist, proj_med)

  ylab <- switch(type,
                 "SSB0" = "Spawning biomass (SSB)",
                 "SSBMSY" = "Spawning biomass (SSB)",
                 "F" = "Fishing mortality",
                 "SPR" = "Equilibrium SPR",
                 "Catch" = "Removals")

  title_txt <- switch(type,
                      "SSB0" = parse(text = "Median~SSB~and~SSB[0]~reference~points"),
                      "SSBMSY" = parse(text = "Median~SSB~and~SSB[MSY]~reference~points"),
                      "F" = parse(text = "Median~SSB~and~F[MSY]~reference~points"),
                      "SPR" = "Median equilibrium SPR",
                      "Catch" = "Median catch")

  plot(range(ay),c(0, 1.2 * max_y),
       typ = "n",yaxs='i',xaxs='i', ylab = ylab, xlab = "Year")
  abline(h=pretty(seq(0, max_y,length.out=15)),col='light grey')
  abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  mtext(title_txt, 3, line = 0.8, font = 2)

  if(type == "SSB0") {
    SSB0d<-apply(MSEproj@RefPoint$Dynamic_Unfished$SSB0, 2, median)
    SSB0a<-apply(MSEproj@RefPoint$ByYear$SSB0, 2, median)

    abline(h=hist[1],lty=1)
    points(ay, rep(hist[1], length(ay)),pch=16)
    lines(ay,SSB0a,lty=2)
    points(ay,SSB0a,pch=1)
    lines(ay,SSB0d,lty=3)
    points(ay,SSB0d,pch=4)

  } else if(type == "SSBMSY") {
    SSBMSY<-apply(MSEproj@RefPoint$ByYear$SSBMSY, 2, median)
    lines(ay,SSBMSY,lty=3)
    points(ay,SSBMSY,pch=4)
  } else if(type == "F") {
    FMSY<-apply(MSEproj@RefPoint$ByYear$FMSY, 2, median)
    lines(ay,FMSY,lty=3)
    points(ay,FMSY,pch=4)
  }

  matlines(c(CurrentYr, py), rep(hist[nyh], nMP) %>% cbind(proj_med) %>% t(),
           type='l', col=MPcols, lty=1, lwd=2.5)
  lines(hy,hist,lwd=5)

  # Legend
  plot(1,1,col='white',xlab="",ylab="",axes=F)
  legend('topleft',legend=MSEproj@MPs,lty = 1, lwd = 2.5, col=MPcols,bty='n',title="MP",cex=1)

  if(type == "SSB0") {
    legend('bottomleft',
           legend=c("Historical SSB", expression(Initial~SSB[0]), expression(Asymptotic~SSB[0]), expression(Dynamic~SSB[0])),
           lwd = c(5, rep(1, 3)),
           pch = c(NA, 16, 1, 4),
           lty=c(1, 1, 2, 3),
           title = "SSB Type",
           bty='n',cex=1)
  } else if(type == "SSBMSY") {
    legend('bottomleft',
           legend=c("Historical SSB", expression(SSB[MSY])),
           lwd = c(5, 1),
           pch = c(NA, 4),
           lty=c(1, 3),
           title = "SSB Type",
           bty='n',cex=1)
  } else if(type == "F") {
    legend('bottomleft',
           legend=c("Historical F", expression(F[MSY])),
           lwd = c(5, 1),
           pch = c(NA, 4),
           lty=c(1, 3),
           title = "F Type",
           bty='n',cex=1)
  }

}

make_PMobj <- function(OBJs, type = c("SSB", "SSB0", "SSBMSY", "F", "SPR", "Catch"),
                       frac = 0.4, year_range, label, ...) {
  type <- match.arg(type)
  dots <- list(...)

  MSEproj<-OBJs$MSEproj
  CurrentYr <- MSEproj@OM$CurrentYr[1]

  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + 1:nyp

  yind <- match(year_range[1]:year_range[2], py)

  if(type == "SSB") {
    xout <- MSEproj@SSB
    ref <- MSEproj@SSB_hist[, match(dots$SSBhist_yr, seq(CurrentYr - nyh + 1, CurrentYr))]
    caption <- paste0("Probability~SSB/SSB[", dots$SSBhist_yr, "]>", frac)
  } else if(type == "SSB0") {
    xout <- MSEproj@SSB

    # Identical regardless of MP, but make same shape as SSB
    ref <- switch(dots$SSB0_type,
                  "Asymptotic" = array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(xout)[c(1, 3 ,2)]) %>%
                    aperm(c(1, 3, 2)),
                  "Initial" = MSEproj@SSB_hist[, 1],
                  "Dynamic" = array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(xout)[c(1, 3, 2)]) %>%
                    aperm(c(1, 3, 2))
                  )

    caption <- paste0("Probability~'SSB'~'/'~'", dots$SSB0_type, "'~SSB[0]>", frac)

    #SSB0d <- array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(SSB)[c(1, 3, 2)]) %>% aperm(c(1, 3, 2))
    #SSB0a <- array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(SSB)[c(1, 3 ,2)]) %>% aperm(c(1, 3, 2))
    #SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[, nyh], dim(SSB0a))
    #SSB0i <- array(MSEproj@SSB_hist[, 1], dim(SSB0a))
  } else if(type == "SSBMSY") {
    xout <- MSEproj@SSB
    ref <- MSEproj@RefPoint$SSBMSY[, , pyind]
    caption <- paste0("Probability~SSB/SSB[MSY]>", frac)
  } else if(type == "F") {
    xout <- MSEproj@RefPoint$FMSY[, , pyind]/MSEproj@FM
    ref <- 1
    caption <- paste0("Probability~F/F[MSY]<", frac)
  } else if(type == "SPR") {
    xout <- MSEproj@SPR$Equilibrium
    ref <- 1
    caption <- paste0("Probability~SPR>", frac)
  } else {
    xout <- MSEproj@Catch
    ref <- MSEproj@CB_hist[, match(dots$Chist_yr, seq(CurrentYr - nyh + 1, CurrentYr))]
    caption <- paste0("Probability~C/C[", dots$Chist_yr, "]>", frac)
  }

  PM <- new("PMobj")
  PM@Name <- label
  PM@Caption <- caption
  PM@Stat <- xout/ref
  PM@Ref <- frac
  PM@Prob <- apply(PM@Stat[, , yind, drop = FALSE] > PM@Ref, c(1, 2), mean)
  PM@Mean <- apply(PM@Stat[, , yind, drop = FALSE] > PM@Ref, 2, mean)
  PM@MPs <- MSEproj@MPs

  return(PM)
}

prob_plot <- function(OBJs, PM_list = list(), xlim = NULL, ylim = NULL, figure = TRUE) {

  MSEproj<-OBJs$MSEproj
  nMP<-MSEproj@nMPs
  CurrentYr <- MSEproj@OM$CurrentYr[1]
  MPcols <- rainbow(nMP,start=0.2,end=1)

  # Probability plots
  nyp<-MSEproj@proyears
  py<-CurrentYr + 1:nyp
  if(missing(xlim) || is.null(xlim) || all(!xlim)) xlim <- range(py)
  if(missing(ylim) || is.null(ylim) || all(!ylim)) ylim <- c(0, 1.05)

  if(figure) {
    # PM_list is either length one or two
    layout(matrix(0:length(PM_list) + 1, nrow = 1), widths = c(rep(0.8/length(PM_list), length(PM_list)), 0.2))
    par(mai=c(0.1,0.6,0.3,0.01),omi=c(0.6,0,0.2,0))
    for(rr in 1:length(PM_list)) {
      Prob_plot_int(x = PM_list[[rr]]@Stat, ref = 1, frac = PM_list[[rr]]@Ref, MPcols = MPcols, py = py,
                    plotx = rr == 1, xlim = xlim, ylim = ylim,
                    ploty = rr == 1, zlab = parse(text = PM_list[[rr]]@Caption),CurrentYr=CurrentYr)
    }
    mtext("Year",1,outer=T,line=2.4)

    plot(1,1,col='white',xlab="",ylab="",axes=F)
    legend('left',legend=MSEproj@MPs,lwd = 2, col=MPcols,bty='n',title="MPs",title.col='black',cex=1)
  } else {
    prob <- sapply(PM_list, function(x) x@Mean) %>%
      structure(dimnames = list(MSEproj@MPs, rep("Probability", length(PM_list))))
    return(prob)
  }
}

Prob_plot_int<-function(x,ref,frac,MPcols,py,plotx,ploty,xlim,ylim,zlab,CurrentYr){
  pp<-apply(x/(ref*frac) > 1,2:3,mean)
  plot(range(py),c(0,1.05),xlim=xlim,ylim=ylim,typ='n',yaxs='i',xaxs='i',xlab="",ylab="")
  #if(plotx) axis(1)
  #if(ploty) axis(2)
  abline(h=seq(0,1,by=0.1),col='grey')
  abline(v=CurrentYr+((1:100)*10),col='grey')
  matlines(py,t(pp),col=MPcols,lwd=2,lty=1,type='l')
  if(ploty) mtext("Probability",2,line=2.5)
  mtext(zlab,3,line=0.6,font=2)
}


stoch_plot<-function(OBJs, MPstoch, qval = 0.9, type = c("SSB0", "SSBMSY", "F", "SPR", "Catch")) {
  type <- match.arg(type)

  qval <- max(0.01, qval)
  qval <- min(0.99, qval)
  type <- match.arg(type)

  MSEproj<-OBJs$MSEproj
  nMP<-MSEproj@nMPs
  CurrentYr <- MSEproj@OM$CurrentYr[1]
  MPcols <- rainbow(nMP,start=0.2,end=1,alpha = 0.5)
  MPlabcols <- rainbow(nMP,start=0.2,end=1)

  MPind<-match(MPstoch,MSEproj@MPs)

  # Probability plots
  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + 1:nyp

  if(type == "SSB0") {
    SSB<-MSEproj@SSB
    # Identical regardless of MP, but make same shape as SSB
    SSB0d <- array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(SSB)[c(1, 3, 2)]) %>% aperm(c(1, 3, 2))
    SSB0a <- array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(SSB)[c(1, 3 ,2)]) %>% aperm(c(1, 3, 2))

    SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[, nyh], dim(SSB0a))
    SSB0i <- array(MSEproj@SSB_hist[, 1], dim(SSB0a))

    refs<-list(SSB0i,SSB0an,SSB0a,SSB0d)
    nrr<-length(refs)

    # Plot SSB as well!
    ylabs <- paste0("SSB/", c("Initial", paste0("Asymptotic~(", CurrentYr, ")"), "Asymptotic", "Dynamic"), "~SSB[0]")

    layout(cbind(matrix(1:nrr,nrow=2,byrow=T),rep(nrr+1,2)),widths=c(1,1,0.5))
    par(mai=c(0.3,0.8,0.1,0.01),omi=c(0.1,0.1,0.1,0))
    for(rr in 1:nrr) {
      Stoch_plot_int(x=SSB,ref=refs[[rr]],ylab=ylabs[rr],py=py,MPcols=MPcols,
                     MPlabcols=MPlabcols,MPind=MPind,qval=qval)
    }

  } else if(type == "SSBMSY") {
    layout(matrix(1:3, nrow = 1), widths=c(1,1,0.5))
    par(mai=c(0.3,0.8,0.1,0.01),omi=c(0.1,0.1,0.1,0))

    SSB<-MSEproj@SSB
    Stoch_plot_int(x = SSB, ref = 1, ylab = "SSB", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval)

    SSBMSY <- MSEproj@RefPoint$SSBMSY[, , pyind]
    Stoch_plot_int(x = SSB, ref = SSBMSY, ylab = "SSB/SSB[MSY]", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval)
  } else if(type == "F") {
    layout(matrix(1:3, nrow = 1), widths=c(1,1,0.5))
    par(mai=c(0.3,0.8,0.1,0.01),omi=c(0.1,0.1,0.1,0))

    FM <- MSEproj@FM
    Stoch_plot_int(x = FM, ref = 1, ylab = "Fishing~mortality", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval)

    FMSY <- MSEproj@RefPoint$FMSY[, , pyind]
    Stoch_plot_int(x = FM, ref = FMSY, ylab = "F/F[MSY]", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval)
  } else if(type == "SPR") {
    layout(matrix(1:2, nrow = 1), widths=c(2,0.5))
    par(mai=c(0.3,0.8,0.1,0.01),omi=c(0.1,0.1,0.1,0))

    Stoch_plot_int(x = MSEproj@SPR$Equilibrium, ref = 1, ylab = "Equilibrium~SPR", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval)
  } else {
    layout(matrix(1:2, nrow = 1), widths=c(2,0.5))
    par(mai=c(0.3,0.8,0.1,0.01),omi=c(0.1,0.1,0.1,0))

    Stoch_plot_int(x = MSEproj@Catch, ref = 1, ylab = "Catch", py = py, MPcols = MPcols,
                   MPlabcols = MPlabcols, MPind = MPind, qval = qval)
  }

  plot(1,1,typ='n',xlab="",ylab="",axes=F)
  legend('left',legend=c(MSEproj@MPs[MPind]),
         col=MPlabcols[MPind],lty = 1, lwd = 10, bty='n',title="MPs", cex = 1)

  mtext("Year", 1, outer = TRUE)
}


# stochastic plot funcs

Stoch_plot_int <- function(x,ref=1,ylab,py,MPcols,MPlabcols,MPind,qval){
  q <- c(0.5 * (1 - qval), 0.5, qval + 0.5 * (1 - qval))
  qs <- apply(x/ref, 2:3, quantile, q)
  plot(range(py), c(0, 1.1 * max(qs)) ,typ='n',yaxs='i',xaxs='i',xlab="",ylab="")
  abline(h = pretty(c(0, 1.1 * max(qs)), 6) , col = 'grey')
  abline(h = c(0.5, 1), col = 'black', lty = 3)

  for(mm in MPind) {
    polygon(c(py, py[length(py):1]), c(qs[1, mm, ], qs[3, mm, length(py):1]),
            col = makeTransparent(MPcols[mm], 70), border = NA)
    lines(py, qs[2, mm, ], col = MPlabcols[mm], lwd = 3)
  }
  mtext(parse(text = ylab), 2, line=2.5)
}


hist_sim <- function(OBJs, MP, sims, type = c("SSB0", "SSBMSY", "F", "SPR", "Catch")) {
  type <- match.arg(type)

  MSEproj<-OBJs$MSEproj

  if(missing(MP)) MP <- MSEproj@MPs[1]
  if(missing(sims)) sims <- 1:max(MSEproj@nsim, 3)

  proyears<-MSEproj@proyears
  nyears<-MSEproj@nyears
  cols<-c('red','blue','black')
  CurrentYr <- MSEproj@OM$CurrentYr[1]

  yrs<-CurrentYr+(-nyears:(proyears-1))
  MPind<-match(MP,MSEproj@MPs)

  SSB<-cbind(MSEproj@SSB_hist[sims, , drop = FALSE],
             MSEproj@SSB[, MPind, ][sims, , drop = FALSE])

  if(type == "SSB0") {

    SSB<-cbind(MSEproj@SSB_hist[sims, , drop = FALSE],
               MSEproj@SSB[, MPind, ][sims, , drop = FALSE])

    SSB0d<-MSEproj@RefPoint$Dynamic_Unfished$SSB0[sims, , drop = FALSE]
    SSB0a<-MSEproj@RefPoint$ByYear$SSB0[sims, , drop = FALSE]
    SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[sims, nyears], dim(SSB0a))
    SSB0i<-SSB[,1]

    par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
    layout(matrix(c(1,1,2,3,4,4,5,6,6),nrow=3,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1,1))

    matplot(yrs,t(SSB0d),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0,max(SSB0d)),ylab="Spawning stock biomass (SSB)")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    #matlines(yrs,t(SSB0a),col=cols,lty=2, pch = 1, typ = 'o')
    matlines(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
    matlines(yrs, matrix(SSB0i, length(yrs), length(sims), byrow = TRUE), lty = 1, col=cols)
  } else if(type == "SSBMSY") {

    SSB <- cbind(MSEproj@SSB_hist[sims, , drop = FALSE],
                 MSEproj@SSB[, MPind, ][sims, , drop = FALSE])
    SSBMSY <- MSEproj@RefPoint$ByYear$SSBMSY[sims, , drop = FALSE]

    par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
    layout(matrix(c(1,1,2,3,3,4),nrow=2,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1.5))

    matplot(yrs,t(SSBMSY),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0,max(SSB, SSBMSY)),ylab="Spawning stock biomass (SSB)")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    matlines(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
  } else if(type == "F") {

    FM <- cbind(MSEproj@FM_hist[sims, , drop = FALSE], MSEproj@FM[, MPind, ][sims, , drop = FALSE])
    FMSY <- MSEproj@RefPoint$ByYear$FMSY[sims, , drop = FALSE]

    par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
    layout(matrix(c(1,1,2,3,3,4),nrow=2,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1.5))

    matplot(yrs,t(FMSY),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0, max(FM, FMSY)),
            ylab="Fishing mortality")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
    matlines(yrs,t(FM),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
  } else if(type == "SPR") {

    SPR <- cbind(OBJs$MSEhist@TSdata$SPR$Equilibrium[sims, , drop = FALSE],
                 MSEproj@SPR$Equilibrium[, MPind, ][sims, , drop = FALSE])

    layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))

    matplot(yrs,t(SPR),col=makeTransparent(cols,80),lty=1,type='l',lwd=5,yaxs='i',ylim=c(0,1),
            ylab="Equilibrium SPR")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  } else {

    Catch <- cbind(MSEproj@CB_hist[sims, , drop = FALSE],
                   MSEproj@Catch[, MPind, ][sims, , drop = FALSE])

    layout(matrix(1:2, nrow = 1), widths = c(0.8, 0.2))

    matplot(yrs,t(Catch),col=makeTransparent(cols,80),lty=1,type='l',lwd=5,yaxs='i',ylim=c(0, max(Catch)),
            ylab="Catch")
    abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  }

  plot(1,1,typ='n',axes=F,xlab="",ylab="")

  if(type == "SSB0") {
    legend('topleft',legend=c(expression(SSB), expression(Dynamic~SSB[0]), expression(Initial~SSB[0])),
           lty=c(1,3,1,1),lwd=c(3,1,1,1),pch=c(NA,4,NA),col=c("#99999995",'black','black','black'),bty='n')
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

    matplot(yrs,t(SSB/SSB0i),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSB0i)),
            ylab=expression(SSB/Initial~SSB[0]))
    abline(h=pretty(c(0,1.1 * max(SSB/SSB0i)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

    matplot(yrs,t(SSB/SSB0d),col=cols,lty=2,type='l',yaxs='i',ylim=c(0, 1.1),ylab=expression(SSB/Dynamic~SSB[0]))
    abline(h=seq(0,1,length.out=6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

    matplot(yrs,t(SSB/SSB0a),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSB0a)),
            ylab=expression(SSB/Asymptotic~SSB[0]))
    abline(h=pretty(c(0,1.1 * max(SSB/SSB0a)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

    matplot(yrs,t(SSB/SSB0an),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSB0an)),
            ylab=parse(text = paste0("SSB/Asymptotic~(", CurrentYr, ")~SSB[0]")))
    abline(h=pretty(c(0,1.1 * max(SSB/SSB0an)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

  } else if(type == "SSBMSY") {

    legend('topleft',legend=c(expression(SSB), expression(SSB[MSY])),
           lty=c(1,3),lwd=c(3,1),pch=c(NA,4),col=c("#99999995",'black'),bty='n')
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

    matplot(yrs,t(SSB/SSBMSY),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(SSB/SSBMSY)),
            ylab=expression(SSB/SSB[MSY]))
    abline(h=pretty(c(0,1.1 * max(SSB/SSBMSY)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')
  } else if(type == "F") {

    legend('topleft',legend=c(expression(F), expression(F[MSY])),
           lty=c(1,3),lwd=c(3,1),pch=c(NA,4),col=c("#99999995",'black'),bty='n')
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

    matplot(yrs,t(FM/FMSY),col=cols,lty=1,type='l',yaxs='i',ylim=c(0, 1.1 * max(FM/FMSY)),
            ylab=expression(F/F[MSY]))
    abline(h=pretty(c(0,1.1 * max(F/FMSY)), 6),col='#99999930')
    abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')
  } else {
    legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')
  }

}

tradeoff_plot <- function(OBJs, PMx, PMy, xlab, ylab) {

  MSEproj<-OBJs$MSEproj
  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1, alpha = 0.5)

  layout(matrix(1:2, nrow = 1), widths = c(0.7, 0.3))

  plot(NULL, NULL, xlim = c(0, 1.05), ylim = c(0, 1.05), xlab = xlab, ylab = ylab)
  abline(h = seq(0, 1, 0.2), col = "grey")
  abline(v = seq(0, 1, 0.2), col = "grey")
  abline(a = 0, b = 1, lty = 3)
  points(PMx@Mean, PMy@Mean, col = MPcols, cex = 1, pch = 16)

  plot(1, 1, typ = "n",axes=F,xlab="",ylab="")
  legend("left", legend = MSEproj@MPs, col = MPcols, pch = 16, bty='n',title="MPs", pt.cex = 1)

}

radar_plot <- function(pm_df, palette = "Set2", custom_pal = NULL, ...) {
  x <- reshape2::melt(pm_df, id.vars = "MP", value.name = "prob",
                      variable.name = "pm")
  g <- ggspider::spider_web(x, "MP", "pm", "prob", leg_main_title = "MP",
                            leg_lty_title = "MP type", palette = palette, ...)
  if ("ggplot" %in% class(g)) {
    g <- g + ggplot2::labs(color = "MP")
  }
  if (!is.null(custom_pal)) {
    suppressMessages({
      g <- g + ggplot2::scale_color_manual(values = custom_pal)
    })
  }
  g
}

