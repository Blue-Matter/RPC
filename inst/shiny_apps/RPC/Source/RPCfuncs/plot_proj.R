
# MSEproj <- readRDS("C:/temp/MSEproj.rda")

# Called functions

B_proj_plot<-function(OBJs){
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

  SSBmed<-apply(MSEproj@SSB, c(2,3), median)
  SSBhist<-apply(MSEproj@SSB_hist, 2, median)
  mSSB<-max(SSBmed, SSBhist)
  SSB0d<-apply(MSEproj@RefPoint$Dynamic_Unfished$SSB0, 2, median)
  SSB0a<-apply(MSEproj@RefPoint$ByYear$SSB0, 2, median)

  plot(range(ay),c(0, 1.2 * mSSB),
       typ = "n",yaxs='i',xaxs='i',xlab = "Spawning biomass (SSB)", ylab = "Year")
  abline(h=pretty(seq(0,mSSB,length.out=15)),col='light grey')
  abline(h=SSB0a[1],lty=1)
  points(ay, rep(SSB0a[1], length(ay)),pch=16)
  abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  lines(ay,SSB0a,lty=2)
  points(ay,SSB0a,pch=1)
  lines(ay,SSB0d,lty=3)
  points(ay,SSB0d,pch=4)

  matlines(c(CurrentYr, py), rep(SSBhist[nyh], nMP) %>% cbind(SSBmed) %>% t(),
           type='l', col=MPcols, lty=1, lwd=2.5)
  lines(hy,SSBhist,lwd=5)

  mtext(expression(Median~SSB~and~SSB[0]~reference~points),3,line=0.8,font=2)

  # Legend
  plot(1,1,col='white',xlab="",ylab="",axes=F)
  legend('topleft',legend=MSEproj@MPs,lty = 1, lwd = 2.5, col=MPcols,bty='n',title="MP",cex=1)
  legend('bottomleft',
         legend=c("Historical SSB", expression(Initial~SSB[0]), expression(Asymptotic~SSB[0]), expression(Dynamic~SSB[0])),
         lwd = c(5, rep(1, 3)),
         pch = c(NA, 16, 1, 4),
         lty=c(1, 1, 2, 3),
         title = "SSB Type",
         bty='n',cex=1)
}

B_prob_plot <- function(OBJs, frac = 0.4, xlim = NULL, ylim = NULL, figure = TRUE) {
  MSEproj<-OBJs$MSEproj
  nMP<-MSEproj@nMPs
  CurrentYr <- MSEproj@OM$CurrentYr[1]
  MPcols <- rainbow(nMP,start=0.2,end=1)

  # Probability plots
  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + 1:nyp
  if(missing(xlim) || is.null(xlim) || all(!xlim)) xlim <- range(py)
  if(missing(ylim) || is.null(ylim) || all(!ylim)) ylim <- c(0, 1.05)

  SSB<-MSEproj@SSB

  # Identical regardless of MP, but make same shape as SSB
  SSB0d <- array(MSEproj@RefPoint$Dynamic_Unfished$SSB0[, pyind], dim(SSB)[c(1, 3, 2)]) %>% aperm(c(1, 3, 2))
  SSB0a <- array(MSEproj@RefPoint$ByYear$SSB0[, pyind], dim(SSB)[c(1, 3 ,2)]) %>% aperm(c(1, 3, 2))

  SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[, nyh], dim(SSB0a))
  SSB0i <- array(MSEproj@SSB_hist[, 1], dim(SSB0a))

  refs<-list(SSB0i,SSB0an,SSB0a,SSB0d)
  nrr<-length(refs)
  zlabs<-c("Initial", paste0("Asymptotic~(", CurrentYr, ")"), "Asymptotic", "Dynamic")

  if(figure) {
    layout(matrix(1:(nrr+1), nrow = 1))
    par(mai=c(0.1,0.2,0.3,0.01),omi=c(0.6,0.35,0.2,0))

    for(rr in 1:nrr) {
      Prob_plot(x = SSB, ref = refs[[rr]], frac = frac, MPcols = MPcols, py = py, plotx = TRUE, xlim = xlim, ylim = ylim,
                ploty = rr == 1, laby = rr == 1, labz = TRUE, zlab = parse(text = paste0(zlabs[rr], "~SSB[0]")))
    }

    plot(1,1,col='white',xlab="",ylab="",axes=F)
    legend('left',legend=MSEproj@MPs,lwd = 2, col=MPcols,bty='n',title="MPs",title.col='black',cex=1)
    mtext("Year",1,outer=T,line=2.4)
  } else {

    year_ind <- match(xlim[1]:xlim[2], py)
    prob <- sapply(refs, function(x) {
      apply(SSB[, , year_ind, drop = FALSE]/x[, , year_ind, drop = FALSE] > frac, 2, mean)
    }) %>% structure(dimnames = list(MSEproj@MPs,
                                     c("Initial", paste0("Asymptotic (", CurrentYr, ")"), "Asymptotic", "Dynamic") %>%
                                       paste("SSB0")))

    return(prob)
  }
}

Prob_plot<-function(x,ref,frac,MPcols,py,plotx,xlim,ylim,ploty,laby,labz,zlab){
  pp<-apply(x/(ref*frac) > 1,2:3,mean)
  plot(range(py),c(0,1.05),xlim=xlim,ylim=ylim,typ='n',yaxs='i',xaxs='i',xlab="",ylab="",axes=F)
  if(plotx)axis(1)
  if(ploty)axis(2)
  abline(h=seq(0,1,by=0.1),col='grey')
  abline(v=CurrentYr+((1:100)*10),col='grey')
  matlines(py,t(pp),col=MPcols,lwd=2,lty=1,type='l')
  if(laby==T)mtext(parse(text = paste0("Probability~SSB/SSB[0]>", frac)),2,line=2.5)
  if(labz==T)mtext(zlab,3,line=0.6,font=2)
}


B_stoch_plot<-function(OBJs, MPstoch, qval = 0.9){
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
  for(rr in 1:nrr) Stoch_plot(x=SSB,ref=refs[[rr]],ylab=ylabs[rr],py=py,MPcols=MPcols,MPlabcols=MPlabcols,MPind=MPind,qval=qval)

  plot(1,1,typ='n',xlab="",ylab="",axes=F)
  legend('left',legend=c(MSEproj@MPs[MPind]),
         col=MPlabcols[MPind],lty = 1, lwd = 10, bty='n',title="MPs",cex=1.5)

  mtext("Year", 1, outer = TRUE)
}


# stochastic plot funcs

Stoch_plot<-function(x,ref,ylab,py,MPcols,MPlabcols,MPind,qval){
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


hist_SSB_sim<-function(OBJs, MP, sims) {

  MSEproj<-OBJs$MSEproj

  if(missing(MP)) MP <- MSEproj@MPs[1]
  if(missing(sims)) sims <- 1:max(MSEproj@nsim, 3)

  proyears<-MSEproj@proyears
  nyears<-MSEproj@nyears
  cols<-c('red','blue','black')
  CurrentYr <- MSEproj@OM$CurrentYr[1]

  yrs<-CurrentYr+(-nyears:(proyears-1))
  MPind<-match(MP,MSEproj@MPs)

  SSB0d<-MSEproj@RefPoint$Dynamic_Unfished$SSB0[sims, , drop = FALSE]
  SSB0a<-MSEproj@RefPoint$ByYear$SSB0[sims, , drop = FALSE]
  SSB0an <- array(MSEproj@RefPoint$ByYear$SSB0[sims, nyears], dim(SSB0a))
  SSB<-cbind(MSEproj@SSB_hist[sims, , drop = FALSE],
             MSEproj@SSB[, MPind, ][sims, , drop = FALSE])
  SSB0i<-SSB[,1]

  par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
  layout(matrix(c(1,1,2,3,4,4,5,6,6),nrow=3,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1,1))

  matplot(yrs,t(SSB0d),col=cols,lty=3,type='o',pch=4,yaxs='i',ylim=c(0,max(SSB0d)),ylab="Spawning stock biomass (SSB)")
  abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  #matlines(yrs,t(SSB0a),col=cols,lty=2, pch = 1, typ = 'o')
  matlines(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=5)
  matlines(yrs, matrix(SSB0i, length(yrs), length(sims), byrow = TRUE), lty = 1, col=cols)

  plot(1,1,typ='n',axes=F,xlab="",ylab="")
  legend('topleft',legend=c(expression(SSB), expression(Dynamic~SSB[0]), expression(Initial~SSB[0])),
         lty=c(1,3,1,1),lwd=c(3,1,1,1),pch=c(NA,4,NA),col=c("#99999995",'black','black','black'),bty='n')
  legend('bottomleft',legend=paste0("Sim #", sims),lwd=3,col=makeTransparent(cols, 80),text.col = cols,bty='n')

  #legend('topleft',legend=c("SSB","SSB0 dynamic","SSB0 initial","SSB0 asymptotic"),
  #       lty=c(1,2,3,1),lwd=c(3,1,1,1),col=c("#99999995",'black','black','black'),bty='n')
  #legend('bottomleft',legend=paste("Sim",1:3)[sims],text.col = cols[sims],bty='n')

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


}

