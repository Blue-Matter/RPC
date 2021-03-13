
# MSEproj <- readRDS("C:/temp/MSEproj.rda")

# med ssb funcs
Med_SSB_plot<-function(MPcols){

  nyh<-MSEproj@nyears
  nyp<-MSEproj@proyears
  hy<-CurrentYr - (nyh:0)
  py<-CurrentYr + (0:(nyp-1))
  ay<-CurrentYr + c(-nyh:(nyp-1))

  SSBmed<-apply(MSEproj@SSB,c(2,3),quantile,p=0.5)
  SSBhist<-c(apply(MSEproj@SSB_hist,2,quantile,p=0.5),SSBmed[1,1])
  mSSB<-max(SSBmed,SSBhist)
  SSB0d<-apply(MSEhist@Ref$Dynamic_Unfished$SSB0,2,quantile,p=0.5)
  SSB0a<-apply(MSEhist@Ref$ByYear$SSB0,2,quantile,p=0.5)


  plot(c(min(hy),max(py)),c(0,mSSB),col='white',yaxs='i',xaxs='i',xlab="",ylab="")
  abline(h=pretty(seq(0,mSSB,length.out=15)),col='light grey')
  abline(h=SSBhist[1],lty=2,col='grey')
  abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  lines(ay,SSB0a,lty=2)
  lines(ay,SSB0d,lty=3)

  matplot(py,t(SSBmed),type='l',xlim=c(min(hy),max(py)),ylim=c(0,mSSB),col=MPcols,lty=1,lwd=2,add=T)
  lines(hy,SSBhist,lwd=2)

  mtext("SSB",2,line=2.5,font=1)
  mtext("Median Spawning Stock Biomass (SSB) and 'SSB0' reference points",3,line=0.8,font=2)

}

Med_SSB_plot_leg<-function(MPcols){

  plot(1,1,col='white',xlab="",ylab="",axes=F)
  legend('topleft',legend=c(MSEproj@MPs),text.col=MPcols,bty='n',title="MPs",title.col='black',cex=1.3)
  legend('bottomleft',legend=c('Initial',"Asymptotic","Dynamic"),
         col=c('grey','black','black'), lty=c(2,2,3),text.col=c('grey','black','black'),
         title="SSB0 reference points",title.col='black',bty='n',cex=1.3)
}

# prob plot funcs
prob_plot_leg<-function(MPcols){

  plot(1,1,col='white',xlab="",ylab="",axes=F)
  legend('left',legend=c(MSEproj@MPs),text.col=MPcols,bty='n',title="MPs",title.col='black',cex=2)

}

Prob_plot<-function(x,ref,frac,MPcols,py,plotx,ploty,laby,labz,zlab){

  pp<-apply(x/(ref*frac) > 1,2:3,mean)
  plot(range(py),c(0,1),col='white',yaxs='i',xaxs='i',xlab="",ylab="",axes=F)
  if(plotx)axis(1)
  if(ploty)axis(2)
  abline(h=seq(0,1,by=0.1),col='grey')
  abline(v=CurrentYr+((1:100)*10),col='grey')
  matplot(py,t(pp),col=MPcols,lwd=2,lty=1,type='l',ylim=c(0,1),add=T)
  if(laby==T)mtext(paste0("Prob (SSB > ",frac," RP)"),2,line=2.5)
  if(labz==T)mtext(zlab,3,line=0.6,font=2)

}


# stochastic plot funcs

Stoch_plot<-function(x,ref,ylab,py,MPcols,MPind){

  qs<-apply(x/ref,2:3,quantile,c(0.05,0.95))
  plot(range(py),c(0,max(qs[,MPind,])),col='white',yaxs='i',xaxs='i',xlab="",ylab="")
  abline(h=seq(0,10,by=0.1),col='grey')
  abline(h=c(0.5,1),col='black')

  for(mm in MPind)    polygon(c(py,py[length(py):1]),c(qs[1,mm,],qs[2,mm,length(py):1]),col=MPcols[mm],border=NA)
  mtext(ylab,2,line=2.5)

}

stoch_plot_leg<-function(MPlabcols,MPind){

  plot(1,1,col='white',xlab="",ylab="",axes=F)
  legend('left',legend=c(MSEproj@MPs[MPind]),text.col=MPlabcols[MPind],bty='n',title="MPs",title.col='black',cex=1.5)

}





# Called functions

B_proj_plot<-function(dummy=1){

  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1)
  layout(matrix(c(1,2),nrow=1),widths=c(1.5,0.5))
  par(mai=c(0.6,0.5,0.5,0.01),omi=c(0.3,0.35,0.3,0))

  # Median plot (top)
  Med_SSB_plot(MPcols)
  Med_SSB_plot_leg(MPcols)

}

B_prob_plot<-function(dummy=1){

  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1)

  # Probability plots
  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + (0:(nyp-1))

  SSB<-MSEproj@SSB
  SSB0d<-aperm(array(rep(MSEhist@Ref$Dynamic_Unfished$SSB0[,pyind],nMP),dim(SSB)[c(1,3,2)]),c(1,3,2)) # make same shape as SSB
  SSB0a<-aperm(array(rep(MSEhist@Ref$ByYear$SSB0[,pyind],nMP),dim(SSB)[c(1,3,2)]),c(1,3,2))           # make same shape as SSB
  SSB0an<-array(MSEhist@Ref$ByYear$SSB0[,nyh],dim(SSB0a))
  SSB0i<-array(MSEproj@SSB_hist[,1],dim(SSB0a))


  fracs<-c(0.5,0.4,0.1)
  nff<-length(fracs)
  refs<-list(SSB0i,SSB0an,SSB0a,SSB0d)
  nrr<-length(refs)
  zlabs<-c("Initial","Asymptotic now","Asymptotic","Dynamic")

  layout(cbind(matrix(1:(nff*nrr),nrow=nff,byrow=T),rep(nff*nrr+1,nff)))
  par(mai=c(0.1,0.2,0.3,0.01),omi=c(0.6,0.35,0.2,0))


  for(ff in 1:nff){

    for(rr in 1:nrr){
      plotx<-F; if(ff==nff){plotx=T}
      ploty<-F; if(rr==1){ploty=T}
      laby<-F; if(rr==1){laby=T}
      labz<-F; if(ff==1){labz=T}; zlab=zlabs[rr]
      Prob_plot(x=SSB,ref=refs[[rr]],frac=fracs[ff],MPcols=MPcols,py=py,plotx=plotx,ploty=ploty,laby=laby,labz=labz,zlab=zlab)
    }

  }

  prob_plot_leg(MPcols)
  mtext("Projection Year",1,outer=T,line=2.4)


}

B_stoch_plot<-function(input){
  # input=list(MPstoch1=MSEproj@MPs[1],MPstoch2=MSEproj@MPs[2],MPstoch3=MSEproj@MPs[3])
  nMP<-MSEproj@nMPs
  MPcols <- rainbow(nMP,start=0.2,end=1,alpha = 0.5)
  MPlabcols <- rainbow(nMP,start=0.2,end=1)

  MPstoch<-c(input$SMP1,input$SMP2)#,input$SMP3)
  MPind<-match(MPstoch,MSEproj@MPs)

  # Probability plots
  nyp<-MSEproj@proyears
  nyh<-MSEproj@nyears
  pyind<-nyh+(1:nyp)
  py<-CurrentYr + (0:(nyp-1))

  SSB<-MSEproj@SSB
  SSB0d<-aperm(array(rep(MSEhist@Ref$Dynamic_Unfished$SSB0[,pyind],nMP),dim(SSB)[c(1,3,2)]),c(1,3,2)) # make same shape as SSB
  SSB0a<-aperm(array(rep(MSEhist@Ref$ByYear$SSB0[,pyind],nMP),dim(SSB)[c(1,3,2)]),c(1,3,2))           # make same shape as SSB
  SSB0an<-array(MSEhist@Ref$ByYear$SSB0[,nyh],dim(SSB0a))
  SSB0i<-array(MSEproj@SSB_hist[,1],dim(SSB0a))

  refs<-list(SSB0i,SSB0an,SSB0a,SSB0d)
  nrr<-length(refs)
  ylabs<-c("SSB/SSB0 Initial","SSB / SSB0 Asymptotic now","SSB / SSB0 Asymptotic","SSB/SSB0 Dynamic")

  layout(cbind(matrix(1:nrr,nrow=2,byrow=T),rep(nrr+1,2)),widths=c(1,1,0.5))
  par(mai=c(0.3,0.8,0.1,0.01),omi=c(0.1,0.1,0.1,0))
  for(rr in 1:nrr)    Stoch_plot(x=SSB,ref=refs[[rr]],ylab=ylabs[rr],py=py,MPcols=MPcols,MPind=MPind)
  stoch_plot_leg(MPlabcols,MPind)


}



hist_SSB_sim<-function(input){

  proyears<-MSEproj@proyears
  nyears<-MSEproj@nyears
  cols<-c('red','blue','black')
  yrs<-CurrentYr+(-nyears:(proyears-1))
  #  psim<-1:2
  dummy<-input$StochB_resample
  psim<-sample(1:MSEproj@nsim,3,replace=F)[1:input$nsim_hist_SSB]
  MPind<-match(input$StochMP,MSEproj@MPs)


  par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
  layout(matrix(c(1,1,2,3,4,4,5,6,6),nrow=3,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1,1))

  SSB0d<-MSEhist@Ref$Dynamic_Unfished$SSB0[psim,,drop=F]
  SSB0a<-MSEhist@Ref$ByYear$SSB0[psim,,drop=F]
  SSB<-cbind(apply(MSEhist@TSdata$SBiomass[psim,,,drop=F],1:2,sum,drop=F),matrix(MSEproj@SSB[psim,MPind,],nrow=length(psim)))
  SSB0i<-SSB[,1]


  matplot(yrs,t(SSB0d),col=cols,lty=2,type='l',yaxs='i',ylim=c(0,max(SSB0d)),ylab="Spawning stock biomass (SSB)")
  abline(v=CurrentYr+c(0,(1:100)*10),col='grey')
  matplot(yrs,t(SSB0a),col=cols,lty=1,lwd=1,type='l',add=T)
  matplot(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=3,add=T)

  abline(h=SSB0i,col=cols,lty=3,lwd=1)
  plot(1,1,col='white',axes=F,xlab="",ylab="")
  legend('topleft',legend=c("SSB","SSB0 dynamic","SSB0 initial","SSB0 asymptotic"),
         lty=c(1,2,3,1),lwd=c(3,1,1,1),col=c("#99999995",'black','black','black'),bty='n')
  legend('bottomleft',legend=paste("Sim",1:3)[psim],text.col = cols[psim],bty='n')

  matplot(yrs,t(SSB/SSB0d),col=cols,lty=2,type='l',yaxs='i',ylim=c(0,1),ylab="SSB / SSB0 dynamic")
  abline(h=seq(0,1,length.out=6),col='#99999930')
  abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

  matplot(yrs,t(SSB/SSB0a),col=cols,lty=1,type='l',yaxs='i',ylim=c(0,1),ylab="SSB / SSB0 asymptotic")
  abline(h=seq(0,1,length.out=6),col='#99999930')
  abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')

  matplot(yrs,t(SSB/SSB0i),col=cols,lty=1,type='l',yaxs='i',ylim=c(0,1),ylab="SSB / SSB0 initial")
  abline(h=seq(0,1,length.out=6),col='#99999930')
  abline(v=CurrentYr+c(0,(1:100)*10),col='#99999930')


  #Brela<-MSEhist@Ref$ByYear$BMSY[psim,1:nyears,drop=F]/MSEhist@Ref$ByYear$SSB0[psim,1:nyears,drop=F]

}

