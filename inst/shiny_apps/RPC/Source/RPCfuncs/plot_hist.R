
# MSEhist<-readRDS("C:/temp/MSEhist.rda")



plotquant<-function(x,p=c(0.05,0.25,0.75,0.95), yrs, cols=list(colm="dark blue", col50='light blue', col90='#60859925'), addline=T, ablines=NA){
  ny<-length(yrs)
  x[x==Inf]<-NA
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T,type=3)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T,type=3)
  polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col=cols$col90)

  polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=cols$col50)
  if(!is.na(ablines[1]))abline(h=ablines,col='#99999980')

  if(addline)for(i in 1:2)lines(yrs,x[i,],col='black',lty=i)
  lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col=cols$colm)
}

tsplot<-function(x,xlab="",ylab="",zeroyint=T,cols=list(colm="dark blue", col50='light blue', col90='#60859925')){
  yrs<-CurrentYr-((dim(x)[2]):1)
  ymin=0
  if(!zeroyint)ymin<-quantile(x,0.01)
  plot(range(yrs),c(ymin,quantile(x,0.99)),col="white",xlab=xlab,ylab=ylab,yaxs='i')
  abline(h=pretty(seq(from=ymin,to=max(x)*1.25,length.out=20)),col="light grey")
  plotquant(x,yrs=yrs,cols=cols)

}

hist_bio<-function(dummy=1){

  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  tsplot(x=apply(MSEhist@TSdata$SBiomass,1:2,sum),xlab="Historical Year",ylab="Spawning biomass")
  tsplot(apply(MSEhist@TSdata$Biomass,1:2,sum),xlab="Historical Year",ylab="Biomass")
  tsplot(apply(MSEhist@TSdata$Number,1:2,sum),xlab="Historical Year",ylab="Numbers")
  tsplot(apply(MSEhist@TSdata$VBiomass,1:2,sum),xlab="Historical Year",ylab="Vulnerable Biomass")
  tsplot(x=log(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage-1)]),xlab="Historical Year",ylab="Recruitment strength",zeroyint=F)
  tsplot(apply(MSEhist@AtAge$Number[,1,,],1:2,sum),xlab="Historical Year",ylab="Recruitment")

}

hist_growth_I<-function(dummy=1)  plot('Growth', MSEhist, plot.num=1)
hist_growth_II<-function(dummy=1)  plot('Growth', MSEhist, plot.num=2)
hist_growth_III<-function(dummy=1)  plot('Growth', MSEhist, plot.num=3)
hist_maturity<-function(dummy=1)  plot('Maturity', MSEhist)
hist_survival<-function(dummy=1)  plot('M', MSEhist)
hist_spatial<-function(dummy=1)  plot('Spatial', MSEhist)



hist_exp<-function(dummy=1){

  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')
  tsplot(apply(MSEhist@TSdata$Landings,1:2,sum),xlab="Historical Year",ylab="Landings",cols=cols)
  tsplot(apply(MSEhist@TSdata$Discards,1:2,sum),xlab="Historical Year",ylab="Discards",cols=cols)

}


hist_SSB_sim<-function(input){

  proyears<-MSEhist@OM@proyears
  nyears<-MSEhist@OM@nyears
  cols<-c('red','green','blue')
  yrs<-CurrentYr-(nyears:1)
  #psim<-1:2
  psim<-1:input$nsim_hist_SSB

  par(mai=c(0.3,0.6,0.1,0.05),omi=c(0.6,0,0,0))
  layout(matrix(c(1,1,2,3,4,4,5,6,6),nrow=3,byrow=T),widths=c(0.5,0.3,0.2),heights=c(1.5,1,1))

  SSB0d<-MSEhist@Ref$Dynamic_Unfished$SSB0[psim,1:nyears,drop=F]
  SSB0a<-MSEhist@Ref$ByYear$SSB0[psim,1:nyears,drop=F]
  SSB<-apply(MSEhist@TSdata$SBiomass[psim,1:nyears,,drop=F],1:2,sum,drop=F)
  SSB0i<-SSB[,1]

  matplot(yrs,t(SSB0d),col=cols,lty=2,type='l',yaxs='i',ylim=c(0,max(SSB0d)),ylab="Spawning stock biomass (SSB)")
  matplot(yrs,t(SSB0a),col=cols,lty=1,lwd=1,type='l',add=T)
  matplot(yrs,t(SSB),col=makeTransparent(cols,80),type='l',lty=1,lwd=3,add=T)
  abline(h=SSB0i,col=cols,lty=3,lwd=1)
  plot(1,1,col='white',axes=F,xlab="",ylab="")
  legend('left',legend=c("SSB","SSB0 dynamic","SSB0 initial","SSB0 asymptotic"),
         lty=c(1,2,3,1),lwd=c(3,1,1,1),col=c("#99999995",'black','black','black'),bty='n')

  matplot(yrs,t(SSB/SSB0d),col=cols,lty=2,type='l',yaxs='i',ylim=c(0,1),ylab="SSB / SSB0 dynamic")
  abline(h=seq(0,1,length.out=6),col='#99999930')
  matplot(yrs,t(SSB/SSB0a),col=cols,lty=1,type='l',yaxs='i',ylim=c(0,1),ylab="SSB / SSB0 asymptotic")
  abline(h=seq(0,1,length.out=6),col='#99999930')
  matplot(yrs,t(SSB/SSB0i),col=cols,lty=1,type='l',yaxs='i',ylim=c(0,1),ylab="SSB / SSB0 initial")
  abline(h=seq(0,1,length.out=6),col='#99999930')

  #Brela<-MSEhist@Ref$ByYear$BMSY[psim,1:nyears,drop=F]/MSEhist@Ref$ByYear$SSB0[psim,1:nyears,drop=F]



}


