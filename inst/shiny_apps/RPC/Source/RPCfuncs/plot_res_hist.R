hist_SSBref_plot<-function(OBJs){

  MSEh<-OBJs$MSEhist
  nyh<-MSEh@OM@nyears

  hy<-CurrentYr - (nyh:1)-1

  SSB<-apply(MSEh@TSdata$SBiomass,1:2,sum)

  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  tsplot(x=SSB,xlab="Year",ylab="Spawning biomass")

  SSB0h<-array(SSB[,1],dim(SSB))
  SSB0d<-MSEh@Ref$Dynamic_Unfished$SSB0
  SSB0a<-MSEh@Ref$ByYear$SSB0

  SSBrh<-SSB/SSB0h
  SSBra<-SSB/SSB0a[,1:nyh]
  SSBrd<-SSB/SSB0d[,1:nyh]

  tsplot(x=SSBrh,xlab="Year",ylab="SSB / initial SSB0")
  tsplot(x=SSBra,xlab="Year",ylab="SSB / asymptotic SSB0")
  tsplot(x=SSBrd,xlab="Year",ylab="SSB / dynamic SSB0")

  lev<-0.4
  pmat4<-cbind(apply(SSBrh>lev,2,mean),apply(SSBra>lev,2,mean),apply(SSBrd>lev,2,mean))
  lev<-0.1
  pmat1<-cbind(apply(SSBrh>lev,2,mean),apply(SSBra>lev,2,mean),apply(SSBrd>lev,2,mean))

  matplot(hy,pmat1,type='l',lty=c(1,1,2),col=c('grey','black','black'),ylab="Probability of exceeding 10%")

  #plot(1,axes=F,ylab="",col='white',xlab="")
  legend('bottomleft',legend=c('Initial',"Asymptotic","Dynamic"),
         col=c('grey','black','black'), lty=c(1,1,2),text.col=c('grey','black','black'),
         title="SSB0 ref pts",title.col='black',bty='n',cex=1)
  matplot(hy,pmat4,type='l',lty=c(1,1,2),col=c('grey','black','black'),ylab="Probability of exceeding 40%")

}


hist_BvsSP_plot<-function(OBJs){

  MSEhist<-OBJs$MSEhist
  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))

  nyh<-MSEhist@OM@nyears
  hy<-CurrentYr - (nyh:1)-1
  B<-apply(MSEhist@TSdata$Biomass,1:2,sum)
  catch<-apply(MSEhist@TSdata$Landings,1:2,sum)
  ind1<-1:(nyh-1)
  ind2<-2:nyh

  SP<-B[,ind2]-B[,ind1]+catch[,ind1]

  tsplot(SP,xlab="Year",ylab="Surplus production",zeroyint=F)
  tsplot(SP/B[,ind1],xlab="Year",ylab="Surplus production / Biomass",zeroyint=F)

  medSP<-apply(SP,2,quantile,p=0.5)
  medB<-apply(B[,ind1],2,quantile,p=0.5)
  plot(medB,medSP,type='l',xlim=quantile(B[,ind2],p=c(0.05,0.95)),ylim=quantile(SP,p=c(0.05,0.95)),xlab="Biomass",ylab="Surplus production")

  matplot(B[,ind2],SP,col="#99999920",add=T,lty=1,pch=19,cex=0.9)
  matplot(t(B[1:3,ind2]),t(SP[1:3,]),col=c("red","green","blue"),add=T,lty=1,pch=19,cex=0.7)
  matplot(t(B[1:3,ind2]),t(SP[1:3,]),col=c("red","green","blue"),add=T,type="l")
  points(medB,medSP,pch=19,cex=0.9)
  lines(medB,medSP,lwd=2)
  legend('topright',legend=c("Median","Sim 1","Sim 2","Sim 3","All sims"),text.col=c("black","red","green","blue","dark grey"),bty="n")

}
