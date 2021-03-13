
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



