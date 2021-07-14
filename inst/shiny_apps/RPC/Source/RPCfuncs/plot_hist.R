
# MSEhist<-readRDS("C:/temp/MSEhist.rda")



plotquant<-function(x,p=c(0.05,0.25,0.75,0.95), yrs, cols=list(colm="dark blue", col50='light blue', col90='#60859925'), addline=T, ablines=NA){

  x[x==Inf]<-NA
  qs <- apply(x, 2, quantile, p = p[c(1,4)], na.rm = TRUE, type = 3)
  qsi <- apply(x, 2, quantile, p = p[2:3], na.rm = TRUE, type = 3)

  if(is.matrix(yrs)) {
    ny <- ncol(yrs)

    qs_yr <- apply(yrs, 2, quantile, p = p[c(1,4)], na.rm = TRUE, type = 3)
    qsi_yr <- apply(yrs, 2, quantile, p = p[2:3], na.rm = TRUE, type = 3)

    polygon(c(qs_yr[1, ], qs_yr[2, ny:1]), c(qs[1,], qs[2,ny:1]), border = NA, col = cols$col90)
    polygon(c(qsi_yr[1, ], qsi_yr[2, ny:1]), c(qsi[1,], qsi[2,ny:1]),border = NA,col = cols$col50)

    if(!is.na(ablines[1])) abline(h = ablines, col = '#99999980')

    if(addline) for(i in 1:2)lines(yrs[i, ],x[i,],col='black',lty=i)
    lines(apply(yrs, 2, median, na.rm = TRUE), apply(x, 2, median, na.rm = TRUE), lwd = 2, col = cols$colm)

  } else {
    ny<-length(yrs)

    polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col=cols$col90)
    polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=cols$col50)

    if(!is.na(ablines[1])) abline(h = ablines, col = '#99999980')

    if(addline) for(i in 1:2) lines(yrs, x[i, ], col = 'black', lty = i)
    lines(yrs, apply(x, 2, median, na.rm = TRUE), lwd = 2, col = cols$colm)
  }

}

tsplot<-function(x,yrs,xlab="",ylab="",zeroyint=TRUE,cols=list(colm="dark blue", col50='light blue', col90='#60859925'),
                 ymax = NULL){

  ymin <- ifelse(zeroyint, 0, quantile(x, 0.01))
  if(is.null(ymax)) ymax <- quantile(x, 0.99)
  plot(range(yrs), c(ymin, ymax), typ = "n",xlab=xlab,ylab=ylab,yaxs='i')
  abline(h=pretty(seq(from=ymin,to=max(x)*1.25,length.out=20)),col="light grey")
  plotquant(x,yrs=yrs,cols=cols)

}

hist_bio<-function(OBJs){

  MSEhist<-OBJs$MSEhist
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  tsplot(x=apply(MSEhist@TSdata$SBiomass,1:2,sum), yrs, xlab="Historical Year", ylab="Spawning biomass")
  tsplot(apply(MSEhist@TSdata$Biomass,1:2,sum), yrs, xlab="Historical Year", ylab="Biomass")
  tsplot(apply(MSEhist@TSdata$Number,1:2,sum), yrs, xlab="Historical Year", ylab="Numbers")
  tsplot(apply(MSEhist@TSdata$VBiomass,1:2,sum), yrs, xlab="Historical Year", ylab="Vulnerable Biomass")

  yrs_rec_dev <- MSEhist@OM@CurrentYr - (MSEhist@OM@nyears+MSEhist@OM@maxage):1 + 1
  tsplot(x=log(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage)]), yrs_rec_dev,
         xlab="Historical Year", ylab="Recruitment strength", zeroyint=F)
  abline(h = 0, lty = 3)
  tsplot(apply(MSEhist@AtAge$Number[,1,,],1:2,sum), yrs, xlab="Historical Year", ylab="Recruitment")

}

# testOM@nsim<-24; MSEhist <- runMSE(testOM,Hist=T)
hist_growth_I<-function(OBJs)  plot('Growth', OBJs$MSEhist, plot.num=1)
hist_growth_II<-function(OBJs)  plot('Growth', OBJs$MSEhist, plot.num=2)
hist_growth_III<-function(OBJs)  plot('Growth', OBJs$MSEhist, plot.num=3)
hist_maturity<-function(OBJs)  plot('Maturity', OBJs$MSEhist)
hist_survival<-function(OBJs)  plot('M', OBJs$MSEhist)
hist_spatial<-function(OBJs)  plot('Spatial', OBJs$MSEhist)
hist_sel<-function(OBJs)  plot('Selectivity', OBJs$MSEhist)


hist_exp <-function(OBJs, yr_FMSY) {
  MSEhist<-OBJs$MSEhist
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

  # Total removals
  if(sum(MSEhist@TSdata$Discards)) {
    tsplot(apply(MSEhist@TSdata$Landings,1:2,sum), yrs, xlab="Year", ylab="Landings", cols=cols)
    tsplot(apply(MSEhist@TSdata$Discards,1:2,sum), yrs, xlab="Year", ylab="Discards", cols=cols)
  } else {
    tsplot(apply(MSEhist@TSdata$Removals,1:2,sum), yrs, xlab="Year", ylab="Removals", cols=cols)
  }

  # Apical F index
  Find <-  MSEhist@SampPars$Fleet$qs * MSEhist@TSdata$Find
  tsplot(Find, yrs, xlab = "Year", ylab = "Apical F", cols=cols)

  # Year specific FMSY
  FMSY <- MSEhist@Ref$ByYear$FMSY[, 1:MSEhist@OM@nyears]
  tsplot(FMSY, yrs, xlab = "Year", ylab = expression(Year-specific~F[MSY]), cols=cols)

  # F/FMSY
  tsplot(Find/FMSY, yrs, xlab = "Year", ylab = expression(F/F[MSY]), cols=cols)
}



