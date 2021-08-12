
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


hist_bio_schedule <- function(OBJs, var = "Len_age", n_age_plot, yr_plot, sim) {

  labs <- c(Len_age = "Mean Length at age", Wt_age = "Weight at age",
            Mat_age = "Maturity", M_ageArray = "Natural mortality")
  ylab <- labs[match(var, names(labs))]
  Hist <- OBJs$MSEhist
  OM <- Hist@OM
  sched <- getElement(Hist@SampPars$Stock, var)

  yr_cal <- 1:(OM@nyears + OM@proyears) - OM@nyears + OM@CurrentYr

  if(missing(yr_plot)) {
    yr_plot <- OM@nyears
  } else {
    yr_plot <- max(1, yr_plot - OM@CurrentYr + OM@nyears)
  }
  if(missing(n_age_plot)) {
    n_age_plot <- OM@maxage + 1
  } else {
    n_age_plot <- max(n_age_plot, 2)
  }
  if(missing(sim)) {
    sim <- 1
  } else {
    sim <- max(sim, 1)
  }

  age <- 1:dim(sched)[2] - 1
  age_plot <- pretty(age, n_age_plot)
  age_plot <- age_plot[age_plot <= max(age)]

  par(mfrow = c(1, 2), mai = c(0.9, 0.9, 0.2, 0.1), omi = c(0, 0, 0, 0))

  matplot(yr_cal, t(sched[sim, age_plot + 1, ]), xlab = "Year", ylab = ylab, typ = 'l', lty = 1,
          xlim = c(min(yr_cal), max(yr_cal) + 0.1 * length(yr_cal)))
  text(max(yr_cal), sched[sim, age_plot + 1, length(yr_cal)], labels = age_plot, col = 1:6, pos = 4)
  abline(v = Hist@OM@CurrentYr, lty = 3)
  title(paste0("Simulation #", sim))

  tsplot(sched[, , yr_plot], age, xlab = "Age", ylab = ylab, ymax = 1.1 * max(sched[, , yr_plot]))
  title(paste("Year", yr_cal[yr_plot]))

  invisible()
}


hist_growth_I<-function(OBJs)  plot('Growth', OBJs$MSEhist, plot.num=1)
hist_growth_II<-function(OBJs)  plot('Growth', OBJs$MSEhist, plot.num=2)
hist_spatial<-function(OBJs)  plot('Spatial', OBJs$MSEhist)

hist_sel <- function(OBJs, yr, maturity = TRUE) {
  MSEhist <- OBJs$MSEhist
  yind <- yr - MSEhist@OM@CurrentYr + MSEhist@OM@nyears # Length 2 vector

  #par(mfcol=c(3,2),mai=c(0.3,0.6,0.3,0.1),omi=c(0.5,0,0,0))
  par(mai=c(0.3,0.6,0.3,0.1),omi=c(0.5,0,0.2,0))
  layout(matrix(c(1:6, rep(7, 3)), nrow = 3), widths = c(1, 1, 0.5))
  cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

  for(y in 1:length(yind)) {
    # Selectivity
    tsplot(MSEhist@SampPars$Fleet$V[, , yind[y]], yrs=0:MSEhist@OM@maxage,
           xlab="",ylab=paste0("Vulnerability", ifelse(maturity, " with maturity", "")),
           cols = cols, zeroyint=F, ymax = 1.1)
    mtext(paste("Year", yr[y]), 3, line = 1, font = 2)
    if(maturity) {
      plotquant(MSEhist@SampPars$Stock$Mat_age[, , yind[y]], yrs=0:MSEhist@OM@maxage, addline = FALSE)
    }

    # Retention
    tsplot(MSEhist@SampPars$Fleet$retA_real[, , yind[y]], yrs=0:MSEhist@OM@maxage,
           xlab="",ylab="Retention",cols = cols, zeroyint=F, ymax = 1.1)

    # Realized Selectivity
    tsplot(MSEhist@SampPars$Fleet$V_real[, , yind[y]], yrs=0:MSEhist@OM@maxage,
           xlab="",ylab="Realized Selectivity",cols = cols, zeroyint=F, ymax = 1.1)

  }
  plot(1, 1, axes = FALSE, typ = "n", ylab = "", xlab = "")
  legend("left", c("Selectivity", "Maturity"), col = c("darkgreen", "darkblue"), lwd = 3, cex = 1.5, bty = "n")
  mtext("Age", 1, outer = TRUE, line = 2)
}



hist_YieldCurve <- function(OBJs, YC_type = 1, yr_bio, yr_sel, F_range) {
  YC_type <- match.arg(YC_type, choices = c(1, 2))

  Hist <- OBJs$MSEhist
  StockPars <- Hist@SampPars$Stock
  FleetPars <- Hist@SampPars$Fleet

  if(missing(yr_bio)) {
    yr_bio <- Hist@OM@nyears
  } else {
    yr_bio <- max(1, yr_bio - Hist@OM@CurrentYr + Hist@OM@nyears)
  }
  if(missing(yr_sel)) {
    yr_sel <- Hist@OM@nyears
  } else {
    yr_sel <- max(1, yr_sel - Hist@OM@CurrentYr + Hist@OM@nyears)
  }

  M <- StockPars$M_ageArray[, , yr_bio]
  Mat_age <- StockPars$Mat_age[, , yr_bio]
  Wt_age <- StockPars$Wt_age[, , yr_bio]
  Fec_age <- StockPars$Fec_Age[, , yr_bio]
  V <- FleetPars$V[, , yr_sel]

  if(missing(F_range)) F_range <- c(1e-8, 3 * max(M))
  F_search <- seq(min(F_range), max(F_range), length.out = 50)

  if(YC_type == 1) {  # Constant R0/h
    YC <- lapply(1:Hist@OM@nsim, function(x) {
      vapply(log(F_search), function(y) {
        MSEtool:::MSYCalcs(y, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
                           Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                           V_at_Age = V[x, ], maxage = StockPars$maxage,
                           R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
                           opt = 2, plusgroup = StockPars$plusgroup)
      }, numeric(11))
    })
  } else { # Constant alpha, beta
    YC <- lapply(1:Hist@OM@nsim, function(x) {
      vapply(log(F_search), function(y) {
        RPC:::MSYCalcs2(y, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
                        Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                        V_at_Age = V[x, ], maxage = StockPars$maxage,
                        R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
                        opt = 2, plusgroup = StockPars$plusgroup, SSBpR0 = StockPars$SSBpR[x, 1])
      }, numeric(11))
    })
  }

  SPR_F <- vapply(1:Hist@OM@nsim, function(x) {
    vapply(F_search, function(y) {
      MSEtool:::Ref_int_cpp(y, M_at_Age = M[x, ],
                            Wt_at_Age = Wt_age[x, ], Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                            V_at_Age = V[x, ], StockPars$SRrel[x], maxage = StockPars$maxage,
                            plusgroup = StockPars$plusgroup)[2, ]
    }, numeric(1))
  }, numeric(length(F_search)))

  Yield <- sapply(YC, function(x) x[1, ])
  SSB <- sapply(YC, function(x) x[3, ])
  SSB_SSB0a <- sapply(YC, function(x) x[4, ])

  par(mfrow = c(2, 2), mai = c(0.9, 0.9, 0.2, 0.1), omi = c(0, 0, 0, 0))
  cols <- list(colm="darkgreen",col50='lightgreen',col90='#40804025')

  tsplot(t(Yield),yrs=F_search,xlab="Fishing mortality",ylab="Yield",cols = cols, zeroyint=F, ymax = 1.1 * max(Yield))
  tsplot(t(Yield),yrs=t(SPR_F),xlab="Spawning potential ratio (SPR)",ylab="Yield",cols = cols, zeroyint=F, ymax = 1.1 * max(Yield))
  tsplot(t(Yield),yrs=t(SSB),xlab="Spawning biomass (SSB)",ylab="Yield",cols = cols, zeroyint=F, ymax = 1.1 * max(Yield))
  tsplot(t(Yield),yrs=t(SSB_SSB0a),xlab=expression(SSB~"/"~"Asymptotic"~SSB[0]),ylab="Yield",cols = cols, zeroyint=F, ymax = 1.1 * max(Yield))

}

