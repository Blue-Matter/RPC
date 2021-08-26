


#' @export
hist_bio<-function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }

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

#' @export
hist_future_recruit <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }

  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1
  par(mfrow=c(2,2),mai=c(0.9,0.9,0.2,0.1),omi=c(0,0,0,0))

  yrs_rec_dev <- MSEhist@OM@CurrentYr - (MSEhist@OM@nyears+MSEhist@OM@maxage):1 + 1
  tsplot(x=log(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage)]), yrs_rec_dev,
         xlab="Historical Year", ylab="Log-recruitment deviation", zeroyint=F)
  abline(h = 0, lty = 3)

  yrs_proj <- MSEhist@OM@CurrentYr + 1:MSEhist@OM@proyears
  tsplot(x=log(MSEhist@TSdata$RecDev[,-c(1:(MSEhist@OM@nyears+MSEhist@OM@maxage))]), yrs_proj,
         xlab="Projection Year", ylab="Log-recruitment deviation", zeroyint=F)
  abline(h = 0, lty = 3)

  hist_mean <- apply(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage)], 2, mean)
  plot(yrs_rec_dev, hist_mean,
       xlab = "Historical Year", ylab = "Annual mean deviation\n(normal space)", typ = "o", pch = 16,
       ylim = c(0, 1.1 * max(hist_mean)))
  abline(h = 0, col = "grey")
  abline(h = 1, lty = 3)

  pro_mean <- apply(MSEhist@TSdata$RecDev[,-c(1:(MSEhist@OM@nyears+MSEhist@OM@maxage))], 2, mean)
  plot(yrs_proj, pro_mean,
       xlab = "Projection Year", ylab = "Annual mean deviation\n(normal space)", typ = "o", pch = 16,
       yli = c(0, 1.1 * max(pro_mean)))
  abline(h = 0, col = "grey")
  abline(h = 1, lty = 3)
}


#' @export
hist_bio_schedule <- function(x, var = "Len_age", n_age_plot, yr_plot, sim) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }
  labs <- c(Len_age = "Mean Length at age", Wt_age = "Weight at age",
            Mat_age = "Maturity", M_ageArray = "Natural mortality")
  ylab <- labs[match(var, names(labs))]

  OM <- MSEhist@OM
  sched <- getElement(MSEhist@SampPars$Stock, var)

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
  abline(v = MSEhist@OM@CurrentYr, lty = 3)
  title(paste0("Simulation #", sim))

  tsplot(sched[, , yr_plot], age, xlab = "Age", ylab = ylab, ymax = 1.1 * max(sched[, , yr_plot]))
  title(paste("Year", yr_cal[yr_plot]))

  invisible()
}


#' @export
hist_growth_I <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }
  plot('Growth', MSEhist, plot.num=1)
}

#' @export
hist_growth_II <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }
  plot('Growth', MSEhist, plot.num=2)
}

#' @export
hist_spatial <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }
  plot('Spatial', MSEhist)
}

#' @export
hist_sel <- function(x, yr, maturity = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }
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


#' @export
hist_YieldCurve <- function(x, yr_bio, yr_sel, F_range) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- OBJs$MSEhist
  } else {
    MSEhist <- x
  }

  #YC_type <- match.arg(YC_type, choices = c(1, 2))
  YC_type <- 1

  if(missing(yr_bio)) {
    yr_bio <- MSEhist@OM@nyears
  } else {
    yr_bio <- max(1, yr_bio - MSEhist@OM@CurrentYr + MSEhist@OM@nyears)
  }
  if(missing(yr_sel)) {
    yr_sel <- MSEhist@OM@nyears
  } else {
    yr_sel <- max(1, yr_sel - MSEhist@OM@CurrentYr + MSEhist@OM@nyears)
  }

  StockPars <- MSEhist@SampPars$Stock
  FleetPars <- MSEhist@SampPars$Fleet
  M <- StockPars$M_ageArray[, , yr_bio]
  Mat_age <- StockPars$Mat_age[, , yr_bio]
  Wt_age <- StockPars$Wt_age[, , yr_bio]
  Fec_age <- StockPars$Fec_Age[, , yr_bio]
  V <- FleetPars$V[, , yr_sel]

  if(missing(F_range)) F_range <- c(1e-8, 3 * max(M))
  F_search <- seq(min(F_range), max(F_range), length.out = 50)

  if(YC_type == 1) {  # Constant R0/h
    YC <- lapply(1:MSEhist@OM@nsim, function(x) {
      vapply(log(F_search), function(y) {
        MSEtool:::MSYCalcs(y, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
                           Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                           V_at_Age = V[x, ], maxage = StockPars$maxage,
                           R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
                           opt = 2, plusgroup = StockPars$plusgroup)
      }, numeric(11))
    })
  } else { # Constant alpha, beta
    YC <- lapply(1:MSEhist@OM@nsim, function(x) {
      vapply(log(F_search), function(y) {
        RPC:::MSYCalcs2(y, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
                        Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                        V_at_Age = V[x, ], maxage = StockPars$maxage,
                        R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
                        opt = 2, plusgroup = StockPars$plusgroup, SSBpR0 = StockPars$SSBpR[x, 1])
      }, numeric(11))
    })
  }

  SPR_F <- vapply(1:MSEhist@OM@nsim, function(x) {
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

