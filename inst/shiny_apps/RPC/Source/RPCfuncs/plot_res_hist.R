hist_SSB <- function(OBJs, figure = TRUE, SSB_y = NA, prob_ratio = NA, prob_ylim = c(0, 1)) {
  MSEh<-OBJs$MSEhist
  nyh<-MSEh@OM@nyears

  hy<-MSEh@OM@CurrentYr - (nyh:1) + 1
  yind <- SSB_y - MSEh@OM@CurrentYr + MSEh@OM@nyears

  SSB<-apply(MSEh@TSdata$SBiomass,1:2,sum)

  if(figure) {
    if(is.na(prob_ratio)) {
      tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
    } else {
      pvec <- apply(SSB > prob_ratio * SSB[, yind], 2, mean)

      plot(hy, pvec, type = 'o', ylim = prob_ylim, col = "black", lwd = 1.75,
           xlab = "Year", ylab = parse(text = paste0("Probability~SSB/SSB[", SSB_y, "]>", prob_ratio)))
    }

  } else {
    if(is.na(prob_ratio)) {
      make_df(SSB, hy)
    } else {
      pvec <- apply(SSB > prob_ratio * SSB[, yind], 2, mean)
      structure(matrix(pvec, ncol = 1),
                dimnames = list(hy, c("Probability")))
    }
  }
}


hist_SSBMSY <- function(OBJs, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1.5)) {

  MSEhist<-OBJs$MSEhist
  nyh<-MSEhist@OM@nyears

  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1

  SSB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)

  # Year specific SSBMSY (constant R0, h)
  SSBMSY <- MSEhist@Ref$ByYear$SSBMSY[, 1:MSEhist@OM@nyears]

  # Year specific FMSY (constant alpha, beta)
  #if(!all(apply(SSBMSY, 1, function(x) all(!diff(x))))) {
  #  StockPars <- MSEhist@SampPars$Stock
  #  FleetPars <- MSEhist@SampPars$Fleet
  #  SSBMSY2 <- vapply(1:MSEhist@OM@nsim, function(x) {
  #    vapply(1:MSEhist@OM@nyears, function(y) {
  #      logFMSY <- optimize(RPC:::MSYCalcs2, log(c(1e-4, 3)), M_at_Age = StockPars$M_ageArray[x, , y],
  #                          Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
  #                          Fec_at_Age = StockPars$Fec_Age[x, , y], V_at_Age = FleetPars$V[x, , y], maxage = StockPars$maxage,
  #                          R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
  #                          opt = 1, plusgroup = StockPars$plusgroup, SSBpR0 = StockPars$SSBpR[x, 1])$minimum
  #      RPC:::MSYCalcs2(logFMSY, M_at_Age = StockPars$M_ageArray[x, , y],
  #                      Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
  #                      Fec_at_Age = StockPars$Fec_Age[x, , y], V_at_Age = FleetPars$V[x, , y], maxage = StockPars$maxage,
  #                      R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
  #                      opt = 2, plusgroup = StockPars$plusgroup, SSBpR0 = StockPars$SSBpR[x, 1])["B"]
  #    }, numeric(1))
  #  }, numeric(MSEhist@OM@nyears)) %>% t()
  #}

  if(figure) {
    if(is.na(prob_ratio)) {
      par(mfrow=c(2,2),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))

      tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
      tsplot(x=SSBMSY,yrs=hy,xlab="Year",ylab=expression(SSB[MSY]))
      tsplot(x=SSB/SSBMSY,yrs=hy,xlab="Year",ylab=expression(SSB/SSB[MSY]))
      #if(!exists("SSBMSY2", inherits = FALSE)) {
      #  tsplot(x=SSBMSY,yrs=hy,xlab="Year",ylab=expression(SSB[MSY]))
      #  tsplot(x=SSB/SSBMSY,yrs=hy,xlab="Year",ylab=expression(SSB/SSB[MSY]))
      #} else {
      #  tsplot(SSBMSY, hy, xlab = "Year", ylab = expression(SSB[MSY]~"(constant"~R[0]~h~")"))
      #  tsplot(SSB/SSBMSY, hy, xlab = "Year", ylab = expression(SSB/SSB[MSY]~"(constant"~R[0]~h~")"))
      #  tsplot(SSBMSY, hy, xlab = "Year", ylab = expression(SSB[MSY]~"(constant"~alpha~beta~")"))
      #  tsplot(SSB/SSBMSY, hy, xlab = "Year", ylab = expression(SSB/SSB[MSY]~"(constant"~alpha~beta~")"))
      #}
    } else {
      pvec <- apply(SSB/SSBMSY > prob_ratio, 2, mean)

      plot(hy, pvec, type = 'o', ylim = prob_ylim, col = "black", lwd = 1.75,
           xlab = "Year", ylab = parse(text = paste0("Probability~SSB/SSB[MSY]>", prob_ratio)))
    }

  } else {
    if(is.na(prob_ratio)) {
      make_df(SSB/SSBMSY, hy)
    } else {
      pvec <- apply(SSB/SSBMSY > prob_ratio, 2, mean)
      structure(matrix(pvec, ncol = 1), dimnames = list(hy, c("Probability")))
    }
  }
}


hist_SSB0 <- function(OBJs, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {

  MSEh<-OBJs$MSEhist
  nyh<-MSEh@OM@nyears

  hy<-MSEh@OM@CurrentYr - (nyh:1) + 1

  SSB<-apply(MSEh@TSdata$SBiomass,1:2,sum)

  SSB0h<-array(SSB[,1],dim(SSB))
  SSB0a<-MSEh@Ref$ByYear$SSB0
  SSB0d<-MSEh@Ref$Dynamic_Unfished$SSB0

  SSBrh<-SSB/SSB0h
  SSBra<-SSB/SSB0a[,1:nyh]
  SSBrd<-SSB/SSB0d[,1:nyh]

  if(figure) {
    if(is.na(prob_ratio)) {
      par(mfcol=c(2,2),mai=c(0.3,0.8,0.2,0.1),omi=c(0.6,0,0,0))
      tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
      tsplot(x=SSBrh,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Initial~SSB[0]))
      tsplot(x=SSBra,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Asymptotic~SSB[0]))
      tsplot(x=SSBrd,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Dynamic~SSB[0]))
    } else {
      pmat <- sapply(list(SSBrh, SSBra, SSBrd), function(x) apply(x > prob_ratio, 2, mean))

      matplot(hy,pmat,type='o', lty = 1:3, ylim = prob_ylim, col = "black", lwd = 1.75,
              pch = c(16, 1, 4),
              xlab = "Year", ylab = parse(text = paste0("Probability~SSB/SSB[0]>", prob_ratio)))

      legend('bottomleft',legend=c("Initial", "Asymptotic", "Dynamic"), lwd = 1.75,
             pch = c(16, 1, 4), lty = 1:3, title = expression(SSB[0]~Type), bty='n',cex=1)
    }

  } else {
    sapply(list(SSBrh, SSBra, SSBrd), function(x) apply(x > prob_ratio, 2, mean)) %>%
      structure(dimnames = list(hy, c("Initial", "Asymptotic", "Dynamic") %>% paste("SSB0")))
  }
}


hist_BvsSP<-function(OBJs, figure = TRUE){

  MSEhist<-OBJs$MSEhist

  nyh<-MSEhist@OM@nyears
  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1
  B<-apply(MSEhist@TSdata$Biomass,1:2,sum)
  catch<-apply(MSEhist@TSdata$Removals,1:2,sum)
  ind1<-1:(nyh-1)
  ind2<-2:nyh

  SP<-B[, ind2]-B[, ind1]+catch[, ind1]
  medSP<-apply(SP, 2, median)
  medB<-apply(B[, ind1], 2, median)
  medSPB<-apply(SP/B[, ind1], 2, median)

  if(figure) {
    par(mfcol=c(2,2),mai=c(0.9,0.6,0.2,0.1),omi=c(0,0,0,0))

    tsplot(SP,yrs=hy[ind1],xlab="Year",ylab="Surplus production",zeroyint=F)
    abline(h = 0, lty = 3)
    tsplot(SP/B[,ind1],yrs=hy[ind1],xlab="Year",ylab="Surplus production / Biomass",zeroyint=F)
    abline(h = 0, lty = 3)

    plot(medB,medSP,type='l',xlim=quantile(B[,ind2],p=c(0.05,0.95)),ylim=quantile(SP,p=c(0.05,0.95)),xlab="Biomass",ylab="Surplus production")

    matplot(B[,ind2],SP,col="#99999920",add=T,lty=1,pch=19,cex=0.9)
    #matplot(t(B[1:3,ind2]),t(SP[1:3,]),col=c("red","green","blue"),add=T,lty=1,pch=19,cex=0.7)
    #matplot(t(B[1:3,ind2]),t(SP[1:3,]),col=c("red","green","blue"),add=T,type="l")
    points(medB,medSP,pch=19,cex=0.9)
    lines(medB,medSP,lwd=2)
    abline(h = 0, lty = 3)
    legend('topright',legend=c("Median","All sims"),text.col=c("black","dark grey"),bty="n")
    #legend('topright',legend=c("Median","Sim 1","Sim 2","Sim 3","All sims"),text.col=c("black","red","green","blue","dark grey"),bty="n")

    plot(medB,medSPB,type='l',xlim=quantile(B[,ind2],p=c(0.05,0.95)),ylim=quantile(SP/B[, ind1],p=c(0.05,0.95)),
         xlab="Biomass",ylab="Surplus production / Biomass")
    matplot(B[,ind2],SP/B[,ind1],col="#99999920",add=T,lty=1,pch=19,cex=0.9)
    points(medB,medSPB,pch=19,cex=0.9)
    lines(medB,medSPB,lwd=2)
    abline(h = 0, lty = 3)
    legend('topright',legend=c("Median","All sims"),text.col=c("black","dark grey"),bty="n")
    #legend('topright',legend=c("Median","Sim 1","Sim 2","Sim 3","All sims"),text.col=c("black","red","green","blue","dark grey"),bty="n")

  } else {
    out <- make_df(SP, hy[-length(hy)])
    return(out)
  }
}


hist_R <- function(OBJs, figure = TRUE, SR_only = FALSE, SR_xlim, SR_ylim, SR_y_RPS0, SR_include) {

  Hist <- OBJs$MSEhist
  out <- stock_recruit_int(Hist)
  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)

  S_IQR <- apply(out$SSB, 2, quantile, probs = c(0.05, 0.95))
  R_IQR <- apply(out$R, 2, quantile, probs = c(0.05, 0.95))

  Rdev <- log(out$R/out$predR_y)

  if(figure) {
    if(SR_only) {
      # Plot stock-recruit relationship
      if(missing(SR_xlim)) SR_xlim <- c(0, max(out$SSB))
      if(missing(SR_ylim)) SR_ylim <- c(0, max(out$R))
      if(missing(SR_y_RPS0)) {
        SR_y_RPS0 <- Hist@OM@nyears
      } else if(SR_y_RPS0 > Hist@OM@nyears) { # convert calendar year to matrix column
        SR_y_RPS0 <- max(1, SR_y_RPS0 - Hist@OM@CurrentYr + Hist@OM@nyears)
      }

      matplot(out$SSB, out$R, typ = "n", xlim = SR_xlim, ylim = SR_ylim,
              xlab = "Spawning biomass", ylab = "Recruitment")
      leg <- leg.text.col <- leg.pch <- leg.lty <- leg.lwd <- NULL

      if(any(SR_include == 2)) { # Plot SR curve
        plotquant(out$predR, yrs = out$predSSB, addline = TRUE)
      }
      if(any(SR_include == 1)) { # Plot individual S-R pairs
        matpoints(out$SSB, out$R, pch = 16, col = "#99999920")
        points(medSSB, medR, pch = 19)

        leg <- c(leg, "Median", "All sims")
        leg.text.col <- c(leg.text.col, "black", "dark grey")
        leg.pch <- c(leg.pch, 16, 16)
        leg.lty <- c(leg.lty, NA, NA)
        leg.lwd <- c(NA, NA)
      }
      abline(h = 0, col = "grey")

      if(any(SR_include == 3)) { # Plot recruits per spawner lines
        StockPars <- Hist@SampPars$Stock
        FleetPars <- Hist@SampPars$Fleet

        RpS_crash <- median(1/Hist@Ref$ByYear$SPRcrash[, 1]/StockPars$SSBpR[, 1])

        RpS_0 <- vapply(1:Hist@OM@nsim, function(x, y) {
          MSEtool:::Ref_int_cpp(1e-8, M_at_Age = StockPars$M_ageArray[x, , y],
                                Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
                                Fec_at_Age = StockPars$Fec_Age[x, , y],
                                V_at_Age = Hist@SampPars$Fleet$V[x, , y],
                                StockPars$SRrel[x], maxage = StockPars$maxage,
                                plusgroup = StockPars$plusgroup)[3, ]
        }, numeric(1), y = SR_y_RPS0) %>% median()

        RpS_med <- apply(out$R/out$SSB, 1, median) %>% median()

        abline(a = 0, b = RpS_0, lty = 2, lwd = 2, col = "blue")
        abline(a = 0, b = RpS_med, lty = 2, lwd = 2)
        abline(a = 0, b = RpS_crash, lty = 2, lwd = 2, col = "red")

        leg <- c(leg, paste0("Unfished (", SR_y_RPS0 + Hist@OM@CurrentYr - Hist@OM@nyears, ") R/S"),
                 "Median hist. R/S", "Maximum R/S")
        leg.text.col <- c(leg.text.col, "blue", "black", "red")
        leg.pch <- c(leg.pch, NA, NA, NA)
        leg.lty <- c(leg.lty, 2, 2, 2)
        leg.lwd <- c(leg.lty, 2, 2, 2)
      }

      if(!is.null(leg)) {
        legend("topright", legend = leg, #text.col = leg.text.col,
               col = leg.text.col, pch = leg.pch, lty = leg.lty, lwd = leg.lwd, bty = "n")
      }
    } else {

      par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))

      # Plot stock-recruit relationship
      #matplot(out$SSB, out$R, typ = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
      #        xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
      #plotquant(out$predR, yrs = out$predSSB, addline = TRUE)
      #points(medSSB, medR, pch = 19)
      #legend("topright", c("Median", "All sims"), text.col = c("black", "dark grey"), bty = "n")
      #abline(h = 0, col = "grey")

      # log-recruitment deviation
      tsplot(Rdev,yrs=out$yrs,xlab="Year",ylab="Log recruitment deviation",zeroyint=FALSE)
      abline(h = 0, lty = 3)

      ## Recruitment deviations vs SSB
      matplot(out$SSB, Rdev, typ = "p", xlim = c(0, max(out$SSB)), #ylim = c(0, max(R)),
              col = "#99999920", pch = 19,
              xlab = "Spawning biomass", ylab = "Log recruitment deviation")
      plotquant(Rdev, yrs = medSSB, addline = TRUE)
      abline(h = 0, lty = 3)

      # Recruitment by year
      tsplot(out$R,yrs=out$yrs,xlab="Year",ylab="Recruitment",zeroyint=TRUE)
      abline(h = 0)
    }

  } else {

    out <- make_df(out$R, out$yrs)
    return(out)
  }

  invisible()
}


hist_RpS <- function(OBJs, figure = TRUE) {
  Hist <- OBJs$MSEhist
  out <- stock_recruit_int(Hist)

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)
  medRpS <- apply(out$R/out$SSB, 2, median)

  if(figure) {
    par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
    # Annual recruits-per-spawner
    tsplot(out$R/out$SSB,yrs=out$yrs,xlab="Year",ylab="Recruits per spawner",zeroyint=TRUE)

    # Plot recruits-per-spawner vs SSB
    plot(out$SSB, out$R/out$SSB, xlim = c(0, 1.1 * max(medSSB)), col="#99999920", pch = 19,
         ylim = c(0, 1.1 * max(medRpS)), xlab = "Spawning biomass", ylab = "Recruits per spawner")
    plotquant(t(t(out$predR)/out$predSSB), yrs = out$predSSB)
    points(medSSB, medRpS, pch = 16)
    legend("topright", c("Median", "All sims"), text.col = c("black", "dark grey"), bty = "n")
    abline(h = 0, col = "grey")

    # Annual log recruits-per-spawner
    tsplot(log(out$R/out$SSB),yrs=out$yrs,xlab="Year",ylab="Log recruits per spawner",zeroyint=FALSE)

    # Plot log recruits-per-spawner vs SSB
    plot(out$SSB, log(out$R/out$SSB), xlim = c(0, 1.1 * max(medSSB)), col="#99999920", pch = 19,
         xlab = "Spawning biomass", ylab = "Log recruits per spawner")
    plotquant(log(t(t(out$predR)/out$predSSB)), yrs = out$predSSB)
    points(medSSB, log(medRpS), pch = 16)
  } else {
    out <- make_df(out$R/out$SSB, out$yrs)
    return(out)
  }

}


hist_Rmax <- function(OBJs, figure = TRUE) {
  Hist <- OBJs$MSEhist
  out <- stock_recruit_int(Hist)

  if(Hist@OM@SRrel == 1) { # Calculate 50% maximum recruitment from the S-R function and corresponding SSB (S50)
    Rmax <-  4 * Hist@SampPars$Stock$R0 * Hist@SampPars$Stock$hs / (5 * Hist@SampPars$Stock$hs - 1)
    Rmax50 <- 0.5 * Rmax
    S50 <- (5 * Hist@SampPars$Stock$hs - 1) / (1 - Hist@SampPars$Stock$hs) /
      (Hist@SampPars$Stock$SSBpR[, 1] * Hist@SampPars$Stock$R0)
    S50 <- 1/S50  # Myers et al. 1994
  } else {
    Rmax <- apply(out$predR, 1, max)
    Rmax50 <- 0.5 * Rmax
    S50 <- 0.231961/Hist@SampPars$Stock$bR[, 1] # Myers et al. 1994
  }

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)

  # Regression
  reg_low <- lapply(1:Hist@OM@nsim, function(i) {
    df <- data.frame(R = out$R[i, ], SSB = out$SSB[i, ]) %>% filter(SSB < S50[i])
    if(nrow(df) > 3) {
      reg <- lm(log(R) ~ log(SSB), data = df)
      df$predict_logR <- predict(reg)
      return(df)
    } else {
      return(NULL)
    }
  })
  reg_hi <- lapply(1:Hist@OM@nsim, function(i) {
    df <- data.frame(R = out$R[i, ], SSB = out$SSB[i, ], iter = i) %>% filter(SSB >= S50[i])
    if(nrow(df) > 3) {
      reg <- lm(log(R) ~ log(SSB), data = df)
      df$predict_logR <- predict(reg)
      return(df)
    } else {
      return(NULL)
    }
  })

  if(figure) {

    par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

    # Plot stock-recruit relationship with SSB 50%Rmax
    matplot(out$SSB, out$R, typ = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
    plotquant(out$predR, yrs = out$predSSB, addline=T)
    points(medSSB, medR, pch = 19)
    abline(v = quantile(S50, probs = c(0.25, 0.5, 0.75)), lty = c(4, 2, 4))
    abline(h = 0, col = "grey")

    # Plot regression line
    matplot(log(out$SSB), log(out$R), typ = "p", col = "#99999920", xlim = range(c(out$SSB, S50)) %>% log(),
            xlab = "log(Spawning biomass)", ylab = "log(Recruitment)", pch = 16)
    abline(v = quantile(S50, probs = c(0.25, 0.5, 0.75)) %>% log(), lty = c(4, 2, 4))
    lapply(1:Hist@OM@nsim, function(i) {
      if(!is.null(reg_hi[[i]])) lines(predict_logR ~ log(SSB), data = reg_hi[[i]], lty = 2)
      if(!is.null(reg_low[[i]])) lines(predict_logR ~ log(SSB), data = reg_low[[i]], lty = 2)
    })

  } else {

    slope_high <- vapply(1:Hist@OM@nsim, function(i) {
      if(!is.null(reg_hi[[i]])) {
        diff(range(log(reg_hi[[i]]$predict_logR)))/diff(range(log(reg_hi[[i]]$SSB)))
      } else {
        NA_real_
      }
    }, numeric(1))
    slope_low <- vapply(1:Hist@OM@nsim, function(i) {
      if(!is.null(reg_low[[i]])) {
        diff(range(log(reg_low[[i]]$predict_logR)))/diff(range(log(reg_low[[i]]$SSB)))
      } else {
        NA_real_
      }
    }, numeric(1))

    out <- lapply(list(S50, slope_high, slope_low), function(x) {
      if(all(is.na(x))) {
        return(rep(NA_real_, 3))
      } else {
        return(quantile(x, probs = c(0.25, 0.5, 0.75)))
      }
    })

    out <- data.frame(do.call(rbind, out), row.names = c("SSB_50%Rmax", "Slope above", "Slope below"))
    names(out) <- c("25%ile", "Median", "75%ile")
    return(out)
  }
  invisible()
}

hist_RpS90 <- function(OBJs, figure = TRUE) {
  Hist <- OBJs$MSEhist
  out <- stock_recruit_int(Hist)

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)
  #medRpS <- medR/medSSB

  RpS_med <- apply(out$R/out$SSB, 1, median)
  RpS_90 <- apply(out$R/out$SSB, 1, quantile, probs = 0.9)
  R_90 <- apply(out$R, 1, quantile, probs = 0.9)
  S_90 <- R_90/RpS_90

  if(figure) {

    par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

    # Plot stock-recruit relationship
    matplot(out$SSB, out$R, typ = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
    #plotquant(out$predR, yrs = out$predSSB, addline=T)
    points(medSSB, medR, pch = 19)
    abline(v = quantile(S_90, probs = c(0.25, 0.5, 0.75)), lwd = 2, lty = c(4, 2, 4))
    abline(h = 0, col = "grey")

    matplot(out$SSB, out$R, typ = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
    #plotquant(out$predR, yrs = out$predSSB, addline=T)
    points(medSSB, medR, pch = 19)
    abline(a = 0, b = quantile(RpS_90, probs = c(0.25, 0.5, 0.75)), lwd = 2, lty = c(4, 2, 4), col = "red")
    abline(h = quantile(R_90, probs = c(0.05, 0.5, 0.95)), lwd = 2, lty = c(4, 2, 4), col = "blue")
    legend("topright", paste("90%ile", c("R/S", "R", "SSB")), col = c("red", "blue", "black"), lwd = 2, lty = 4, bty = "n")
    abline(h = 0, col = "grey")

  } else {

    out <- lapply(list(RpS_90, R_90, S_90), quantile, probs = c(0.25, 0.5, 0.75))
    out <- data.frame(do.call(rbind, out), row.names = c("90%ile R/S", "90%ile Recruitment", "90%ile SSB"))
    names(out) <- c("25%ile", "Median", "75%ile")
    return(out)

  }
  invisible()
}


make_df <- function(x, Year, probs = c(0.25, 0.5, 0.75)) {
  xx <- t(apply(x, 2, quantile, probs = probs))
  structure(xx, dimnames = list(Year, c("Lower quartile", "Median", "Upper quartile")))
}

stock_recruit_int <- function(Hist) {
  yrs <- Hist@OM@CurrentYr - Hist@OM@nyears:1 + 1

  R <- predR_y <- apply(Hist@AtAge$Number[, 1, , ], 1:2, sum)
  SSB <- apply(Hist@TSdata$SBiomass, 1:2, sum)

  predSSB <- seq(0, 1.1 * max(SSB), length.out = 100)
  predR <- matrix(0, Hist@OM@nsim, length(predSSB))
  SRrel <- Hist@SampPars$Stock$SRrel[1]
  for(i in 1:Hist@OM@nsim) {
    if(SRrel == 1) {
      predR[i, ] <- 4 * Hist@SampPars$Stock$R0[i] * Hist@SampPars$Stock$hs[i] * predSSB/
        (Hist@SampPars$Stock$SSBpR[i, 1] * Hist@SampPars$Stock$R0[i] * (1 - Hist@SampPars$Stock$hs[i]) +
           (5 * Hist@SampPars$Stock$hs[i] - 1) * predSSB)
      predR_y[i, ] <- 4 * Hist@SampPars$Stock$R0[i] * Hist@SampPars$Stock$hs[i] * SSB[i, ]/
        (Hist@SampPars$Stock$SSBpR[i, 1] * Hist@SampPars$Stock$R0[i] * (1 - Hist@SampPars$Stock$hs[i]) +
           (5 * Hist@SampPars$Stock$hs[i] - 1) * SSB[i, ])
    } else {
      predR[i, ] <- Hist@SampPars$Stock$aR[i, 1] * predSSB * exp(-Hist@SampPars$Stock$bR[i, 1] * predSSB)
      predR_y[i, ] <- Hist@SampPars$Stock$aR[i, 1] * SSB[i, ] * exp(-Hist@SampPars$Stock$bR[i, 1] * SSB[i, ])
    }
  }

  list(R = R, SSB = SSB, predR_y = predR_y, predR = predR, predSSB = predSSB, yrs = yrs)
}




hist_SPR <- function(OBJs, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {

  MSEhist <- OBJs$MSEhist
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  Fmed <- MSEhist@Ref$ByYear$Fmed
  StockPars <- MSEhist@SampPars$Stock

  if(figure) {
    if(is.na(prob_ratio)) {
      par(mfcol=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
      cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

      # Equilibrium and dynamic SPR
      tsplot(MSEhist@TSdata$SPR$Equilibrium, yrs, xlab="Year", ylab="Equilibrium SPR", cols=cols, ymax = 1.05)
      tsplot(MSEhist@TSdata$SPR$Dynamic, yrs, xlab="Year", ylab="Dynamic SPR", cols=cols, ymax = 1.05)

      # SPRcrash
      SPR_crash <- MSEhist@Ref$ByYear$SPRcrash[, 1:length(yrs)]
      tsplot(SPR_crash, yrs, xlab="Year", ylab=expression(SPR[crash]), cols=cols,
             ymax = 1.1 * max(SPR_crash))

      # SPR/SPRcrash
      tsplot((1 - MSEhist@TSdata$SPR$Equilibrium)/(1 - SPR_crash), yrs, xlab="Year",
             ylab=expression((1-SPR[eq])/(1-SPR[crash])), cols=cols)
    } else {
      pvec <- apply(MSEhist@TSdata$SPR$Equilibrium > prob_ratio, 2, mean)

      plot(yrs, pvec, type = 'o', ylim = prob_ylim, col = "black", lwd = 1.75,
           xlab = "Year", ylab = parse(text = paste0("Probability~SPR[eq]>", prob_ratio)))
    }
  } else {
    if(is.na(prob_ratio)) {
      make_df(MSEhist@TSdata$SPR$Equilibrium, yrs)
    } else {
      pvec <- apply(MSEhist@TSdata$SPR$Equilibrium > prob_ratio, 2, mean)
      structure(matrix(pvec, ncol = 1),
                dimnames = list(yrs, c("Probability")))
    }
  }
}

hist_exp <- function(OBJs, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
  MSEhist<-OBJs$MSEhist
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  # Apical F index
  Find <-  MSEhist@SampPars$Fleet$qs * MSEhist@TSdata$Find

  # Year specific FMSY (constant R0, h)
  FMSY <- MSEhist@Ref$ByYear$FMSY[, 1:MSEhist@OM@nyears]

  if(figure) {
    if(is.na(prob_ratio)) {
      par(mfcol=c(2,2),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
      cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

      # Total removals
      if(sum(MSEhist@TSdata$Discards)) {
        tsplot(apply(MSEhist@TSdata$Landings,1:2,sum), yrs, xlab="Year", ylab="Landings", cols=cols)
        tsplot(apply(MSEhist@TSdata$Discards,1:2,sum), yrs, xlab="Year", ylab="Discards", cols=cols)
      } else {
        tsplot(apply(MSEhist@TSdata$Removals,1:2,sum), yrs, xlab="Year", ylab="Removals", cols=cols)
      }
      tsplot(Find, yrs, xlab = "Year", ylab = "Apical F", cols=cols)
      tsplot(FMSY, yrs, xlab = "Year", ylab = expression(F[MSY]), cols=cols)
      tsplot(Find/FMSY, yrs, xlab = "Year", ylab = expression(F/F[MSY]), cols=cols)
    } else {
      pvec <- apply(Find/FMSY > prob_ratio, 2, mean)

      plot(yrs, pvec, type = 'o', ylim = prob_ylim, col = "black", lwd = 1.75,
           xlab = "Year", ylab = parse(text = paste0("Probability~F/F[MSY]>", prob_ratio)))
    }
  } else {
    if(is.na(prob_ratio)) {
      make_df(Find, yrs)
    } else {
      pvec <- apply(Find/FMSY > prob_ratio, 2, mean)
      structure(matrix(pvec, ncol = 1),
                dimnames = list(yrs, c("Probability")))
    }
  }

  #if(all(apply(FMSY, 1, function(x) all(!diff(x))))) {
  #  tsplot(FMSY, yrs, xlab = "Year", ylab = expression(F[MSY]), cols=cols)
  #  tsplot(Find/FMSY, yrs, xlab = "Year", ylab = expression(F/F[MSY]), cols=cols)
  #} else {
  #  # Year-specific FMSY (constant R0/h)
  #  tsplot(FMSY, yrs, xlab = "Year", ylab = expression(F[MSY]~"(constant"~R[0]~h~")"), cols=cols)
#
  #  # Year specific FMSY (constant alpha, beta)
  #  StockPars <- MSEhist@SampPars$Stock
  #  FleetPars <- MSEhist@SampPars$Fleet
  #  FMSY2 <- sapply(1:MSEhist@OM@nsim, function(x) {
  #    sapply(1:MSEhist@OM@nyears, function(y) {
  #      optimize(RPC:::MSYCalcs2, log(c(1e-4, 3)), M_at_Age = StockPars$M_ageArray[x, , y],
  #               Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
  #               Fec_at_Age = StockPars$Fec_Age[x, , y], V_at_Age = FleetPars$V[x, , y], maxage = StockPars$maxage,
  #               R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
  #               opt = 1, plusgroup = StockPars$plusgroup, SSBpR0 = StockPars$SSBpR[x, 1])$minimum %>% exp()
  #    })
  #  }) %>% t()
  #  tsplot(FMSY2, yrs, xlab = "Year", ylab = expression(F[MSY]~"(constant"~alpha~beta~")"), cols=cols)
#
  #  # F/FMSY (constant R0, h)
  #  tsplot(Find/FMSY, yrs, xlab = "Year", ylab = expression(F/F[MSY]~"(constant"~R[0]~h~")"), cols=cols)
#
  #  # F/FMSY (constant alpha, beta)
  #  tsplot(Find/FMSY2, yrs, xlab = "Year", ylab = expression(F/F[MSY]~"(constant"~alpha~beta~")"), cols=cols)
  #}

}







hist_Frep <- function(OBJs, yr_SPRcrash) {
  MSEhist<-OBJs$MSEhist
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
  cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')


  if(!is.null(MSEhist@TSdata$SPR)) {

    # Equilibrium and dynamic SPR
    tsplot(MSEhist@TSdata$SPR$Equilibrium, yrs, xlab="Year", ylab="Equilibrium SPR", cols=cols, ymax = 1.05)
    tsplot(MSEhist@TSdata$SPR$Dynamic, yrs, xlab="Year", ylab="Dynamic SPR", cols=cols, ymax = 1.05)

    # SPRcrash
    tsplot(MSEhist@Ref$ByYear$SPRcrash[, 1:length(yrs)], yrs, xlab="Year", ylab=expression(SPR[crash]), cols=cols)

    # SPR/SPRcrash
    if(missing(yr_SPRcrash)) yr_SPRcrash <- MSEhist@OM@nyears
    SPR_crash <- MSEhist@Ref$ByYear$SPRcrash[, yr_SPRcrash]
    tsplot((1 - MSEhist@TSdata$SPR$Equilibrium)/(1 - SPR_crash), yrs, xlab="Year",
           ylab=expression((1-SPR[eq])/(1-SPR[crash])), cols=cols)

    # SPR MSY
    #FMSY <- MSEhist@Ref$ByYear$FMSY
    #StockPars <- MSEhist@SampPars$Stock
    #SPR_MSY <- vapply(1:MSEhist@OM@nsim, function(x) {
    #  vapply(1:MSEhist@OM@nyears, function(y) {
    #    MSEtool:::Ref_int_cpp(FMSY[x, y], M_at_Age = StockPars$M_ageArray[x, , y],
    #                          Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
    #                          V_at_Age = MSEhist@SampPars$Fleet$V[x, , y],
    #                          StockPars$SRrel[x], maxage = StockPars$maxage,
    #                          plusgroup = StockPars$plusgroup)[2, ]
    #  }, numeric(1))
    #}, numeric(MSEhist@OM@nyears))
    #tsplot(t(SPR_MSY), yrs, xlab="Year", ylab=expression(SPR[MSY]), cols=cols)
#
    ## SPR/SPR MSY
    #tsplot((1 - MSEhist@TSdata$SPR$Equilibrium)/(1 - t(SPR_MSY)), yrs, xlab="Year",
    #       ylab=expression((1-SPR[eq])/(1-SPR[MSY])), cols=cols)
  }

}
