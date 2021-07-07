hist_SSBref<-function(OBJs, figure = TRUE, SSBtab = c("SSB", "Initial", "Asymptotic", "Dynamic")) {
  SSBtab <- match.arg(SSBtab)

  MSEh<-OBJs$MSEhist
  nyh<-MSEh@OM@nyears

  hy<-MSEh@OM@CurrentYr - (nyh:1) + 1

  SSB<-apply(MSEh@TSdata$SBiomass,1:2,sum)

  SSB0h<-array(SSB[,1],dim(SSB))
  SSB0d<-MSEh@Ref$Dynamic_Unfished$SSB0
  SSB0a<-MSEh@Ref$ByYear$SSB0

  SSBrh<-SSB/SSB0h
  SSBra<-SSB/SSB0a[,1:nyh]
  SSBrd<-SSB/SSB0d[,1:nyh]

  lev<-0.4
  pmat4<-cbind(apply(SSBrh>lev,2,mean),apply(SSBra>lev,2,mean),apply(SSBrd>lev,2,mean))
  lev<-0.1
  pmat1<-cbind(apply(SSBrh>lev,2,mean),apply(SSBra>lev,2,mean),apply(SSBrd>lev,2,mean))

  if(figure) {
    par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
    tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
    tsplot(x=SSBrh,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Initial~SSB[0]))
    tsplot(x=SSBra,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Asymptotic~SSB[0]))
    tsplot(x=SSBrd,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Dynamic~SSB[0]))

    matplot(hy,pmat1,type='l',lty=c(1,1,2),col=c('grey','black','black'),ylab="Probability of below 10% ref pt")

    #plot(1,axes=F,ylab="",col='white',xlab="")
    legend('bottomleft',legend=c('Initial',"Asymptotic","Dynamic"),
           col=c('grey','black','black'), lty=c(1,1,2),text.col=c('grey','black','black'),
           title=expression(SSB[0]~ref~pts),title.col='black',bty='n',cex=1)
    matplot(hy,pmat4,type='l',lty=c(1,1,2),col=c('grey','black','black'),ylab="Probability of below 40% ref pt")
  } else {

    if(SSBtab == "SSB") {
      out <- make_df(SSB, hy)
    }
    return(out)
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


hist_R <- function(OBJs, figure = TRUE) {

  Hist <- OBJs$MSEhist
  out <- stock_recruit_int(Hist)
  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)

  S_IQR <- apply(out$SSB, 2, quantile, probs = c(0.05, 0.95))
  R_IQR <- apply(out$R, 2, quantile, probs = c(0.05, 0.95))

  Rdev <- log(out$R/out$predR_y)

  if(figure) {
    par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))

    # Recruitment by year
    tsplot(out$predR_y,yrs=out$yrs,xlab="Year",ylab="Recruitment",zeroyint=TRUE)
    abline(h = 0)

    # Plot stock-recruit relationship
    matplot(out$SSB, out$R, typ = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
    plotquant(out$predR, yrs = out$predSSB, addline = TRUE)
    points(medSSB, medR, pch = 19)
    legend("topright", c("Median", "All sims"), text.col = c("black", "dark grey"), bty = "n")
    abline(h = 0, col = "grey")

    # log-recruitment deviation
    tsplot(Rdev,yrs=out$yrs,xlab="Year",ylab="Log recruitment deviation",zeroyint=FALSE)
    abline(h = 0, lty = 3)

    ## Recruitment deviations vs SSB
    matplot(out$SSB, Rdev, typ = "p", xlim = c(0, max(out$SSB)), #ylim = c(0, max(R)),
            col = "#99999920", pch = 19,
            xlab = "Spawning biomass", ylab = "Log recruitment deviation")
    plotquant(Rdev, yrs = medSSB, addline = TRUE)
    abline(h = 0, lty = 3)
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
  structure(xx, dimnames = list(Year, c("25%ile", "Median", "75%ile")))
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
