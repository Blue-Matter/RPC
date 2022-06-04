
#' @name plot_LRP
#' @title Plot historical dynamics relative to candidate limit reference points
#' @description Generate LRP figures. See details below on various functions.
#' @param x An object of class \linkS4class{Hist}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEhist} containing
#' the Hist object.
#' @param figure Character, whether to return a time series plot (\code{"ts"}), probability plot (\code{"prob"}), or no figure (\code{"none"}).
#' @param prob_ratio Numeric. If \code{figure = "prob"}, numeric that indicates a threshold. Functions return
#' annual probabilities of exceeding this threshold.
#' @param prob_ylim The y-axis range of the figure if \code{figure = "prob"}.
#' @return Returns invisibly a list with matrices and arrays used to generate various plots associated with
#' time series and probabilities.
#' @examples
#' Hist <- MSEtool::runMSE(MSEtool::testOM, Hist = TRUE)
#'
#' LRP <- LRP_SSBhist(Hist, SSB_y = 50, prob_ratio = 0.5)
#' str(LRP)
#'
#' LRP_50Rmax(Hist)
#' @references
#' Mace, P.M. 1994. Relationships between Common Biological Reference Points Used as Thresholds and Targets of Fisheries Management Strategies.
#' CJFAS. 51:110-122. https://doi.org/10.1139/f94-013
#'
#' Myers, et al. 1994. In search of thresholds for recruitment overfishing. ICES JMS. 51:191–205. https://doi.org/10.1006/jmsc.1994.1020
#' @author Q. Huynh
NULL

#' @rdname plot_LRP
#' @details \code{LRP_SSBhist} returns annual spawning biomass (SSB) relative to the annual probability that SSB exceeds some historical value
#' (corresponding to the year in \code{SSB_y}).
#' @param SSB_y The year (relative to OM@@CurentYr) to compare annual SSB values (only if prob_ratio is a numeric).
#' @export
LRP_SSBhist <- function(x, figure = c("ts", "prob", "none"), SSB_y, prob_ratio = 1, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  if(missing(SSB_y)) SSB_y <- MSEhist@OM@CurrentYr

  yind <- SSB_y - MSEhist@OM@CurrentYr + MSEhist@OM@nyears
  SSB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)
  LRP <- list(Method = "Historical SSB",
              Ratio = prob_ratio,
              Historical_Year = SSB_y,
              Historical_SB = SSB[, yind],
              Year = MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1,
              SBiomass = SSB)

  LRP$Prob <- apply(LRP$SBiomass > LRP$Ratio * LRP$Historical_SB, 2, mean) %>%
    matrix(ncol = 1) %>% structure(dimnames = list(LRP$Year, c("Probability")))

  LRP$Quantile <- make_df(LRP$SBiomass/LRP$Historical_SB, LRP$Year)

  if(figure == "ts") { # Plot B/B_hist
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow=c(1,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))

    tsplot(LRP$SBiomass, yrs = LRP$Year, xlab = "Year", ylab = "Spawning biomass (SSB)")
    tsplot(LRP$SBiomass/LRP$Historical_SB, yrs = LRP$Year, xlab = "Year",
           ylab = parse(text = paste0("SSB/SSB[", SSB_y, "]")))
    mtext("Year", side = 1, outer = TRUE, line = 1)

  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~SSB/SSB[", SSB_y, "]>", prob_ratio)))

  }

  invisible(LRP)
}


#' @rdname plot_LRP
#' @details \code{LRP_SSBMSY} returns annual SSB/SSBMSY or the annual probability that SSB/SSBMSY exceeds \code{prob_ratio}.
#' @export
LRP_SSBMSY <- function(x, figure = c("ts", "prob", "none"), prob_ratio = 1, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  SSB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)

  # Year specific SSBMSY (constant alpha, beta)
  SSBMSY <- MSEhist@Ref$ByYear$SSBMSY[, 1:MSEhist@OM@nyears]

  LRP <- list(Method = "SSBMSY",
              Ratio = prob_ratio,
              SSBMSY = SSBMSY,
              Year = MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1,
              SBiomass = SSB)

  LRP$Prob <- apply(LRP$SBiomass > LRP$Ratio * LRP$SSBMSY, 2, mean) %>%
    matrix(ncol = 1) %>% structure(dimnames = list(LRP$Year, c("Probability")))

  LRP$Quantile <- make_df(LRP$SBiomass/LRP$SSBMSY, LRP$Year)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))

    tsplot(x = LRP$SBiomass, yrs = LRP$Year, xlab = "Year", ylab = "Spawning biomass (SSB)")
    tsplot(x = LRP$SSBMSY, yrs = LRP$Year, xlab = "Year", ylab = expression(SSB[MSY]))
    tsplot(x = LRP$SBiomass/LRP$SSBMSY, yrs = LRP$Year, xlab = "Year", ylab = expression(SSB/SSB[MSY]))
    mtext("Year", side = 1, outer = TRUE, line = 1)

  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~SSB/SSB[MSY]>", prob_ratio)))

  }

  invisible(LRP)
}

#' @rdname plot_LRP
#' @details \code{LRP_SSB0} returns annual SSB/SSB0 or the annual probability that SSB/SSB0 exceeds \code{prob_ratio}.
#' @param type For \code{LRP_SSB0}, whether the SSB0 is the equilibrium, initial, or dynamic value (see App for description).
#' @export
#' @export
LRP_SSB0 <- function(x, figure = c("ts", "prob", "none"), type = c("equilibrium", "initial", "dynamic"),
                     prob_ratio = 0.4, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)
  type <- match.arg(type)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  SSB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)

  LRP <- list(Method = paste("SSB0", type),
              Ratio = prob_ratio,
              SSB0 = switch(type,
                            "equilibrium" = MSEhist@Ref$ByYear$SSB0[, 1:MSEhist@OM@nyears],
                            "initial" = MSEhist@Ref$ByYear$SSB0[, 1],
                            "dynamic" = MSEhist@Ref$Dynamic_Unfished$SSB0[, 1:MSEhist@OM@nyears]),
              Year = MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1,
              SBiomass = SSB)

  LRP$Prob <- apply(LRP$SBiomass/LRP$SSB0 > prob_ratio, 2, mean) %>% matrix(ncol = 1) %>%
    structure(dimnames = list(LRP$Year, "Probability"))

  LRP$Quantile <- make_df(LRP$SBiomass/LRP$SSB0, LRP$Year)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfcol=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))

    tsplot(x=LRP$SBiomass,yrs=LRP$Year,xlab="Year",ylab="Spawning biomass (SSB)")

    ylab <- switch(type,
                   "equilibrium" = expression(Equilibrium~SSB[0]),
                   "initial" = expression(Initial~SSB[0]),
                   "dynamic" = expression(Dynamic~SSB[0]))
    if(type == "initial") {
      tsplot(x=matrix(LRP$SSB0, MSEhist@OM@nsim, MSEhist@OM@nyears),yrs=LRP$Year,xlab="Year",ylab=ylab)
    } else {
      tsplot(x=LRP$SSB0,yrs=LRP$Year,xlab="Year",ylab=ylab)
    }

    ylab <- switch(type,
                   "equilibrium" = expression(SSB~"/"~Equilibrium~SSB[0]),
                   "initial" = expression(SSB~"/"~Initial~SSB[0]),
                   "dynamic" = expression(SSB~"/"~Dynamic~SSB[0]))
    tsplot(x=LRP$SBiomass/LRP$SSB0,yrs=LRP$Year,xlab="Year",ylab=ylab)

    mtext("Year", side = 1, outer = TRUE, line = 1)

  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~SSB/",
                                    switch(type,
                                           "equilibrium" = "Equilibrium",
                                           "initial" = "Initial",
                                           "dynamic" = "Dynamic"),
                                    "SSB[0]>",
                                    prob_ratio)))

  }

  invisible(LRP)

}

#' @rdname plot_LRP
#' @details \code{LRP_SP} returns annual surplus production (annual change in biomass - catch) and per capita surplus production.
#' @param Bunit The metric for stock biomass, either total biomass, vulnerable biomass, or spawning biomass.
#' @export
LRP_SP <- function(x, figure = c("ts", "phase", "none"), Bunit = c("B", "VB", "SSB")) {
  figure <- match.arg(figure)
  Bunit <- match.arg(Bunit)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  nyh<-MSEhist@OM@nyears
  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1

  B <- switch(Bunit,
              "B" = apply(MSEhist@TSdata$Biomass,1:2,sum),
              "VB" = apply(MSEhist@TSdata$VBiomass,1:2,sum),
              "SSB" = apply(MSEhist@TSdata$SBiomass,1:2,sum))

  catch<-apply(MSEhist@TSdata$Removals,1:2,sum)

  ind1<-2:nyh-1
  ind2<-2:nyh

  SP<-B[, ind2]-B[, ind1]+catch[, ind1]
  SPB <- SP/B[, ind1]

  medSP<-apply(SP, 2, median)
  medB<-apply(B[, ind1], 2, median)
  medSPB<-apply(SP/B[, ind1], 2, median)

  yr_lab <- seq(1, nyh, by = 5)

  LRP <- list(Method = "Low Biomass, Low Surplus Production",
              Year = hy,
              Biomass = B,
              Removals = catch,
              SP = SP,
              Quantile = make_df(SP, hy[-length(hy)]))

  if(figure == "ts") {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow=c(1,2),mai=c(0.9,0.9,0.2,0.1),omi=c(0,0,0,0))

    tsplot(SP,yrs=hy[ind1],xlab="Year",ylab="Surplus production",zeroyint=F)
    abline(h = 0, lty = 3)

    tsplot(SPB,yrs=hy[ind1],xlab="Year",ylab="Surplus production / Biomass",zeroyint=F)
    abline(h = 0, lty = 3)

  } else if(figure == "phase") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow=c(1,2),mai=c(0.9,0.9,0.2,0.1),omi=c(0,0,0,0))

    plot(medB,medSP,type='l', xlim = c(0, 1.1 * max(B[, ind1])),
         xlab="Biomass",ylab="Surplus production")
    matpoints(B[,ind1],SP,col="#99999920",pch=19,cex=0.9)
    points(medB,medSP,pch=19,cex=0.9)
    lines(medB,medSP,lwd=1.5)
    abline(h = 0, lty = 3)
    text(medB[yr_lab], medSP[yr_lab], labels = hy[yr_lab], pos = 2)
    legend('topright',legend=c("Median","All sims"),text.col=c("black","dark grey"),bty="n")

    plot(medB,medSPB,type='l', xlim = c(0, 1.1 * max(B[, ind1])),
         xlab="Biomass",ylab="Surplus production / Biomass")
    matplot(B[,ind1],SPB,col="#99999920",add=T,lty=1,pch=19,cex=0.9)
    points(medB,medSPB,pch=19,cex=0.9)
    lines(medB,medSPB,lwd=1.5)
    abline(h = 0, lty = 3)
    text(medB[yr_lab], medSPB[yr_lab], labels = hy[yr_lab], pos = 2)
    legend('topright',legend=c("Median","All sims"),text.col=c("black","dark grey"),bty="n")

  }

  invisible(LRP)
}

#' @rdname plot_LRP
#' @details \code{LRP_R} returns either annual recruitment (as a figure or table) or a stock recruit figure.
#' @param SR_xlim Optional x-axis range for the stock recruit plot. Only used if \code{figure = "SR"}.
#' @param SR_ylim Optional y-axis range for the stock recruit plot. Only used if \code{figure = "SR"}.
#' @param SR_y_RPS0 The year (relative to OM@@CurrentYr) for which to plot unfished recruits per spawner,
#' Only used if \code{figure = "SR"}, and \code{any(SR_include == 3)}.
#' @param SR_include A vector including any of \code{c(1, 2, 3)} that indicates what to plot in the stock-recruit figure. 1 = individual S-R pairs,
#' 2 = stock-recruit relationship, 3 = reference recruits-per-spawner (R/S) lines
#' (maximum R/S corresponding to stock-recruit alpha, median historical R/S, and year-specific unfished R/S).
#' @export
LRP_R <- function(x, figure = c("ts", "SR", "none"), SR_xlim, SR_ylim, SR_y_RPS0, SR_include = 1:3) {
  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  out <- stock_recruit_int(MSEhist)
  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)

  S_IQR <- apply(out$SSB, 2, quantile, probs = c(0.05, 0.95))
  R_IQR <- apply(out$R, 2, quantile, probs = c(0.05, 0.95))

  Rdev <- log(out$R/out$predR_y)
  medRdev <- apply(Rdev, 2, median)

  LRP <- list(Method = "Stock-recruit",
              Year = out$yrs,
              SBiomass = out$SSB,
              Rec = out$R,
              RecDev = Rdev,
              Quantile = make_df(out$R, out$yrs))

  if(figure == "ts") {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))

    # log-recruitment deviation
    tsplot(Rdev,yrs=out$yrs,xlab="Year",ylab="Log recruitment deviation",zeroyint=FALSE)
    abline(h = 0, lty = 3)

    ## Recruitment deviations vs SSB
    matplot(out$SSB, Rdev, type = "p", xlim = c(0, max(out$SSB)), #ylim = c(0, max(R)),
            col = "#99999920", pch = 19,
            xlab = "Spawning biomass", ylab = "Log recruitment deviation")
    lines(medSSB, medRdev, typ = "o", pch = 16, lwd = 2, col = "dark blue")
    #plotquant(Rdev, yrs = medSSB, addline = TRUE)
    abline(h = 0, lty = 3)

    # Recruitment by year
    tsplot(out$R,yrs=out$yrs,xlab="Year",ylab="Recruitment",zeroyint=TRUE)
    abline(h = 0)

  } else if(figure == "SR") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    # Plot stock-recruit relationship
    if(missing(SR_xlim)) SR_xlim <- c(0, max(out$SSB))
    if(missing(SR_ylim)) SR_ylim <- c(0, max(out$R))
    if(missing(SR_y_RPS0)) {
      SR_y_RPS0 <- MSEhist@OM@nyears
    } else { #if(SR_y_RPS0 > MSEhist@OM@nyears) { # convert calendar year to matrix column
      SR_y_RPS0 <- try(max(1, SR_y_RPS0 - MSEhist@OM@CurrentYr + MSEhist@OM@nyears), silent = TRUE)
      if(is.character(SR_y_RPS0)) SR_y_RPS0 <- MSEhist@OM@nyears
    }

    # Plot nothing
    matplot(out$SSB, out$R, type = "n", xlim = SR_xlim, ylim = SR_ylim,
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
    abline(h = 0, v = 0, col = "grey")

    if(any(SR_include == 2)) { # Plot SR curve
      SR_curve <- plotquant2(out$predR, yrs = out$predSSB)

      lines(SR_curve$med$x, SR_curve$med$y, col = "darkblue", lwd = 2)
      #lines(SR_curve$poly_inner$x, SR_curve$poly_inner$y, col = "darkblue", lty = 3)
      lines(SR_curve$poly_outer$x, SR_curve$poly_outer$y, col = "darkblue", lty = 3)
    }

    if(any(SR_include == 1)) { # Plot individual S-R pairs

      matpoints(out$SSB, out$R, col = "#99999920", pch = 16)
      points(medSSB, medR, pch = 16)
      text(medSSB, medR, labels = out$yrs, pos = 3)

    }

    if(any(SR_include == 3)) { # Plot recruits per spawner lines

      StockPars <- MSEhist@SampPars$Stock
      FleetPars <- MSEhist@SampPars$Fleet

      RpS_crash <- median(1/MSEhist@Ref$ByYear$SPRcrash[, 1]/StockPars$SSBpR[, 1])

      RpS_0 <- vapply(1:MSEhist@OM@nsim, function(x, y) {
        MSEtool:::Ref_int_cpp(0, M_at_Age = StockPars$M_ageArray[x, , y],
                              Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
                              Fec_at_Age = StockPars$Fec_Age[x, , y],
                              V_at_Age = MSEhist@SampPars$Fleet$V[x, , y],
                              maxage = StockPars$maxage,
                              plusgroup = StockPars$plusgroup)[3, ]
      }, numeric(1), y = SR_y_RPS0) %>% median()

      RpS_med <- apply(out$R/out$SSB, 1, median) %>% median()

      abline(a = 0, b = RpS_0, lty = 2, lwd = 2, col = "blue")
      abline(a = 0, b = RpS_med, lty = 2, lwd = 2, col = "black")
      abline(a = 0, b = RpS_crash, lty = 2, lwd = 2, col = "red")

      legend("bottomright",
             parse(text = c(paste0("Unfished~(", SR_y_RPS0 + MSEhist@OM@CurrentYr - MSEhist@OM@nyears, ")~R/S"),
                            "Median~hist.~R/S", "Maximum~R/S")),
             title = "Recruits per spawner",
             col = c("blue", "black", "red"), lty = 2, lwd = 2)

    }
  }

  invisible(LRP)
}

#' @rdname plot_LRP
#' @details \code{LRP_RPS} returns annual recruits-per-spawner.
#' @export
LRP_RPS <- function(x, figure = c("ts", "none")) {
  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  out <- stock_recruit_int(MSEhist)

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)
  medRpS <- apply(out$R/out$SSB, 2, median)

  LRP <- list(Method = "Recruits per spawner",
              Year = out$yrs,
              RPS = out$R/out$SSB,
              SBiomass = out$SSB,
              Quantile = make_df(out$R/out$SSB, out$yrs)
              )

  if(figure == "ts") {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
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
  }

  invisible(LRP)

}




#' @rdname plot_LRP
#' @details \code{LRP_SPR} returns annual spawning potential ratio.
#' @export
LRP_SPR <- function(x, figure = c("ts", "prob", "none"), prob_ratio = 0.4, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  LRP <- list(Method = "Spawning Potential Ratio (equilibrium)",
              Ratio = prob_ratio,
              Year = MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1,
              SPR_eq = MSEhist@TSdata$SPR$Equilibrium,
              SPR_dyn = MSEhist@TSdata$SPR$Dynamic,
              SPR_crash = MSEhist@Ref$ByYear$SPRcrash[, 1:MSEhist@OM@nyears])

  LRP$Prob <- apply(LRP$SPR_eq > LRP$Ratio, 2, mean) %>% matrix(ncol = 1) %>%
    structure(dimnames = list(LRP$Year, c("Probability")))

  LRP$Quantile <- make_df(LRP$SPR_eq, LRP$Year)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfcol=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
    cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

    # Equilibrium and dynamic SPR
    tsplot(LRP$SPR_eq, LRP$Year, xlab="Year", ylab="Equilibrium SPR", cols=cols, ymax = 1.05)
    tsplot(LRP$SPR_dyn, LRP$Year, xlab="Year", ylab="Dynamic SPR", cols=cols, ymax = 1.05)

    # SPRcrash
    tsplot(LRP$SPR_crash, LRP$Year, xlab="Year", ylab=expression(SPR[crash]), cols=cols,
           ymax = 1.1 * max(LRP$SPR_crash))

    # SPR/SPRcrash
    tsplot((1 - LRP$SPR_eq)/(1 - LRP$SPR_crash), LRP$Year, xlab="Year",
           ylab=expression((1-SPR[eq])/(1-SPR[crash])), cols=cols)

  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~SPR[eq]>", prob_ratio)))
  }

  invisible(LRP)
}


#' @rdname plot_LRP
#' @details \code{LRP_FMSY} returns annual F and F/FMSY.
#' @export
LRP_FMSY <- function(x, figure = c("ts", "prob", "none"), prob_ratio = 1, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  # Apical F index
  Find <-  MSEhist@SampPars$Fleet$qs * MSEhist@TSdata$Find

  # M of mature animals
  M <- MSEhist@SampPars$Stock$Marray[, 1:MSEhist@OM@nyears]

  # Year specific FMSY
  FMSY <- MSEhist@Ref$ByYear$FMSY[, 1:MSEhist@OM@nyears]

  LRP <- list(Method = "FMSY",
              Year = yrs,
              Ratio = prob_ratio,
              FMSY = FMSY,
              FM = Find)

  LRP$Prob <- apply(Find/FMSY < prob_ratio, 2, mean) %>% matrix(ncol = 1) %>%
    structure(dimnames = list(yrs, c("Probability")))
  LRP$Quantile <- make_df(Find/FMSY, yrs)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    cols <- list(colm = "darkgreen", col50 = "lightgreen", col90 = "#40804025")

    # Total removals
    if(sum(MSEhist@TSdata$Discards)) {
      par(mfcol=c(3,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
      tsplot(apply(MSEhist@TSdata$Landings,1:2,sum), yrs, xlab="Year", ylab="Landings", cols=cols)
      tsplot(apply(MSEhist@TSdata$Discards,1:2,sum), yrs, xlab="Year", ylab="Discards", cols=cols)
    } else {
      par(mfcol=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
      tsplot(apply(MSEhist@TSdata$Removals,1:2,sum), yrs, xlab="Year", ylab="Catch", cols=cols)
    }

    tsplot(M, yrs, xlab = "Year", ylab = "Apical F (green), M (blue)", ymax = 1.1 * max(Find, M))
    plotquant(Find, yrs = yrs, cols = cols)

    tsplot(FMSY, yrs, xlab = "Year", ylab = expression(F[MSY]), cols=cols)
    tsplot(Find/FMSY, yrs, xlab = "Year", ylab = expression(F/F[MSY]), cols=cols)

  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~F/F[MSY]<", prob_ratio)))
  }

  invisible(LRP)
}






#' @rdname plot_LRP
#' @details \code{LRP_Fmed} returns F and Fmed (the fishing mortality corresponding to the historical median recruits per spawner,
#' frequently also referred to as the F-replacement F).
#' @export
LRP_Fmed <- function(x, figure = c("ts", "prob", "none"), prob_ratio = 1, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  # Apical F index
  Find <-  MSEhist@SampPars$Fleet$qs * MSEhist@TSdata$Find

  # Year specific FMSY (constant R0, h)
  Fmed <- MSEhist@Ref$ByYear$Fmed[, 1:MSEhist@OM@nyears]

  LRP <- list(Method = "Replacement F (median recruits per spawner)",
              Year = yrs,
              FM = Find,
              Fmed = Fmed)

  LRP$Prob <- apply(Find/Fmed < prob_ratio, 2, mean) %>% matrix(ncol = 1) %>%
    structure(dimnames = list(yrs, c("Probability")))
  LRP$Quantile <- make_df(Find/Fmed, yrs)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

    # Total removals
    if(sum(MSEhist@TSdata$Discards)) {
      par(mfcol=c(3,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
      tsplot(apply(MSEhist@TSdata$Landings,1:2,sum), yrs, xlab="Year", ylab="Landings", cols=cols)
      tsplot(apply(MSEhist@TSdata$Discards,1:2,sum), yrs, xlab="Year", ylab="Discards", cols=cols)
    } else {
      par(mfcol=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
      tsplot(apply(MSEhist@TSdata$Removals,1:2,sum), yrs, xlab="Year", ylab="Catch", cols=cols)
    }
    tsplot(Find, yrs, xlab = "Year", ylab = "Apical F", cols=cols)
    tsplot(Fmed, yrs, xlab = "Year", ylab = expression(F[med]), cols=cols)
    tsplot(Find/Fmed, yrs, xlab = "Year", ylab = expression(F/F[med]), cols=cols)

  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~F/F[med]<", prob_ratio)))
  }

  invisible(LRP)
}


#' @rdname plot_LRP
#' @details \code{LRP_50Rmax} returns  \code{SSB50\%Rmax}, the SSB corresponding to 50% of maximum recruitment from the stock-recruit relationship.
#' @export
LRP_50Rmax <- function(x, figure = c("ts", "prob", "none"), prob_ratio = 1, prob_ylim = c(0, 1)) {
  figure <- match.arg(figure)
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  out <- stock_recruit_int(MSEhist)

  out_Rmax <- calculate_SSB50(MSEhist)
  Rmax <- out_Rmax$Rmax
  Rmax50 <- out_Rmax$Rmax50
  S50 <- out_Rmax$SSB50

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)

  LRP <- list(Method = "SSB at 50% Rmax (SRR)",
              Ratio = prob_ratio,
              Year = out$yrs,
              SSB_50Rmax = S50,
              SBiomass = out$SSB)

  LRP$Prob <- apply(out$SSB/S50 > prob_ratio, 2, mean) %>% matrix(ncol = 1) %>%
    structure(dimnames = list(out$yrs, c("Probability")))

  LRP$Quantile <- make_df(out$SSB/S50, out$yrs)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

    # Plot stock-recruit relationship with SSB 50%Rmax
    matplot(out$SSB, out$R, type = "p", col = "#99999920", xlim = c(0, 1.1 * max(medSSB)), ylim = c(0, 1.1 * max(medR)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 4)
    plotquant(out$predR, yrs = out$predSSB, addline=T)
    points(medSSB, medR, pch = 19)
    abline(v = quantile(S50, c(0.25, 0.5, 0.75)), lty = 2, lwd = c(1, 2, 1))
    abline(h = 0, v = 0, col = "grey")
    legend("topright", c("All Sims", "Median", expression(SSB["50%"~Rmax])), pch = c(4, 16, NA),
           lty = c(NA, NA, 2), lwd = c(NA, NA, 2), bty = "n")

    # Historical SSB/SSB 50%Rmax
    tsplot(x = LRP$SBiomass/LRP$SSB_50Rmax, yrs = LRP$Year, xlab = "Year", ylab = expression(SSB["50%"~Rmax]))

    # Plot regression line
    #reg_low <- lapply(1:MSEhist@OM@nsim, function(i) Rmax_regression(R = out$R[i, ], SSB = out$SSB[i, ], S50 = S50[i], type = "low"))
    #reg_hi <- lapply(1:MSEhist@OM@nsim, function(i) Rmax_regression(R = out$R[i, ], SSB = out$SSB[i, ], S50 = S50[i], type = "high"))

    #matplot(log(out$SSB), log(out$R), type = "p", col = "#99999920", xlim = range(c(out$SSB, S50)) %>% log(),
    #        xlab = "log(Spawning biomass)", ylab = "log(Recruitment)", pch = 4)
    #points(log(medSSB), log(medR), pch = 19)
    #abline(v = log(S50), col = "#99999920", lty = 2)
    #abline(v = median(S50) %>% log(), lty = 2, lwd = 2)
    #lapply(1:MSEhist@OM@nsim, function(i) {
    #  if(!is.null(reg_hi[[i]])) lines(predict_logR ~ log(SSB), data = reg_hi[[i]], lty = 2) #col = "#99999920")
    #  if(!is.null(reg_low[[i]])) lines(predict_logR ~ log(SSB), data = reg_low[[i]], lty = 2) #col = "#99999920")
    #})
  } else if(figure == "prob") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(5, 4, 1, 1))

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~SSB/SSB[\"50%\"~Rmax]>", prob_ratio)))

  }

  invisible(LRP)
}

#' @rdname plot_LRP
#' @details \code{LRP_RPS90} returns \code{SSB 90\%ile R/S}, the SSB corresponding to the intersection of the 90the percentile of both historical recruitment
#' and recruits-per-spawner.
#' @export
#' @export
LRP_RPS90 <- function(x, figure = c("ts", "prob", "none"), prob_ratio = 1, prob_ylim = c(0, 1)) {

  figure <- match.arg(figure)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  out <- stock_recruit_int(MSEhist)

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)
  #medRpS <- medR/medSSB

  RpS_med <- apply(out$R/out$SSB, 1, median)
  RpS_90 <- apply(out$R/out$SSB, 1, quantile, probs = 0.9)
  R_90 <- apply(out$R, 1, quantile, probs = 0.9)
  S_90 <- R_90/RpS_90

  LRP <- list(Method = "SSB at 90%ile R and 90%ile RPS (no SRR)",
              Ratio = prob_ratio,
              Year = out$yrs,
              SSB_90RPS = S_90,
              Rec = out$R,
              SBiomass = out$SSB)
  LRP$Prob <- apply(out$SSB/S_90 > prob_ratio, 2, mean) %>% matrix(ncol = 1) %>%
    structure(dimnames = list(out$yrs, c("Probability")))
  LRP$Quantile <-  make_df(out$SSB/S_90, out$yrs)

  if(figure == "ts") {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

    # Plot stock-recruit relationship
    matplot(out$SSB, out$R, type = "p", col = "#99999920", xlim = c(0, 1.1 * max(medSSB)), ylim = c(0, 1.1 * max(medR)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 4)
    plotquant(out$predR, yrs = out$predSSB, addline=T)
    points(medSSB, medR, pch = 19)

    # 90%ile R/S lines
    abline(a = 0, b = median(RpS_90), lty = 2, lwd = 2, col = "red")
    abline(a = 0, b = quantile(RpS_90, 0.25), lty = 2, col = "red")
    abline(a = 0, b = quantile(RpS_90, 0.75), lty = 2, col = "red")

    # 90%ile recruitment
    abline(h = quantile(R_90, c(0.25, 0.5, 0.75)), lty = 2, lwd = c(1, 2, 1), col = "blue")

    # LRP at intersection
    abline(v = quantile(S_90, c(0.25, 0.5, 0.75)), lty = 2, lwd = c(1, 2, 1))

    # Legend
    legend("topright", c("All sims", "Median", paste("90%ile", c("R/S", "R", "SSB (LRP)"))),
           col = c("black", "black", "red", "blue", "black"), pch = c(4, 16, NA, NA, NA),
           lwd = c(NA, NA, 2, 2, 2), lty = c(NA, NA, 4, 4, 4))
    abline(h = 0, v = 0, col = "grey")

    # Historical SSB/SSB 50%Rmax
    tsplot(x = LRP$SBiomass/LRP$SSB_90RPS, yrs = LRP$Year, xlab = "Year", ylab = expression(SSB["90%"~"R/S"]))


  } else if(figure == "prob") {

    plot(LRP$Year, LRP$Prob,
         typ = "o",
         pch = 16,
         ylim = prob_ylim,
         xlab = "Year",
         ylab = parse(text = paste0("Probability~SSB/SSB[\"90%ile\"~R/S]>", prob_ratio)))
  }

  invisible(LRP)

}


