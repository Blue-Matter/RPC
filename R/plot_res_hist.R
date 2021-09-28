
#' @name plot-Hist-prob
#' @title Plot historical dynamics with probabilities
#' @description Generate figures associated with probabilities
#' @param x An object of class \linkS4class{Hist}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEhist} which is
#' the Hist object.
#' @param figure Logical, whether to return a figure (TRUE) or data frame (FALSE).
#' @param prob_ratio If NA, returns annual values. Otherwise, a numeric that indicates a threshold. Functions return
#' annual probabilities of exceeding this threshold.
#' @param prob_ylim The y-axis range of the figure if prob_ratio is a numeric. Only used if \code{figure = TRUE}.
#' @return If \code{figure = TRUE}, various plots using either base graphics or ggplot2 showing annual values (\code{prob_ratio = NA}) or
#' probabilities (\code{prob_ratio} is numeric). If \code{figure = FALSE}, returns a data frame displaying quantiles (\code{prob_ratio = NA})
#' or probabilities (\code{prob_ratio} is numeric).
#' @examples
#' Hist <- MSEtool::runMSE(Hist = TRUE)
#'
#' hist_SSB(Hist)
#' hist_SSB(Hist, SSB_y = 50, prob_ratio = 0.5)
#'
#' hist_SSB(Hist, figure = FALSE)
#' hist_SSB(Hist, figure = FALSE, SSB_y = 50, prob_ratio = 0.5)
#' @author Q. Huynh
NULL

#' @rdname plot-Hist-prob
#' @details \code{hist_SSB} returns annual SSB or the annual probability that SSB exceeds some historical value (corresponding to the year in
#' argument \code{SSB_y}).
#' @param SSB_y The year (relative to OM@@CurentYr) to compare annual SSB values (only if prob_ratio is a numeric).
#' @export
hist_SSB <- function(x, figure = TRUE, SSB_y = NA, prob_ratio = NA, prob_ylim = c(0, 1)) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  if(!is.na(prob_ratio) && is.na(SSB_y)) {
    stop("A year is needed to compare SSB in year specified in argument SSB_y")
  }

  nyh<-MSEhist@OM@nyears

  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1
  yind <- SSB_y - MSEhist@OM@CurrentYr + MSEhist@OM@nyears

  SSB<-apply(MSEhist@TSdata$SBiomass,1:2,sum)

  if(figure) {
    if(is.na(prob_ratio)) {
      tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
    } else {
      data.frame(Year = hy, pvec = apply(SSB > prob_ratio * SSB[, yind], 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y = parse(text = paste0("Probability~SSB/SSB[", SSB_y, "]>", prob_ratio)))
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


#' @rdname plot-Hist-prob
#' @details \code{hist_SSBMSY} returns annual SSB/SSBMSY or the annual probability that SSB/SSBMSY exceeds \code{prob_ratio}.
#' @export
hist_SSBMSY <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1.5)) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  nyh<-MSEhist@OM@nyears

  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1

  SSB <- apply(MSEhist@TSdata$SBiomass,1:2,sum)

  # Year specific SSBMSY (constant alpha, beta)
  SSBMSY <- MSEhist@Ref$ByYear$SSBMSY[, 1:MSEhist@OM@nyears]

  if(figure) {
    if(is.na(prob_ratio)) {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mfrow=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))

      tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
      tsplot(x=SSBMSY,yrs=hy,xlab="Year",ylab=expression(SSB[MSY]))
      tsplot(x=SSB/SSBMSY,yrs=hy,xlab="Year",ylab=expression(SSB/SSB[MSY]))
    } else {
      data.frame(Year = hy, pvec = apply(SSB/SSBMSY > prob_ratio, 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y = parse(text = paste0("Probability~SSB/SSB[MSY]>", prob_ratio)))
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

#' @rdname plot-Hist-prob
#' @details \code{hist_SSB0} returns annual SSB/SSB0 or the annual probability that SSB/SSB0 exceeds \code{prob_ratio}.
#' @export
#' @export
hist_SSB0 <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  nyh<-MSEhist@OM@nyears

  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1

  SSB<-apply(MSEhist@TSdata$SBiomass,1:2,sum)

  SSB0h<-array(SSB[,1],dim(SSB))
  SSB0a<-MSEhist@Ref$ByYear$SSB0
  SSB0d<-MSEhist@Ref$Dynamic_Unfished$SSB0

  SSBrh<-SSB/SSB0h
  SSBra<-SSB/SSB0a[,1:nyh]
  SSBrd<-SSB/SSB0d[,1:nyh]

  if(figure) {
    if(is.na(prob_ratio)) {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mfcol=c(2,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))
      tsplot(x=SSB,yrs=hy,xlab="Year",ylab="Spawning biomass (SSB)")
      tsplot(x=SSBrh,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Initial~SSB[0]))
      tsplot(x=SSBra,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Asymptotic~SSB[0]))
      tsplot(x=SSBrd,yrs=hy,xlab="Year",ylab=expression(SSB~"/"~Dynamic~SSB[0]))
      mtext("Year", side = 1, outer = TRUE, line = 1)
    } else {
      pmat <- sapply(list(SSBra, SSBrh, SSBrd), function(x) apply(x > prob_ratio, 2, mean)) %>%
        structure(dimnames = list(NULL, c("Asymptotic~SSB[0]", "Initial~SSB[0]", "Dynamic~SSB[0]")))
      data.frame(Year = hy) %>% cbind(pmat) %>% reshape2::melt(id.vars = "Year") %>%
        ggplot(aes(Year, value, linetype = variable, shape = variable)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y = parse(text = paste0("Probability~SSB/SSB[0]>", prob_ratio))) +
        scale_shape_manual(name = expression(SSB[0]~Type), values = c(21, 16, 4), labels = scales::label_parse()) +
        scale_linetype_manual(name = expression(SSB[0]~Type), values = c(2, 1, 3), labels = scales::label_parse())
    }

  } else {
    sapply(list(SSBrh, SSBra, SSBrd), function(x) apply(x > prob_ratio, 2, mean)) %>%
      structure(dimnames = list(hy, c("Initial", "Asymptotic", "Dynamic") %>% paste("SSB0")))
  }
}

#' @rdname plot-Hist-prob
#' @details \code{hist_BvsSP} returns annual surplus production (annual change in biomass - catch) and per capita surplus production.
#' @export
hist_BvsSP<-function(x, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  nyh<-MSEhist@OM@nyears
  hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1
  B<-apply(MSEhist@TSdata$Biomass,1:2,sum)
  catch<-apply(MSEhist@TSdata$Removals,1:2,sum)
  ind1<-2:nyh-1
  ind2<-2:nyh

  SP<-B[, ind2]-B[, ind1]+catch[, ind1]
  medSP<-apply(SP, 2, median)
  medB<-apply(B[, ind1], 2, median)
  medSPB<-apply(SP/B[, ind1], 2, median)

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfcol=c(2,2),mai=c(0.9,0.9,0.2,0.1),omi=c(0,0,0,0))

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

#' @rdname plot-Hist-prob
#' @details \code{hist_R} returns either annual recruitment (as a figure or table) or a stock recruit figure.
#' @param SR_only Logical, whether to plot annual recruitment (FALSE) or the stock-recruit figure (TRUE). Only used if \code{figure = TRUE}.
#' @param SR_xlim Optional x-axis range for the stock recruit plot. Only used if \code{SR_only = TRUE} and \code{figure = TRUE}.
#' @param SR_ylim Optional y-axis range for the stock recruit plot. Only used if \code{SR_only = TRUE} and \code{figure = TRUE}.
#' @param SR_y_RPS0 The year (relative to OM@@CurrentYr) for which to plot unfished recruits per spawner,
#' Only used if \code{SR_only = TRUE} and \code{figure = TRUE}, and \code{any(SR_include == 3)}.
#' @param SR_include A vector including any of c(1, 2, 3) that indicates what to plot in the stock-recruit figure. 1 = individual S-R pairs,
#' 2 = stock-recruit relationship, 3 = reference recruits-per-spawner (R/S) lines
#' (maximum R/S corresponding to stock-recruit alpha, median historical R/S, and year-specific unfished R/S).
#' @export
hist_R <- function(x, figure = TRUE, SR_only = FALSE, SR_xlim, SR_ylim, SR_y_RPS0, SR_include = 1:3) {
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

  if(figure) {
    if(SR_only) {
      # Plot stock-recruit relationship
      if(missing(SR_xlim)) SR_xlim <- c(0, max(out$SSB))
      if(missing(SR_ylim)) SR_ylim <- c(0, max(out$R))
      if(missing(SR_y_RPS0)) {
        SR_y_RPS0 <- MSEhist@OM@nyears
      } else { #if(SR_y_RPS0 > MSEhist@OM@nyears) { # convert calendar year to matrix column
        SR_y_RPS0 <- try(max(1, SR_y_RPS0 - MSEhist@OM@CurrentYr + MSEhist@OM@nyears), silent = TRUE)
        if(is.character(SR_y_RPS0)) SR_y_RPS0 <- MSEhist@OM@nyears
      }

      dat_out <- data.frame(R = as.numeric(out$R), SSB = as.numeric(out$SSB), Type = "All sims")

      g <- ggplot(dat_out, aes(SSB, R, shape = Type)) +
        geom_blank() +
        theme_bw() +
        coord_cartesian(xlim = SR_xlim, ylim = SR_ylim) +
        labs(x = "Spawning biomass", y = "Recruitment")

      if(any(SR_include == 2)) { # Plot SR curve
        SR_curve <- plotquant2(out$predR, yrs = out$predSSB)
        g <- g + geom_polygon(data = SR_curve$poly_outer, aes(x, y, fill = Quantile), inherit.aes = FALSE) +
          geom_polygon(data = SR_curve$poly_inner, aes(x, y, fill = Quantile), inherit.aes = FALSE) +
          geom_line(data = SR_curve$med, aes(x, y), inherit.aes = FALSE, colour = "darkblue") +
          scale_fill_manual(values = c("50th Percentile" = "light blue", "90th Percentile" = "#60859925")) +
          guides(fill = "none")

      }
      if(any(SR_include == 1)) { # Plot individual S-R pairs
        meds <- data.frame(Year = out$yrs, SSB = medSSB, R = medR, Type = "Median")

        g <- g + geom_point(colour = "#99999920") +
          geom_point(data = meds) +
          ggrepel::geom_text_repel(data = meds, aes(label = Year)) +
          scale_shape_manual(name = "Stock-recruit values", values = c("All sims" = 4, "Median" = 16))
      }

      if(any(SR_include == 3)) { # Plot recruits per spawner lines
        StockPars <- MSEhist@SampPars$Stock
        FleetPars <- MSEhist@SampPars$Fleet

        RpS_crash <- median(1/MSEhist@Ref$ByYear$SPRcrash[, 1]/StockPars$SSBpR[, 1])

        RpS_0 <- vapply(1:MSEhist@OM@nsim, function(x, y) {
          MSEtool:::Ref_int_cpp(1e-8, M_at_Age = StockPars$M_ageArray[x, , y],
                                Wt_at_Age = StockPars$Wt_age[x, , y], Mat_at_Age = StockPars$Mat_age[x, , y],
                                Fec_at_Age = StockPars$Fec_Age[x, , y],
                                V_at_Age = MSEhist@SampPars$Fleet$V[x, , y],
                                maxage = StockPars$maxage,
                                plusgroup = StockPars$plusgroup)[3, ]
        }, numeric(1), y = SR_y_RPS0) %>% median()

        RpS_med <- apply(out$R/out$SSB, 1, median) %>% median()

        ablines <- data.frame(b = c(RpS_0, RpS_med, RpS_crash), a = 0,
                              Type = c(paste0("Unfished~(", SR_y_RPS0 + MSEhist@OM@CurrentYr - MSEhist@OM@nyears, ")~R/S"),
                                       "Median~hist.~R/S", "Maximum~R/S"))

        g <- g + geom_abline(data = ablines, aes(slope = b, colour = Type, intercept = a), size = 0.75, linetype = 2) +
          scale_colour_manual(name = "R/S", values = c("blue", "black", "red"), labels = scales::label_parse())

      }
      return(g)

    } else {

      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))

      # Plot stock-recruit relationship
      #matplot(out$SSB, out$R, type = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
      #        xlab = "Spawning biomass", ylab = "Recruitment", pch = 16)
      #plotquant(out$predR, yrs = out$predSSB, addline = TRUE)
      #points(medSSB, medR, pch = 19)
      #legend("topright", c("Median", "All sims"), text.col = c("black", "dark grey"), bty = "n")
      #abline(h = 0, col = "grey")

      # log-recruitment deviation
      tsplot(Rdev,yrs=out$yrs,xlab="Year",ylab="Log recruitment deviation",zeroyint=FALSE)
      abline(h = 0, lty = 3)

      ## Recruitment deviations vs SSB
      matplot(out$SSB, Rdev, type = "p", xlim = c(0, max(out$SSB)), #ylim = c(0, max(R)),
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

#' @rdname plot-Hist-prob
#' @details \code{hist_RpS} returns annual recruits-per-spawner.
#' @export
hist_RpS <- function(x, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  out <- stock_recruit_int(MSEhist)

  medSSB <- apply(out$SSB, 2, median)
  medR <- apply(out$R, 2, median)
  medRpS <- apply(out$R/out$SSB, 2, median)

  if(figure) {
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
  } else {
    out <- make_df(out$R/out$SSB, out$yrs)
    return(out)
  }

}

#' @name plot-Hist-diagnostic
#' @title Biomass limit reference points methods
#' @description Two figures showing diagnostics associated with methods for identifying biomass limit reference points using
#' the stock-recruit relationship.
#' @param x An object of class \linkS4class{Hist}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEhist} which is
#' the Hist object.
#' @param figure Logical, whether to return a stock-recruit figure (TRUE) or data frame reporting the limit reference point (FALSE).
#' @param prob_ratio If NA, returns the limit reference point. Otherwise, a numeric that indicates the threshold of exceeding the reference point.
#' Functions return annual probabilities of exceeding this threshold.
#' @param prob_ylim The y-axis range of the figure if prob_ratio is a numeric. Only used if \code{figure = TRUE}.
#' @references
#' Mace, P.M. 1994. Relationships between Common Biological Reference Points Used as Thresholds and Targets of Fisheries Management Strategies.
#' CJFAS. 51:110-122. https://doi.org/10.1139/f94-013
#'
#' Myers, et al. 1994. In search of thresholds for recruitment overfishing. ICES JMS. 51:191–205. https://doi.org/10.1006/jmsc.1994.1020
#' @examples
#' Hist <- MSEtool::runMSE(Hist = TRUE)
#'
#' hist_Rmax(Hist)
#' hist_Rmax(Hist, prob_ratio = 0.5)
#'
#' hist_Rmax(Hist, figure = FALSE)
#' hist_Rmax(Hist, figure = FALSE, prob_ratio = 0.5)
NULL



#' @rdname plot-Hist-prob
#' @details \code{hist_SPR} returns annual spawning potential ratio.
#' @export
hist_SPR <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  Fmed <- MSEhist@Ref$ByYear$Fmed
  StockPars <- MSEhist@SampPars$Stock

  if(figure) {
    if(is.na(prob_ratio)) {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
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
      data.frame(Year = yrs, pvec = apply(MSEhist@TSdata$SPR$Equilibrium > prob_ratio, 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y =  parse(text = paste0("Probability~SPR[eq]>", prob_ratio)))
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


#' @rdname plot-Hist-prob
#' @details \code{hist_exp} returns annual F and F/FMSY.
#' @export
hist_exp <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  # Apical F index
  Find <-  MSEhist@SampPars$Fleet$qs * MSEhist@TSdata$Find

  # Year specific FMSY (constant R0, h)
  FMSY <- MSEhist@Ref$ByYear$FMSY[, 1:MSEhist@OM@nyears]

  if(figure) {
    if(is.na(prob_ratio)) {
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
      tsplot(FMSY, yrs, xlab = "Year", ylab = expression(F[MSY]), cols=cols)
      tsplot(Find/FMSY, yrs, xlab = "Year", ylab = expression(F/F[MSY]), cols=cols)
    } else {

      data.frame(Year = yrs, pvec = apply(Find/FMSY < prob_ratio, 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y =  parse(text = paste0("Probability~F/F[MSY]<", prob_ratio)))

    }
  } else {
    if(is.na(prob_ratio)) {
      make_df(Find, yrs)
    } else {
      pvec <- apply(Find/FMSY < prob_ratio, 2, mean)
      structure(matrix(pvec, ncol = 1),
                dimnames = list(yrs, c("Probability")))
    }
  }

}






#' @rdname plot-Hist-prob
#' @details \code{hist_Fmed} returns F and Fmed (the fishing mortality corresponding to the historical median recruits per spawner,
#' frequently also referred to as the F-replacement F).
#' @export
hist_Fmed <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
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

  if(figure) {
    if(is.na(prob_ratio)) {
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
    } else {

      data.frame(Year = yrs, pvec = apply(Find/Fmed < prob_ratio, 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y =  parse(text = paste0("Probability~F/F[med]<", prob_ratio)))

    }
  } else {
    if(is.na(prob_ratio)) {
      make_df(Find/Fmed, yrs)
    } else {
      pvec <- apply(Find/Fmed < prob_ratio, 2, mean)
      structure(matrix(pvec, ncol = 1),
                dimnames = list(yrs, c("Probability")))
    }
  }
}


