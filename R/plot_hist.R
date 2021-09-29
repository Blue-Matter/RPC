
#' @name plot-Hist
#' @title Plot historical dynamics
#' @description Various plots for plotting historical time series for the operating model.
#' @param x An object of class \linkS4class{Hist}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEhist} which is
#' the Hist object.
#' @return Various plots using base graphics
#' @examples
#' Hist <- MSEtool::runMSE(Hist = TRUE)
#' hist_bio(Hist)
#' @author Q. Huynh
NULL

#' @rdname plot-Hist
#' @details \code{hist_bio} plots time series of biomass, abundance, and recruitment.
#' @export
hist_bio<-function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

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

#' @rdname plot-Hist
#' @details \code{hist_future_recruit} plots historical and future recruitment deviations.
#' @export
hist_future_recruit <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

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


#' @rdname plot-Hist
#' @details \code{hist_bio_schedule} plots in biological at age parameters.
#' @param var A string to indicate which object to plot from OM@@cpars.
#' @param n_age_plot The number of ages to plot in the left figure.
#' @param yr_plot The year (relative to OM@@CurrentYr) to plot for the right figure.
#' @param sim The simulation to plot for the left figure.
#' @export
hist_bio_schedule <- function(x, var = c("Len_age", "Wt_age", "Mat_age", "M_ageArray"), n_age_plot, yr_plot, sim) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  var <- match.arg(var)
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

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 2), mai = c(0.9, 0.9, 0.2, 0.1), omi = c(0, 0, 0, 0))

  matplot(yr_cal, t(sched[sim, age_plot + 1, ]), xlab = "Year", ylab = ylab, type = 'l', lty = 1,
          xlim = c(min(yr_cal), max(yr_cal) + 0.1 * length(yr_cal)))
  text(max(yr_cal), sched[sim, age_plot + 1, length(yr_cal)], labels = age_plot, col = 1:6, pos = 4)
  abline(v = MSEhist@OM@CurrentYr, lty = 3)
  title(paste0("Simulation #", sim))

  tsplot(sched[, , yr_plot], age, xlab = "Age", ylab = ylab, ymax = 1.1 * max(sched[, , yr_plot]))
  title(paste("Year", yr_cal[yr_plot]))

  invisible()
}

#' @rdname plot-Hist
#' @details \code{hist_bio_change} plots alternative projection dynamics by changing either the mean or slope.
#' @param var A string to indicate which object to plot from OM@@cpars.
#' @param change_mean The percent change in the mean of the new parameters relative to the old.
#' @param change_slope The percent change year-over-year in the projection parameters relative to the last projection year.
#' @param figure Logical, whether to return a figure (TRUE) or the updated array (FALSE) for \code{hist_bio_change}.
#' @export
hist_bio_change <- function(x, var = c("Wt_age", "M_ageArray"), change_mean = 0, change_slope = 0,
                            n_age_plot = 10, sim = 1, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  var <- match.arg(var)
  labs <- c(Wt_age = "Weight at age", M_ageArray = "Natural mortality")
  ylab <- labs[match(var, names(labs))]

  OM <- MSEhist@OM
  sched <- sched_change <- getElement(MSEhist@SampPars$Stock, var)

  yr_cal <- 1:(OM@nyears + OM@proyears) - OM@nyears + OM@CurrentYr

  if(change_mean) {
    sched_change[, , OM@nyears + 1:OM@proyears] <- (1 + change_mean) * sched[, , OM@nyears + 1:OM@proyears]
  }
  if(change_slope) {
    slope_array <- array((1 + change_slope) ^ c(1:OM@proyears), c(OM@proyears, OM@maxage + 1, OM@nsim)) %>%
      aperm(3:1)
    slope_sched <- sched[, , OM@nyears] %>% array(c(OM@nsim, OM@maxage + 1, OM@proyears))
    sched_change[, , OM@nyears + 1:OM@proyears] <- slope_sched * slope_array
  }

  if(figure) {
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

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(1, 2), mai = c(0.9, 0.9, 0.6, 0.1), omi = c(0, 0, 0, 0))

    matplot(yr_cal, t(sched[sim, age_plot + 1, ]), xlab = "Year", ylab = ylab, type = 'l', lty = 1,
            xlim = c(min(yr_cal), max(yr_cal) + 0.1 * length(yr_cal)))
    text(max(yr_cal), sched[sim, age_plot + 1, length(yr_cal)], labels = age_plot, col = 1:6, pos = 4)
    abline(v = MSEhist@OM@CurrentYr, lty = 3)
    title(paste0("Current operating model\nSimulation #", sim))

    matplot(yr_cal, t(sched_change[sim, age_plot + 1, ]), xlab = "Year", ylab = ylab, type = 'l', lty = 1,
            xlim = c(min(yr_cal), max(yr_cal) + 0.1 * length(yr_cal)))
    text(max(yr_cal), sched_change[sim, age_plot + 1, length(yr_cal)], labels = age_plot, col = 1:6, pos = 4)
    abline(v = MSEhist@OM@CurrentYr, lty = 3)
    title(paste0("Updated operating model\nSimulation #", sim))
  }
  invisible(sched_change)
}

#' @export
hist_resample_recruitment <- function(x, dist = c("Lognormal", "Pareto"), LnSD = 0.7, LnAC = 0, Pshape = 1.1,
                                      figure = TRUE, nsim_plot = 5) {
  dist <- match.arg(dist)

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  Perr_y <- MSEhist@SampPars$Stock$Perr_y
  nsim_plot <- min(nsim_plot, nrow(Perr_y))

  if(dist == "Lognormal") {
    Perr_new <- MSEtool:::sample_recruitment(Perr_hist = log(Perr_y[, 1:(MSEhist@OM@nyears+MSEhist@OM@maxage)]),
                                             proyears = MSEhist@OM@proyears, procsd = LnSD, AC = LnAC)
    Perr_new <- exp(Perr_new)
  } else {
    Perr_new <- sample_pareto(nsim = nrow(Perr_y), proyears = MSEhist@OM@proyears, shape = Pshape)
  }

  Perr_y[, MSEhist@OM@nyears + MSEhist@OM@maxage + 1:MSEhist@OM@proyears] <- Perr_new

  if(figure) {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mfrow=c(1,2),mai=c(0.9,0.9,0.6,0.1),omi=c(0,0,0,0))

    yrs_hist <- MSEhist@OM@CurrentYr - (MSEhist@OM@nyears+MSEhist@OM@maxage):1 + 1
    yrs_proj <- MSEhist@OM@CurrentYr + 1:MSEhist@OM@proyears
    y <- c(yrs_hist, yrs_proj)

    cur_dev <- MSEhist@TSdata$RecDev[1:nsim_plot, ]
    matplot(y, t(cur_dev), lty = 1, type = "l",
            ylim = c(0, 1.1 * max(cur_dev)), xlab = "Year", ylab = "Recruitment deviations")
    abline(h = 0, col = "grey")
    abline(h = 1, lty = 3)
    abline(v = MSEhist@OM@CurrentYr, lty = 3)
    title(paste("Current operating model\n", nsim_plot, "simulations"))

    new_dev <- Perr_y[1:nsim_plot, ]
    matplot(y, t(new_dev), lty = 1, type = "l",
            ylim = c(0, 1.1 * max(new_dev)), xlab = "Year", ylab = "Recruitment deviations")
    abline(h = 0, col = "grey")
    abline(h = 1, lty = 3)
    abline(v = MSEhist@OM@CurrentYr, lty = 3)
    title(paste("Updated operating model\n", nsim_plot, "simulations"))
  }

  invisible(Perr_y)
}

#' @rdname plot-Hist
#' @details \code{hist_growth_I} plots histograms of von Bertalanffy parameters.
#' @export
hist_growth_I <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  plot('Growth', MSEhist, plot.num=1)
}

#' @rdname plot-Hist
#' @details \code{hist_growth_II} plots histograms of von Bertalanffy parameters by year.
#' @export
hist_growth_II <- function(x) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  plot('Growth', MSEhist, plot.num=2)
}

#' @rdname plot-Hist
#' @details \code{hist_spatial} plots histograms of the parameters for spatial movement in a two-area model (set all to 0.5 to functionally create a single area model).
#' @param type Type of spatial plot
#' @param ... arguments to \link[MSEtool]{plot_mov}
#' @export
hist_spatial <- function(x, type = c("par", "matrix", "all"), ...) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  type <- match.arg(type)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  if(type == "par") {
    plot('Spatial', MSEhist)
  } else {
    plot_mov(MSEhist@SampPars$Stock$mov, type = type, ...)
  }

}

#' @rdname plot-Hist
#' @details \code{hist_sel} plots selectivity/retention at age for two different years in the OM.
#' @param yr A length-2 vector for the years (relative to OM@@CurrentYr) to plot selectivity.
#' @param maturity Logical, whether to plot maturity along with selectivity.
#' @export
hist_sel <- function(x, yr, maturity = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  if(missing(yr)) {
    yr <- c(MSEhist@OM@CurrentYr - MSEhist@OM@nyears + 1, MSEhist@OM@CurrentYr)
  } else if(length(yr) != 2) {
    stop("yr must be a length two vector")
  }
  yind <- yr - MSEhist@OM@CurrentYr + MSEhist@OM@nyears # Length 2 vector

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
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

#' @rdname plot-Hist
#' @details \code{hist_YieldCurve} plots the yield curve.
#' @param yr_bio The year (relative to OM@@CurrentYr) for the biological parameters (growth, M, maturity).
#' @param yr_sel The year (relative to OM@@CurrentYr) for the selectivity parameters.
#' @param F_range Length two vector for the range of F to plot the yield curve. By default, \code{c(1e-8, 3 * max(M))}.
#' @export
hist_YieldCurve <- function(x, yr_bio, yr_sel, F_range) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

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
  YC <- lapply(1:MSEhist@OM@nsim, function(x) {
    sapply(log(F_search), function(y) {
      MSEtool:::MSYCalcs(y, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
                         Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                         V_at_Age = V[x, ], maxage = StockPars$maxage,
                         R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
                         SSBpR = StockPars$SSBpR[x, 1],
                         opt = 2, plusgroup = StockPars$plusgroup)
    })
  })

  SPR_F <- vapply(1:MSEhist@OM@nsim, function(x) {
    vapply(F_search, function(y) {
      MSEtool:::Ref_int_cpp(y, M_at_Age = M[x, ],
                            Wt_at_Age = Wt_age[x, ], Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
                            V_at_Age = V[x, ], maxage = StockPars$maxage,
                            plusgroup = StockPars$plusgroup)[2, ]
    }, numeric(1))
  }, numeric(length(F_search)))

  Y <- sapply(YC, function(x) x["Yield", ])
  SSB <- sapply(YC, function(x) x["SB", ])
  SSB_SSB0a <- sapply(YC, function(x) x["SB_SB0", ])

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(2, 2), mai = c(0.9, 0.9, 0.2, 0.1), omi = c(0, 0, 0, 0))
  cols <- list(colm="darkgreen",col50='lightgreen',col90='#40804025')

  tsplot(t(Y),yrs=F_search,xlab="Fishing mortality",ylab="Yield",cols = cols)
  tsplot(t(Y),yrs=t(SPR_F),xlab="Spawning potential ratio (SPR)",ylab="Yield",cols = cols)
  tsplot(t(Y),yrs=t(SSB),xlab="Spawning biomass (SSB)",ylab="Yield",cols = cols)
  tsplot(t(Y),yrs=t(SSB_SSB0a),xlab=expression(SSB~"/"~"Asymptotic"~SSB[0]),ylab="Yield",cols = cols)

}

