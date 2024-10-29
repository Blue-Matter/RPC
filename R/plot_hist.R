
#' @name plot_hist
#' @title Plot historical dynamics of the operating model. See details below.
#' @description Various plots for plotting historical time series for the operating model.
#' @param x An object of class \linkS4class{Hist}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEhist} which is
#' the Hist object.
#' @param figure Logical, whether to generate a figure or just return a list of values (invisibly).
#' @return Various plots using base graphics. Returns invisibly a named list, where each entry is usually a matrix with rows indexing simulation
#' and columns indexing year.
#' @examples
#' library(MSEtool)
#' Hist <- MSEtool::runMSE(Hist = TRUE, silent = TRUE)
#' bio <- hist_bio(Hist)
#' bio$SBiomass
#' @author Q. Huynh
NULL

#' @rdname plot_hist
#' @details \code{hist_bio} returns time series of spawning biomass (SBiomass), total biomass (Biomass), abundance (Number),
#' recruitment (Rec), and recruitment deviates (RecDev).
#' @export
hist_bio<-function(x, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  bio <- list(Year = MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1,
              SBiomass = apply(MSEhist@TSdata$SBiomass,1:2,sum),
              Biomass = apply(MSEhist@TSdata$Biomass,1:2,sum),
              Number = apply(MSEhist@TSdata$Number,1:2,sum),
              VBiomass = apply(MSEhist@TSdata$VBiomass,1:2,sum),
              Rec = apply(MSEhist@AtAge$Number[,1,,],1:2,sum),
              Year_RecDev = MSEhist@OM@CurrentYr - (MSEhist@OM@nyears+MSEhist@OM@maxage):1 + 1,
              RecDev = log(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage)])
              )

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mfcol=c(2,3),mai=c(0.3,0.6,0.2,0.1),omi=c(0.6,0,0,0))
    tsplot(bio$SBiomass, bio$Year, xlab="Historical Year", ylab="Spawning biomass")
    tsplot(bio$Biomass, bio$Year, xlab="Historical Year", ylab="Biomass")
    tsplot(bio$Number, bio$Year, xlab="Historical Year", ylab="Numbers")
    tsplot(bio$VBiomass, bio$Year, xlab="Historical Year", ylab="Vulnerable Biomass")

    tsplot(bio$RecDev, bio$Year_RecDev,
           xlab="Historical Year", ylab="Recruitment strength", zeroyint=F)
    abline(h = 0, lty = 3)
    tsplot(bio$Rec, bio$Year, xlab="Historical Year", ylab="Recruitment")

  }

  invisible(bio)
}

#' @rdname plot_hist
#' @details \code{hist_future_recruit} returns historical and future recruitment deviations.
#' @export
hist_future_recruit <- function(x, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  bio <- list(Year_Hist = MSEhist@OM@CurrentYr - (MSEhist@OM@nyears+MSEhist@OM@maxage):1 + 1,
              RecDev_Hist = log(MSEhist@TSdata$RecDev[,1:(MSEhist@OM@nyears+MSEhist@OM@maxage)]),
              Year_Proj = MSEhist@OM@CurrentYr + 1:MSEhist@OM@proyears,
              RecDev_Proj = log(MSEhist@TSdata$RecDev[,-c(1:(MSEhist@OM@nyears+MSEhist@OM@maxage))])
              )

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mfrow=c(2,2),mai=c(0.9,0.9,0.2,0.1),omi=c(0,0,0,0))

    tsplot(bio$RecDev_Hist, bio$Year_Hist,
           xlab="Historical Year", ylab="Log-recruitment deviation", zeroyint=F)
    abline(h = 0, lty = 3)

    tsplot(bio$RecDev_Proj, bio$Year_Proj,
           xlab="Projection Year", ylab="Log-recruitment deviation", zeroyint=F)
    abline(h = 0, lty = 3)

    hist_mean <- apply(exp(bio$RecDev_Hist), 2, mean)
    plot(bio$Year_Hist, hist_mean,
         xlab = "Historical Year", ylab = "Annual mean deviation\n(normal space)", typ = "o", pch = 16,
         ylim = c(0, 1.1 * max(hist_mean)))
    abline(h = 0, col = "grey")
    abline(h = 1, lty = 3)

    pro_mean <- apply(exp(bio$RecDev_Proj), 2, mean)
    plot(bio$Year_Proj, pro_mean,
         xlab = "Projection Year", ylab = "Annual mean deviation\n(normal space)", typ = "o", pch = 16,
         yli = c(0, 1.1 * max(pro_mean)))
    abline(h = 0, col = "grey")
    abline(h = 1, lty = 3)
  }

  invisible(bio)

}


#' @rdname plot_hist
#' @details \code{hist_bio_schedule} plots in biological at age parameters. The corresponding array is indexed by simulation, age, and year.
#' @param var A string to indicate which object to plot from OM@@cpars.
#' @param n_age_plot The number of ages to plot in the left figure.
#' @param yr_plot The year (relative to OM@@CurrentYr) to plot for the right figure.
#' @param sim The individual simulation to plot for the left figure.
#' @export
hist_bio_schedule <- function(x, var = c("Len_age", "Wt_age", "Mat_age", "M_ageArray"), n_age_plot, yr_plot, sim,
                              figure = TRUE) {
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

  if(var == "Mat_age") {
    Fec <- MSEhist@SampPars$Stock$Fec_Age
    CalcProd <- MSEhist@SampPars$Stock$Mat_age * MSEhist@SampPars$Stock$Wt_age
    if(!identical(Fec[, -1, ], CalcProd[, -1, ])) {
      sched <- Fec
      ylab <- "Fecundity"
    }
  }

  yr_cal <- 1:(OM@nyears + OM@proyears) - OM@nyears + OM@CurrentYr

  bio <- list(yr_cal, sched) %>% structure(names = c("Year", var))

  if(figure) {
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
  }

  invisible(bio)
}

#' @rdname plot_hist
#' @details \code{hist_bio_change} plots alternative projection dynamics by changing either the mean or slope. Returns an updated array of
#' parameters.
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

  invisible(list(yr_cal, sched_change) %>% structure(names = c("Year", var)))
}


#' @rdname plot_hist
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

#' @rdname plot_hist
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

#' @rdname plot_hist
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

#' @rdname plot_hist
#' @details \code{hist_sel} plots selectivity/retention at age for two different years in the OM. Returns an array indexed by simulation,
#' age, year.
#' @param yr A length-2 vector for the years (relative to OM@@CurrentYr) to plot selectivity.
#' @param maturity Logical, whether to plot maturity along with selectivity.
#' @export
hist_sel <- function(x, yr, maturity = TRUE, figure = TRUE) {
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

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    #par(mfcol=c(3,2),mai=c(0.3,0.6,0.3,0.1),omi=c(0.5,0,0,0))
    par(mai=c(0.3,0.6,0.3,0.1),omi=c(0.5,0,0.2,0))
    if(maturity) {
      layout(matrix(c(1:6, rep(7, 3)), nrow = 3), widths = c(1, 1, 0.5))
    } else {
      layout(matrix(1:6, nrow = 3))
    }
    cols=list(colm="darkgreen",col50='lightgreen',col90='#40804025')

    for(y in 1:length(yind)) {
      # Selectivity with maturity/fecundity
      if(maturity) {
        Fec <- MSEhist@SampPars$Stock$Fec_Age
        CalcProd <- MSEhist@SampPars$Stock$Mat_age * MSEhist@SampPars$Stock$Wt_age
        if(identical(Fec[, -1, ], CalcProd[, -1, ])) {
          mat_plot <- MSEhist@SampPars$Stock$Mat_age[, , yind[y]]
          ylab_sel <- "Vulnerability with maturity"
          legend_sel <- "Maturity"
        } else {
          mat_plot <- Fec[, , yind[y]]/max(Fec[, , yind[y]])
          ylab_sel <- "Vulnerability with\nrelative fecundity"
          legend_sel <- "Fecundity"
        }
      } else {
        ylab_sel <- "Vulnerability"
      }
      tsplot(MSEhist@SampPars$Fleet$V[, , yind[y]], yrs=0:MSEhist@OM@maxage,
             xlab="",ylab= ylab_sel,cols = cols, ymax = 1.1)
      mtext(paste("Year", yr[y]), 3, line = 1, font = 2)
      if(maturity) {
        plotquant(mat_plot, yrs=0:MSEhist@OM@maxage, addline = FALSE)
      }

      # Retention
      tsplot(MSEhist@SampPars$Fleet$retA_real[, , yind[y]], yrs=0:MSEhist@OM@maxage,
             xlab="",ylab="Retention",cols = cols, ymax = 1.1)

      # Realized Selectivity
      tsplot(MSEhist@SampPars$Fleet$V_real[, , yind[y]], yrs=0:MSEhist@OM@maxage,
             xlab="",ylab="Realized Selectivity",cols = cols, ymax = 1.1)

    }
    if(maturity) {
      plot(1, 1, axes = FALSE, typ = "n", ylab = "", xlab = "")
      legend("left", c("Selectivity", legend_sel), col = c("darkgreen", "darkblue"), lwd = 3, cex = 1.5, bty = "n")
      mtext("Age", 1, outer = TRUE, line = 2)
    }
  }

  bio <- list(Year = seq(MSEhist@OM@CurrentYr - MSEhist@OM@nyears + 1, MSEhist@OM@CurrentYr + MSEhist@OM@proyears),
              Vulnerability = MSEhist@SampPars$Fleet$V,
              Retention = MSEhist@SampPars$Fleet$retA_real)

  invisible(bio)
}

#' @rdname plot_hist
#' @details \code{hist_YieldCurve} plots the yield curve as a function of F, SPR (spawning potential ratio), spawning biomass (SBiomass),
#' and spawning depletion (SB_SB0). Matrices are indexed by simulation (rows) and F (columns).
#' @param yr_bio The year (relative to OM@@CurrentYr) for the biological parameters (growth, M, maturity).
#' @param yr_sel The year (relative to OM@@CurrentYr) for the selectivity parameters.
#' @param F_range Length two vector for the range of F to plot the yield curve. By default, \code{c(1e-8, 3 * max(M))}.
#' @export
hist_YieldCurve <- function(x, yr_bio, yr_sel, F_range, figure = TRUE, sims) {
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

  if (missing(sims) || is.null(sims)) {
    sims <- 1:MSEhist@OM@nsim
  } else if (max(sims) > MSEhist@OM@nsim) {
    sims <- sims[sims < MSEhist@OM@nsim]
  }

  if (is.null(StockPars$spawn_time_frac)) StockPars$spawn_time_frac <- rep(0, MSEhist@OM@nsim)

  YC <- lapply(sims, function(x) {
    sapply(log(F_search), function(y) {
      MSYCalcs(y, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
               Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
               V_at_Age = V[x, ], maxage = StockPars$maxage,
               R0x = StockPars$R0[x], SRrelx = StockPars$SRrel[x], hx = StockPars$hs[x],
               SSBpR = StockPars$SSBpR[x, 1],
               opt = 2, plusgroup = StockPars$plusgroup,
               spawn_time_frac = StockPars$spawn_time_frac[x])
    })
  })

  SPR_F <- vapply(sims, function(x) {
    vapply(log(F_search), function(ff) {
      MSYCalcs(ff, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
               Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
               V_at_Age = V[x, ], maxage = StockPars$maxage,
               R0x = 1, SRrelx = 4L, hx = 1,
               SSBpR = 0,
               opt = 2, plusgroup = StockPars$plusgroup,
               spawn_time_frac = StockPars$spawn_time_frac[x])["SB_SB0"]
    }, numeric(1))
  }, numeric(length(F_search)))

  Y <- sapply(YC, function(x) x["Yield", ])
  SSB <- sapply(YC, function(x) x["SB", ])
  SSB_SSB0a <- sapply(YC, function(x) x["SB_SB0", ])

  out <- list(FM = F_search,
              Yield = t(Y),
              SPR = t(SPR_F),
              SBiomass = t(SSB),
              SB_SB0 = t(SSB_SSB0a))

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(2, 2), mai = c(0.9, 0.9, 0.2, 0.1), omi = c(0, 0, 0, 0))
    cols <- list(colm="darkgreen",col50='lightgreen',col90='#40804025')

    tsplot(out$Yield,yrs=F_search,xlab="Fishing mortality",ylab="Yield",cols = cols)
    tsplot(out$Yield,yrs=out$SPR,xlab="Spawning potential ratio (SPR)",ylab="Yield",cols = cols)
    tsplot(out$Yield,yrs=out$SBiomass,xlab="Spawning biomass (SSB)",ylab="Yield",cols = cols)
    tsplot(out$Yield,yrs=out$SB_SB0,xlab=expression(SSB~"/"~"Equilibrium"~SSB[0]),ylab="Yield",cols = cols)
  }

  invisible(out)

}



#' @rdname plot_hist
#' @details \code{hist_resample_recruit} generates new recruitment deviations (with mean = 1) for the projection period. Returns
#' a list with an updated matrix for \code{OM@cpars$Perr_y}.
#' @param dist Character to denote to sample either from a lognormal distribution or Pareto distribution.
#' @param mu The mean of the distribution (default = 1).
#' @param LnSD If Lognormal, the standard deviation.
#' @param LnAC If Lognormal, the autocorrelation (in log-space).
#' @param Pshape If Pareto, the shape parameter. See \link[EnvStats]{Pareto}. The location parameter is calculated such that the mean = 1.
#' @param nsim_plot The number of simulations to plot if figure is TRUE.
#' @export
hist_resample_recruitment <- function(x, dist = c("Lognormal", "Pareto"), mu = 1, LnSD = 0.7, LnAC = 0, Pshape = 1.1,
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
    Perr_new <- exp(Perr_new) * mu
  } else {
    Perr_new <- sample_pareto(nsim = nrow(Perr_y), proyears = MSEhist@OM@proyears, shape = Pshape, mu = mu)
  }

  Perr_y[, MSEhist@OM@nyears + MSEhist@OM@maxage + 1:MSEhist@OM@proyears] <- Perr_new

  yrs_hist <- MSEhist@OM@CurrentYr - (MSEhist@OM@nyears+MSEhist@OM@maxage):1 + 1
  yrs_proj <- MSEhist@OM@CurrentYr + 1:MSEhist@OM@proyears
  y <- c(yrs_hist, yrs_proj)

  bio <- list(Year = y, RecDev = Perr_y)

  if(figure) {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mfrow=c(1,2),mai=c(0.9,0.9,0.6,0.1),omi=c(0,0,0,0))

    cur_dev <- MSEhist@TSdata$RecDev[1:nsim_plot, ]
    matplot(bio$Year, t(cur_dev), lty = 1, type = "l",
            ylim = c(0, 1.1 * max(cur_dev)), xlab = "Year", ylab = "Recruitment deviations")
    abline(h = 0, col = "grey")
    abline(h = 1, lty = 3)
    abline(v = MSEhist@OM@CurrentYr, lty = 3)
    title(paste("Current operating model\n", nsim_plot, "simulations"))

    new_dev <- Perr_y[1:nsim_plot, ]
    matplot(bio$Year, t(new_dev), lty = 1, type = "l",
            ylim = c(0, 1.1 * max(new_dev)), xlab = "Year", ylab = "Recruitment deviations")
    abline(h = 0, col = "grey")
    abline(h = 1, lty = 3)
    abline(v = MSEhist@OM@CurrentYr, lty = 3)
    title(paste("Updated operating model\n", nsim_plot, "simulations"))
  }

  invisible(bio)
}

#' @rdname plot_hist
#' @details \code{hist_SRR_change} re-fits stock recruit function and generates a list with new stock-recruitment parameters \code{OM@SRrel}, \code{OM@cpars$R0}, \code{OM@cpars$hs},
#' and historical recruitment deviations \code{OM@cpars$Perr_y}.
#' @param SR_new A new stock-recruit relationship (1 = Beverton-Holt, 2 = Ricker)
#' @param h_mult Scalar for the new steepness value (a multiple of the old steepness parameter).
#' @param y_fit Length two vector for the range of years of SSB and recruit pairs used to fit the SR function.
#' @param sims A subset of simulations for plotting. Some functions have a low limit by default, i.e. 25, to reduce time to generate plots). Set to \code{NULL} to plot all simulations.
#' @examples
#'
#' # Example of backend use of `hist_SRR_change`
#' OM <- SubCpars(RPC::DFO_4X5Y_Haddock_2015, 1:2)
#' Hist <- runMSE(OM, Hist = TRUE)
#' vars <- hist_SRR_change(Hist, h_mult = 0.6) # Steepness is sixty percent of whatever is in the OM
#'
#' OM@cpars$R0 <- vars$R0
#' OM@cpars$hs <- vars$h
#' OM@SRrel <- vars$SRrel
#' OM@cpars$Perr_y <- Hist@SampPars$Stock$Perr_y
#' OM@cpars$Perr_y[, 1:(OM@maxage + OM@nyears)] <- vars$Perr_y
#' Hist_new <- runMSE(OM, Hist = TRUE)
#' @export
hist_SRR_change <- function(x, SR_new = 1, h_mult = 1, y_fit, figure = TRUE, sims = 1:25) {
  #SR_new <- match.arg(SR_new)
  SRR <- switch(SR_new,
                "1" = "BH",
                "2" = "Ricker")

  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  if(missing(y_fit)) {
    y <- 1:MSEhist@OM@nyears
  } else {
    stopifnot(length(y_fit) == 2)
    stopifnot(diff(y_fit) >= 3)
    y_fit <- seq(y_fit[1], y_fit[2])
    y <- y_fit - MSEhist@OM@CurrentYr + MSEhist@OM@nyears
  }

  SSBpR <- MSEhist@SampPars$Stock$SSBpR[, 1]

  if(all(SR_new == MSEhist@OM@SRrel) && h_mult == 1 && length(y) == MSEhist@OM@nyears) { # Do nothing

    h_new <- MSEhist@SampPars$Stock$hs
    R0_new <- MSEhist@SampPars$Stock$R0
    Perr_y_new <- MSEhist@SampPars$Stock$Perr_y[, 1:(MSEhist@OM@maxage + MSEhist@OM@nyears)]

    Arec_new <- switch(SR_new,
                       "1" = 4 * h_new/(1 - h_new)/SSBpR,
                       "2" = MSEhist@SampPars$Stock$aR[, 1])

  } else {

    SBiomass <- apply(MSEhist@TSdata$SBiomass, 1:2, sum)
    Rec <- apply(MSEhist@AtAge$Number[, 1, , ], 1:2, sum)

    newSR <- lapply(1:MSEhist@OM@nsim, function(x) {
      phi0 <- SSBpR[x]

      h <- MSEhist@SampPars$Stock$hs[x]

      h_new <- h * h_mult
      if(h_new < 0.21) h_new <- 0.21
      if(SR_new == 1 && h_new > 0.99) h_new <- 0.99

      opt <- optimize(SAMtool:::get_SR, interval = c(-100, 100), E = SBiomass[x, y], R = Rec[x, y], EPR0 = phi0,
                      type = SRR, fix_h = TRUE, h = h_new)

      SAMtool:::get_SR(opt$minimum, E = SBiomass[x, ], R = Rec[x, ], EPR0 = phi0, type = SRR,
                       fix_h = TRUE, h = h_new, opt = FALSE)
    })

    h_new <- sapply(newSR, getElement, "h")
    R0_new <- sapply(newSR, getElement, "R0")
    Arec_new <- sapply(newSR, getElement, "Arec")

    Rpred_new <- sapply(newSR, getElement, "Rpred") %>% t()
    RecDev <- Rec/Rpred_new

    Perr_y_new <- local({ # See MSEtool::Assess2OM
      maxage <- MSEhist@OM@maxage
      n_age <- maxage + 1
      nsim <- MSEhist@OM@nsim
      nyears <- MSEhist@OM@nyears

      surv <- sapply(1:nsim, function(x) {
        SAMtool:::calc_NPR(exp(-MSEhist@SampPars$Stock$M_ageArray[x, , 1]), n_age, MSEhist@SampPars$Stock$plusgroup)
      }) %>% t()

      N <- apply(MSEhist@AtAge$Number, 1:3, sum)
      Perr <- array(NA_real_, c(nsim, maxage + nyears))

      Perr[, n_age:1] <- N[, , 1]/(R0_new * surv)
      Perr[, maxage + 2:nyears] <- RecDev[, 2:nyears]
      Perr
    })

  }

  bio <- list(SRrel = SR_new, R0 = R0_new, h = h_new, Perr_y = Perr_y_new)
  bio$Quantile <- cbind(Arec_new, h_new, SSBpR) %>%
    make_df(c("SRR alpha", "Steepness", "Corresponding phi_0")) %>% round(digits = 2)

  if(figure) {

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    par(mfcol=c(2,2),mai=c(0.9,0.9,0.6,0.1),omi=c(0,0,0,0))

    out <- stock_recruit_int(MSEhist)
    medSSB <- apply(out$SSB, 2, median)
    medR <- apply(out$R, 2, median)

    ##### Plot old stock-recruit relationship
    if (missing(sims) || is.null(sims)) {
      sims <- 1:nrow(out$SSB)
    } else if (max(sims) > nrow(out$SSB)) {
      sims <- sims[sims < nrow(out$SSB)]
    }

    SSB_sim <- out$SSB[sims, , drop = FALSE]
    R_sim <- out$R[sims, , drop = FALSE]

    matplot(SSB_sim, R_sim,
            type = "p", col = "#99999920", xlim = c(0, 1.1 * max(medSSB)), ylim = c(0, 1.1 * max(medR)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 4, main = "Current operating model")
    plotquant(out$predR, yrs = out$predSSB, addline=T)
    points(medSSB, medR, pch = 19)
    abline(h = 0, v = 0, col = "grey")

    # log-recruitment deviation
    tsplot(log(MSEhist@SampPars$Stock$Perr_y[, MSEhist@OM@maxage + 1:MSEhist@OM@nyears]),
           yrs=out$yrs,xlab="Year",ylab="Log recruitment deviation",zeroyint=FALSE)
    abline(h = 0, lty = 3)
    title("Current operating model")

    ##### Plot new
    out2 <- local({
      MSEhist2 <- MSEhist
      MSEhist2@SampPars$Stock$SRrel[] <- SR_new
      MSEhist2@SampPars$Stock$R0[] <- R0_new
      MSEhist2@SampPars$Stock$hs[] <- h_new

      MSEhist2@SampPars$Stock$aR[] <- SRalphaconv(h_new, SSBpR, SR = 2)
      MSEhist2@SampPars$Stock$bR <- SRbetaconv(h_new, R0_new, SSBpR, SR = 2) %>% matrix(ncol = 1)
      stock_recruit_int(MSEhist2)
    })

    matplot(SSB_sim, R_sim,
            type = "p", col = "#99999920", xlim = c(0, 1.1 * max(medSSB)), ylim = c(0, 1.1 * max(medR)),
            xlab = "Spawning biomass", ylab = "Recruitment", pch = 4, main = "New operating model")
    plotquant(out2$predR, yrs = out2$predSSB, addline=T)
    points(medSSB, medR, pch = 19)
    abline(h = 0, v = 0, col = "grey")

    if(length(y) < MSEhist@OM@nyears) {
      text(medSSB[y], medR[y], label = y_fit, pos = 3)
    }

    # log-recruitment deviation
    tsplot(log(Perr_y_new[, MSEhist@OM@maxage + 1:MSEhist@OM@nyears]),
           yrs=out$yrs,xlab="Year",ylab="Log recruitment deviation",zeroyint=FALSE)
    abline(h = 0, lty = 3)
    title("New operating model")

  }

  invisible(bio)
}

#' @rdname plot_hist
#' @details \code{hist_phi0} returns a list containing a matrix (by simulation and year)
#' of unfished spawning biomass per recruit (\code{phi0}).
#' @export
hist_phi0 <- function(x, figure = TRUE) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }

  phi0 <- MSEhist@Ref$ByYear$SSB0/MSEhist@Ref$ByYear$R0

  if (any(is.na(phi0))) {
    phi0 <- sapply(1:MSEhist@OM@nsim, function(x) {
      sapply(1:(MSEhist@OM@nyears + MSEhist@OM@proyears), function(y) {
        calc_phi0(surv = exp(-MSEhist@SampPars$Stock$M_ageArray[x, , y]),
                  Fec = MSEhist@SampPars$Stock$Fec_Age[x, , y],
                  plusgroup = MSEhist@SampPars$Stock$plusgroup)
      })
    }) %>% t()
  }

  nyh <- MSEhist@OM@nyears
  hy <- MSEhist@OM@CurrentYr - (nyh:1) + 1
  py <- MSEhist@OM@CurrentYr + 1:MSEhist@OM@proyears

  bio <- list(Year = c(hy, py), phi0 = phi0)

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow=c(1,2),mai=c(0.3,0.9,0.2,0.1),omi=c(0.6,0,0,0))

    tsplot(bio$phi0[, 1:MSEhist@OM@nyears], yrs = hy, xlab = "Year", ylab = expression(phi[0]),
           ymax = 1.1 * max(bio$phi0))
    tsplot(bio$phi0[, MSEhist@OM@nyears + 1:MSEhist@OM@proyears], yrs = py, xlab = "Year", ylab = expression(phi[0]),
           ymax = 1.1 * max(bio$phi0))
    mtext("Year", side = 1, outer = TRUE, line = 1)
  }

  invisible(bio)
}


#' @rdname plot_hist
#' @details \code{hist_per_recruit} returns a list containing a matrix (by simulation and F)
#' of yield per recruit (YPR) and spawning potential ratio (SPR).
#' @export
hist_per_recruit <- function(x, yr_bio, yr_sel, F_range, figure = TRUE, sims) {
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

  if (missing(sims) || is.null(sims)) {
    sims <- 1:MSEhist@OM@nsim
  } else if (max(sims) > MSEhist@OM@nsim) {
    sims <- sims[sims < MSEhist@OM@nsim]
  }

  if (is.null(StockPars$spawn_time_frac)) StockPars$spawn_time_frac <- rep(0, MSEhist@OM@nsim)

  per_recruit <- sapply(sims, function(x) {
    vapply(log(F_search), function(ff) {
      MSYCalcs(ff, M_at_Age = M[x, ], Wt_at_Age = Wt_age[x, ],
               Mat_at_Age = Mat_age[x, ], Fec_at_Age = Fec_age[x, ],
               V_at_Age = V[x, ], maxage = StockPars$maxage,
               R0x = 1, SRrelx = 4L, hx = 1,
               SSBpR = 0,
               opt = 2, plusgroup = StockPars$plusgroup,
               spawn_time_frac = StockPars$spawn_time_frac[x])[c("Yield", "SB_SB0")]
    }, numeric(2))
  }, simplify = "array")

  YPR <- per_recruit["Yield", , ]
  SPR_F <- per_recruit["SB_SB0", , ]
  out <- list(FM = F_search,
              YPR = t(YPR),
              SPR = t(SPR_F))

  if(figure) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(1, 2), mai = c(0.9, 0.9, 0.2, 0.1), omi = c(0, 0, 0, 0))
    cols <- list(colm="darkgreen",col50='lightgreen',col90='#40804025')

    tsplot(out$SPR,yrs=out$FM,xlab="Fishing mortality",ylab="Spawning potential ratio",cols = cols)
    tsplot(out$YPR,yrs=out$FM,xlab="Fishing mortality",ylab="Yield per recruit",cols = cols)
  }

  invisible(out)

}
