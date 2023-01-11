



plotquant<-function(x,p=c(0.05,0.25,0.75,0.95), yrs, cols=list(colm="dark blue", col50='light blue', col90='#60859925'), addline=T, ablines=NA){

  x[is.infinite(x)]<-NA
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

plotquant2 <- function(x, p = c(0.05,0.25,0.75,0.95), yrs) { # For ggplot

  x[is.infinite(x)]<-NA
  qs <- apply(x, 2, quantile, p = p[c(1,4)], na.rm = TRUE, type = 3)
  qsi <- apply(x, 2, quantile, p = p[2:3], na.rm = TRUE, type = 3)

  if(is.matrix(yrs)) {
    ny <- ncol(yrs)

    qs_yr <- apply(yrs, 2, quantile, p = p[c(1,4)], na.rm = TRUE, type = 3)
    qsi_yr <- apply(yrs, 2, quantile, p = p[2:3], na.rm = TRUE, type = 3)

    poly_outer <- data.frame(x = c(qs_yr[1, ], qs_yr[2, ny:1]), y = c(qs[1,], qs[2,ny:1]),
                             Quantile = paste0(100 * (p[4]-p[1]), "th percentile"))
    poly_inner <- data.frame(x = c(qsi_yr[1, ], qsi_yr[2, ny:1]), y = c(qsi[1,], qsi[2,ny:1]),
                             Quantile = paste0(100 * (p[3]-p[2]), "th percentile"))

    med = data.frame(x = apply(yrs, 2, median, na.rm = TRUE), y = apply(x, 2, median, na.rm = TRUE))

  } else {
    ny<-length(yrs)
    poly_outer <- data.frame(x = c(yrs, yrs[ny:1]), y = c(qs[1,], qs[2,ny:1]),
                             Quantile = paste0(100 * (p[4]-p[1]), "th percentile"))
    poly_inner <- data.frame(x = c(yrs, yrs[ny:1]), y = c(qsi[1,], qsi[2,ny:1]),
                             Quantile = paste0(100 * (p[3]-p[2]), "th percentile"))

    med = data.frame(x = yrs, y = apply(x, 2, median, na.rm = TRUE))
  }

  list(med = med, poly_outer = poly_outer, poly_inner = poly_inner)
}


tsplot<-function(x,yrs,xlab="",ylab="",zeroyint=TRUE,cols=list(colm="dark blue", col50='light blue', col90='#60859925'),
                 ymax = NULL){
  x[is.infinite(x)] <- NA_real_
  ymin <- ifelse(zeroyint, 0, 0.9 * min(x, na.rm = TRUE))
  if(is.null(ymax)) ymax <- 1.1 * max(x, na.rm = TRUE)
  plot(range(yrs), c(ymin, ymax), typ = "n",xlab=xlab,ylab=ylab,yaxs='i')
  abline(h=pretty(seq(ymin, ymax, length.out = 20)), col = "light grey")
  plotquant(x,yrs=yrs,cols=cols)

}

#' @importFrom dplyr filter
Rmax_regression <- function(R, SSB, S50, type = c("low", "high")) {
  type <- match.arg(type)
  if(type == "low") {
    df <- data.frame(R = R, SSB = SSB) %>% dplyr::filter(SSB < S50)
  } else {
    df <- data.frame(R = R, SSB = SSB) %>% dplyr::filter(SSB >= S50)
  }
  if(nrow(df) > 3) {
    reg <- lm(log(R) ~ log(SSB), data = df)
    df$predict_logR <- predict(reg)
    return(df)
  } else {
    return(NULL)
  }
}

make_df <- function(x, Year, probs = c(0.25, 0.5, 0.75)) {
  xx <- t(apply(x, 2, quantile, probs = probs))
  structure(xx, dimnames = list(Year, c("Lower quartile", "Median", "Upper quartile")))
}

stock_recruit_int <- function(MSEhist) {
  yrs <- MSEhist@OM@CurrentYr - MSEhist@OM@nyears:1 + 1

  R <- predR_y <- apply(MSEhist@AtAge$Number[, 1, , ], 1:2, sum)
  SSB <- apply(MSEhist@TSdata$SBiomass, 1:2, sum)

  predSSB <- seq(0, 1.1 * max(SSB), length.out = 100)
  predR <- matrix(0, MSEhist@OM@nsim, length(predSSB))

  SRrel <- unique(MSEhist@OM@SRrel)
  R0 <- MSEhist@SampPars$Stock$R0
  hs <- MSEhist@SampPars$Stock$hs
  phi <- MSEhist@SampPars$Stock$SSBpR[, 1]

  aR <- MSEhist@SampPars$Stock$aR[, 1]
  bR <- 1/rowSums(1/MSEhist@SampPars$Stock$bR)
  for(i in 1:MSEhist@OM@nsim) {
    if(SRrel == 1) {
      predR[i, ] <- 4 * R0[i] * hs[i] * predSSB/(phi[i] * R0[i] * (1 - hs[i]) + (5 * hs[i] - 1) * predSSB)
      predR_y[i, ] <- 4 * R0[i] * hs[i] * SSB[i, ]/(phi[i] * R0[i] * (1 - hs[i]) + (5 * hs[i] - 1) * SSB[i, ])
    } else {
      predR[i, ] <- aR[i] * predSSB * exp(-bR[i] * predSSB)
      predR_y[i, ] <- aR[i] * SSB[i, ] * exp(-bR[i] * SSB[i, ])
    }
  }
  list(R = R, SSB = SSB, predR_y = predR_y, predR = predR, predSSB = predSSB, yrs = yrs)
}


calculate_SSB50 <- function(MSEhist) {
  if(all(MSEhist@OM@SRrel == 1)) { # Calculate 50% maximum recruitment from the S-R function and corresponding SSB (S50) Myers et al. 1994
    Rmax <-  4 * MSEhist@SampPars$Stock$R0 * MSEhist@SampPars$Stock$hs / (5 * MSEhist@SampPars$Stock$hs - 1)
    Rmax50 <- 0.5 * Rmax
    S50 <- MSEhist@SampPars$Stock$SSBpR[, 1] * MSEhist@SampPars$Stock$R0 * (1 - MSEhist@SampPars$Stock$hs) /
      (5 * MSEhist@SampPars$Stock$hs - 1)
  } else {
    aR <- MSEhist@SampPars$Stock$aR[, 1]
    phi <- MSEhist@SampPars$Stock$SSBpR[, 1]
    bR <- log(aR * phi)/MSEhist@Ref$ReferencePoints$SSB0

    Rmax <- aR/bR/exp(1)
    Rmax50 <- 0.5 * Rmax
    S50 <- 0.231961/bR
  }
  list(Rmax = Rmax, Rmax50 = Rmax50, SSB50 = S50)
}

#' @importFrom EnvStats rpareto
sample_pareto <- function(nsim, proyears, shape = 1.1, mu = 1, seed) {
  if(!missing(seed)) set.seed(seed)
  rpar <- generate_pareto_par(shape, mu)
  Perr_proj <- EnvStats::rpareto(nsim * proyears, location = rpar$location, shape = shape) %>%
    matrix(nsim, proyears, byrow = TRUE)
  return(Perr_proj)
}

generate_pareto_par <- function(shape, mu = 1) {
  shape <- max(shape, 1.001)
  if(shape <= 1) { #warning("Mean is infinite because shape <= 1.")
    mu <- Inf
    location <- NA
  } else {
    #mu <- 1
    location <- mu * (shape - 1) / shape
  }

  if(shape <= 2) { #warning("Variance is infinite because shape <= 2.")
    variance <- Inf
  } else {
    variance <- location * shape / (shape - 1) / (shape - 1) / (shape - 2)
  }

  return(list(mu = mu, location = location, variance = variance))
}

MSYCalcs <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, Fec_at_Age, V_at_Age, maxage,
                     relRfun, SRRpars,
                     R0x = 1, SRrelx = 4L, hx = 1, SSBpR = 0, opt = 1L, plusgroup = 1L) {

  if (packageVersion("MSEtool") >= "3.6.1") {

    if(missing(relRfun)) {
      relRfun <- function(...) invisible()
      SRRpars <- data.frame()
    }

    MSEtool:::MSYCalcs(logF = logF, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age,
                       Mat_at_Age = Mat_at_Age, Fec_at_Age = Fec_at_Age,
                       V_at_Age = V_at_Age, maxage = maxage,
                       relRfun = relRfun,
                       SRRpars = SRRpars,
                       R0x = R0x,
                       SRrelx = SRrelx, hx = hx,
                       SSBpR = SSBpR,
                       opt = opt, plusgroup = plusgroup)

  } else if(missing(relRfun)) {

    MSEtool:::MSYCalcs(logF = logF, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age,
                       Mat_at_Age = Mat_at_Age, Fec_at_Age = Fec_at_Age,
                       V_at_Age = V_at_Age, maxage = maxage,
                       R0x = R0x,
                       SRrelx = SRrelx, hx = hx,
                       SSBpR = SSBpR,
                       opt = opt, plusgroup = plusgroup)

  } else {
    stop("Need to update to MSEtool 3.6.1")
  }
}


# #' @importFrom changepoint cpt.mean
# do_cp <- function(x, type = c("SP", "SPB"), ncp = 1, med_only = FALSE, figure = TRUE) {
#   if(inherits(x, "reactivevalues")) {
#     MSEhist <- x$MSEhist
#   } else {
#     MSEhist <- x
#   }
#   type <- match.arg(type)
#   nyh<-MSEhist@OM@nyears
#   hy<-MSEhist@OM@CurrentYr - (nyh:1) + 1
#
#   if(type == "SP") {
#     B <- apply(MSEhist@TSdata$Biomass,1:2,sum)
#     catch <- apply(MSEhist@TSdata$Removals,1:2,sum)
#     ind1<-2:nyh-1
#     ind2<-2:nyh
#
#     #SP<-B[, ind2]-B[, ind1]+catch[, ind1]
#     #SPB <- SP/B[, ind1]
#     #medSP<-apply(SP, 2, median)
#     #medB<-apply(B[, ind1], 2, median)
#     #medSPB<-apply(SP/B[, ind1], 2, median)
#     #yr_lab <- seq(1, nyh, by = 5)
#
#     y1 <- B[, ind2]-B[, ind1]+catch[, ind1] # SP
#     ylab <- "Surplus production"
#
#     yr <- hy[-length(hy)]
#
#   } else if(type == "SPB") {
#     B <- apply(MSEhist@TSdata$Biomass,1:2,sum)
#     catch <- apply(MSEhist@TSdata$Removals,1:2,sum)
#     ind1<-2:nyh-1
#     ind2<-2:nyh
#
#     y1 <- B[, ind2]-B[, ind1]+catch[, ind1] # SP
#     y1 <- y1/B[, ind1]
#     ylab <- "Surplus production / Biomass"
#
#     yr <- hy[-length(hy)]
#   }
#   y1m <- apply(y1, 2, median)
#
#   if(med_only) {
#     cp <- changepoint::cpt.mean(y1m, method = "BinSeg", penalty = "AIC", Q = ncp)
#
#     if(figure) {
#       plot(yr, y1m, typ = "o", xlab = "Year", ylab = ylab)
#       means <- cp@param.est$mean
#       nseg <- length(means)
#       cpts.to.plot <- c(0, cp@cpts)
#       for (i in 1:nseg) {
#         segments(yr[cpts.to.plot[i] + 1], means[i], yr[cpts.to.plot[i + 1]], means[i], col = "red")
#       }
#     }
#
#   } else {
#     cp <- changepoint::cpt.mean(y1, method = "BinSeg", penalty = "AIC", Q = ncp)
#
#     cp_vals <- sapply(cp, function(x) {
#       cpts.to.plot <- c(0, x@cpts)
#       means <- x@param.est$mean
#       means[findInterval(1:max(cpts.to.plot) - 1, cpts.to.plot)]
#     })
#
#     tsplot(y1, yr, xlab = "Year", ylab = ylab, zeroyint = FALSE)
#     lines(yr, apply(cp_vals, 1, median), col = "red", lwd = 3)
#   }
#   return(invisible(cp))
# }
