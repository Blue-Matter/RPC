



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

plotquant2 <- function(x, p = c(0.05,0.25,0.75,0.95), yrs) { # For ggplot

  x[x==Inf]<-NA
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

  ymin <- ifelse(zeroyint, 0, 0.9 * min(x, na.rm = TRUE))
  if(is.null(ymax)) ymax <- 1.1 * max(x, na.rm = TRUE)
  plot(range(yrs), c(ymin, ymax), typ = "n",xlab=xlab,ylab=ylab,yaxs='i')
  abline(h=pretty(seq(from=ymin,to=max(x)*1.25,length.out=20)),col="light grey")
  plotquant(x,yrs=yrs,cols=cols)

}


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

  SRrel <- MSEhist@SampPars$Stock$SRrel[1]
  R0 <- MSEhist@SampPars$Stock$R0
  hs <- MSEhist@SampPars$Stock$hs
  phi <- MSEhist@SampPars$Stock$SSBpR[, 1]

  aR <- MSEhist@SampPars$Stock$aR[, 1]
  bR <- MSEhist@SampPars$Stock$bR[, 1]
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
