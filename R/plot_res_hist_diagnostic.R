



#' @rdname plot-Hist-diagnostic
#' @details \code{hist_Rmax} returns either \code{SSB50\%Rmax}, the SSB corresponding to 50% of maximum recruitment from the stock-recruit relationship
#' (with regression lines of recruits vs. spawners above and below this value), or the annual probability of exceeding some ratio of
#' \code{SSB50\%Rmax}.
#' @export
hist_Rmax <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
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

  # Regression
  reg_low <- lapply(1:MSEhist@OM@nsim, function(i) Rmax_regression(R = out$R[i, ], SSB = out$SSB[i, ], S50 = S50[i], type = "low"))
  reg_hi <- lapply(1:MSEhist@OM@nsim, function(i) Rmax_regression(R = out$R[i, ], SSB = out$SSB[i, ], S50 = S50[i], type = "high"))

  if(figure) {

    if(is.na(prob_ratio)) {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

      # Plot stock-recruit relationship with SSB 50%Rmax
      matplot(out$SSB, out$R, type = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
              xlab = "Spawning biomass", ylab = "Recruitment", pch = 4)
      plotquant(out$predR, yrs = out$predSSB, addline=T)
      points(medSSB, medR, pch = 19)
      abline(v = S50, col = "#99999920", lty = 2)
      abline(v = median(S50), lty = 2, lwd = 2)
      abline(h = 0, col = "grey")
      legend("topright", c("All Sims", "Median", expression(SSB["50%"~Rmax])), pch = c(4, 16, NA),
             lty = c(NA, NA, 2), lwd = c(NA, NA, 2), bty = "n")

      # Plot regression line
      matplot(log(out$SSB), log(out$R), type = "p", col = "#99999920", xlim = range(c(out$SSB, S50)) %>% log(),
              xlab = "log(Spawning biomass)", ylab = "log(Recruitment)", pch = 4)
      points(log(medSSB), log(medR), pch = 19)
      abline(v = log(S50), col = "#99999920", lty = 2)
      abline(v = median(S50) %>% log(), lty = 2, lwd = 2)
      lapply(1:MSEhist@OM@nsim, function(i) {
        if(!is.null(reg_hi[[i]])) lines(predict_logR ~ log(SSB), data = reg_hi[[i]], lty = 2) #col = "#99999920")
        if(!is.null(reg_low[[i]])) lines(predict_logR ~ log(SSB), data = reg_low[[i]], lty = 2) #col = "#99999920")
      })
    } else {

      data.frame(Year = out$yrs, pvec = apply(out$SSB/S50 > prob_ratio, 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y = parse(text = paste0("Probability~SSB/SSB[\"50%\"~Rmax]>", prob_ratio))) +
        ggtitle(parse(text = paste0("Probability~SSB/SSB[\"50%\"~Rmax]>", prob_ratio)))
    }

  } else {

    if(is.na(prob_ratio)) {
      slope_high <- vapply(1:MSEhist@OM@nsim, function(i) {
        if(!is.null(reg_hi[[i]])) {
          diff(range(log(reg_hi[[i]]$predict_logR)))/diff(range(log(reg_hi[[i]]$SSB)))
        } else {
          NA_real_
        }
      }, numeric(1))
      slope_low <- vapply(1:MSEhist@OM@nsim, function(i) {
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

    } else {

      pvec <- apply(out$SSB/S50 > prob_ratio, 2, mean)
      return(structure(matrix(pvec, ncol = 1),
                       dimnames = list(out$yrs, c("Probability"))))

    }
  }
  invisible()
}

#' @rdname plot-Hist-diagnostic
#' @details \code{hist_RpS90} returns either \code{SSB 90\%ile R/S}, the SSB corresponding to the intersection of the 90the percentile of both historical recruitment
#' and recruits-per-spawner, or the annual probability of exceeding some ratio of \code{SSB 90\%ile R/S}.
#' @export
#' @export
hist_RpS90 <- function(x, figure = TRUE, prob_ratio = NA, prob_ylim = c(0, 1)) {
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

  if(figure) {

    if(is.na(prob_ratio)) {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par))
      par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

      # Plot stock-recruit relationship
      matplot(out$SSB, out$R, type = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
              xlab = "Spawning biomass", ylab = "Recruitment", pch = 4)
      plotquant(out$predR, yrs = out$predSSB, addline=T)
      points(medSSB, medR, pch = 19)
      abline(v = S_90, lwd = 2, lty = 2, col = "#99999920")
      abline(v = median(S_90), lwd = 2, lty = 2)
      abline(h = 0, col = "grey")

      matplot(out$SSB, out$R, type = "p", col = "#99999920", xlim = c(0, max(out$SSB)), ylim = c(0, max(out$R)),
              xlab = "Spawning biomass", ylab = "Recruitment", pch = 4)
      #plotquant(out$predR, yrs = out$predSSB, addline=T)
      points(medSSB, medR, pch = 19)
      abline(a = 0, b = median(RpS_90), lwd = 2, lty = 2, col = "red")
      abline(h = median(R_90), lwd = 2, lty = 2, col = "blue")

      lapply(RpS_90, function(x) abline(a = 0, b = x, lwd = 2, lty = 2, col = makeTransparent("red", 20)))
      lapply(R_90, function(x) abline(h = x, lwd = 2, lty = 2, col = makeTransparent("blue", 20)))

      legend("topright", c("All sims", "Median", paste("90%ile", c("R/S", "R", "SSB"))),
             col = c("black", "black", "red", "blue", "black"), pch = c(4, 16, NA, NA, NA),
             lwd = c(NA, NA, 2, 2, 2), lty = c(NA, NA, 4, 4, 4), bty = "n")
      abline(h = 0, col = "grey")
    } else {

      data.frame(Year = out$yrs, pvec = apply(out$SSB/S_90 > prob_ratio, 2, mean)) %>%
        ggplot(aes(Year, pvec)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        coord_cartesian(ylim = prob_ylim) +
        labs(y = parse(text = paste0("Probability~SSB/SSB[\"90%ile\"~R/S]>", prob_ratio))) +
        ggtitle(parse(text = paste0("Probability~SSB/SSB[\"90%ile\"~R/S]>", prob_ratio)))
    }

  } else {

    if(is.na(prob_ratio)) {

      out <- lapply(list(RpS_90, R_90, S_90), quantile, probs = c(0.25, 0.5, 0.75))
      out <- data.frame(do.call(rbind, out), row.names = c("90%ile R/S", "90%ile Recruitment", "90%ile SSB"))
      names(out) <- c("25%ile", "Median", "75%ile")
      return(out)

    } else {

      pvec <- apply(out$SSB/S_90 > prob_ratio, 2, mean)
      return(structure(matrix(pvec, ncol = 1),
                       dimnames = list(out$yrs, c("Probability"))))

    }

  }
  invisible()
}

