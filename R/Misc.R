
#' @describeIn runMSEhist Modifies an OM with fewer simulations and projection years if desired.
#' @param nsim The number of simulations in the reduced OM.
#' @param proyears The number of projection years in the reduced OM.
#' @export
modOM <- function(OM, nsim, proyears) {
  nsim_full <- OM@nsim
  if(!missing(nsim) && nsim < nsim_full) {
    OM<-MSEtool::SubCpars(OM,sims=1:nsim)
    OM@nsim<-nsim
  }
  proyears_full <- OM@proyears
  if(!missing(proyears) && proyears < proyears_full) { # Add to MSEtool
    OM@proyears <- proyears
    cpars <- OM@cpars
    n_cpars <- names(cpars)

    yr_diff <- proyears_full - proyears
    cpars_out <- lapply(n_cpars, function(xx) {
      x <- cpars[[xx]]
      if(xx %in% c("Asize", "Find", "AddIbeta", "Data")) { # Matrices or arrays without year dimensions
        return(x)
      } else if(xx == "MPA") {
        yr_remove <- (nrow(x) - yr_diff + 1):nrow(x)
        return(x[-yr_remove, ])
      } else if(is.matrix(x)) {
        yr_remove <- (ncol(x) - yr_diff + 1):ncol(x)
        return(x[, -yr_remove])
      } else if(is.array(x)) {

        ldim <- length(dim(x))
        yr_remove <- (dim(x)[ldim] - yr_diff + 1):dim(x)[ldim]

        if(ldim == 3) return(x[, , -yr_remove, drop = FALSE])
        if(ldim == 4) return(x[, , , -yr_remove, drop = FALSE])
        if(ldim == 5) return(x[, , , , -yr_remove, drop = FALSE])
      } else {
        return(x)
      }
    })
    OM@cpars <- structure(cpars_out, names = n_cpars)
  }
  OM@cpars$control <- list(progress = TRUE, ntrials = 1000, fracD = 0.2)
  return(OM)
}

#' Generate an operating model
#'
#' Returns a Hist object with shiny progress bars.
#'
#' @param OM An OM object.
#' @export
runMSEhist <- function(OM) {
  shiny::withProgress(message = "Constructing operating model", value = 0, {
    MSEhist <- runMSE(OM, Hist = TRUE, silent = TRUE)
  })
  return(MSEhist)
}


