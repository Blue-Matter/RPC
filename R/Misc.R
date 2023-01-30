
#' @describeIn runMSEhist Modifies an OM with fewer simulations and projection years if desired.
#' @param nsim The number of simulations in the reduced OM.
#' @param proyears The number of projection years in the reduced OM.
#' @export
modOM <- function(OM, nsim, proyears) {
  nsim_full <- OM@nsim

  if(!missing(nsim) && nsim < nsim_full) {
    OM <- MSEtool::SubCpars(OM, sims = 1:nsim)
    OM@nsim<-nsim
  }

  proyears_full <- OM@proyears
  if(!missing(proyears) && proyears < proyears_full) {
    OM <- MSEtool::SubCpars(OM, proyears = proyears)
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


#' @importFrom graphics grid matplot
plot.default <- function(...) graphics::plot.default(..., panel.first = graphics::grid())
matplot <- function(...) graphics::matplot(..., panel.first = graphics::grid())
