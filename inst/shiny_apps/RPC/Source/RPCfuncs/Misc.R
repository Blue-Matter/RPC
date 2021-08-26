
doprogress<<-function(message,duration=1,n=20){
  withProgress(message = message, value = 0, {
    inc<-duration/n
    for (i in 1:n) {
      incProgress(1/n, detail = round(i*(100/n)))
      Sys.sleep(inc)
    }
  })
}

modOM<<-function(OM_temp,nsim,proyears){
  OM_temp<-MSEtool::SubCpars(OM_temp,sims=1:nsim)
  OM_temp@nsim<-nsim
  proyears_full <- OM_temp@proyears
  if(!missing(proyears) && proyears < proyears_full) { # Add to MSEtool
    OM_temp@proyears <- proyears
    cpars <- OM_temp@cpars
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
    OM_temp@cpars <- structure(cpars_out, names = n_cpars)
  }
  OM_temp@cpars$control=list(progress=TRUE,ntrials=1000,fracD=0.2)
  OM_temp
}

runMSEhist<<-function(OM){
  withProgress(message = "Constructing operating model", value = 0, {
    MSEhist <- runMSE(OM, Hist = TRUE, silent = TRUE)
  })
  MSEhist
  #saveRDS(MSEhist,"C:/temp/MSEhist.rda") #
}

LowSlopes<-function(OMin, except = NULL) {

  nms <- slotNames(OMin)
  # exceptions
  if (is.null(except)) except <- "EVERYTHING"
  exclude <- unique(grep(paste(except, collapse = "|"), nms, value = FALSE))

  vars <- c("inc","sd")
  ind <- unique(grep(paste(vars, collapse = "|"), nms, value = FALSE))
  ind <- ind[(!(nms[ind] %in% exclude))]
  for (X in seq_along(ind)) {
    slot(OMin, nms[ind[X]]) <- c(0, 1e-10)
  }

  return(OMin)

}

