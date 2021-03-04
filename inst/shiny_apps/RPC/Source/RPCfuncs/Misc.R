
doprogress<<-function(message,duration=1,n=20){
  withProgress(message = message, value = 0, {
    inc<-duration/n
    for (i in 1:n) {
      incProgress(1/n, detail = round(i*(100/n)))
      Sys.sleep(inc)
    }
  })
}

modOM<<-function(OM_temp,nsim){
  OM_temp<-MSEtool::SubCpars(OM_temp,sims=1:nsim)
  OM_temp@nsim<-nsim
  OM_temp@cpars$control=list(progress=TRUE,ntrials=1000,fracD=0.2)
  OM_temp
}

runMSEhist<<-function(OM){
  withProgress(message = "Constructing operating model", value = 0, {
    MSEhist<<-runMSE(OM,Hist=T,extended=T)
  })
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

