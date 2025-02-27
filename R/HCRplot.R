
#' @name plot-MP
#' @title Functions to plot MPs
#' @description Various functions to plot the MP on the RPC App.

#' @rdname plot-MP
#' @details \code{HCR_plot} plots the selected harvest control rule.
#' @param input The input list from the shiny server
#' @export
HCR_plot<-function(input){

  if(input$MS_IVar=="SSB_SSBMSY") {
    xmax <- 2.5
  } else {
    xmax <- 1.2
  }

  if(input$MS_control==1){
    xs<-c(0,xmax)
    ys<-rep(input$CP_yint,2)
    linecol='orange'
    xlab1 <- "SSB"

  }else{
    xs<-c(0,input$CP_1_x,input$CP_2_x,xmax)
    ys<-c(input$CP_1_y,input$CP_1_y,input$CP_2_y,input$CP_2_y)
    linecol="#0000ff95"
    xlab1 <- switch(input$MS_IVar,
                    "SSB_SSBMSY" = "SSB/SSB[MSY]",
                    "SSB_SSB0" = "SSB/Initial~SSB[0]",
                    "SSB_dSSB0" = "SSB/Dynamic~SSB[0]",
                    "F_FMSY" = "F/F[MSY]",
                    "F_F01" = "F/F[0.1]",
                    "F_FSPR" = paste0("F/F[", 100 * input$SPR_OCP, "~'%'~SPR]")
    )
  }

  if(input$MS_Origin=="Perfect"){
    lab2="(perfect~information)"
  }else if(input$MS_Origin=="SCA_Pope"){
    lab2="(estimated~by~stock~assessment)"
  }else {
    lab2="(from~assessment~emulator)"
  }

  ylab1 <- switch(input$MS_DVar,
                  "FMSY" = "F/F[MSY]",
                  "F01" = "F/F[0.1]",
                  "Fmax" = "F/F[max]",
                  "FSPR" = paste0("F/F[", 100 * input$SPR_targ, "~'%'~SPR]")
  )

  xlab<-parse(text = paste0(xlab1, "~", lab2))
  ylab<-parse(text = paste0(ylab1, "~", lab2))


  ylim=c(0,max(1.1,max(ys)*1.1))
  xlim=c(0,xmax)
  par(mai=c(1,1,0.1,0.1))
  plot(xs,ys,yaxs='i',xaxs='i',type='l',col='white',ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab)
  abline(v=seq(0,5,by=0.1),col='light grey')
  abline(h=seq(0,5,by=0.1),col='light grey')
  abline(v=c(0.5,1,1.5,2,2.5),col='dark grey')
  abline(h=c(0.5,1,1.5,2,2.5),col='dark grey')

  lines(xs,ys,col=linecol,lwd=3)


  if(input$MS_control==2){
    points(xs[2:3],ys[2:3],pch=19,col='orange',cex=1.8)
    text(xs[2:3]-(xlim[2]-xlim[1])/50,ys[2:3]+(ylim[2]-ylim[1])/20,c(1,2),col='orange',font=2,cex=1.25)
  }
}

#' @rdname plot-MP
#' @details \code{CurF_plot} plots the projection F relative to historical F.
#' @param x An object of class \linkS4class{Hist}, or a shiny \code{reactivevalues} object containing a slot named \code{MSEhist} which is
#' the Hist object.
#' @param F_ratio The ratio of projection F and that from the last historical year.
#' @export
CurF_plot <- function(x, F_ratio = 1) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  yrs <- 1:(MSEhist@OM@nyears + MSEhist@OM@proyears) + MSEhist@OM@CurrentYr - MSEhist@OM@nyears

  Find <- MSEhist@SampPars$Fleet$qs * MSEhist@SampPars$Fleet$Find
  Find_pro <- F_ratio * matrix(Find[, MSEhist@OM@nyears], nrow(Find), MSEhist@OM@proyears)

  tsplot(x = cbind(Find, Find_pro), yrs = yrs, xlab = "Year", ylab = "Fishing mortality")
  abline(v =  MSEhist@OM@CurrentYr, lty = 2)
}

#' @rdname plot-MP
#' @details \code{CurC_plot} plots the projection catch relative to historical.
#' @param C_ratio The ratio of projection catch and that from the last historical year.
#' @export
CurC_plot <- function(x, C_ratio = 1) {
  if(inherits(x, "reactivevalues")) {
    MSEhist <- x$MSEhist
  } else {
    MSEhist <- x
  }
  yrs <- 1:(MSEhist@OM@nyears + MSEhist@OM@proyears) + MSEhist@OM@CurrentYr - MSEhist@OM@nyears

  Removals <- apply(MSEhist@TSdata$Removals, 1:2, sum)
  Cpro <- C_ratio * matrix(Removals[, MSEhist@OM@nyears], nrow(Removals), MSEhist@OM@proyears)

  tsplot(x = cbind(Removals, Cpro), yrs = yrs, xlab = "Year", ylab = "Catch")
  abline(v =  MSEhist@OM@CurrentYr, lty = 2)
}

