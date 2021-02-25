
plotIB <- function(dummy=1){
  #IB_list<<-list("Large underages" = "IB_n30", "Underages" = "IB_n30_n10","Slight underages" = "IB_n10_0",
  #               "Taken exactly"="IB_n5_5","Slight overages"="IB_0_10","Overages"="IB_10_30","Large overages"="IB_30")

  IB_nams<-unlist(IB_list)#c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

  cond<-IB_nams%in%input$IB

  if(sum(cond)>0){

    par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    IB_max<-max(IB_maxes[cond])
    IB_min<-min(IB_mins[cond])

    set.seed(1)

    ts1<-c(15:20,c(41:50)/2,rep(25.5,10),(51:30)/2)
    ny1<-length(ts1)
    ts1<-ts1*exp(rnorm(ny1,0,0.2))
    ts1<-ts1/mean(ts1)*8

    ts2<-c(20:10,rep(10.5,25),(6:20)*2,seq(40,1,length.out=10))
    ny2<-length(ts2)
    ts2<-ts2*exp(rnorm(ny2,0,0.4))
    ts2<-ts2/mean(ts2)

    cols<-colsbox<-c(fcol,"black",icol)
    colsbox[2]<-'white'

    # plot TS2
    yrs<-Current_Year+(1:ny2)
    ny<-length(yrs)
    UB<- IB_max*ts2
    LB<- IB_min*ts2
    plot(yrs,ts2,col="white",ylim=c(0,max(UB,ts2)),xlab="",ylab="",type='l')
    if(IB_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(IB_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts2[ny:1]),border=NA,col=cols[3])
    }
    if(IB_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(IB_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts2[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts2,col=cols[2],lwd=1)
    mtext("Example 1",3,line=0.8)

    legend('topleft',legend=c("Overages","TAC","Underages"),
           fill=colsbox,border='white',col=cols,lty=c(NA,1,NA),bty='n',cex=0.8)

    # plot TS1
    yrs<-Current_Year+(1:ny1)
    ny<-length(yrs)
    UB<- IB_max*ts1
    LB<- IB_min*ts1
    plot(yrs,ts1,col="white",ylim=c(0,max(UB,ts1)),xlab="",ylab="",type='l')
    if(IB_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(IB_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts1[ny:1]),border=NA,col=cols[3])
    }
    if(IB_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(IB_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts1[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts1,col=cols[2],lwd=1)
    mtext("Example 2",3,line=0.8)
    mtext("Year",1,line=1,outer=T)
    mtext("Catches (thousand tonnes)",2,line=0.7,outer=T)


  }else{
    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< questionnaire incomplete >", col="red")

  }


}



plotIV <- function(){

  IB_nams<-c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

  cond<-IB_nams%in%input$IB

  IV_nams<-unlist(IV_list)#c("IV_1","IV_1_5","IV_5_10","IV_10_20","IV_20_40")
  cond2<-IV_nams%in%input$IV

  if(sum(cond)>0){

    if(sum(cond2)>0){

      IB_max<-max(IB_maxes[cond])
      IB_min<-min(IB_mins[cond])

      set.seed(1)

      # plot TS2
      par(mai=c(0.4,0.65,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
      layout(matrix(c(1,2),nrow=1),widths=c(2,1))

      pch=19
      ny<-40
      yrs<-2018+(1:ny)

      maxcol<-icol
      mincol<-fcol

      gen_ts<-function(sig,ny=40)exp(rnorm(ny,-0.5*sig^2,sig))
      sigI_max<-max(IV_maxes[cond2])
      sigI_min<-min(IV_mins[cond2])

      IU_max<-IB_max*gen_ts(sigI_max,ny)
      IU_min<-IB_max*gen_ts(sigI_min,ny)
      IL_max<-IB_min*gen_ts(sigI_max,ny)
      IL_min<-IB_min*gen_ts(sigI_min,ny)
      ylim=c(min(c(IU_max,IU_min,IL_max,IL_min))-0.5,quantile(c(IU_max,IU_min,IL_max,IL_min),0.97))

      plot(yrs,rep(1,ny),col="black",ylim=ylim,xlab="",ylab="",type='l',lwd=2)
      #abline(h=0,col='light grey')
      mtext("Year",1,line=2.5)
      mtext("Implemented / Recommended",2,line=2.5)
      lines(yrs,IU_max,col=maxcol)
      lines(yrs,IU_min,col=mincol)
      lines(yrs,IL_max,col=maxcol,lty=2)
      lines(yrs,IL_min,col=mincol,lty=2)
      legend('bottomleft',legend=c("Highest level","Lowest level"),lty=c(1,2),bty='n')
      legend('bottomright',legend=c("Highest variability","Lowest variability"),text.col=c(maxcol,mincol),bty='n')

      minadjust=0.5
      maxadjust=0.5
      dU_max<-density(IB_max*gen_ts(sigI_max,10000),adjust=maxadjust)
      dU_min<-density(IB_max*gen_ts(sigI_min,10000),adjust=minadjust)
      dL_max<-density(IB_min*gen_ts(sigI_max,10000),adjust=maxadjust)
      dL_min<-density(IB_min*gen_ts(sigI_min,10000),adjust=minadjust)
      scale<-max(dU_max$y, dU_min$y, dL_max$y, dL_min$y)

      plot(c(0,2),ylim,axes=F,xlab="",ylab="",col="white")
      abline(h=1,lwd=2)
      #abline(h=0,col='light grey')
      powplot<-0.66
      polygon(x=1+(dU_max$y/scale)^powplot,y=dU_max$x,col=maxcol,border=maxcol)
      polygon(x=1+(dU_min$y/scale)^powplot,y=dU_min$x,col=mincol,border=mincol)

      polygon(x=(dL_max$y/scale)^powplot,y=dL_max$x,col=maxcol,border=maxcol)
      polygon(x=(dL_min$y/scale)^powplot,y=dL_min$x,col=mincol,border=mincol)

      legend('bottomright',legend=paste("V =",c(sigI_max,sigI_min)),text.font=2,text.col=c(maxcol,mincol),bty='n')

    }else{

      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< questionnaire incomplete >", col="red")

    }

  }else{

    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< Management Q2 is unspecified >", col="red")

  }

}

plotIB_E <- function(dummy=1){
  #IB_list<<-list("Large underages" = "IB_n30", "Underages" = "IB_n30_n10","Slight underages" = "IB_n10_0",
  #               "Taken exactly"="IB_n5_5","Slight overages"="IB_0_10","Overages"="IB_10_30","Large overages"="IB_30")

  IBE_nams<-unlist(IBE_list)#c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

  cond<-IBE_nams%in%input$IBE

  if(sum(cond)>0){

    par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    IB_max<-max(IB_maxes[cond])
    IB_min<-min(IB_mins[cond])

    set.seed(1)

    ts1<-c(10:30,c(61:50)/2,rep(25.5,10),(51:30)/2)
    ny1<-length(ts1)
    ts1<-ts1*exp(rnorm(ny1,0,0.05))
    ts1<-10*ts1/mean(ts1)

    ts2<-c(50:10,rep(10.5,25),(6:20)*2,seq(40,30,length.out=10))
    ny2<-length(ts2)
    ts2<-ts2*exp(rnorm(ny2,0,0.15))
    ts2<-1000*ts2/mean(ts2)

    cols<-colsbox<-c(fcol,"black",icol)
    colsbox[2]<-'white'

    # plot TS2
    yrs<-Current_Year+(1:ny2)
    ny<-length(yrs)
    UB<- IB_max*ts2
    LB<- IB_min*ts2
    plot(yrs,ts2,col="white",ylim=c(0,max(UB,ts2)),xlab="",ylab="",type='l')
    if(IB_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(IB_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts2[ny:1]),border=NA,col=cols[3])
    }
    if(IB_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(IB_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts2[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts2,col=cols[2],lwd=1)
    mtext("Example 1",3,line=0.8)

    legend('topleft',legend=c("Overages","TAE","Underages"),
           fill=colsbox,border='white',col=cols,lty=c(NA,1,NA),bty='n',cex=0.8)

    # plot TS1
    yrs<-Current_Year+(1:ny1)
    ny<-length(yrs)
    UB<- IB_max*ts1
    LB<- IB_min*ts1
    plot(yrs,ts1,col="white",ylim=c(0,max(UB,ts1)),xlab="",ylab="",type='l')
    if(IB_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(IB_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts1[ny:1]),border=NA,col=cols[3])
    }
    if(IB_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(IB_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts1[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts1,col=cols[2],lwd=1)
    mtext("Example 2",3,line=0.8)
    mtext("Year",1,line=1,outer=T)
    mtext("Effort (boat days)",2,line=0.7,outer=T)


  }else{
    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< questionnaire incomplete >", col="red")

  }


}



plotIV_E <- function(){

  IBE_nams<-c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

  cond<-IBE_nams%in%input$IBE

  IVE_nams<-unlist(IVE_list)#c("IV_1","IV_1_5","IV_5_10","IV_10_20","IV_20_40")
  cond2<-IVE_nams%in%input$IVE

  if(sum(cond)>0){

    if(sum(cond2)>0){

      IB_max<-max(IB_maxes[cond])
      IB_min<-min(IB_mins[cond])

      set.seed(1)

      # plot TS2
      par(mai=c(0.4,0.65,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
      layout(matrix(c(1,2),nrow=1),widths=c(2,1))

      pch=19
      ny<-40
      yrs<-2018+(1:ny)

      maxcol<-icol
      mincol<-fcol

      gen_ts<-function(sig,ny=40)exp(rnorm(ny,-0.5*sig^2,sig))
      sigI_max<-max(IV_maxes[cond2])
      sigI_min<-min(IV_mins[cond2])

      IU_max<-IB_max*gen_ts(sigI_max,ny)
      IU_min<-IB_max*gen_ts(sigI_min,ny)
      IL_max<-IB_min*gen_ts(sigI_max,ny)
      IL_min<-IB_min*gen_ts(sigI_min,ny)
      ylim=c(min(c(IU_max,IU_min,IL_max,IL_min))-0.5,quantile(c(IU_max,IU_min,IL_max,IL_min),0.97))

      plot(yrs,rep(1,ny),col="black",ylim=ylim,xlab="",ylab="",type='l',lwd=2)
      #abline(h=0,col='light grey')
      mtext("Year",1,line=2.5)
      mtext("Implemented / Recommended",2,line=2.5)
      lines(yrs,IU_max,col=maxcol)
      lines(yrs,IU_min,col=mincol)
      lines(yrs,IL_max,col=maxcol,lty=2)
      lines(yrs,IL_min,col=mincol,lty=2)
      legend('bottomleft',legend=c("Highest level","Lowest level"),lty=c(1,2),bty='n')
      legend('bottomright',legend=c("Highest variability","Lowest variability"),text.col=c(maxcol,mincol),bty='n')

      minadjust=0.5
      maxadjust=0.5
      dU_max<-density(IB_max*gen_ts(sigI_max,10000),adjust=maxadjust)
      dU_min<-density(IB_max*gen_ts(sigI_min,10000),adjust=minadjust)
      dL_max<-density(IB_min*gen_ts(sigI_max,10000),adjust=maxadjust)
      dL_min<-density(IB_min*gen_ts(sigI_min,10000),adjust=minadjust)
      scale<-max(dU_max$y, dU_min$y, dL_max$y, dL_min$y)

      plot(c(0,2),ylim,axes=F,xlab="",ylab="",col="white")
      abline(h=1,lwd=2)
      #abline(h=0,col='light grey')
      powplot<-0.66
      polygon(x=1+(dU_max$y/scale)^powplot,y=dU_max$x,col=maxcol,border=maxcol)
      polygon(x=1+(dU_min$y/scale)^powplot,y=dU_min$x,col=mincol,border=mincol)

      polygon(x=(dL_max$y/scale)^powplot,y=dL_max$x,col=maxcol,border=maxcol)
      polygon(x=(dL_min$y/scale)^powplot,y=dL_min$x,col=mincol,border=mincol)

      legend('bottomright',legend=paste("V =",c(sigI_max,sigI_min)),text.font=2,text.col=c(maxcol,mincol),bty='n')

    }else{

      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< questionnaire incomplete >", col="red")

    }

  }else{

    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< Management Q4 is unspecified >", col="grey")

  }

}

plotIB_SL <- function(dummy=1){
  #IB_list<<-list("Large underages" = "IB_n30", "Underages" = "IB_n30_n10","Slight underages" = "IB_n10_0",
  #               "Taken exactly"="IB_n5_5","Slight overages"="IB_0_10","Overages"="IB_10_30","Large overages"="IB_30")

  IBSL_nams<-unlist(IBSL_list)#c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

  cond<-IBSL_nams%in%input$IBSL

  if(sum(cond)>0){

    par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    IB_max<-max(IB_maxes[cond])
    IB_min<-min(IB_mins[cond])

    set.seed(1)

    ts1<-c(15:20,c(41:50)/2,rep(25.5,10),(51:30)/2)
    ny1<-length(ts1)
    ts1<-ts1*exp(rnorm(ny1,0,0.05))
    ts1<-8*ts1/mean(ts1)

    ts2<-c(20:10,rep(10.5,25),(6:12)*2,seq(24,20,length.out=10))
    ny2<-length(ts2)
    ts2<-ts2*exp(rnorm(ny2,0,0.2))
    ts2<-40*ts2/mean(ts2)

    cols<-colsbox<-c(fcol,"black",icol)
    colsbox[2]<-'white'

    # plot TS2
    yrs<-Current_Year+(1:ny2)
    ny<-length(yrs)
    UB<- IB_max*ts2
    LB<- IB_min*ts2
    plot(yrs,ts2,col="white",ylim=c(0,max(UB,ts2)),xlab="",ylab="",type='l')
    if(IB_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(IB_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts2[ny:1]),border=NA,col=cols[3])
    }
    if(IB_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(IB_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts2[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts2,col=cols[2],lwd=1)
    mtext("Example 1",3,line=0.8)

    legend('topleft',legend=c("Larger","Size limit","Smaller"),
           fill=colsbox,border='white',col=cols,lty=c(NA,1,NA),bty='n',cex=0.8)

    # plot TS1
    yrs<-Current_Year+(1:ny1)
    ny<-length(yrs)
    UB<- IB_max*ts1
    LB<- IB_min*ts1
    plot(yrs,ts1,col="white",ylim=c(0,max(UB,ts1)),xlab="",ylab="",type='l')
    if(IB_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(IB_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts1[ny:1]),border=NA,col=cols[3])
    }
    if(IB_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(IB_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts1[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts1,col=cols[2],lwd=1)
    mtext("Example 2",3,line=0.8)
    mtext("Year",1,line=1,outer=T)
    mtext("Minimum size taken (cm)",2,line=0.7,outer=T)

  }else{
    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< questionnaire incomplete >", col="red")

  }

}



plotIV_SL <- function(){

  IBSL_nams<-c("IB_n30", "IB_n30_n10","IB_n10_0","IB_n5_5","IB_0_10","IB_10_30","IB_30")

  cond<-IBSL_nams%in%input$IBSL

  IVSL_nams<-unlist(IVSL_list)#c("IV_1","IV_1_5","IV_5_10","IV_10_20","IV_20_40")
  cond2<-IVSL_nams%in%input$IVSL

  if(sum(cond)>0){

    if(sum(cond2)>0){

      IB_max<-max(IB_maxes[cond])
      IB_min<-min(IB_mins[cond])

      set.seed(1)

      # plot TS2
      par(mai=c(0.4,0.65,0.01,0.01), omi=c(0.4,0.01,0.55,0.01), cex.main = 1.5, cex.lab=1.35 )
      layout(matrix(c(1,2),nrow=1),widths=c(2,1))

      pch=19
      ny<-40
      yrs<-2018+(1:ny)

      maxcol<-icol
      mincol<-fcol

      gen_ts<-function(sig,ny=40)exp(rnorm(ny,-0.5*sig^2,sig))
      sigI_max<-max(IV_maxes[cond2])
      sigI_min<-min(IV_mins[cond2])

      IU_max<-IB_max*gen_ts(sigI_max,ny)
      IU_min<-IB_max*gen_ts(sigI_min,ny)
      IL_max<-IB_min*gen_ts(sigI_max,ny)
      IL_min<-IB_min*gen_ts(sigI_min,ny)
      ylim=c(min(c(IU_max,IU_min,IL_max,IL_min))-0.5,quantile(c(IU_max,IU_min,IL_max,IL_min),0.97))

      plot(yrs,rep(1,ny),col="black",ylim=ylim,xlab="",ylab="",type='l',lwd=2)
      #abline(h=0,col='light grey')
      mtext("Year",1,line=2.5)
      mtext("Implemented / Recommended",2,line=2.5)
      lines(yrs,IU_max,col=maxcol)
      lines(yrs,IU_min,col=mincol)
      lines(yrs,IL_max,col=maxcol,lty=2)
      lines(yrs,IL_min,col=mincol,lty=2)
      legend('bottomleft',legend=c("Highest level","Lowest level"),lty=c(1,2),bty='n')
      legend('bottomright',legend=c("Highest variability","Lowest variability"),text.col=c(maxcol,mincol),bty='n')

      minadjust=0.5
      maxadjust=0.5
      dU_max<-density(IB_max*gen_ts(sigI_max,10000),adjust=maxadjust)
      dU_min<-density(IB_max*gen_ts(sigI_min,10000),adjust=minadjust)
      dL_max<-density(IB_min*gen_ts(sigI_max,10000),adjust=maxadjust)
      dL_min<-density(IB_min*gen_ts(sigI_min,10000),adjust=minadjust)
      scale<-max(dU_max$y, dU_min$y, dL_max$y, dL_min$y)

      plot(c(0,2),ylim,axes=F,xlab="",ylab="",col="white")
      abline(h=1,lwd=2)
      #abline(h=0,col='light grey')
      powplot<-0.66
      polygon(x=1+(dU_max$y/scale)^powplot,y=dU_max$x,col=maxcol,border=maxcol)
      polygon(x=1+(dU_min$y/scale)^powplot,y=dU_min$x,col=mincol,border=mincol)

      polygon(x=(dL_max$y/scale)^powplot,y=dL_max$x,col=maxcol,border=maxcol)
      polygon(x=(dL_min$y/scale)^powplot,y=dL_min$x,col=mincol,border=mincol)

      legend('bottomright',legend=paste("V =",c(sigI_max,sigI_min)),text.font=2,text.col=c(maxcol,mincol),bty='n')

    }else{

      par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
      plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
      text(10,0.5,"< questionnaire incomplete >", col="red")

    }

  }else{

    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< Management Q6 is unspecified >", col="grey")

  }

}

