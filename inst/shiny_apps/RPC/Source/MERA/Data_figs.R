plotCB <- function(dummy=1){

  CB_nams<-unlist(CB_list)#c("CB_n50_n30", "CB_n30_n10","CB_n10_0","CB_n5_5","CB_0_10")
  cond<-CB_nams%in%input$CB

  if(sum(cond)>0){

    par(mfrow=c(1,2),mai=c(0.3,0.5,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    Cbias_max<-max(CB_maxes[cond])
    Cbias_min<-min(CB_mins[cond])

    set.seed(2)

    ts1<-c(1:20,c(41:50)/2,rep(25.5,30))
    ny1<-length(ts1)
    ts1<-ts1*exp(rnorm(ny1,0,0.2))
    ts1<-ts1/mean(ts1)*8

    ts2<-c(seq(1,10,length.out=5),seq(10,5,length.out=15),seq(5.5,10,length.out=5),rep(11,25))
    ny2<-length(ts2)
    ts2<-ts2*exp(rnorm(ny2,0,0.25))
    ts2<-ts2/mean(ts2)

    cols<-colsbox<-c(fcol,'black',icol)
    colsbox[2]<-'white'

    # plot TS2
    yrs<-2017-(ny2:1)-1
    ny<-length(yrs)
    UB<- Cbias_max*ts2
    LB<- Cbias_min*ts2
    plot(yrs,ts2,col="white",ylim=c(0,max(UB,ts2)),xlab="",ylab="",type='l')
    if(Cbias_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(Cbias_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts2[ny:1]),border=NA,col=cols[3])
    }
    if(Cbias_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(Cbias_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts2[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts2,col=cols[2],lwd=1)
    mtext("Example 1",3,line=0.8)

    legend('topleft',legend=c("Catches over-reported","Catches taken","Catches under-reported"),
           fill=colsbox,border='white',col=cols,lty=c(NA,1,NA),bty='n',cex=0.8)

    # plot TS1
    yrs<-2017-(ny1:1)-1
    ny<-length(yrs)
    UB<- Cbias_max*ts1
    LB<- Cbias_min*ts1
    plot(yrs,ts1,col="white",ylim=c(0,max(UB,ts1)),xlab="",ylab="",type='l')
    if(Cbias_max<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,UB[ny:1]),border=NA,col=cols[3])
    }else if(Cbias_min<1){
      polygon(c(yrs,yrs[ny:1]),c(LB,ts1[ny:1]),border=NA,col=cols[3])
    }
    if(Cbias_min>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,LB[ny:1]),border=NA,col=cols[1])
    }else if(Cbias_max>1){
      polygon(c(yrs,yrs[ny:1]),c(UB,ts1[ny:1]),border=NA,col=cols[1])
    }
    lines(yrs,ts1,col=cols[2],lwd=1)
    mtext("Example 2",3,line=0.8)
    mtext("Year",1,line=1,outer=T)
    mtext("Catches (taken) (tonnes)",2,line=0.7,outer=T)


  }else{

    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< questionnaire incomplete >", col="red")

  }

}



plotBeta <- function(){

  Beta_nams<-unlist(Beta_list)#list("Beta_200_300", "Beta_125_200","Beta_80_125", "Beta_50_80","Beta_33_50")
  cond<-Beta_nams%in%input$Beta

  if(sum(cond)>0){

    par(mfrow=c(1,2),mai=c(0.6,0.7,0.01,0.01), omi=c(0.4,0.4,0.55,0.1),cex.main = 1.5, cex.lab=1.35 )
    Beta_max<-max(Beta_maxes[cond])
    Beta_min<-min(Beta_mins[cond])

    dep<-seq(0,1,length.out=100)
    plot(c(0,1),c(0,1),type='l',lwd=2,xlab="",ylab="")
    mtext("Real Stock Depletion (SSB relative to unfished)",1,line=2.2,xlab="",ylab="")
    mtext("Relative abundance index",2,line=2.2)
    mtext("Index relative to real depletion",3,line=0.5)
    Imax<-dep^Beta_max
    Imin<-dep^Beta_min
    lines(dep,Imax,col=icol)
    lines(dep,Imin,col=fcol)

    set.seed(2)

    ts1<-seq(1,0.4,length.out=61)*(2+(cos((-0:60)/10.18))/3)*exp(rnorm(61,0,0.05))
    ts1<-ts1/max(ts1)
    tLB<-ts1^Beta_max
    tUB<-ts1^Beta_min
    ts1<-ts1/mean(ts1)
    tLB<-tLB/mean(tLB)
    tUB<-tUB/mean(tUB)
    ny<-length(ts1)

    yrs<-Current_Year-(ny:1)
    plot(yrs,ts1,lwd=2,type="l",ylim=range(c(ts1,tLB,tUB)),xlab="",ylab="")
    mtext("Year",1,line=2.2)
    mtext("Relative abundance",2,line=2.2)
    mtext("Example indices",3,line=0.5)

    lines(yrs,tLB,col=icol)
    lines(yrs,tUB,col=fcol)

    legend('topright',c(paste0("Beta = ",c(Beta_max,Beta_min)),"True biomass"),text.col=c(icol,fcol,"black"),text.font=c(1,1,2),bty='n',cex=0.85)

  }else{

    par(mar=c(3,3,0.01,0.01), cex.main = 1.5, cex.lab=1.35 )
    plot(c(1,20),c(0,1),col="white",axes=FALSE,xlab="",ylab="")
    text(10,0.5,"< questionnaire incomplete >", col="red")

  }

}



