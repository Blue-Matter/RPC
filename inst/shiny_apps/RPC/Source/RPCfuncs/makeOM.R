
getminmax<-function(panel,parameter,PanelState){

  loc<-match(parameter,inputnames[[panel]])
  mins<-get(paste0(parameter,"_mins"))
  maxs<-get(paste0(parameter,"_maxes"))
  cond<-unlist(PanelState[[panel]][loc])
  range(mins[cond],maxs[cond])

}


rnorm_T<-function(n=1,mean=0,sd=1,trunc=90){

  incomplete=T
  ntrial<-n*10
  out<-rep(NA,n)
  fillind<-(1:n)[is.na(out)]
  nfill<-length(fillind)
  LB<-((100-trunc)/2)/100
  UB<-1-LB
  #i<-0

  while(incomplete){
    #i<-i+1
    #print(i)
    samps<-rnorm(ntrial,mean,sd)
    cond<-samps > qnorm(LB,mean,sd) & samps < qnorm(UB, mean, sd)
    canfill<-sum(cond)
    if(canfill >= nfill){
      out[fillind]<-samps[cond][1:nfill]
      incomplete=F
    }else{
      out[fillind[1:canfill]]<-samps[cond]
      fillind<-(1:n)[is.na(out)]
      nfill<-length(fillind)
    }

  }

  if(all(out > qnorm(LB,mean,sd) & out < qnorm(UB, mean, sd))){
    return(out)
  }else{
    message("An error in rnorm_T occurred")
    return(NULL)
  }

}

samp_par<-function(n,type="Truncated normal",LB,UB,trunc=99){

  if(LB==UB){
    out<-rep(LB,n)
  }else{
    if(type=="Uniform"){
      out<-runif(n,LB,UB)
    }else{
      mu<-mean(c(LB,UB))
      UBtemp<-qnorm(1-(100-trunc)/200,mu,1)
      sd<-(UB-mu)/(UBtemp-mu)
      out<-rnorm_T(n,mu,sd,trunc)
    }
  }
  out

}



makeOM<-function(input, PanelState,nsim, nyears=NA,maxage=NA,proyears=NA,UseQonly=F){

  # ---- Misc OM building ------------------------------------------------------------------------------------
  #nsim<-input$nsim
  type<-"Truncated normal"#$input$Distribution # sampling distribution
  trunc<-90# input$IQRange     # inter quantile range (trunc, a % e.g. 90)

  OM1<-LowSlopes(MSEtool::testOM)

  #if(input$use_seed) OM1@seed<-input$seed

  OM1@R0<-1e9
  OM1@Linf<-c(100,100)
  OM1@L50<-NaN
  OM1@K<-NaN
  OM1@isRel<-"FALSE"

  OM1@Name<-input$Name
  OM1@Species<-input$Species
  OM1@Region<-input$Region
  OM1@Agency<-input$Agency
  Nyears<-input$Lyear-input$Syear+1
  OM1@nyears<-Nyears

  OM1@Source<-input$Author
  OM1@interval<-1#input$interval
  OM1@proyears<-proyears #input$proyears


  loc<-match("Err",inputnames[[3]])                                                        # D1 -----------
  cond<-as.vector(unlist(PanelState[[3]][loc]))
  Dquality<-as.vector(unlist(Err_list)[cond])

  if(Dquality=="Err_perf"){
    temp<-new('OM',Albacore,Generic_Fleet,Perfect_Info,Perfect_Imp)
  }else if(Dquality=="Err_good"){
    temp<-new('OM',Albacore,Generic_Fleet,Precise_Unbiased,Perfect_Imp)
  }else if(Dquality=="Err_mod"){
    temp<-new('OM',Albacore,Generic_Fleet,Generic_obs,Perfect_Imp)
  }else{
    temp<-new('OM',Albacore,Generic_Fleet,Imprecise_Biased,Perfect_Imp)
  }

  OM1<-Replace(OM1,temp,Sub="Obs")
  OM1@nsim<-nsim
  # ---- Fishery characteristics ---------------------------------------------------------------------------
  #saveRDS(PanelState,"C:/temp/PanelState.rda")
  OM1@M<-getminmax(1,"M",PanelState)                                                        # F2 ----------
  OM1@L50<-getminmax(1,"LM",PanelState)                                                     # F9 ----------
  OM1@maxage<-maxage<-min(ceiling(-log(0.1)/min(OM1@M)),50)
  #saveRDS(OM1,"C:/temp/OM1a.rda")

  # --- Life history imputation
  OMtemp<-OM1
  OMtemp@nsim<-nsim*20
  OMtemp<-LH2OM(OMtemp, dist='norm',plot=F) # get sample
  #saveRDS(OMtemp,"C:/temp/OMtemp.rda")
  OM1@K<-quantile(OMtemp@cpars$K,c(0.1,0.9),filterMK=T) # 80th percentile from LH2OM

  OM1<-LH2OM(OM1, dist='norm',plot=F,filterMK=T) # truncated sample
  #saveRDS(OM1,"C:/temp/OM1.rda")

  # PanelState<-readRDS("C:/temp/PanelState.rda")
  # OM1a<-readRDS("C:/temp/OM1a.rda")
  # OMtemp<-readRDS("C:/temp/OMtemp.rda")
  # OM1<-readRDS("C:/temp/OM1.rda")

  OM1@L50<-quantile(OM1@cpars$L50,c(0.1,0.90))
  OM1@L50_95<-c(10,10)
  OM1@Linf<-c(100,100)
  OM1@D<-getminmax(1,"D",PanelState)                                                        # F3 -----------
  OM1@h<-getminmax(1,"h",PanelState)                                                        # F4 -----------

  # Ftrend and error
  trends<-effort_mat()
  trends<-trends/apply(trends,1,mean)
  nt<-dim(trends)[1]

  Esd<-getminmax(1,"F",PanelState)                                                         # F6 ----------
  Esd_max<-Esd[2]
  Esd_min<-Esd[1]
  Esdrand<-samp_par(nsim,type=type,Esd_min,Esd_max,trunc=trunc) #runif(nsim,Esd_min,Esd_max)
  Emu<-(-0.5*Esdrand^2)
  Esdarray<-array(exp(rnorm(nsim*Nyears,Emu,Esdrand)),c(nsim,Nyears))

  qhs<-getminmax(1,"qh",PanelState)
  qhssim<-samp_par(nsim,type=type,qhs[1],qhs[2],trunc=trunc) #(nsim,qhs[1],qhs[2])
  qssim<-1+qhssim/100                                                   # F7 ----------
  trendsamp<-ceiling(runif(nsim)*nt)

  Find<-array(NA,c(nsim,Nyears))
  for(i in 1:nsim)Find[i,]<-trends[trendsamp[i],]*Esdarray[i,]* qssim[i]^((1:Nyears)-(Nyears/2))

  # --- Future catchability ----------

  OM1@qinc<-getminmax(1,"q",PanelState)                                                     # F8 ----------

  # --- Selectivity -----------------------

  Sel50<-getminmax(1,"sel",PanelState)                                                     # F10 ----------
  Sel50sim<-samp_par(nsim,type=type,Sel50[1],Sel50[2],trunc=trunc) #runif(nsim,Sel50[1],Sel50[2])

  L5<-OM1@cpars$Linf*Sel50sim*0.8
  LFS<-OM1@cpars$Linf*Sel50sim*1.2
  cond<-LFS>0.95*OM1@cpars$Linf
  LFS[cond]<-0.95*OM1@cpars$Linf[cond]
  Linf<-rep(100,nsim)

  OM1@Vmaxlen<-getminmax(1,"dome",PanelState)                                               # F11 ----------

  # --- Discarding ------------------------

  OM1@DR<-getminmax(1,"DR",PanelState) # F12 ----------
  #DR<-matrix(samp_par(nsim,type=type,OM1@DR[1],OM1@DR[2],trunc=trunc),ncol=nsim,nrow=nyears+proyears,byrow=T)

  OM1@Fdisc<-getminmax(1,"PRM",PanelState)                                                  # F13 ----------

  # --- Recruitment deviations ------------

  OM1@Perr<-getminmax(1,"sigR",PanelState)                                                  # F14 ----------

  # --- MPAs ------------------------------

  nareas<-3

  Ahrng<-getminmax(1,"Ah",PanelState) # size / frac habitat area 3                         # F15 ----------
  Vhrng<-getminmax(1,"Vh",PanelState) # prob staying in area 3                             # F16 ----------
  Arng<-getminmax(1,"A",PanelState)   # size / frac habitat area 1                         # F17 ----------
  Vrng<-getminmax(1,"V",PanelState)   # prob staying in area 3                             # F18 ----------

  Ahsim<-samp_par(nsim,type=type,Ahrng[1],Ahrng[2],trunc=trunc) #runif(nsim,Ahrng[1],Ahrng[2])
  Vhsim<-samp_par(nsim,type=type,Vhrng[1],Vhrng[2],trunc=trunc) #runif(nsim,Vhrng[1],Vhrng[2])
  Asim<-samp_par(nsim,type=type,Arng[1],Arng[2],trunc=trunc) #runif(nsim,Arng[1],Arng[2])
  Vsim<-samp_par(nsim,type=type,Vrng[1],Vrng[2],trunc=trunc) #runif(nsim,Vrng[1],Vrng[2])

  ilogit<-function(x)log(x/(1-x))
  logit<-function(x)exp(x)/(1+exp(x))

  mov1<-mov2<-array(NA,c(nsim,2,2))

  for(i in 1:nsim){
    mov1[i,,]<-getmov2(i,Vsim,Asim)
    mov2[i,,]<-getmov2(i,Vhsim,Ahsim)
  }

  V2<-apply(cbind(mov1[,2,2], # staying in areas 2 and 3 minus staying in area 3
                  mov2[,2,2]), # staying in areas 2 and 3 minus staying in area 1
            1,mean) # a WRONG GUESS of the prob_staying in area 2 - need to do the linear equation modelling for this.

  Sz2<-1-(Ahsim+Asim)
  Asize<-cbind(Asim,Sz2,Ahsim) # area 1 is Asim as future MPs close area 1
  probs<-cbind(Vsim,V2,Vhsim)

  mov<-array(NA,c(nsim, maxage+1, nareas, nareas, Nyears+proyears))
  for(i in 1:nsim)mov[i,,,,]<-array(rep(makemov(fracs=Asize[i,], prob=probs[i,]),each=maxage+1),c(maxage+1,nareas,nareas,Nyears+proyears))

  OM1@cpars$MPA<-matrix(1,nrow=OM1@nyears+OM1@proyears,ncol=3)
  OM1@cpars$MPA[1:(Nyears-1),3]<-0
  OM1@cpars$MPA[Nyears:proyears,1]<-0

  # Initial depletion                                                                      # F19 ----------
  initDrng<-getminmax(1,"Dh",PanelState)
  initD<-samp_par(nsim,type=type,initDrng[1],initDrng[2],trunc=trunc) #runif(nsim,initDrng[1],initDrng[2])

  # ---- Management parameters -----------------------------------------------------------------------------------------------

  OM1@TACFrac<-getminmax(2,"IB",PanelState)                                                 # M2 -----------
  OM1@TACSD<-getminmax(2,"IV",PanelState)                                                   # M3 -----------

  OM1@TAEFrac<-getminmax(2,"IBE",PanelState)                                                # M4 -----------
  OM1@TAESD<-getminmax(2,"IVE",PanelState)                                                  # M5 -----------

  OM1@SizeLimFrac<-getminmax(2,"IBSL",PanelState)                                           # M6 -----------
  OM1@SizeLimSD<-getminmax(2,"IVSL",PanelState)                                             # M7 -----------


  # ---- Data parameters -----------------------------------------------------------------------------------------------------

  CB_rng<-getminmax(3,"CB",PanelState)                                                     # D2 -----------
  Cbias<-samp_par(nsim,type=type,CB_rng[1],CB_rng[2],trunc=trunc) #runif(nsim,CB_rng[1],CB_rng[2])

  OM1@beta<-getminmax(3,"Beta",PanelState)                                                  # D3 -----------


  # ---- Custom parameters ---------------------------------------------------------------------------------------------------

  slots2cpars<-c("D","h","Vmaxlen","Fdisc","Perr","TACFrac","TACSD",
    "TAEFrac","TAESD","SizeLimFrac","SizeLimSD","beta") # all slots that need making into cpars vectors

  makevec<-function(i,OM1,slots2cpars,nsim,type,trunc){
    LB<-slot(OM1,slots2cpars[i])[1]
    UB<-slot(OM1,slots2cpars[i])[2]
    OM1@cpars[[slots2cpars[i]]]<-samp_par(nsim,type=type,LB,UB,trunc)
    OM1
  }

  for(i in 1:length(slots2cpars))OM1<-makevec(i,OM1,slots2cpars,nsim,type,trunc)

  OM1@cpars<-c(OM1@cpars,list(Find=Find,L5=L5,LFS=LFS,Asize=Asize,mov=mov,initD=initD,Cbias=Cbias,
                            control=list(progress=T,ntrials=1000,fracD=0.2)))#,DR=DR))


  AM("------------- New OM made --------------")
  #MadeOM(1)
  OM1 # OM

}
