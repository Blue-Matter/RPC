
# Effort sketching functions

getyrs<-function(){
  Syear:Lyear
}

# Get effort matrix
# matrix interpolation
mat_inter<-function(df){
  x<-df$x-min(df$x)+1
  approx(x,y=df$y,xout=1:max(x))$y
}

# linear interpolation of effort
effort_mat<<-function(){

  yrs<-getyrs()
  lyr<-yrs[length(yrs)]
  nseries<-eff_values$series
  nt<-length(yrs)
  effmat<-array(NA,c(nseries,nt))

  for(i in 1:nseries){

    df<-eff_values$df[eff_values$df$series==i,]

    if(i == nseries){

      if(nrow(df)==1){ # interpolation not possible only one datum
        df<-rbind(df,data.frame(x=lyr,y=df$y,series=df$series))
      }
      if(!(lyr%in%df$x)){ # did not specify last data point of last series
        df<-rbind(df,data.frame(x=lyr,y=df$y[nrow(df)],series=df$series[nrow(df)]))
      }

    }

    effmat[i,]<-mat_inter(df)
    if(all(effmat[i,]==0))effmat[i,]<-0.5

  }

  effmat

}

eff_backwards<<-function(MSClog){

  if("Nyears"%in%names(MSClog[[3]])){
    Nyears<-MSClog[[3]]$Nyears
    yrs<-Current_Year-(Nyears:1)
  }else{
    yrs<-MSClog[[3]]$Syear:MSClog[[3]]$Lyear
    Nyears<-length(yrs)
  }
  loc<-match("FP",inputnames[[1]])
  sels<-unlist(PanelState[[1]][loc])
  cond<-(1:length(sels))[sels]
  nt<-length(cond)
  M1sim<-M1s[cond]
  M2sim<-M2s[cond]
  sd1sim<-sd1s[cond]
  sd2sim<-sd2s[cond]
  h2sim<-h2s[cond]
  locsim<-PanelState[[4]][[1]]
  stmagsim<-PanelState[[4]][[2]]
  cosim<-PanelState[[4]][[3]]
  Find<-array(NA,c(nt,Nyears))

  for(i in 1:nt)Find[i,]<-Ftrendfunc(M1=M1sim[i],M2=M2sim[i],sd1=sd1sim[i],sd2=sd2sim[i],h2=h2sim[i],ny=Nyears,loc=locsim,start_mag=2-stmagsim,co=cosim,bm=F,plot=F)
  Find<-Find/apply(Find,1,max)

  df<-NULL
  for(i in 1:nt){

    df<-rbind(df,data.frame(x=yrs,y=Find[i,],series=rep(i,Nyears)))

  }

  eff_values$df<-df
  eff_values$stack<-df
  eff_values$series<-nt


}
