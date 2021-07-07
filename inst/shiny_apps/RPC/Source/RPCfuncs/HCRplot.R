
HCR_plot<-function(input){

  if(input$MS_IVar==1){
    xmax<-2.5
  }else{
    xmax=1.2
  }

  if(input$MS_IVar==1){
    xlab1="SSB/SSB[MSY]"
  }else if(input$MS_IVar==2){
    xlab1="SSB/Initial~SSB[0]"
  }else if(input$MS_IVar==3){
    xlab1="SSB/Dynamic~SSB[0]"
  }else if(input$MS_IVar==4){
    xlab1="F/F[MSY]"
  }else if(input$MS_IVar==5){
    xlab1="F/F[0.1]"
  }else{
    xlab1=paste0("F/F[", 100 * input$SPR_OCP, "~'%'~SPR]")
  }

  if(input$MS_Origin=="Perfect"){
    lab2="(perfect~information)"
  }else if(input$MS_Origin=="SCA_Pope"){
    lab2="(estimated~by~stock~assesment)"
  }else {
    lab2="(from~assessment~emulator)"
  }

  if(input$MS_DVar==1){
    ylab1="F/F[MSY]"
  }else if(input$MS_DVar==2){
    ylab1="F/F[0.1]"
  }else if(input$MS_DVar==3){
    ylab1="F/F[max]"
  }else{
    ylab1=paste0("F/F[", 100 * input$SPR_targ, "~'%'~SPR]")
  }

  xlab<-parse(text = paste0(xlab1, "~", lab2))
  ylab<-parse(text = paste0(ylab1, "~", lab2))

  if(input$MS_control==1){
    xs<-c(0,xmax)
    ys<-rep(input$CP_yint,2)
    linecol='orange'

  }else{
    xs<-c(0,input$CP_1_x,input$CP_2_x,xmax)
    ys<-c(input$CP_1_y,input$CP_1_y,input$CP_2_y,input$CP_2_y)
    linecol="#0000ff95"
  }

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
