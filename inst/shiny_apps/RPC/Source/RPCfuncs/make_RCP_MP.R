
getMPs<-function(type="Frat"){

  if(type=="Frat")MPstr_temp<-c('CurF_200','CurF_150','CurF_100','CurF_050','CurF_000')
  if(type=="Crat")MPstr_temp<-c('CurC_200','CurC_150','CurC_100','CurC_050','CurC_000')
  if(type=="DFO")MPstr_temp<-c('SCA_4010','SCA_6020', 'SCA_MSY','SCA_75MSY')

  MPs$All<<-c(MPs$All,MPstr_temp)

  if(MPs$Sel[1]==""){
    MPs$Sel<<-MPstr_temp
  }else{
    MPs$Sel<<-c(MPs$Sel,MPstr_temp)
  }


}




make_RPC_MP<-function(input) {
  Assess <- input$MS_Origin
  Ass <<- get(Assess)

  if(input$MS_IVar==1) { # SSBMSY
    OCP_type <- "SSB_SSBMSY"
  } else if(input$MS_IVar==2) { # initial SSB0
    OCP_type <- "SSB_SSB0"
  } else if(input$MS_IVar==3) {
    OCP_type <- "SSB_dSSB0"
  } else if(input$MS_IVar==4) {
    OCP_type <- "F_FMSY"
  } else if(input$MS_IVar==5) {
    OCP_type <- "F_F01"
  } else if(input$MS_IVar==6) {
    OCP_type <- "F_FSPR"
  }
  SPR_OCP <- ifelse(input$MS_IVar==6, input$SPR_OCP, NA_real_)

  if(input$MS_DVar==1) {
    Ftarget_type="FMSY"
  } else if(input$MS_DVar==2) {
    Ftarget_type="F01"
  } else if(input$MS_DVar==3) {
    Ftarget_type="Fmax"
  } else{
    Ftarget_type="FSPR"
  }
  SPR_targ <- ifelse(input$MS_DVar==4, input$SPR_targ, NA_real_)

  if(input$MS_control==1){
    relF_min=input$CP_yint
    relF_max=input$CP_yint
    LOCP <- TOCP <- 0
  }else{
    relF_min=input$CP_1_x
    relF_max=input$CP_2_x
    LOCP=input$CP_1_y
    TOCP=input$CP_2_y
  }

  #if(input$MS_Label==""){
  #  MPstr_prefix<-"MP"
  #}else{
  #  MPstr_prefix<-input$MS_Label
  #}

  #MPstr_temp<-paste(MPstr_prefix,"1",sep="_")

  #while(MPstr_prefix %in% MPs$Sel){
  #  ind<-as.numeric(strsplit(MPstr_temp,"_")[[1]][2])+1
  #  MPstr_temp<-paste(MPstr_prefix,ind,sep="_")
  #}
  MPstr_temp <- input$MS_HCR_Label

  MPs$All <<- c(MPs$All, MPstr_temp)

  if(MPs$Sel[1]==""){
    MPs$Sel<<-MPstr_temp
  }else{
    MPs$Sel<<-c(MPs$Sel,MPstr_temp)
  }

  if(packageVersion("SAMtool") >= "1.2.1") {
    assign(MPstr_temp, make_MP(Ass, HCR_segment, OCP_type = OCP_type, Ftarget_type = Ftarget_type,
                               OCP = c(LOCP, TOCP), relF = c(relF_min, relF_max),
                               SPR_OCP = SPR_OCP, SPR_targ = SPR_targ),
           envir = .GlobalEnv)
    AM(paste0("Management Procedure '", MPstr_temp,"' constructed as:\n",
              paste0("  make_MP(Assess = ", Assess, ", HCR = HCR_ramp, OCP_type = \"", OCP_type, "\", Ftarget_type = \"", Ftarget_type,
                     "\", LOCP = ", LOCP, ", TOCP = ", TOCP, ", relF_min = ", relF_min, ", refF_max = ", relF_max,
                     ", SPR_OCP = ", SPR_OCP, ", SPR_targ = ", SPR_targ, ")")))
  } else {
    assign(MPstr_temp, make_MP(Ass, HCR_ramp, OCP_type = OCP_type, Ftarget_type = Ftarget_type, LOCP = LOCP, TOCP = TOCP,
                               relF_min = relF_min, relF_max = relF_max, SPR_OCP = SPR_OCP, SPR_targ = SPR_targ),
           envir = .GlobalEnv)
    AM(paste0("Management Procedure '", MPstr_temp,"' constructed as:\n",
              paste0("  make_MP(Assess = ", Assess, ", HCR = HCR_ramp, OCP_type = \"", OCP_type, "\", Ftarget_type = \"", Ftarget_type,
                     "\", LOCP = ", LOCP, ", TOCP = ", TOCP, ", relF_min = ", relF_min, ", refF_max = ", relF_max,
                     ", SPR_OCP = ", SPR_OCP, ", SPR_targ = ", SPR_targ, ")")))
  }
}
