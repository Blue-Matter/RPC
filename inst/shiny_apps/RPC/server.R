

options(shiny.maxRequestSize=1000*1024^2)

server <- function(input, output, session) {

  #All_MPs<<-avail('MP')
  #Sel_MPs<<-""

  OMs<<-unique(avail('OM')[avail('OM')!='testOM'], c("DFO_BoF_Herring","DFO_DEMO1","DFO_DEMO2","DFO_Inside_YE_Rockfish"))


  # ---- Initialize Reactive Values -----
  # Operating model selected, loaded or sketched
  OM_L<-reactiveVal(0)
  output$OM_L <- reactive({ OM_L()})
  outputOptions(output,"OM_L",suspendWhenHidden=FALSE)

  MSErun<-reactiveVal(0)
  output$MSErun <- reactive({MSErun()})
  outputOptions(output, "MSErun",suspendWhenHidden=FALSE)

  MPs<<-reactiveValues(Sel="",All=avail('MP'))
  output$Sel <- reactive({ MPs$Sel})
  output$All <- reactive({MPs$All})
  outputOptions(output,"All",suspendWhenHidden=FALSE)
  outputOptions(output,"Sel",suspendWhenHidden=FALSE)
  updateSelectInput(session,"HS_sel",choices=avail('MP'),selected="")

  for (fl in list.files("./Source/MERA")) source(file.path("./Source/MERA", fl), local = TRUE)

  # MERA questions

  Fpanel<-reactiveVal(0)
  Mpanel<-reactiveVal(0)
  Dpanel<-reactiveVal(0)
  output$Fpanel <- reactive({ Fpanel()})
  output$Mpanel <- reactive({ Mpanel()})
  output$Dpanel <- reactive({ Dpanel()})
  outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Mpanel",suspendWhenHidden=FALSE)
  outputOptions(output,"Dpanel",suspendWhenHidden=FALSE)


  # Log ----------------------------------------------------------
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)
  Copyright<-"Creative Commons"

  Log_text <- reactiveValues(text=paste0("-------- Start of Session -------- \nSession ID: ",SessionID,"\nUser ID: ",USERID))
  output$Log <- renderText(Log_text$text)
  AM<<-function(newtext)    Log_text$text<-paste(newtext, Log_text$text, sep = "\n")


  # Fishery panel ---------------------------------------------------

  # OM select
  observeEvent(input$SelectOM,{
    OM_temp <- get(input$SelectOMDD)
    OM <<- modOM(OM_temp,nsim)
    runMSEhist(OM)
    OM_L(1)
    MSErun(0)
    updateVerticalTabsetPanel(session,'Main',selected=3)
    AM(paste("Operating model",input$SelectOMDD,"selected"))

  })

  # OM load
  observeEvent(input$Load_OM,{

    filey<-input$Load_OM

    tryCatch({

      OM_temp<-readRDS(file=filey$datapath)
      OM<<-modOM(OM_temp,nsim)


    },

    error = function(e){

      AM(paste0(e,"\n"))
      shinyalert("File read error", "This does not appear to be a MSEtool OM object, saved by saveRDS()", type = "error")
      AM(paste0("Operating model failed to load: ", filey$datapath))
      return(0)

    })

    if(class(OM)=='OM'){

      AM(paste0("Operating model loaded: ", filey$datapath))
      runMSEhist(OM)
      OM_L(1)
      MSErun(0)
      updateVerticalTabsetPanel(session,'Main',selected=3)

    }else{

      shinyalert("Incorrect class of object", "This file should be an object of MSEtool class 'OM'", type = "error")
      AM(paste0("Object not of class 'OM'", filey$datapath))

    }

  })

  observeEvent(input$BuildOM,{
    temp<-checkQs(input)

    if(temp$error){
      shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered or answered incorrectly:",paste(temp$probQs,collapse=", ")), type = "warning")
    }else{
      doprogress("Reading sketch",1)
      OM<<-makeOM(input,PanelState,nsim)
    }

    runMSEhist(OM)
    OM_L(1)
    MSErun(0)
    updateVerticalTabsetPanel(session,'Main',selected=3)
    AM(paste0("Operating model sketched: ", OM@Name))

  })


  # Management Strategy Panel -----------------------------------------------------

  output$HSplot <- renderPlot(HCR_plot(input))

  observeEvent(input$CP_1_x,{

    x2<-input$CP_2_x
    x1<-input$CP_1_x
    if(x2<x1)updateSliderInput(session,"CP_2_x",value=x1)

  })

  observeEvent(input$CP_2_x,{

    x1<-input$CP_1_x
    x2<-input$CP_2_x
    if(x1>x2)updateSliderInput(session,"CP_1_x",value=x2)

  })

  observeEvent(input$HS_sel,{

    if(MPs$Sel[1]==""){
      MPs$Sel<<-input$HS_sel
    }else{
      MPs$Sel<<-c(MPs$Sel,input$HS_sel)
    }

    updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)

  })


  observeEvent(input$Build_MS,{

    make_RPC_MP(input)
    updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)

  })


  # Results panel ------------------------------------------------

  observeEvent(input$runMSE,{

    withProgress({
      MSEproj<<-Project(MSEhist, MPs=unique(MPs$Sel), extended = T)
      SMP1<-MSEproj@MPs[1]
      SMP2 <- MSEproj@MPs[MSEproj@nMPs]
      #if(MSEproj@nMPs>2)SMP3 <- MSEproj@MPs[3]

      updateSelectInput(session,'SMP1',choices=MSEproj@MPs,selected = SMP1)
      updateSelectInput(session,'SMP2',choices=MSEproj@MPs,selected = SMP2)
      updateSelectInput(session,'StochMP',choices=MSEproj@MPs,selected = SMP1)
      # updateSelectInput(session,'SMP3',choices=MSEproj@MPs,selected = SMP3)

      MSErun(1)

      #saveRDS(MSEproj,"C:/temp/MSEproj.rda")

    })
  })




  output$B_proj_plot <- renderPlot(B_proj_plot())
  output$B_prob_plot <- renderPlot(B_prob_plot())
  output$B_stoch_plot <- renderPlot(B_stoch_plot(input))
  output$plot_hist_SSB_sim <- renderPlot(hist_SSB_sim(input))

  # OM panel --------------------------------------------------

  output$plot_hist_bio <- renderPlot(hist_bio())
  output$plot_hist_growth_I <- renderPlot(hist_growth_I())
  output$plot_hist_growth_II <- renderPlot(hist_growth_II())
  output$plot_hist_growth_III <- renderPlot(hist_growth_III())
  output$plot_hist_maturity <- renderPlot(hist_maturity())
  output$plot_hist_survival <- renderPlot(hist_survival())
  output$plot_hist_spatial <- renderPlot(hist_spatial())
  output$plot_hist_exp <- renderPlot(hist_exp())


  # Log  --------------------------------------------------------

  output$Download_Log <-downloadHandler(

    filename = function(){"RPC_Log.txt"}, #"report.html",

    content = function(file) {
      writeLines(paste(Log_text$text, collapse = ", "), file)
    }

  )




  # MERA panel --------------------------------------------------------

  UpPanelState<-function(input){
    for(i in 1:2){
      for(j in 1:length(PanelState[[i]])) {
        value<-sapply(inputnames[[i]][j],function(x) input[[x]])
        PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
      }
    }
    i<-3
    for(j in 2:length(PanelState[[i]])) {
      value<-sapply(inputnames[[i]][j],function(x) input[[x]])
      PanelState[[i]][[j]] <<- get(MasterList[[i]][j])%in%value
    }
  }

  checkQs<-function(input){

    Qs<-unlist(lapply(PanelState[1:3],function(x)lapply(x,sum)))
    Qs[names(Qs)=="Fpanel4"]<-6
    Qnams<-c(paste0("F",2:19),paste0("M",1:7),paste0("D",1:4))

    if(any(!(eff_values$df$x %in% input$Syear:input$Lyear))){
      Qs[names(Qs)=="Fpanel4"]<-0 # error in effort prescription
      AM("Error. Inconsistency in Questionnaire specification: The years specified in Fishery Question 1 do not match the effort drawn in Fishery Question 5")
    }
    list(error=any(Qs==0),probQs=Qnams[Qs==0])

  }

  output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})
  output$Mpanelout <- renderText({ paste("Management",Mpanel(),"/ 7")})
  output$Dpanelout <- renderText({ paste("Data",Dpanel(),"/ 3")})

  makeState<-function(x)c(F,rep(T,length(get(x))-2),F)

  Fpanel_names<-c("M_list","D_list","h_list","FP_list","F_list","qh_list","q_list","LM_list","sel_list","dome_list","DR_list","PRM_list","sigR_list","Ah_list","Vh_list","A_list","V_list","Dh_list")
  Mpanel_names<-c("M1_list","IB_list","IV_list","IBE_list","IVE_list","IBSL_list","IVSL_list")
  Dpanel_names<-c("D1_list","CB_list","Beta_list","Err_list")

  MasterList<<-list(Fpanel_names,Mpanel_names,Dpanel_names)

  PanelState<<-list(Fpanel=lapply(Fpanel_names, makeState),
                    Mpanel=lapply(Mpanel_names, makeState),
                    Dpanel=lapply(Dpanel_names, makeState))

  PanelState[[1]][[18]]<<-c(F,F,F,F,T) # Exception is the final fishery initial depletion
  PanelState[[3]][[4]]<<-c(F,F,F,T) # Exception is the final selection of the data menu - quality is a radio button default to data-poor

  getinputnames<-function(x)strsplit(x,"_")[[1]][1]

  inputnames<<-list(Fpanel=lapply(Fpanel_names, getinputnames),
                    Mpanel=lapply(Mpanel_names, getinputnames),
                    Dpanel=lapply(Dpanel_names, getinputnames))

  inputtabs<-as.vector(unlist(inputnames))

  # Record all changes to tabs
  observeEvent(sapply(inputtabs, function(x) input[[x]]),{
    UpPanelState(input)
  })

  # ======================= Explanatory Plots ===================================
  # Scheme
  fcol = rgb(0.4,0.8,0.95) #"#0299f"
  fcol2 = "dark grey"
  icol <- "dodgerblue4"
  maxcol="cadetblue"
  mincol="dark grey"

  # Fishery
  output$plotM <- renderPlot(plotM())
  output$plotD <- renderPlot(plotD())
  output$ploth <- renderPlot(ploth())
  output$plotFP <- renderPlot(plotFP())
  output$plotF <- renderPlot(plotF())
  output$plotqh <- renderPlot(plotqh())
  output$plotq <- renderPlot(plotq())
  output$plotLM <- renderPlot(plotLM())
  output$plotmat <- renderPlot(plotmat())
  output$plotsel <- renderPlot(plotsel())
  output$plotdome <- renderPlot(plotdome())
  output$plotDR <- renderPlot(plotDR())
  output$plotPRM <- renderPlot(plotPRM())
  output$plotsigR <- renderPlot(plotsigR())
  output$plotAh <- renderPlot(plotAh())
  output$plotVh <- renderPlot(plotVh())
  output$plotA <- renderPlot(plotA())
  output$plotV <- renderPlot(plotV())
  output$plotDh <- renderPlot(plotDh()) #19

  # Management
  output$plotIB <- renderPlot(plotIB())
  output$plotIV <- renderPlot(plotIV())
  output$plotIB_E <- renderPlot(plotIB_E())
  output$plotIV_E <- renderPlot(plotIV_E())
  output$plotIB_SL <- renderPlot(plotIB_SL())
  output$plotIV_SL <- renderPlot(plotIV_SL()) # 7

  # Data
  output$plotBeta <- renderPlot(plotBeta())
  output$plotCB <- renderPlot(plotCB())

  observeEvent(input$Fback,{

    if(input$tabs1==1 && Fpanel() >0){
      Fpanel(Fpanel()-1)
    }else if(input$tabs1==2 && Mpanel() >0){
      Mpanel(Mpanel()-1)
    }else if(input$tabs1==3 && Dpanel() >0){
      Dpanel(Dpanel()-1)
    }

  })

  observeEvent(input$Fcont,{

    if(input$tabs1==1 && Fpanel() < 19){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 7){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 3){
      Dpanel(Dpanel()+1)
    }

  })

  observeEvent(input$Fcont_red,{

    if(input$tabs1==1 && Fpanel() < 19){
      Fpanel(Fpanel()+1)
    }else if(input$tabs1==2 && Mpanel() < 7){
      Mpanel(Mpanel()+1)
    }else if(input$tabs1==3 && Dpanel() < 3){
      Dpanel(Dpanel()+1)
    }

  })


  # ---- Fishery all toggles -----------------

  observeEvent(input$All_M,  updateCheckboxGroupInput(session,"M",choices=M_list,selected=M_list))
  observeEvent(input$All_D,  updateCheckboxGroupInput(session,"D",choices=D_list,selected=D_list))
  observeEvent(input$All_h,  updateCheckboxGroupInput(session,"h",choices=h_list,selected=h_list))
  observeEvent(input$All_F, updateCheckboxGroupInput(session,"F",choices=F_list,selected=F_list))
  observeEvent(input$All_qh, updateCheckboxGroupInput(session,"qh",choices=q_list,selected=q_list))
  observeEvent(input$All_q,  updateCheckboxGroupInput(session,"q",choices=q_list,selected=q_list))
  observeEvent(input$All_LM, updateCheckboxGroupInput(session,"LM",choices=LM_list,selected=LM_list))
  observeEvent(input$All_sel,updateCheckboxGroupInput(session,"sel",choices=sel_list,selected=sel_list))
  observeEvent(input$All_dome, updateCheckboxGroupInput(session,"dome",choices=dome_list,selected=dome_list))
  observeEvent(input$All_DR, updateCheckboxGroupInput(session,"DR",choices=DR_list,selected=DR_list))
  observeEvent(input$All_PRM, updateCheckboxGroupInput(session,"PRM",choices=PRM_list,selected=PRM_list))
  observeEvent(input$All_sigR, updateCheckboxGroupInput(session,"sigR",choices=sigR_list,selected=sigR_list))
  observeEvent(input$All_Ah, updateCheckboxGroupInput(session,"Ah",choices=Ah_list,selected=Ah_list[[1]]))
  observeEvent(input$All_Vh, updateCheckboxGroupInput(session,"Vh",choices=Vh_list,selected=Vh_list[[length(Vh_list)]]))
  observeEvent(input$All_A, updateCheckboxGroupInput(session,"A",choices=A_list,selected=input$Ah))
  observeEvent(input$All_V, updateCheckboxGroupInput(session,"V",choices=V_list,selected=input$Vh))
  observeEvent(input$All_Dh,
               if(input$All_Dh == 0 | input$All_Dh%%2 == 0){
                 updateCheckboxGroupInput(session,"Dh",choices=Dh_list,selected=Dh_list[[5]])
               }else{
                 updateCheckboxGroupInput(session,"Dh",choices=Dh_list,selected=Dh_list[[5]])
               }
  )

  # ---- Management all toggles -------------
  observeEvent(input$All_M1, updateCheckboxGroupInput(session,"M1",choices=M1_list,selected=M1_list))
  observeEvent(input$All_IB, updateCheckboxGroupInput(session,"IB",choices=IB_list,selected=IB_list))
  observeEvent(input$All_IV, updateCheckboxGroupInput(session,"IV",choices=IV_list,selected=IV_list))
  observeEvent(input$All_IBE, updateCheckboxGroupInput(session,"IBE",choices=IBE_list,selected=input$IB))
  observeEvent(input$All_IVE, updateCheckboxGroupInput(session,"IVE",choices=IVE_list,selected=input$IV))
  observeEvent(input$All_IBSL, updateCheckboxGroupInput(session,"IBSL",choices=IBSL_list,selected=IBSL_list[length(IB_list)-match(input$IB,IB_list)+1]))
  observeEvent(input$All_IVSL, updateCheckboxGroupInput(session,"IVSL",choices=IVSL_list,selected=input$IV))

  # ---- Data all toggles -------------
  observeEvent(input$All_D1, updateCheckboxGroupInput(session,"D1",choices=D1_list,selected=D1_list))
  observeEvent(input$All_CB, updateCheckboxGroupInput(session,"CB",choices=CB_list,selected=CB_list))
  observeEvent(input$All_Beta, updateCheckboxGroupInput(session,"Beta",choices=Beta_list,selected=Beta_list))
  observeEvent(input$All_Err, updateRadioButtons(session,"Err",choices=Err_list,selected="Err_bad"))




  # Effort sketching

  eff_values <- reactiveValues(df=data.frame(x=c(1951,1980,2018), y=c(0,0.5,0.5), series=rep(1,3)),
                               series=1,
                               stack=data.frame(x=c(1951,1980,2018), y=c(0,0.5,0.5), series=rep(1,3)))

  reset_eff_values<-function(){
    eff_values$df=data.frame(x=c(input$Syear,floor(mean(c(input$Syear,input$Lyear))),input$Lyear), y=c(0,0.5,0.5), series=rep(1,3))
    eff_values$series=1
    eff_values$stack=data.frame(x=c(input$Syear,floor(mean(c(input$Syear,input$Lyear))),input$Lyear), y=c(0,0.5,0.5), series=rep(1,3))
  }

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Yr. = ", round(e$x, 1), " Eff. = ", round(e$y, 3), "\n")
    }
    temp <- input$plot_hover
    if(!is.null(temp$x) & !is.null(temp$y)) temp$x <- round(temp$x,0)
    if(!is.null(temp)){
      paste0(xy_str(temp))
    }else{
      " "
    }
  })

  output$effort_plot <- renderPlot({
    # first series
    plotFP()
    #print(eff_values$df) # for debugging & use elsewhere in the app
  })

  observeEvent(input$plot_click$x, {

    newX <- round(input$plot_click$x,0)
    newY <- input$plot_click$y

    # check if x value already exists
    ind <- which(eff_values$df$x == newX & eff_values$df$series == eff_values$series)
    if (length(ind)>0) eff_values$df <- eff_values$df[-ind,]

    eff_values$stack <- data.frame(x=c(eff_values$stack$x,newX),
                                   y=c(eff_values$stack$y, newY),
                                   series=c(eff_values$stack$series, eff_values$series))

    tempDF <- data.frame(x=c(eff_values$df$x, newX),
                         y=c(eff_values$df$y, newY),
                         series=c(eff_values$df$series, eff_values$series))

    tempDF <- dplyr::arrange(tempDF, series, x)

    eff_values$df <- tempDF

  })


  observeEvent(input$new_series, {
    # check that last series is complete
    lastX <- eff_values$df$x[nrow(eff_values$df)]
    lstYr<-input$Lyear
    initYr<-input$Syear
    #tempyrs<-getyrs()
    #nyears <- length(tempyrs)
    #initYr <- tempyrs[1] # initial year
    #lstYr <- initYr + nyears-1
    yvals <- 0 # initial effort
    AM(paste("lastX",lastX))
    AM(paste("lstYr",lstYr))
    if (lastX != lstYr) {
      #showNotification("Series must include last historical year", type="error")
      #shinyalert("Incomplete effort series", paste0("Series must include last historical year (",CurrentYr,")"), type = "info")
      vec<-c(input$Lyear,eff_values$df$y[nrow(eff_values$df)],eff_values$df$series[nrow(eff_values$df)])
      eff_values$df<-rbind(eff_values$df,vec)
      eff_values$stack<-rbind(eff_values$stack,vec)
    }

    eff_values$series <-eff_values$series+1
    eff_values$df <- data.frame(x=c(eff_values$df$x, initYr),
                                y=c(eff_values$df$y, yvals),
                                series=c(eff_values$df$series, eff_values$series))

  })

  observeEvent(input$undo_last, {
    # remove the last point
    if (nrow(eff_values$df)>1) {
      nrows <- nrow(eff_values$stack)
      last_vals <- eff_values$stack[nrows,]
      eff_values$stack <- eff_values$stack[1:(nrows-1),]
      eff_values$df <- dplyr::anti_join(eff_values$df, last_vals, by=c('x', 'y', 'series'))
    }
  })

  observeEvent(input$reset_plot, {
    reset_eff_values()
  })




  observeEvent(input$MS_Frat,{getMPs('Frat'); updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)})
  observeEvent(input$MS_Crat,{getMPs('Crat'); updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)})
  observeEvent(input$MS_DFO,{getMPs('DFO'); updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)})
  observeEvent(input$MS_Clear,{AM("MP selection cleared"); MPs$Sel<<-""; updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)})

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

}
