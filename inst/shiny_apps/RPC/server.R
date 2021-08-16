


options(shiny.maxRequestSize=1000*1024^2)

server <- function(input, output, session) {

  #for (fl in list.files("./Source/Local")) source(file.path("./Source/Local", fl), local = TRUE)

  #All_MPs<<-avail('MP')
  #Sel_MPs<<-""

  plotres<-100
  OMs<<-unique(avail('OM', msg = FALSE)[avail('OM', msg = FALSE)!='testOM'],
               c("DFO_BoF_Herring","DFO_DEMO1","DFO_DEMO2","DFO_Inside_YE_Rockfish"))

  # ---- Initialize Reactive Values -----
  # Operating model selected, loaded or sketched
  OM_L<-reactiveVal(0)
  output$OM_L <- reactive({ OM_L()})
  outputOptions(output,"OM_L",suspendWhenHidden=FALSE)

  MPsSpec<-reactiveVal(0)
  output$MPsSpec <- reactive({MPsSpec()})
  outputOptions(output, "MPsSpec",suspendWhenHidden=FALSE)

  MSErun<-reactiveVal(0)
  output$MSErun <- reactive({MSErun()})
  outputOptions(output, "MSErun",suspendWhenHidden=FALSE)

  MPs<<-reactiveValues(Sel="No_Fishing",All=avail('MP', msg = FALSE))
  output$Sel <- reactive({MPs$Sel})
  output$All <- reactive({MPs$All})
  outputOptions(output,"All",suspendWhenHidden=FALSE)
  outputOptions(output,"Sel",suspendWhenHidden=FALSE)
  #updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)

  OBJs<<-reactiveValues(MSEhist="",MSEproj="")

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

  # Load session ------------------------------------------------
  observeEvent(input$Load_session, {
    filey <- input$Load_session
    tryCatch({
      prev_session <- readRDS(file = filey$datapath)
      stopifnot(inherits(prev_session, "list"))

      OBJs$MSEhist <<- prev_session$MSEhist
      OBJs$MSEproj <<- prev_session$MSEproj

      if(inherits(prev_session$MSEproj, "MSE")) {
        AM(paste("MSE results loaded:", filey$name))
        OM_L(1)
        MSErun(1)
        updateVerticalTabsetPanel(session, "Main", selected = 5)

      } else if(inherits(prev_session$MSEhist, "Hist")) {
        AM(paste("Operating model loaded:", filey$name))
        OM_L(1)
        MSErun(0)
        updateVerticalTabsetPanel(session, "Main", selected = 3)

      } else {
        AM(paste("No operating model was found in file:", filey$name))
      }

      if(!is.null(prev_session$MPs)) {
        MPs$All <<- prev_session$MPs$All
        MPs$Sel <<- prev_session$MPs$Sel
      }
      if(is.environment(prev_session$MPs_save) && length(ls(envir = prev_session$MPs_save))) {
        MP_out <- ls(envir = prev_session$MPs_save)
        lapply(ls(envir = prev_session$MPs_save),
               function(x) assign(x, get(x, envir = prev_session$MPs_save), envir = .GlobalEnv))
      }
    },

    error = function(e){
      AM(paste0(e,"\n"))
      shinyalert("File read error", "Previous RPC session could not be loaded.", type = "error")
      AM(paste0("Previous RPC session file could not be loaded: ", filey$name))
      return(0)
    })
  })

  # Save session ------------------------------------------------
  output$Save_session <- downloadHandler(
    filename = paste0("RPC-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rpc"),
    content = function(file) {
      MPs_globalenv <- sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "MP"))
      MPs_save <- new.env()
      if(sum(MPs_globalenv)) {
        lapply(names(MPs_globalenv)[MPs_globalenv],
               function(x) assign(x, value = get(x, envir = .GlobalEnv), envir = MPs_save))
      }
      saveRDS(list(MSEhist = OBJs$MSEhist, MSEproj = OBJs$MSEproj,
                   MPs = list(All = MPs$All, Sel = MPs$Sel), MPs_env = MPs_save),
              file = file)
    }
  )

  # Fishery panel ---------------------------------------------------

  # OM select
  observeEvent(input$SelectOMDD,{
    OM_temp <- get(input$SelectOMDD)
    updateSliderInput(session, "Custom_nsim",
                      min = min(OM_temp@nsim, 3), max = OM_temp@nsim, value = min(OM_temp@nsim, nsim))
    updateSliderInput(session, "Custom_proyears",
                      min = min(OM_temp@proyears, 5), max = OM_temp@proyears, value = OM_temp@proyears)
  })

  observeEvent(input$SelectOM,{
    OM <- modOM(get(input$SelectOMDD), input$Custom_nsim, input$Custom_proyears)
    OBJs$MSEhist <<- runMSEhist(OM)
    OM_L(1)
    MSErun(0)
    updateVerticalTabsetPanel(session,'Main',selected=3)
    AM(paste("Operating model",input$SelectOMDD,"selected"))
  })

  # OM load
  observeEvent(input$Load_OMprelim,{
    filey <- input$Load_OMprelim
    tryCatch({

      OM_temp <- readRDS(file=filey$datapath)
      stopifnot(inherits(OM_temp, "OM"))

      updateSliderInput(session, "Custom_nsim_load",
                        min = min(OM_temp@nsim, 3), max = OM_temp@nsim, value = min(OM_temp@nsim, nsim))
      updateSliderInput(session, "Custom_proyears_load",
                        min = min(OM_temp@proyears, 5), max = OM_temp@proyears, value = OM_temp@proyears)
      AM(paste0("Operating model loaded: ", filey$name))
    },

    error = function(e){

      AM(paste0(e,"\n"))
      shinyalert("File read error", "This does not appear to be a MSEtool OM object, saved by saveRDS()", type = "error")
      AM(paste0("Operating model failed to load: ", filey$name))
      return(0)

    })
  })

  observeEvent(input$Load_OM,{
    OM_temp <- readRDS(file = input$Load_OMprelim$datapath)
    OM <- modOM(OM_temp, input$Custom_nsim_load, input$Custom_proyears_load)
    OBJs$MSEhist <<- runMSEhist(OM)

    OM_L(1)
    MSErun(0)
    updateVerticalTabsetPanel(session,'Main',selected=3)
  })

  observeEvent(input$BuildOM,{
    temp<-checkQs(input)

    if(temp$error){
      shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered or answered incorrectly:",paste(temp$probQs,collapse=", ")), type = "warning")
    }else{
      doprogress("Reading sketch",1)
      OM<-makeOM(input,PanelState,nsim)
    }

    OBJs$MSEhist <<- runMSEhist(OM)
    OM_L(1)
    MSErun(0)
    updateVerticalTabsetPanel(session,'Main',selected=3)
    AM(paste0("Operating model sketched: ", OM@Name))

  })


  # Historical results panel -----------------------------------------------------
  observeEvent(input$HistRes1, {
    req(inherits(OBJs$MSEhist, "Hist"))
    SSB_max <- apply(OBJs$MSEhist@TSdata$SBiomass, 1:2, sum) %>% max() %>% ceiling()
    R_max <- apply(OBJs$MSEhist@AtAge$Number[, 1, , ], 1:2, sum) %>% max() %>% ceiling()

    Hist_yr <- seq(OBJs$MSEhist@OM@CurrentYr - OBJs$MSEhist@OM@nyears + 1, OBJs$MSEhist@OM@CurrentYr)

    updateSliderInput(session, "SSB_y", min = min(Hist_yr), max = max(Hist_yr), value = max(Hist_yr))

    updateSliderInput(session, "SR_xrange", min = 0, max = SSB_max, value = c(0, 1.1 * SSB_max), step = SSB_max/100)
    updateSliderInput(session, "SR_yrange", min = 0, max = R_max, value = c(0, 1.1 * R_max), step = R_max/100)
    updateSliderInput(session, "SR_y_RPS0", min = min(Hist_yr), max = max(Hist_yr), value = max(Hist_yr))

  })

  output$hist_SSB_plot <- renderPlot(hist_SSB(OBJs), res = plotres)
  output$hist_SSB_table <- renderTable(hist_SSB(OBJs, figure = FALSE), rownames = TRUE)

  observeEvent({
    input$SSB_y
    input$SSB_prob
    input$SSB_yrange
  }, {
    output$hist_SSB_prob <- renderPlot(hist_SSB(OBJs, prob_ratio = input$SSB_prob,
                                                SSB_y = input$SSB_y, prob_ylim = input$SSB_yrange),
                                        res = plotres)
    output$hist_SSB_prob_table_label <- renderText(paste("Annual probability that SSB has exceeded", 100 * input$SSB0_prob, "% SSB in", input$SSB_y))
    output$hist_SSB_prob_table <- renderTable(hist_SSB(OBJs, figure = FALSE, prob_ratio = input$SSB_prob,
                                                       SSB_y = input$SSB_y), rownames = TRUE)
  })

  output$hist_SSB0_plot<-renderPlot(hist_SSB0(OBJs),res=plotres)
  observeEvent({
    input$SSB0_prob
    input$SSB0_yrange
  }, {
    output$hist_SSB0_prob <- renderPlot(hist_SSB0(OBJs, prob_ratio = input$SSB0_prob, prob_ylim = input$SSB0_yrange),
                                        res = plotres)
    output$hist_SSB0_table_label <- renderText(paste0("Annual probability that SSB/SSB0 > ", input$SSB0_prob))
    output$hist_SSB0_table <- renderTable(hist_SSB0(OBJs, figure = FALSE, prob_ratio = input$SSB0_prob), rownames = TRUE)
  })

  output$hist_SSBMSY_plot <- renderPlot(hist_SSBMSY(OBJs), res = plotres)
  observeEvent({
    input$SSBMSY_prob
    input$SSBMSY_yrange
  }, {
    output$hist_SSBMSY_prob <- renderPlot(hist_SSBMSY(OBJs, prob_ratio = input$SSBMSY_prob, prob_ylim = input$SSBMSY_yrange),
                                          res = plotres)
    output$hist_SSBMSY_table_label <- renderText(paste0("Annual probability that SSB/SSBMSY > ", input$SSBMSY_prob))
    output$hist_SSBMSY_table <- renderTable(hist_SSBMSY(OBJs, figure = FALSE, prob_ratio = input$SSBMSY_prob), rownames = TRUE)
  })

  output$hist_R_plot<-renderPlot(hist_R(OBJs),res=plotres)
  output$hist_R_table<-renderTable(hist_R(OBJs, figure = FALSE), rownames = TRUE, digits = 2)

  output$hist_exp <- renderPlot(hist_exp(OBJs),res=plotres)
  output$hist_exp2 <- renderTable(hist_exp(OBJs, figure = FALSE), rownames = TRUE)
  output$hist_SPR <- renderPlot(hist_SPR(OBJs),res=plotres)
  output$hist_SPR2 <- renderTable(hist_SPR(OBJs, figure = FALSE), rownames = TRUE)

  # Probability
  observeEvent({
    input$exp_type
    input$FMSY_prob
    input$SPR_prob
    input$exp_yrange
  }, {
    if(input$exp_type == "F") {
      output$hist_exp_prob <- renderPlot(hist_exp(OBJs, prob_ratio = input$FMSY_prob, prob_ylim = input$exp_yrange))
      output$hist_exp_table_label <- renderText({
        paste0("Annual probability that F/FMSY > ", 100 * input$FMSY_prob, "%.")
      })
      output$hist_exp_table <- renderTable(hist_exp(OBJs, figure = FALSE, prob_ratio = input$FMSY_prob),
                                           rownames = TRUE)
    } else {
      output$hist_exp_prob <- renderPlot(hist_SPR(OBJs, prob_ratio = input$SPR_prob, prob_ylim = input$exp_yrange))
      output$hist_exp_table_label <- renderText({
        paste0("Annual probability that equilibrium SPR > ", input$SPR_prob)
      })
      output$hist_exp_table <- renderTable(hist_SPR(OBJs, figure = FALSE, prob_ratio = input$SPR_prob),
                                           rownames = TRUE)
    }
  })

  observeEvent({
    input$HistRes1
    input$SR_plot_options
    input$SR_xrange
    input$SR_yrange
    input$SR_y_RPS0
  }, {
    #y_RPS0 <- input$SR_y_RPS0 - OBJs$MSEhist@OM@CurrentYr + OBJs$MSEhist@OM@nyears
    y_RPS0 <- input$SR_y_RPS0

    output$hist_SR_plot <- renderPlot(hist_R(OBJs, SR_only = TRUE,
                                             SR_xlim = input$SR_xrange, SR_ylim = input$SR_yrange, SR_y_RPS0 = y_RPS0,
                                             SR_include = input$SR_plot_options),
                                      res = plotres)
  })

  output$hist_BvsSP_plot<-renderPlot(hist_BvsSP(OBJs),res=plotres)
  output$hist_BvsSP_table<-renderTable(hist_BvsSP(OBJs, figure = FALSE), rownames = TRUE, digits = 0)

  output$hist_RpS_plot<-renderPlot(hist_RpS(OBJs),res=plotres)
  output$hist_RpS_table<-renderTable(hist_RpS(OBJs, figure = FALSE), rownames = TRUE)

  output$hist_Rmax_plot<-renderPlot(hist_Rmax(OBJs),res=plotres)
  output$hist_Rmax_table<-renderTable(hist_Rmax(OBJs, figure = FALSE), rownames = TRUE)

  output$hist_RpS90_plot<-renderPlot(hist_RpS90(OBJs),res=plotres)
  output$hist_RpS90_table<-renderTable(hist_RpS90(OBJs, figure = FALSE), rownames = TRUE)

  # Management Strategy Panel -----------------------------------------------------
  output$MS_FixF_ratio_label <- renderText(paste0("Ratio of F relative to last historical year (", OBJs$MSEhist@OM@CurrentYr, "). Set to 1 for status quo."))
  observeEvent(input$MS_FixF_ratio, {
    updateTextInput(session, "MS_FixF_Label", value = paste0("CurF_", 100 * input$MS_FixF_ratio))
  })

  output$MS_FixC_ratio_label <- renderText(paste0("Ratio of catch relative to last historical year (", OBJs$MSEhist@OM@CurrentYr, "). Set to 1 for status quo."))
  observeEvent(input$MS_FixC_ratio, {
    updateTextInput(session, "MS_FixC_Label", value = paste0("CurC_", 100 * input$MS_FixC_ratio))
  })

  observeEvent({
    input$MS_Origin
    input$MS_DVar
    input$MS_control
    input$MS_IVar
    input$CP_yint
    input$CP_1_x
    input$CP_1_y
    input$CP_2_x
    input$CP_2_y
  }, {

    mod_name <- switch(input$MS_Origin, "Perfect" = "Perf", "SCA_Pope" = "Assess", "Shortcut2" = "Short")
    output_name <- switch(input$MS_DVar, "1" = "FMSY", "2" = "F01", "3" = "Fmax", "4" = "FSPR")

    if(input$MS_control == 1) {
      output_val <- input$CP_yint * 100
      MS_Name <- paste0(mod_name, "_", output_val, output_name)
    } else {
      output_val <- paste0(100 * input$CP_2_y, "/", 100 * input$CP_1_y)

      OCP_name <- switch(input$MS_IVar, "1" = "SSBMSY", "2" = "iSSB0", "3" = "dSSB0",
                         "4" = "FMSY", "5" = "F01", "6" = "FSPR")
      OCP_val <- paste0(100 * input$CP_2_x, "/", 100 * input$CP_1_x)
      MS_Name <- paste0(mod_name, "_", output_val, output_name, "_", OCP_val, OCP_name)
    }

    updateTextInput(session, "MS_HCR_Label", value = MS_Name)
  })

  output$DLM_URL <- renderText("")
  observeEvent(input$MS_DLM, {
    output$DLM_URL <- renderText(paste0("https://dlmtool.openmse.com/reference/", input$MS_DLM, ".html"))
  })

  observeEvent(input$MS_Import_file, {
    filey <- input$MS_Import_file
    updateTextInput(session, "MS_Import_Label", value = filey$name)

    tryCatch({
      MP_out <- readRDS(file = filey$datapath)
      stopifnot(typeof(MP_out) == "closure" && inherits(MP_out, "MP"))
    }, error = function(e) {
      AM(paste0(e,"\n"))
      shinyalert(paste0("No MP was found in file: ", filey$name), type = "error")
      AM(paste0("No MP was found in file: ", filey$name))
      return(0)
    })
  })

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

  observeEvent(input$HS_sel, {
    if(MPs$Sel[1]==""){
      MPs$Sel<<-"No_Fishing"
    }else{
      MPs$Sel<<-c(MPs$Sel,input$HS_sel)
    }
    MPsSpec(1)
    updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
  })

  observeEvent(input$Build_MS_FixF, {
    if(!nchar(input$MS_FixF_Label)) {
      shinyalert("No name for the MP was provided.", type = "error")
    } else if(input$MS_FixF_Label %in% MPs$Sel) {
      AM(paste0("Error: ", input$MS_FixF_Label, " already selected. Choose another name."))
    } else {
      MPs$All <<- c(MPs$All, input$MS_FixF_Label)
      MPs$Sel <<- c(MPs$Sel, input$MS_FixF_Label)

      MP_out <- CurF
      formals(MP_out)$val <- input$MS_FixF_ratio
      assign(input$MS_FixF_Label, structure(MP_out, class = "MP"), envir = .GlobalEnv)

      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
      MPsSpec(1)
    }
  })

  observeEvent(input$Build_MS_FixC, {
    if(!nchar(input$MS_FixC_Label)) {
      shinyalert("No name for the MP was provided.", type = "error")
    } else if(input$MS_FixC_Label %in% MPs$Sel) {
      AM(paste0("Error: ", input$MS_FixC_Label, " MP already selected. Choose another name."))
    } else {
      MPs$All <<- c(MPs$All, input$MS_FixC_Label)
      MPs$Sel <<- c(MPs$Sel, input$MS_FixC_Label)

      MP_out <- CurC
      formals(MP_out)$val <- input$MS_FixC_ratio
      assign(input$MS_FixC_Label, structure(MP_out, class = "MP"), envir = .GlobalEnv)

      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
      MPsSpec(1)
    }
  })

  observeEvent(input$Build_MS,{
    make_RPC_MP(input)
    updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
    MPsSpec(1)

  })

  observeEvent(input$Build_MS_DLM, {
    if(!input$MS_DLM %in% MPs$Sel) {
      MPs$Sel <<- c(MPs$Sel, input$MS_DLM)
      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
      MPsSpec(1)
    }
  })

  observeEvent(input$Build_MS_import, {
    if(!nchar(input$MS_Import_Label)) {
      shinyalert("No name for the MP was provided.", type = "error")
    }

    filey <- input$MS_Import_file
    tryCatch({
      MP_out <- readRDS(file = filey$datapath)
      stopifnot(typeof(MP_out) == "closure" && inherits(MP_out, "MP"))

      if(input$MS_Import_Label %in% MPs$Sel) {
        AM(paste0("Error: ", input$MS_Import_Label, " MP already selected. Choose another name."))
      } else {
        assign(input$MS_Import_Label, MP_out, envir = .GlobalEnv)

        MPs$All <<- c(MPs$All, input$MS_Import_Label)
        MPs$Sel <<- c(MPs$Sel, input$MS_Import_Label)

        updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
        MPsSpec(1)
      }
    }, error = function(e) {
      AM(paste0(e,"\n"))
      shinyalert(paste0("No MP was found in file: ", filey$name), type = "error")
      AM(paste0("No MP was found in file: ", filey$name))
      return(0)
    })
  })

  observeEvent(input$MS_Clear_Last, {
    if(length(MPs$Sel) > 1) {
      AM(paste0(MPs$Sel[length(MPs$Sel)], " MP removed"))
      MPs$Sel <<- MPs$Sel[-length(MPs$Sel)]
      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
    }
  })

  observeEvent(input$MS_Clear_All, {
    AM("MP selection cleared")
    MPs$Sel <<- "No_Fishing"
    updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
    #MPsSpec(1)
  })


  # Results panel ------------------------------------------------

  observeEvent(input$runMSE,{
    tryCatch(
      {
        withProgress(message="Running MSE Simulation test:", {
          MSEproj <- Project(OBJs$MSEhist, MPs=unique(MPs$Sel), extended = TRUE)
          OBJs$MSEproj <<- MSEproj
          MSErun(1)
          #saveRDS(MSEproj,"C:/temp/MSEproj.rda")
        })
      },
      error = function(e){
        shinyalert("MSE did not run", paste("Error:",e), type = "error")
        AM(paste0(e,"\n"))
        MSErun(0)
      }
    )
  })

  output$B_proj_plot <- renderPlot(B_proj_plot(OBJs),res=plotres)

  observeEvent({
    input$Res
    input$SSB0
  }, {
    req(inherits(OBJs$MSEproj, "MSE"))
    updateSliderInput(session, "SSB0_MSE_xrange",
                      min = OBJs$MSEproj@OM$CurrentYr[1] + 1,
                      max = OBJs$MSEproj@OM$CurrentYr[1] + OBJs$MSEproj@proyears,
                      value = OBJs$MSEproj@OM$CurrentYr[1] + c(1, OBJs$MSEproj@proyears))

    updateSelectInput(session, "SMP1", choices = OBJs$MSEproj@MPs, selected = OBJs$MSEproj@MPs[1])
    updateSelectInput(session, "SMP2", choices = OBJs$MSEproj@MPs, selected = OBJs$MSEproj@MPs[OBJs$MSEproj@nMPs])
    updateSelectInput(session, "StochMP", choices = OBJs$MSEproj@MPs, selected = OBJs$MSEproj@MPs[1])
  })

  observeEvent({
    input$SSB0_MSE_prob
    input$SSB0_MSE_yrange
    input$SSB0_MSE_xrange
  }, {
    req(inherits(OBJs$MSEproj, "MSE"))
    output$B_prob_table_label <- renderText({
      paste0("Probability that SSB exceeds ", 100*input$SSB0_MSE_prob, "% SSB0 during ",
             paste0(input$SSB0_MSE_xrange, collapse = " - "))
    })
    output$B_prob_plot <- renderPlot(B_prob_plot(OBJs, frac = input$SSB0_MSE_prob,
                                                 xlim = input$SSB0_MSE_xrange, ylim = input$SSB0_MSE_yrange),
                                     res=plotres)
    output$B_prob_table <- renderTable(B_prob_plot(OBJs, frac = input$SSB0_MSE_prob,
                                                   xlim = input$SSB0_MSE_xrange, figure = FALSE),
                                       rownames = TRUE)
  })

  observeEvent({
    input$SMP1
    input$SMP2
    input$SSB0_MSE_quantile
  }, {
    output$B_stoch_plot <- renderPlot(B_stoch_plot(OBJs, c(input$SMP1, input$SMP2), qval = input$SSB0_MSE_quantile), res=plotres)
  })

  observeEvent({
    input$StochB_resample
    input$StochMP
  }, {
    req(inherits(OBJs$MSEproj, "MSE"))
    sims <- sample(1:OBJs$MSEproj@nsim, 3, replace = FALSE)[1:input$nsim_hist_SSB]
    output$plot_hist_SSB_sim <- renderPlot(hist_SSB_sim(OBJs, input$StochMP, sims),res=plotres)
  })



  # OM panel --------------------------------------------------
  observeEvent({
    input$OM_hist
    input$OM_hist_bio
  }, {
    req(inherits(OBJs$MSEhist, "Hist"))
    MSEhist <- OBJs$MSEhist
    yr_cal <- seq(MSEhist@OM@CurrentYr - MSEhist@OM@nyears + 1,
                  MSEhist@OM@CurrentYr + MSEhist@OM@proyears)
    updateSliderInput(session, "bio_schedule_sim", min = 1, max = MSEhist@OM@nsim, value = 1, step = 1)
    updateSliderInput(session, "bio_schedule_year", min = min(yr_cal), max = max(yr_cal),
                      value = MSEhist@OM@CurrentYr, step = 1)
    updateSliderInput(session, "bio_schedule_nage", "Number of ages", min = 2, max = MSEhist@OM@maxage+1,
                      value = MSEhist@OM@maxage+1, step = 1)

    Frange_max <- 1.1 * max(MSEhist@Ref$ByYear$Fcrash) %>% round(2)
    updateSliderInput(session, "YC_Frange", min = 0, max = Frange_max, value = c(0, Frange_max), step = 0.01)
    updateSliderInput(session, "YC_y_bio", min = min(yr_cal), max = max(yr_cal), value = MSEhist@OM@CurrentYr)
    updateSliderInput(session, "YC_y_sel", min = min(yr_cal), max = max(yr_cal), value = MSEhist@OM@CurrentYr)

    updateSliderInput(session, "sel_y", min = min(yr_cal), max = max(yr_cal),
                      value = c(min(yr_cal), MSEhist@OM@CurrentYr))
  })

  output$plot_hist_bio <- renderPlot(hist_bio(OBJs),res=plotres)

  output$bio_year_text <- renderText({
    paste0("Right figure: year", ifelse(inherits(OBJs$MSEhist, "Hist"),
                                        paste0(" (last historical year: ", OBJs$MSEhist@OM@CurrentYr, ")"),
                                        ""))
  })

  output$YC_bio_text <- renderText({
    paste0("Year for biological parameters", ifelse(inherits(OBJs$MSEhist, "Hist"),
                                        paste0(" (last historical year: ", OBJs$MSEhist@OM@CurrentYr, ")"),
                                        ""))
  })

  output$sel_y_text <- renderText({
    paste0("Year", ifelse(inherits(OBJs$MSEhist, "Hist"),
                          paste0(" (last historical year: ", OBJs$MSEhist@OM@CurrentYr, ")"),
                          ""))
  })

  observeEvent({
    input$OM_hist
    input$OM_hist_exp
    input$bio_schedule
    input$bio_schedule_sim
    input$bio_schedule_year
    input$bio_schedule_nage
  }, {
    req(inherits(OBJs$MSEhist, "Hist"))
    output$plot_hist_age_schedule <- renderPlot(
      hist_bio_schedule(OBJs, var = input$bio_schedule, n_age_plot = input$bio_schedule_nage,
                        yr_plot = input$bio_schedule_year, sim = input$bio_schedule_sim),
      res = plotres
    )
  })

  output$plot_hist_growth_I <- renderPlot(hist_growth_I(OBJs),res=plotres)
  output$plot_hist_growth_II <- renderPlot(hist_growth_II(OBJs),res=plotres)

  output$plot_hist_spatial <- renderPlot(hist_spatial(OBJs),res=plotres)

  observeEvent({
    input$YC_Frange
    input$YC_y_bio
    input$YC_y_sel
    input$YC_calc
  }, {

    output$hist_YC_plot <- renderPlot(hist_YieldCurve(OBJs, YC_type = input$YC_calc,
                                                      yr_bio = input$YC_y_bio, yr_sel = input$YC_y_sel,
                                                      F_range = input$YC_Frange),
                                      res = plotres)
  }
  )

  observeEvent(input$sel_y, output$plot_hist_sel <- renderPlot(hist_sel(OBJs, input$sel_y),res=plotres))



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





  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

}
