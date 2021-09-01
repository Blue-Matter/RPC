


options(shiny.maxRequestSize=1000*1024^2)

server <- function(input, output, session) {

  plotres<-100
  OMs<<-unique(avail('OM', msg = FALSE)[avail('OM', msg = FALSE)!='testOM'],
               c("DFO_BoF_Herring","DFO_DEMO1","DFO_DEMO2","DFO_Inside_YE_Rockfish"))

  # ---- Initialize Reactive Values -----
  # Operating model selected, loaded or sketched
  OM_upload <- reactiveVal(0)
  output$OM_upload <- reactive(OM_upload())
  outputOptions(output, "OM_upload", suspendWhenHidden = FALSE)

  OM_L<-reactiveVal(0)
  output$OM_L <- reactive({ OM_L()})
  outputOptions(output,"OM_L",suspendWhenHidden=FALSE)

  MPsSpec<-reactiveVal(1)
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

  OBJs <<- reactiveValues(MSEhist = "", MSEproj = "", name = "", OM = "")

  PMs <<- reactiveValues(names = character(0))
  output$PMs <- reactive({ PMs$names })
  outputOptions(output, "PMs", suspendWhenHidden = FALSE)


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
      OBJs$name <<- prev_session$name
      OBJs$OM <<- prev_session$OM

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
        updateSelectInput(session, "HS_sel", choices = MPs$All, selected = MPs$Sel)

        MPs_to_add <- ls(envir = prev_session$MPs_env)
        if(length(MPs_to_add)) {
          lapply(MPs_to_add, function(x) assign(x, get(x, envir = prev_session$MPs_env), envir = .GlobalEnv))
        }

        lapply(MPs$Sel, function(x) {
          MPdesc[[x]] <- prev_session$MPdesc[[x]]
          MPinterval[[x]] <- prev_session$MPinterval[[x]]
        })

        AM(paste("MPs loaded from previous session:", paste(MPs$Sel, collapse = ", ")))
      }

      if(is.environment(prev_session$PMenv)) {
        PM <- ls(envir = prev_session$PMenv)
        if(length(PM)) {
          lapply(PM, function(x) PMenv[[x]] <- prev_session$PMenv[[x]])
          PMs$names <<- PM
          updateSelectInput(session, "PM_display", choices = PM)
          updateSelectInput(session, "PM1", choices = PM, selected = PM[1])
          updateSelectInput(session, "PM2", choices = PM, selected = PM[length(PM)])

          AM(paste("Performance metrics loaded from previous session:", paste(PM, collapse = ", ")))
        }
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
      out <- list(MSEhist = OBJs$MSEhist, MSEproj = OBJs$MSEproj, name = OBJs$name, OM = OBJs$OM, MPs = list(All = MPs$All, Sel = MPs$Sel))
      out$MPs_env <- new.env()
      out$MPdesc <- new.env()
      out$MPinterval <- new.env()
      out$PMenv <- new.env()

      # Only grabs MPs that are 'custom' made i.e. not in openMSE packages
      MPs_globalenv <- sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "MP"))
      if(sum(MPs_globalenv)) {
        lapply(names(MPs_globalenv)[MPs_globalenv],
               function(x) {
                 MP <- get(x, envir = .GlobalEnv)
                 if(!is.null(attr(MP, "RPC"))) out$MPs_env[[x]] <- MP
               })
      }
      if(length(MPs$Sel)) {
        lapply(MPs$Sel, function(x) {
          out$MPdesc[[x]] <- MPdesc[[x]]
          out$MPinterval[[x]] <- MPinterval[[x]]
        })
      }
      if(length(PMs$names)) lapply(PMs$names, function(x) out$PMenv[[x]] <- PMenv[[x]])

      saveRDS(out, file = file)
    }
  )

  # Download report ---------------------------------------------------
  output$OM_Rep <- downloadHandler(
    filename = paste0("RPC-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html"),
    content = function(file) {
      "Blank report"
      #report <- file.path()
      #rmarkdown::render(Rmd, output_file = file)
    }
  )

  # Fishery panel ---------------------------------------------------
  shinyjs::disable("DD_update")
  shinyjs::disable("Load_OM")

  observeEvent(input$Select, {
    req(input$Select == 3)
    updateSliderInput(session, "DD_nsim", min = 3, max = 100, value = 24)
    updateSliderInput(session, "DD_proyears", min = 5, max = 100, value = 50)
  })

  # OM select
  observeEvent(input$SelectOMDD,{
    OM_temp <- get(input$SelectOMDD)
    updateSliderInput(session, "DD_nsim", min = min(OM_temp@nsim, 3), max = OM_temp@nsim, value = min(OM_temp@nsim, nsim))
    updateSliderInput(session, "DD_proyears", min = min(OM_temp@proyears, 5), max = OM_temp@proyears, value = OM_temp@proyears)
    toggleDropdownButton("DD_Settings", session)
  })

  observeEvent(input$SelectOM,{
    OM <- get(input$SelectOMDD)
    OM_temp <- modOM(OM, input$DD_nsim, input$DD_proyears)
    OBJs$OM <<- OM
    OBJs$MSEhist <<- runMSEhist(OM_temp)
    OBJs$name <<- input$SelectOMDD
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
      OBJs$OM <<- OM_temp

      updateTextInput(session, "Load_OMname",
                      value = local({
                        name <- filey$name %>% as.character() %>% strsplit("[.]") %>% getElement(1)
                        paste(name[-length(name)], collapse = ".")
                      }))

      updateSliderInput(session, "DD_nsim", min = min(OM_temp@nsim, 3), max = OM_temp@nsim, value = min(OM_temp@nsim, nsim))
      updateSliderInput(session, "DD_proyears", min = min(OM_temp@proyears, 5), max = OM_temp@proyears, value = OM_temp@proyears)
      toggleDropdownButton("DD_Settings", session)
      shinyjs::enable("Load_OM")

      AM(paste0("Operating model loaded: ", filey$name))
      OM_upload(1)

    },

    error = function(e){

      AM(paste0(e,"\n"))
      shinyalert("File read error", "This does not appear to be a MSEtool OM object, saved by saveRDS()", type = "error")
      AM(paste0("Operating model failed to load: ", filey$name))
      return(0)

    })
  })

  observeEvent(input$Load_OM,{
    tryCatch({

      OM <- modOM(OBJs$OM, input$DD_nsim, input$DD_proyears)
      OBJs$MSEhist <<- runMSEhist(OM)

      if(nchar(input$Load_OMname)) {
        OBJs$name <<- input$Load_OMname
      } else {
        OBJs$name <<- "Operating model uploaded from file."
      }

      OM_L(1)
      MSErun(0)
      updateVerticalTabsetPanel(session,'Main',selected=3)
    },
    error = function(e) {

      AM(paste0(e,"\n"))
      shinyalert("File read error", "This does not appear to be a MSEtool OM object, saved by saveRDS()", type = "error")
      AM(paste0("Operating model failed to load: ", filey$name))
      return(0)

    })
  })

  observeEvent(input$BuildOM,{
    temp<-checkQs(input)

    if(temp$error){
      shinyalert("Incomplete Questionnaire", text=paste("The following questions have not been answered or answered incorrectly:",paste(temp$probQs,collapse=", ")), type = "warning")
    }else{
      doprogress("Reading sketch",1)
      OM <- makeOM(input, PanelState, nsim = 100, proyears = 100)
      OM_temp <- modOM(OM, input$DD_nsim, input$DD_proyears)

      OBJs$OM <<- OM
      OBJs$MSEhist <<- runMSEhist(OM_temp)
      OBJs$name <<- "Operating model built from MERA"

      OM_L(1)
      MSErun(0)
      updateVerticalTabsetPanel(session,'Main',selected=3)
      AM(paste0("Operating model sketched: ", OM@Name))
    }
  })


  observeEvent({
    input$DD_nsim
    input$DD_proyears
  }, {
    req(OBJs$MSEhist)
    toggleState(id = "DD_update", condition = input$DD_nsim != OBJs$MSEhist@OM@nsim || input$DD_proyears != OBJs$MSEhist@OM@proyears)
  })

  observeEvent(input$DD_update, {
    OM <- modOM(OBJs$OM, input$DD_nsim, input$DD_proyears)
    OBJs$MSEhist <<- runMSEhist(OM)

    OM_L(1)
    MSErun(0)
    updateVerticalTabsetPanel(session,'Main',selected=3)
    shinyjs::disable("DD_update")
    AM(paste0("Operating model ", OBJs$name, " updated with ", input$DD_nsim, " simulations and ", input$DD_proyears, " projection years."))
  })


  # Historical results panel -----------------------------------------------------
  output$plot_hist_bio <- renderPlot(hist_bio(OBJs),res=plotres)

  observeEvent({
    input$HistRes1
    input$SSB
    input$SSBhist
  }, {
    req(OBJs$MSEhist)
    output$OM_name <- renderTable({
      x <- c("Name of operating model" = OBJs$name,
             "Number of simulations" = OBJs$MSEhist@OM@nsim,
             "Historical years" = paste0(OBJs$MSEhist@OM@CurrentYr - OBJs$MSEhist@OM@nyears + 1, "-", OBJs$MSEhist@OM@CurrentYr, " (",
                                         OBJs$MSEhist@OM@nyears, " years)"),
             "Projection years" = paste0(OBJs$MSEhist@OM@CurrentYr + 1, "-", OBJs$MSEhist@OM@CurrentYr + OBJs$MSEhist@OM@proyears, " (",
                                         OBJs$MSEhist@OM@proyears, " years)"))
      as.data.frame(x)
    }, colnames = FALSE, rownames = TRUE)

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

  output$hist_SSB0_plot<-renderPlot(hist_SSB0(OBJs),res=plotres)
  output$hist_SSBMSY_plot <- renderPlot(hist_SSBMSY(OBJs), res = plotres)

  observeEvent({
    input$SSB_prob_type
    input$SSB_y
    input$SSB_prob
    input$SSB_yrange
  }, {
    if(input$SSB_prob_type == 1) {
      updateSliderInput(session, "SSB_prob", max = 2)
      output$hist_SSB_prob <- renderPlot(hist_SSB(OBJs, prob_ratio = input$SSB_prob, SSB_y = input$SSB_y, prob_ylim = input$SSB_yrange),
                                         res = plotres)
      output$hist_SSB_prob_table <- renderTable(hist_SSB(OBJs, figure = FALSE, prob_ratio = input$SSB_prob, SSB_y = input$SSB_y),
                                                rownames = TRUE)
      output$SSB_threshold_label <- renderUI("Historical SSB threshold")
    } else if(input$SSB_prob_type == 2) {
      updateSliderInput(session, "SSB_prob", max = 1)
      output$hist_SSB_prob <- renderPlot(hist_SSB0(OBJs, prob_ratio = input$SSB_prob, prob_ylim = input$SSB_yrange),
                                         res = plotres)
      output$hist_SSB_table <- renderTable(hist_SSB0(OBJs, figure = FALSE, prob_ratio = input$SSB0_prob), rownames = TRUE)
      output$SSB_threshold_label <- renderUI(HTML("<p>SSB/SSB<sub>0</sub> threshold</p>"))
    } else {
      updateSliderInput(session, "SSB_prob", max = 2)
      output$hist_SSB_prob <- renderPlot(hist_SSBMSY(OBJs, prob_ratio = input$SSB_prob, prob_ylim = input$SSB_yrange),
                                            res = plotres)
      output$hist_SSB_table <- renderTable(hist_SSBMSY(OBJs, figure = FALSE, prob_ratio = input$SSBMSY_prob), rownames = TRUE)
      output$SSB_threshold_label <- renderUI(HTML("<p>SSB/SSB<sub>MSY</sub> threshold</p>"))
    }

    output$hist_SSB_prob_table_label <- renderUI({
      SSB_prob_type <- switch(input$SSB_prob_type,
                              "1" = paste("SSB in ", input$SSB_y),
                              "2" = "SSB<sub>0</sub>",
                              "3" = "SSB<sub>MSY</sub>"
      )
      HTML(paste0("<p>Annual probability that SSB has exceeded ", 100 * input$SSB_prob, "% ", SSB_prob_type, "</p>"))
    })

  })

  output$hist_R_plot<-renderPlot(hist_R(OBJs),res=plotres)
  output$hist_R_table<-renderTable(hist_R(OBJs, figure = FALSE), rownames = TRUE, digits = 2)

  output$hist_exp <- renderPlot(hist_exp(OBJs),res=plotres)
  output$hist_exp2 <- renderTable(hist_exp(OBJs, figure = FALSE), rownames = TRUE)

  output$hist_Fmed <- renderPlot(hist_Fmed(OBJs), res = plotres)
  output$hist_Fmed2 <- renderTable(hist_Fmed(OBJs, figure = FALSE), rownames = TRUE)

  output$hist_SPR <- renderPlot(hist_SPR(OBJs),res=plotres)
  output$hist_SPR2 <- renderTable(hist_SPR(OBJs, figure = FALSE), rownames = TRUE)

  # Probability
  observeEvent({
    input$exp_type
    input$FMSY_prob
    input$SPR_prob
    input$exp_yrange
  }, {
    if(input$exp_type == "FMSY") {
      output$hist_exp_prob <- renderPlot(hist_exp(OBJs, prob_ratio = input$FMSY_prob, prob_ylim = input$exp_yrange))
      output$hist_exp_table_label <- renderText({
        paste0("Annual probability that F/FMSY < ", 100 * input$FMSY_prob, "%.")
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

  observeEvent({
    input$HistRes1
    input$Rmax_prob
    input$Rmax_yrange
  }, {
    output$hist_Rmax_prob <- renderPlot(hist_Rmax(OBJs, prob_ratio = input$Rmax_prob, prob_ylim = input$Rmax_yrange), res = plotres)
    output$hist_Rmax_prob_table <- renderTable(hist_Rmax(OBJs, figure = FALSE, prob_ratio = input$Rmax_prob), rownames = TRUE)

    output$hist_Rmax_prob_table_label <- renderUI({
      HTML(paste0("<p>Annual probability that SSB/SSB<sub>50% Rmax</sub> > ", 100 * input$Rmax_prob, "%</p>"))
    })
  })

  output$hist_RpS90_plot<-renderPlot(hist_RpS90(OBJs),res=plotres)
  output$hist_RpS90_table<-renderTable(hist_RpS90(OBJs, figure = FALSE), rownames = TRUE)

  observeEvent({
    input$HistRes1
    input$RpS90_prob
    input$RpS90_yrange
  }, {
    output$hist_RpS90_prob <- renderPlot(hist_RpS90(OBJs, prob_ratio = input$RpS90_prob, prob_ylim = input$RpS90_yrange), res = plotres)
    output$hist_RpS90_prob_table <- renderTable(hist_RpS90(OBJs, figure = FALSE, prob_ratio = input$RpS90_prob), rownames = TRUE)

    output$hist_RpS90_prob_table_label <- renderUI({
      HTML(paste0("<p>Annual probability that SSB/SSB<sub>90%ile R/S</sub> > ", 100 * input$RpS90_prob, "%</p>"))
    })
  })

  # Management Strategy Panel -----------------------------------------------------
  shinyjs::disable("Build_MS_import")

  # MP setup -----------------------------------------------------
  output$MS_FixF_ratio_label <- renderText(paste0("Ratio of F relative to last historical year (", OBJs$MSEhist@OM@CurrentYr, "). Set to 1 for status quo."))
  observeEvent(input$MS_FixF_ratio, {
    updateTextInput(session, "MS_FixF_Label", value = paste0("CurF_", 100 * input$MS_FixF_ratio))
    output$MS_FixF_plot <- renderPlot(CurF_plot(OBJs, input$MS_FixF_ratio), res = plotres)
  })

  output$MS_FixC_ratio_label <- renderText(paste0("Ratio of catch relative to last historical year (", OBJs$MSEhist@OM@CurrentYr, "). Set to 1 for status quo."))
  observeEvent(input$MS_FixC_ratio, {
    updateTextInput(session, "MS_FixC_Label", value = paste0("CurC_", 100 * input$MS_FixC_ratio))
    output$MS_FixC_plot <- renderPlot(CurC_plot(OBJs, input$MS_FixC_ratio), res = plotres)
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
    updateTextInput(session, "MS_HCR_Label", value = make_HCR_name(input))
  })

  observeEvent(input$MS_DLM, {
    page_name <- help(input$MS_DLM) %>% as.character() %>% strsplit("/") %>% getElement(1)
    url <- paste0("https://dlmtool.openmse.com/reference/", page_name[length(page_name)], ".html")
    output$DLM_iframe <- renderUI(tags$iframe(src = url, width = "100%", height = "460px", style = "overflow-y:scroll"))
    toggleState("Build_MS_DLM", condition = all(input$MS_DLM != MPs$Sel))
  })

  observeEvent(input$MS_Import_file, {
    filey <- input$MS_Import_file
    updateTextInput(session, "MS_Import_Label", value = local({
      name <- filey$name %>% strsplit("[.]") %>% getElement(1)
      paste(name[-length(name)], collapse = ".")
    }))

    tryCatch({
      MP_out <- readRDS(file = filey$datapath)
      stopifnot(typeof(MP_out) == "closure" && inherits(MP_out, "MP"))
      shinyjs::enable("Build_MS_import")

    }, error = function(e) {
      AM(paste0(e,"\n"))
      shinyalert(paste0("No MP was found in file: ", filey$name), type = "error")
      AM(paste0("No MP was found in file: ", filey$name))
      return(0)
    })
  })

  output$HSplot <- renderPlot(HCR_plot(input), res = plotres)

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

  #observeEvent(input$HS_sel, {
  #  if(MPs$Sel[1]==""){
  #    MPs$Sel<<-"No_Fishing"
  #  }else{
  #    MPs$Sel<<-c(MPs$Sel,input$HS_sel)
  #  }
  #  MPsSpec(1)
  #  updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
  #})

  # MP create and save -----------------------------------------------------
  observeEvent(input$Build_MS_FixF, {
    if(!nchar(input$MS_FixF_Label)) {
      shinyalert("No name for the MP was provided.", type = "error")
    } else if(input$MS_FixF_Label %in% MPs$Sel) {
      shinyalert(paste0(input$MS_FixF_Label, " is already used. Choose another name."), type = "error")
    } else {
      MPs$All <<- c(MPs$All, input$MS_FixF_Label)
      MPs$Sel <<- c(MPs$Sel, input$MS_FixF_Label)

      assign(input$MS_FixF_Label, make_FixF_MP(input$MS_FixF_ratio), envir = .GlobalEnv)

      MP_txt <- paste0("Set constant F at ", 100*input$MS_FixF_ratio, "% F from last historical year (",
                       OBJs$MSEhist@OM@CurrentYr, ")")
      assign(input$MS_FixF_Label, MP_txt, envir = MPdesc)

      assign(input$MS_FixF_Label, expression(OBJs$MSEhist@OM@proyears + 1), envir = MPinterval)

      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
      MPsSpec(1)
    }
  })

  observeEvent(input$Build_MS_FixC, {
    if(!nchar(input$MS_FixC_Label)) {
      shinyalert("No name for the MP was provided.", type = "error")
    } else if(input$MS_FixC_Label %in% MPs$Sel) {
      shinyalert(paste0(input$MS_FixC_Label, " is already used. Choose another name."), type = "error")
    } else {
      MPs$All <<- c(MPs$All, input$MS_FixC_Label)
      MPs$Sel <<- c(MPs$Sel, input$MS_FixC_Label)

      assign(input$MS_FixC_Label, make_FixC_MP(input$MS_FixC_ratio), envir = .GlobalEnv)

      MP_txt <- paste0("Set constant catch at ", 100*input$MS_FixF_ratio, "% catch from last historical year (",
                       OBJs$MSEhist@OM@CurrentYr, ")")
      assign(input$MS_FixC_Label, MP_txt, envir = MPdesc)

      assign(input$MS_FixC_Label, expression(OBJs$MSEhist@OM@proyears + 1), envir = MPinterval)

      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
      MPsSpec(1)
    }
  })

  observeEvent(input$Build_MS,{
    if(!nchar(input$MS_HCR_Label)) {
      shinyalert("No name for the MP was provided.", type = "error")
    } else if(input$MS_HCR_Label %in% MPs$Sel) {
      shinyalert(paste0(input$MS_HCR_Label, " is already used. Choose another name."), type = "error")
    } else {
      MPs$All <<- c(MPs$All, input$MS_HCR_Label)
      MPs$Sel <<- c(MPs$Sel, input$MS_HCR_Label)

      MP_out <- make_RPC_MP(input)
      assign(input$MS_HCR_Label, MP_out, envir = .GlobalEnv)

      MP_txt <- make_HCR_name(input, "description")
      assign(input$MS_HCR_Label, MP_txt, envir = MPdesc)

      assign(input$MS_HCR_Label, input$MS_interval, envir = MPinterval)

      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
      MPsSpec(1)
    }
  })

  observeEvent(input$Build_MS_DLM, {
    if(!input$MS_DLM %in% MPs$Sel) {
      MPs$Sel <<- c(MPs$Sel, input$MS_DLM)

      page_name <- help(input$MS_DLM) %>% as.character() %>% strsplit("/") %>% getElement(1)
      url <- paste0("https://dlmtool.openmse.com/reference/", page_name[length(page_name)], ".html")

      MP_txt <- paste("DLMtool MP with update interval of", input$MS_DLM_interval, "years, see", url)
      assign(input$MS_DLM, MP_txt, envir = MPdesc)

      assign(input$MS_DLM, input$MS_DLM_interval, envir = MPinterval)

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
        attr(MP_out, "RPC") <- TRUE
        assign(input$MS_Import_Label, MP_out, envir = .GlobalEnv)

        MPs$All <<- c(MPs$All, input$MS_Import_Label)
        MPs$Sel <<- c(MPs$Sel, input$MS_Import_Label)

        if(nchar(input$MS_Import_Description)) {
          MP_txt <- input$MS_Import_Description
        } else {
          MP_txt <- "Imported MP. No description provided."
        }
        assign(input$MS_Import_Label, MP_txt, envir = MPdesc)

        assign(input$MS_Import_Label, input$MS_Import_interval, envir = MPinterval)

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
    if(any(MPs$Sel != "No_Fishing")) {
      AM(paste(MPs$Sel[length(MPs$Sel)], "MP removed"))
      MPs$Sel <<- MPs$Sel[-length(MPs$Sel)]
      updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
    }
  })

  observeEvent(input$MS_Clear_All, {
    AM("Removing all MPs except 'No_Fishing'")
    MPs$Sel <<- "No_Fishing"
    updateSelectInput(session,"HS_sel",choices=MPs$All,selected=MPs$Sel)
    #MPsSpec(1)
  })

  output$MS_summary <- renderTable(
    data.frame(MP = MPs$Sel, Description = vapply(MPs$Sel, get, character(1), envir = MPdesc, inherits = FALSE))
  )

  output$Save_MS_FixF <- downloadHandler(
    filename = paste0(input$MS_FixF_Label, ".rds"),
    content = function(file) saveRDS(make_FixF_MP(input$MS_FixF_ratio), file = file)
  )

  output$Save_MS_FixC <- downloadHandler(
    filename = paste0(input$MS_FixC_Label, ".rds"),
    content = function(file) saveRDS(make_FixC_MP(input$MS_FixC_ratio), file = file)
  )

  output$Save_MS <- downloadHandler(
    filename = paste0(input$MS_HCR_Label, ".rds"),
    content = function(file) saveRDS(make_RPC_MP(input, FALSE), file = file)
  )


  # Results panel ------------------------------------------------

  observeEvent(input$runMSE,{
    tryCatch(
      {
        withProgress(message="Running MSE Simulation test:", {
          OBJs$MSEhist@OM@interval <<-
            sapply(MPs$Sel, function(x) get(x, envir = MPinterval, inherits = FALSE) %>% eval())
          OBJs$MSEproj <<- Project(OBJs$MSEhist, MPs = MPs$Sel, extended = TRUE)
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

  output$proj_plot <- renderPlot(proj_plot(OBJs),res=plotres)
  observeEvent({
    input$Res
    input$proj_type
  }, {
    req(OBJs$MSEproj)
    output$proj_plot <- renderPlot(proj_plot(OBJs, type = input$proj_type),res=plotres)
  })

  observeEvent({
    input$Res
  }, {
    req(OBJs$MSEproj)

    updateSelectInput(session, "SMP1", choices = OBJs$MSEproj@MPs, selected = OBJs$MSEproj@MPs[1])
    updateSelectInput(session, "SMP2", choices = OBJs$MSEproj@MPs, selected = OBJs$MSEproj@MPs[OBJs$MSEproj@nMPs])
    updateSelectInput(session, "StochMP", choices = OBJs$MSEproj@MPs, selected = OBJs$MSEproj@MPs[1])

    updateSliderInput(session, "prob_yrange",
                      min = OBJs$MSEproj@OM$CurrentYr[1] + 1,
                      max = OBJs$MSEproj@OM$CurrentYr[1] + OBJs$MSEproj@proyears,
                      value = OBJs$MSEproj@OM$CurrentYr[1] + c(1, OBJs$MSEproj@proyears))

    updateSliderInput(session, "SSBhist_yr",
                      min = OBJs$MSEproj@OM$CurrentYr[1] - OBJs$MSEproj@nyears + 1,
                      max = OBJs$MSEproj@OM$CurrentYr[1],
                      value = OBJs$MSEproj@OM$CurrentYr[1])

    updateSliderInput(session, "Chist_yr",
                      min = OBJs$MSEproj@OM$CurrentYr[1] - OBJs$MSEproj@nyears + 1,
                      max = OBJs$MSEproj@OM$CurrentYr[1],
                      value = OBJs$MSEproj@OM$CurrentYr[1])
  })

  observeEvent({
    input$stoch_type
    input$SMP1
    input$SMP2
    input$stoch_quantile
  }, {
    output$stoch_plot <- renderPlot(stoch_plot(OBJs, c(input$SMP1, input$SMP2), qval = input$stoch_quantile,
                                               type = input$stoch_type), res=plotres)
  })

  observeEvent({
    input$sim_type
    input$StochB_resample
    input$StochMP
  }, {
    req(OBJs$MSEproj)
    sims <- sample(1:OBJs$MSEproj@nsim, 3, replace = FALSE)[1:input$nsim_hist]
    output$plot_hist_sim <- renderPlot(hist_sim(OBJs, MP = input$StochMP, sims = sims, type = input$sim_type),res=plotres)
  })

  # Performance metrics panel ------------------------------------------------

  observeEvent({
    input$prob_type
    input$prob_range
    input$prob_yrange

    input$SSBhist_thresh
    input$SSBhist_yr
    input$SSB0_thresh
    input$SSB0_type
    input$SSBMSY_thresh
    input$FMSY_thresh
    input$SPR_thresh
    input$Chist_thresh
    input$Chist_yr
  }, {
    req(OBJs$MSEproj)

    PM_name <- switch(input$prob_type,
                      "SSB" = paste0(100 * input$SSBhist_thresh, "%SSB", input$SSBhist_yr),
                      "SSB0" = paste0(100 * input$SSB0_thresh, "%B0_", input$SSB0_type),
                      "SSBMSY" = paste0(100 * input$SSBMSY_thresh, "%BMSY"),
                      "F" = paste0(100 * input$FMSY_thresh, "%FMSY"),
                      "SPR" = paste0(100 * input$SPR_thresh, "%SPR"),
                      "Catch" = paste0(100 * input$Chist_thresh, "%C", input$Chist_yr)) %>%
      paste0("_", paste(input$prob_yrange, collapse = "-"))
    updateTextInput(session, "PM_name", value = PM_name)

    label <- local({
      out_type <- ifelse(grepl("SSB", input$prob_type), "SSB", input$prob_type)
      out_thresh <- switch(input$prob_type,
                           "SSB" = paste0(100 * input$SSBhist_thresh, "% of ", input$SSBhist_yr, " SSB"),
                           "SSB0" = paste0(100 * input$SSB0_thresh, "% of ", input$SSB0_type, " SSB0"),
                           "SSBMSY" = paste0(100 * input$SSBMSY_thresh, "% SSBMSY"),
                           "F" = paste0(100 * input$FMSY_thresh, "% FMSY"),
                           "SPR" = paste0(100 * input$SPR_thresh, "%"),
                           "Catch" = paste0(100 * input$Chist_thresh, "% of ", input$Chist_yr, " catch")
      )
      paste0("Probability that ", out_type, ifelse(input$prob_type == "F", " does not exceed ", " exceeds "),
             out_thresh, " during ", paste0(input$prob_yrange, collapse = " - "))
    })
    output$prob_table_label <- renderText(label)

    PMobj <- make_PMobj(OBJs, type = input$prob_type,
                        frac = switch(input$prob_type,
                                      "SSB" = input$SSBhist_thresh,
                                      "SSB0" = input$SSB0_thresh,
                                      "SSBMSY" = input$SSBMSY_thresh,
                                      "F" = input$FMSY_thresh,
                                      "SPR" = input$SPR_thresh,
                                      "Catch" = input$Chist_thresh),
                        year_range = input$prob_yrange, label = label,
                        SSBhist_yr = input$SSBhist_yr,
                        SSB0_type = input$SSB0_type,
                        Chist_yr = input$Chist_yr)
    attr(PMobj, "label") <- label
    assign("PM_temp", PMobj, envir = PMenv)

    output$prob_plot <- renderPlot(prob_plot(OBJs, PM_list = list(PMobj),
                                             xlim = input$prob_yrange, ylim = input$prob_range),
                                   res=plotres)
    output$prob_table <- renderTable(prob_plot(OBJs, PM_list = list(PMobj), figure = FALSE),
                                     rownames = TRUE)
  })

  observeEvent(input$PM_add, {
    if(!nchar(input$PM_add)) {
      shinyalert("No name for the performance metric was provided.", type = "error")
    } else if(length(PMs$names) && any(input$PM_name == PMs$names)) {
      shinyalert("PM name is already being used. Re-name this performance metric.", type = "error")
    } else {
      PM_temp <- get("PM_temp", envir = PMenv, inherits = FALSE)
      assign(input$PM_name, PM_temp, envir = PMenv)
      PMs$names <<- c(PMs$names, input$PM_name)
      AM(paste("New performance metric (PM) has been added:", input$PM_name))

      updateSelectInput(session, "PM1", choices = PMs$names, selected = PMs$names[1])
      updateSelectInput(session, "PM2", choices = PMs$names, selected = PMs$names[length(PMs$names)])
      updateSelectInput(session, "PM_display", choices = PMs$names)
    }
  })

  observeEvent({
    input$PM1
    input$PM2
  }, {
    req(length(PMs$names) > 0)
    PM_list <- lapply(PMs$names, function(x) get(x, envir = PMenv, inherits = FALSE)) %>%
      structure(names = PMs$names)
    output$PM_table <- renderTable({
      prob_plot(OBJs, PM_list = PM_list, figure = FALSE) %>%
        structure(dimnames = list(PM_list[[1]]@MPs, PMs$names))
    }, rownames = TRUE)

    output$PM_table2 <- renderTable({
      data.frame(Name = PMs$names, Description = sapply(PM_list, attr, "label"))
    })

    output$PM_lollipop <- renderPlot(lollipop_plot(OBJs, PM_list), res = plotres)

    output$PM_radar <- renderPlot(radar_plot(OBJs, PM_list), res = plotres)

    output$PM_tradeoff <- renderPlot({
      tradeoff_plot(OBJs, PM_list[[input$PM1]], PM_list[[input$PM2]], input$PM1, input$PM2)
    }, res = plotres)
  })

  observeEvent(input$PM_Clear, {
    nPM <- length(PMs$names)
    if(nPM) {
      AM(paste("Removing performance metric:", input$PM_display))
      PMs$names <<- PMs$names[input$PM_display != PMs$names]

      nPM2 <- length(PMs$names)
      if(nPM2) {
        updateSelectInput(session, "PM1", choices = PMs$names, selected = PMs$names[1])
        updateSelectInput(session, "PM2", choices = PMs$names, selected = PMs$names[length(PMs$names)])
        updateSelectInput(session, "PM_display", choices = PMs$names)
      } else {
        updateSelectInput(session, "PM1", choices = "")
        updateSelectInput(session, "PM2", choices = "")
        updateSelectInput(session, "PM_display", choices = "")
        AM("All performance metrics have been removed.")
      }
    }
  })

  observeEvent(input$PM_Clear_All, {
    if(length(PMs$names)) {
      PMs$names <<- character(0)
      AM("All performance metrics have been removed.")
      updateSelectInput(session, "PM1", choices = "")
      updateSelectInput(session, "PM2", choices = "")
      updateSelectInput(session, "PM_display", choices = "")
    }
  })

  # OM panel --------------------------------------------------
  observeEvent({
    input$OM_hist
  }, {
    req(OBJs$MSEhist)
    MSEhist <- OBJs$MSEhist
    yr_cal <- seq(MSEhist@OM@CurrentYr - MSEhist@OM@nyears + 1,
                  MSEhist@OM@CurrentYr + MSEhist@OM@proyears)
    updateSliderInput(session, "bio_schedule_sim", min = 1, max = MSEhist@OM@nsim, value = 1, step = 1)
    updateSliderInput(session, "bio_schedule_year", min = min(yr_cal), max = max(yr_cal),
                      value = MSEhist@OM@CurrentYr, step = 1)
    updateSliderInput(session, "bio_schedule_nage", min = 2, max = MSEhist@OM@maxage+1,
                      value = MSEhist@OM@maxage+1, step = 1)

    Frange_max <- 1.1 * max(MSEhist@Ref$ByYear$Fcrash) %>% round(2)
    updateSliderInput(session, "YC_Frange", min = 0, max = Frange_max, value = c(0, Frange_max), step = 0.01)
    updateSliderInput(session, "YC_y_bio", min = min(yr_cal), max = max(yr_cal), value = MSEhist@OM@CurrentYr)
    updateSliderInput(session, "YC_y_sel", min = min(yr_cal), max = max(yr_cal), value = MSEhist@OM@CurrentYr)

    updateSliderInput(session, "sel_y", min = min(yr_cal), max = max(yr_cal),
                      value = c(min(yr_cal), MSEhist@OM@CurrentYr))
  })

  output$bio_year_text <- renderText({
    req(OBJs$MSEhist)
    paste0("Right figure: year (last historical year: ", OBJs$MSEhist@OM@CurrentYr, ")")
  })

  output$YC_bio_text <- renderText({
    req(OBJs$MSEhist)
    paste0("Year for biological parameters (last historical year: ", OBJs$MSEhist@OM@CurrentYr, ")")
  })

  output$sel_y_text <- renderText({
    req(OBJs$MSEhist)
    paste0("Years (last historical year: ", OBJs$MSEhist@OM@CurrentYr, ")")
  })

  observeEvent({
    input$OM_hist
    input$OM_hist_exp
    input$bio_schedule
    input$bio_schedule_sim
    input$bio_schedule_year
    input$bio_schedule_nage
  }, {
    req(OBJs$MSEhist)
    output$plot_hist_age_schedule <- renderPlot(
      hist_bio_schedule(OBJs, var = input$bio_schedule, n_age_plot = input$bio_schedule_nage,
                        yr_plot = input$bio_schedule_year, sim = input$bio_schedule_sim),
      res = plotres
    )
  })

  output$plot_hist_growth_I <- renderPlot(hist_growth_I(OBJs),res=plotres)
  output$plot_hist_growth_II <- renderPlot(hist_growth_II(OBJs),res=plotres)

  output$plot_hist_spatial <- renderPlot(hist_spatial(OBJs),res=plotres)

  output$plot_future_recruit <- renderPlot(hist_future_recruit(OBJs), res = plotres)

  observeEvent({
    input$YC_Frange
    input$YC_y_bio
    input$YC_y_sel
    #input$YC_calc
  }, {

    output$hist_YC_plot <- renderPlot(hist_YieldCurve(OBJs, #YC_type = input$YC_calc,
                                                      yr_bio = input$YC_y_bio, yr_sel = input$YC_y_sel,
                                                      F_range = input$YC_Frange),
                                      res = plotres)
  }
  )

  observeEvent(input$sel_y, {
    output$plot_hist_sel <- renderPlot(hist_sel(OBJs, input$sel_y),res=plotres)
  })



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

  # Dynamically update global variable Syear, Current_Year and Lyear is fixed to 2021
  observeEvent(input$Syear, Syear <<- input$Syear)

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
  observeEvent({
    input$Syear
    input$Lyear
  }, {
    eff_values <<- reactiveValues(df=data.frame(x=c(Syear,floor(mean(c(Syear, Lyear))), Lyear), y=c(0,0.5,0.5), series=rep(1,3)),
                                  series=1,
                                  stack=data.frame(x=c(Syear,floor(mean(c(Syear, Lyear))), Lyear), y=c(0,0.5,0.5), series=rep(1,3)))

    reset_eff_values<<-function(){
      eff_values$df <<- data.frame(x=c(input$Syear,floor(mean(c(input$Syear,input$Lyear))),input$Lyear), y=c(0,0.5,0.5), series=rep(1,3))
      eff_values$series <<- 1
      eff_values$stack <<- data.frame(x=c(input$Syear,floor(mean(c(input$Syear,input$Lyear))),input$Lyear), y=c(0,0.5,0.5), series=rep(1,3))
    }
  })


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
    if (length(ind)>0) eff_values$df <<- eff_values$df[-ind,]

    eff_values$stack <<- data.frame(x=c(eff_values$stack$x,newX),
                                   y=c(eff_values$stack$y, newY),
                                   series=c(eff_values$stack$series, eff_values$series))

    tempDF <- data.frame(x=c(eff_values$df$x, newX),
                         y=c(eff_values$df$y, newY),
                         series=c(eff_values$df$series, eff_values$series))

    tempDF <- dplyr::arrange(tempDF, series, x)

    eff_values$df <<- tempDF

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
      eff_values$df<<-rbind(eff_values$df,vec)
      eff_values$stack<<-rbind(eff_values$stack,vec)
    }

    eff_values$series <<-eff_values$series+1
    eff_values$df <<- data.frame(x=c(eff_values$df$x, initYr),
                                y=c(eff_values$df$y, yvals),
                                series=c(eff_values$df$series, eff_values$series))

  })

  observeEvent(input$undo_last, {
    # remove the last point
    if (nrow(eff_values$df)>1) {
      nrows <- nrow(eff_values$stack)
      last_vals <- eff_values$stack[nrows,]
      eff_values$stack <<- eff_values$stack[1:(nrows-1),]
      eff_values$df <<- dplyr::anti_join(eff_values$df, last_vals, by=c('x', 'y', 'series'))
    }
  })

  observeEvent(input$reset_plot, {
    reset_eff_values()
  })





  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

}
