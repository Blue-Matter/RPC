

fluidPage(
  useShinyalert(),
  includeScript(path = "www/js/js4checkbox.js"),
  includeScript(path = "www/js/index.js"),
  tags$head(
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),

    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$style(".fa-cogs {color:red}"),
    tags$style(HTML("
                    #SessionID{font-size:12px;}
                    #Load_OM{height: 150px}
                    ")),
    tags$style(HTML("
        /* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */

        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');

        /* Font of SLICK title */

      ")),

    tags$script('
                var dimension = [0, 0];
                $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                });
                $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                });
            ')
  ),

  # === Header ==============================================================================================================================================================
  source(file.path("ui", "ui_header.R"), local = TRUE)$value,

  # === Main Window =========================================================================================================================================================

  column(12, # General tab panel
    verticalTabsetPanel(id="Main",selected=1,
      verticalTabPanel(h5("Home"),
                       column(12,
                              h4("Welcome to the Alpha test of Reference Point Calculator: a simulation tool for informating the selection of reference points and harvest control rules."),
                              h5("If possible please leave any feedback on the App in
                                 the", a("RPC Feedback google sheet",href="https://docs.google.com/spreadsheets/d/1rqB0719h5Wx7X9wdCaFJHzt3p91vV4bRQdFAm55J6AE/edit?usp=sharing", target="_blank")),
                              hr(),
                              column(12,style='padding-top:0px',
                                     h5("There are four steps for using the App:"),
                                     column(6,h5("Step 1: Specify an operating model"),img(src = "Step1.png",height=220,width=440)),
                                     column(6,h5("Step 2: Examine historical fishery"),img(src = "Step2a.png",height=220,width=440)),
                                     column(6,h5("Step 3: Define management procedures"),img(src = "Step2.png",height=220,width=440)),
                                     column(6,h5("Step 4: Run MSE and visualize outcomes"),img(src = "Step3b.png",height=220,width=440))
                              ),
                              style='height:800px'
                       ),

                       id="Home",
                       value=1,
                       box_height='65px'
                       ),

      verticalTabPanel(id="Fishery",value=2,
                       h5(strong('Step 1. Specify Operating Model')),
                       column(12, style='height:800px; padding-left:10px',
                              h5("The first step is to specify your fishery by either selecting a comparable fishery from those available in the app, loading a
                                 compatible openMSE operating model or sketching the fishery dynamics using the MERA system."),
                              h5("Alternatively, load a previously saved RPC session (Select 'File' in upper right)."),
                              hr(),
                              HTML("<br>"),
                              radioButtons('Select',label=NULL,choiceNames=c('Select','Load','Sketch'),choiceValues=c(1,2,3),inline=T),
                              conditionalPanel("input.Select==1",
                                               h5("Select an example operating model"),
                                               column(12,style="padding-top:30px;padding-bottom:10px;padding-left:50px",
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("SelectOMDD", choices=OMs, label=NULL, selected=OMs[1])),
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",actionButton("SelectOM",label = "Select",style="color:red",icon=icon('cogs'),width='150px',height='20px')),
                                                      sliderInput("Custom_nsim", "Number of simulations", min = max(get(OMs[1])@nsim, 3), max = get(OMs[1])@nsim, value = min(nsim, get(OMs[1])@nsim), step = 1, round = TRUE),
                                                      sliderInput("Custom_proyears", "Number of projection years", min = 2, max = get(OMs[1])@proyears, value = get(OMs[1])@proyears, step = 1, round = TRUE)
                                               )
                              ),

                              conditionalPanel("input.Select==2",
                                               h5("Load an openMSE compatible operating model object from file"),
                                               column(12,style="padding-top:30px;padding-bottom:10px;padding-left:50px",
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",
                                                          tipify(
                                                            fileInput("Load_OMprelim", label=NULL,
                                                                      buttonLabel=h5('Browse',style='color:red; padding:0px;height:14px')),
                                                            title = "File name"
                                                          )
                                                      ),
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",
                                                          actionButton("Load_OM",label = "Select",style="color:red",icon=icon('cogs'),width='150px',height='20px')),
                                                      sliderInput("Custom_nsim_load", "Number of simulations", min = 0, max = 0, value = 0, step = 1),
                                                      sliderInput("Custom_proyears_load", "Number of projection years", min = 0, max = 0, value = 0, step = 1)
                                               )
                              ),

                              source(file.path("ui", "ui_MERA.R"), local = TRUE)$value # MERA

                              ), box_height='95px'), # end of Fishery vertical Tabpanel

      verticalTabPanel(id="HistResults", value=3,
                       h5(strong("Step 2. Examine Historical Fishery")),
                       column(12, style='height:800px',
                              conditionalPanel('output.OM_L==0',{
                                h5("Please select, load or sketch an operating model in step 1 above.")
                              }),
                              conditionalPanel('output.OM_L==1',{
                                h5("These figures are intended to help guide selection of biomass limit reference points based on guidance from the literature.")
                              }),
                              hr(),

                              conditionalPanel('output.OM_L==1',

                                source(file.path("ui", "ui_hist.R"), local = TRUE)$value

                              )
                       ),
                       box_height='95px'),

      verticalTabPanel(id="MS",value=4,
                    h5(strong("Step 3. Define Management Procedures")),
                    column(12, style='height:800px',
                       column(12, style='padding-left:0px',
                              h5("Once an operating model is specified you can define management procedures (MPs) that make management recommendations
                                 based on estimates of stock status and exploitation rates. When all desired MPs are chosen, proceed to Step 4."),
                              hr()
                       ),

                       conditionalPanel('output.OM_L==0',
                          column(12, style="800px",
                                 h5("Operating model has not been selected, loaded or sketched yet. Please specify an operating model in the 'Step 1. Specify Operating Model' panel above.",style="color:darkgrey")
                          )
                       ),

                       conditionalPanel('output.OM_L==1',
                                        source(file.path("ui", "ui_MP.R"), local = TRUE)$value,
                                        column(12, style="padding-left:0px; height:150px",
                                              hr(),
                                              #uiOutput("SelectedHS"),
                                              div(style="display: inline-block;vertical-align:top; width: 1100px;",
                                                  selectInput("HS_sel",label="Selected Management procedures:",choices="No_Fishing",selected="No_Fishing",multiple=TRUE,width='1000px')),
                                              div(style="display: inline-block; width: 300px;", br(),
                                                  actionGroupButtons(
                                                    inputIds = c("MS_Clear_Last", "MS_Clear_All"),
                                                    labels = c("Clear Last", "Clear All")
                                                  )
                                              )
                                                  #actionButton("MS_Clear_Last",label = "Clear Last",style="color:red;",width='150px',height='20px'),
                                                  #actionButton("MS_Clear_All",label = "Clear All",style="color:red;",width='150px',height='20px')),
                                        )
                       )
                    ), # end of column 12
                       box_height='95px'),

      verticalTabPanel(id="Results", value=5,
                       h5(strong("Step 4. Management Outcomes")),
                       column(12, style='height:800px',

                              conditionalPanel('output.MPsSpec==0',{
                                h5("Please select at least one management procedure in Step 3 above",style="color:darkgrey")
                              }),
                              hr(),

                              conditionalPanel('output.MPsSpec==1',{

                                column(4,conditionalPanel('output.MSErun==0',h5("Run an MSE simulation test of the MP(s): ",style="color:black")))
                                column(4,actionButton("runMSE",label = "Run MSE Simulation Test",style="color:red;",width='200px',height='20px'))

                              }),

                              column(12, HTML('<br>'),

                                     conditionalPanel('output.MSErun==0',{
                                       h5("Please run an MSE simulation test to see results",style="color:darkgrey")
                                     }),

                                     conditionalPanel('output.MSErun==1',{
                                       source(file.path("ui", "ui_MSE.R"), local = TRUE)$value # MSE plots
                                     })

                              )
                       ),
                       box_height='95px'),

      verticalTabPanel(id="Hist", value=6,
                       h5("Detailed Operating Model Info"),
                       column(12, style='height:800px',
                              h5("This panel provides a complete description of the parameters and dynamics of the operating model used in testing."),
                              hr(),

                              conditionalPanel('output.OM_L==0',{
                                h5("Operating model has not been selected, loaded or sketched yet. Please specify an operating model in the 'Step 1: Specify Fishery' panel above.",style="color:darkgrey")

                              }),
                              conditionalPanel('output.OM_L==1',{
                                source(file.path("ui", "ui_OM.R"), local = TRUE)$value # Additional OM plots
                              })

                       ),
                       box_height='95px'),
      contentWidth=11

    ) # end of tabsetpanel
  ), # end of main window for general

  column(12, verbatimTextOutput("Log",placeholder=TRUE), style="padding-top:30px"),
  column(12, column(10), column(2,downloadButton("Download_Log","Download Log",style="height:28px"))),

  bsTooltip("Log","Application Log"),

  column(12, br(), br(), style="height:40px;  text-align: center;",textOutput("SessionID")),

  column(12, br(), style="height:40px; text-align: center", h6("Copyright (c) Blue Matter Science Ltd, 2021"))

) # end of fluid page
