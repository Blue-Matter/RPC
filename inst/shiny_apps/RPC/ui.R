

fluidPage(
  useShinyjs(),
  useShinyalert(force = TRUE),
  includeScript(path = "www/js/js4checkbox.js"),
  includeScript(path = "www/js/index.js"),
  tags$head(
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),

    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$style(".fa-cogs {color:red}"),
    tags$style(HTML("
                    #SessionID{font-size:12px;}
                    ")),
    tags$style(HTML("
        /* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */

        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');

        /* Font of SLICK title */

      ")),

    tags$style("td.OMtable {padding: 5px 20px 5px 5px;}"),

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
            '),
    tags$script(
    "
    // Enable navigation prompt
    window.onbeforeunload = function() {
        return 'Leave site? All work will be lost unless saved.';
    };
    ")
  ),

  # === Header ==============================================================================================================================================================
  source(file.path("ui", "ui_header.R"), local = TRUE)$value,

  # === Main Window =========================================================================================================================================================

  column(12, # General tab panel
    verticalTabsetPanel(id="Main",selected=1,
      verticalTabPanel(h5("Home"),
                       column(12,
                              h4("Welcome to Reference Point Calculator: a simulation tool for informing the selection of reference points and harvest control rules."),
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
                       column(12, style='height:800px; padding-left:10px; overflow-y:scroll"',
                              h5("In Step 1, the operating model is loaded into the app. The operating model specifies both the historical
                              and future dynamics of the stock. There are currently three ways to specify the operating model: (1) selecting
                              a pre-made operating model; (2) loading a customized operating model compatible with ",
                              a("openMSE", href = "https://openmse.com/", target = "_blank"),
                              "; or (3) sketching and outlining the fishery dynamics using a questionnaire."),
                              h5("Alternatively, load a previously saved RPC session by selecting 'File' in upper right."),
                              h5("At any time, the number of simulation replicates and future projection years for testing management procedures
                              can be adjusted in the 'Settings' tab in the upper right."),
                              hr(),
                              HTML("<br>"),
                              radioButtons('Select',label=NULL,choiceNames=c('Select','Load','Sketch'),choiceValues=c(1,2,3),inline=T),
                              conditionalPanel("input.Select==1",
                                               h5("Select an example operating model, and update the number of simulations and projection years as desired."),
                                               tags$h5("Documentation of these operating models are available at: ",
                                                       tags$a("https://blue-matter.github.io/openMSE/RPC_OM/index.html", target = "_blank", href = "https://blue-matter.github.io/openMSE/RPC_OM/index.html")),
                                               column(12,style="padding-top:30px;padding-bottom:10px;padding-left:50px",
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",
                                                          selectInput("SelectOMDD", choices = OMs, label = NULL, selected = NULL)),
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",
                                                          actionButton("SelectOM",label = "Select",style="color:red",icon=icon('cogs'),width='150px',height='20px')),
                                                      #tableOutput("SelectOMinfo")
                                                      uiOutput("SelectOMinfo")
                                               )
                              ),

                              conditionalPanel("input.Select==2",
                                               h5("Load an openMSE compatible operating model object from file. Once the file is uploaded, update the name, and adjust the number of simulations and projection years, in order to build the operating model."),
                                               tags$h5("Various functions are available in the MSEtool R package to generate an operating model from a stock assessment, including: ",
                                                       tags$a("SS2OM()", target = "_blank", href = "https://msetool.openmse.com/reference/SS2MOM.html"),
                                                       tags$a("iSCAM2OM()", target = "_blank", href = "https://msetool.openmse.com/reference/iSCAM2MOM.html"),
                                                       tags$a("Awatea2OM()", target = "_blank", href = "https://msetool.openmse.com/reference/Awatea2OM.html"),
                                                       ". More generally, an operating model can be generated from abundance-at-age arrays and biological parameters provided to function ",
                                                       tags$a("Assess2OM()", target = "_blank", href = "https://msetool.openmse.com/reference/Assess2OM.html")),
                                               column(12,style="padding-top:30px;padding-bottom:10px;padding-left:50px",

                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",
                                                          fileInput("Load_OMprelim", label = NULL, buttonLabel = div('Browse',style='color:red'))
                                                      ),
                                                      div(style="display: inline-block;vertical-align:top; width: 250px;",
                                                          actionButton("Load_OM",label = "Build",style="color:red",icon=icon('cogs'),width='150px',height='20px')),

                                                      conditionalPanel("output.OM_upload == 1",
                                                                       textInput("Load_OMname", "Name of operating model", value = "")
                                                                       )
                                                      )
                              ),

                              source(file.path("ui", "ui_MERA.R"), local = TRUE)$value # MERA

                              ), box_height='95px'), # end of Fishery vertical Tabpanel

      verticalTabPanel(id="HistResults", value=3,
                       h5(strong("Step 2. Examine Historical Fishery")),
                       column(12, style='height:800px; overflow-y:scroll',
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
                    column(12, style='height:800px;',
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
                                              HTML("<h5><strong>Selected management procedures:</strong></h5>"),
                                              div(style="display: inline-block; vertical-align:top; width: 1000px;",
                                                  selectInput("HS_sel",label=NULL,choices="No_Fishing",selected="No_Fishing",multiple=TRUE,width='1000px')),
                                              div(style="display: inline-block; vertical-align:top; width: 300px;",
                                                  actionGroupButtons(
                                                    inputIds = c("MS_Clear_Last", "MS_Clear_All"),
                                                    labels = c("Remove Last", "Remove All")
                                                  )
                                              )
                                        )
                       )
                    ), # end of column 12
                       box_height='95px'),

      verticalTabPanel(id="Results", value=5,
                       h5(strong("Step 4. Management Outcomes")),
                       column(12, style='height:800px; overflow-y:scroll',

                              conditionalPanel('output.OM_L==0',{
                                h5("Operating model has not been selected, loaded or sketched yet. Please specify an operating model in the 'Step 1. Specify Operating Model' panel above.",style="color:darkgrey")
                              }),
                              hr(),

                              conditionalPanel('output.MPsSpec==1',{

                                column(4,conditionalPanel('output.MSErun==0',h5("Run an MSE simulation test of the MP(s): ",style="color:black")))
                                column(4,actionButton("runMSE",label = "Run MSE Simulation Test",style="color:red;",width='200px',height='20px'))

                              }),

                              column(12, br(),

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
                       column(12, style='height:800px; overflow-y:scroll',
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
