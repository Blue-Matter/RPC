
column(12,
       column(1, h1("RPC"), style = "height:65px"),
       column(5, h3("reference point calculator (alpha)"), style = "height:65px;padding-top:8px"),


       column(6, style = "padding-top:25px",

              div(style = "display: inline-block;vertical-align:top;float:right;",
                  dropdownButton(inputId = "FPAT_File",
                                 column(12,
                                        h5(tags$b("Glossary", style = "color:#347ab6")),
                                        column(12,
                                               tabsetPanel(
                                                 tabPanel(h5("RPC", style = "color:black"), HTML("<br>"), DT::dataTableOutput('CMPhelp'), value = 1),
                                                 tabPanel(h5("MSE", style = "color:black"), HTML("<br>"), DT::dataTableOutput('PMhelp'), value = 2),
                                                 tabPanel(h5("MERA", style = "color:black"), HTML("<br>"), value=3)
                                               ) # end of dropdownbutton CMP
                                        ),
                                        column(12, HTML("<br>")),

                                        h5(tags$b("Contact", style = "color:#347ab6")),
                                        column(12,
                                               h5("For technical questions or bug reports please contact ", a("tom@bluematterscience.com", href = "mailto:tom@bluematterscience.com", target = "_blank"), style = "color:grey")
                                        ),
                                        h5(tags$b("Software", style = "color:#347ab6")),
                                        column(12,
                                               h5(paste0("RPC v", packageVersion("RPC")), style = "color:grey"),
                                               tags$a(img(src = "openMSE.png", height = 35, width = 115),href="https://www.openmse.com", target = '_blank')
                                        ),
                                        h5(tags$b("Acknowledgements", style = "color:#347ab6")),
                                        column(12,
                                               h5("DFO, WG members")
                                        ),
                                        column(12, tags$a(img(src = "bluematter.png", height = 38, width = 80), href = "https://www.bluematterscience.com", target = '_blank'))
                                 ),

                                 label = "Help",
                                 icon = icon("info"),
                                 status = "dropdownbutton",
                                 right = TRUE,
                                 circle = FALSE,
                                 width = "800px"
                  )
              ), # end of help menu dropdown

              # Reports menu dropdown
              div(style="display: inline-block;vertical-align:top; float:right;",

                  dropdownButton(
                    column(12,
                           column(9, h5("Operating model", style = "font-weight:bold;color:#347ab6")),
                           column(3, downloadButton("OM_Rep", " ")),
                           column(9, h5("MPs", style = "font-weight:bold;color:#347ab6")),
                           column(3, downloadButton("Cond_Rep", " ")),
                           column(9, h5("RPC results", style = "font-weight:bold;color:#347ab6")),
                           column(3, downloadButton("FPAT_Rep"," "))
                    ),
                    inputId = "Reports",
                    label = "Reports",
                    icon = icon("newspaper"),
                    status = "dropdownbutton",
                    circle = FALSE,
                    right = TRUE,
                    width ="300px"
                  )

              ), # end of reports menu dropdown

              # Settings menu dropdown
              div(style = "display: inline-block;vertical-align:top; float:right;",

                  dropdownButton(
                    column(12,
                           h5(tags$b("Settings for controlling MSE specifications", style = "color:#347ab6")),
                    ),
                    inputId = "DD_Settings",
                    label = "Settings",
                    icon = icon("sliders-h"),
                    status = "dropdownbutton",
                    circle = FALSE,
                    right = TRUE,
                    width = "400px"
                  )
              ), # end of settings menu dropdown

              # File menu dropdown
              div(style = "display: inline-block;vertical-align:top; float:right;",

                  dropdownButton(
                    column(12,
                           tags$hr(style = "margin-top: 3px; margin-bottom: 3px"),
                           h5(tags$b("RPC Session", style = "color:#347ab6")),
                           column(6, h5("Load (.rpc)", style = "color:grey"), tipify(fileInput("Load_session", label = NULL, accept = c("rpc",".rpc")), title = "Load a previous session including calculated results")),
                           column(1),
                           column(5, h5("Save (.rpc)", style = "color:grey"), downloadButton("Save_session", "", width = "100px"))
                    ),
                    inputId = "DD_file",
                    label = "File",
                    icon = icon("file"),
                    status = "dropdownbutton",
                    circle = FALSE,
                    right = TRUE,
                    width = "400px"
                  )
              ) # end of file menu dropdown
       )# end of column 6

)  # end of tool bar