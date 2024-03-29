
column(12,
       column(1, h1("RPC"), style = "height:65px"),
       column(5, h3(paste("Reference Point Calculator", packageVersion("RPC"))), style = "height:65px;padding-top:8px"),


       column(6, style = "padding-top:25px",

              div(style = "display: inline-block;vertical-align:top;float:right;",
                  dropdownButton(inputId = "RPC_File",
                                 column(12,
                                        h5(tags$b("User Guide", style = "color:#347ab6")),
                                        column(12,
                                               h5("The user guide is available at",
                                                  a("https://blue-matter.github.io/openMSE/RPC-User-Guide.html",
                                                    href = "https://blue-matter.github.io/openMSE/RPC-User-Guide.html",
                                                    target = "_blank"), style = "color:grey")
                                               ),
                                        #h5(tags$b("Glossary", style = "color:#347ab6")),
                                        #column(12,
                                        #       tabsetPanel(
                                        #         tabPanel(h5("RPC", style = "color:black"), HTML("<br>"), DT::dataTableOutput('CMPhelp'), value = 1),
                                        #         tabPanel(h5("MSE", style = "color:black"), HTML("<br>"), DT::dataTableOutput('PMhelp'), value = 2),
                                        #         tabPanel(h5("MERA", style = "color:black"), HTML("<br>"), value=3)
                                        #       ) # end of dropdownbutton CMP
                                        #),
                                        br(), br(),
                                        h5(tags$b("Contact"), style = "color:#347ab6"),
                                        column(12,
                                               h5("For technical questions, requests for features, or bug reports, please submit an issue on ",
                                                  a("GitHub", href = "https://www.github.com/Blue-Matter/RPC/issues", target = "_blank"),
                                                  " or contact ",
                                                  a("Blue Matter Science", href = "mailto:quang@bluematterscience.com", target = "_blank"), style = "color:grey"),
                                        ),
                                        br(), br(),
                                        h5(tags$b("Software", style = "color:#347ab6")),
                                        column(12,
                                               h5(paste0("RPC v", packageVersion("RPC"), ", powered by MSEtool v", packageVersion("MSEtool"),
                                                         ", DLMtool v", packageVersion("DLMtool"), ", SAMtool v", packageVersion("SAMtool"),
                                                         ", in R ", getRversion()),
                                                  style = "color:grey"),
                                               tags$a(img(src = "openMSE.png", height = 35, width = 115),href="https://www.openmse.com", target = '_blank')
                                        ),
                                        br(), br(),
                                        h5(tags$b("Acknowledgements", style = "color:#347ab6")),
                                        column(12,
                                               h5("Department of Fisheries and Oceans Canada")
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

                  dropdownButton(inputId = "Report",
                    column(12,
                           column(9, h5("Download RPC report", style = "font-weight:bold;color:#347ab6")),
                           column(3, downloadButton("RPC_Rep"," "))
                    ),
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

                  dropdownButton(inputId = "DD_Settings",
                    column(12,
                           h5(tags$b("Settings for controlling MSE specifications", style = "color:#347ab6")),
                           sliderInput("DD_nsim", "Number of simulations", min = 3, max = 100, value = 24, step = 1),
                           sliderInput("DD_proyears", "Number of projection years", min = 5, max = 100, value = 50, step = 1),
                           conditionalPanel("output.OM_L == 1",
                                            actionButton("DD_update", label = "Update", icon = icon("redo"), style = "color:red")
                                            )
                    ),
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

                  dropdownButton(inputId = "DD_file",
                    column(12, style = "margin-bottom: 20px",
                           h5(tags$b("RPC Session", style = "color:#347ab6")),
                           h5("Load (.rpc)", style = "color:grey"),
                           tipify(fileInput("Load_session", label = NULL, accept = c("rpc", ".rpc")),
                                  title = "Load a previous session including calculated results"),
                           h5("Save (.rpc)", style = "color:grey"),
                           downloadButton("Save_session", "Save as..", width = "200px"),

                    ),
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
