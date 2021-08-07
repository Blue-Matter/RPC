tabsetPanel(id="MP_select", selected = 1,

            tabPanel(h5("Fixed F"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Specify an MP that sets fishing mortality (F) relative to last historical year", style='font-weight:bold'),
                            column(4,
                                   column(3,h5('MP Name:')),
                                   column(9,textInput("MS_FixF_Label",label=NULL, value = "CurF_100",
                                                      placeholder = "Name of management procedure")),
                                   sliderInput("MS_FixF_ratio", textOutput("MS_FixF_ratio_label"),
                                               min = 0, max = 5, value = 1, step = 0.05)
                                   )
                            ),
                     actionButton("Build_MS_FixF","Build management procedure",style='color:red',icon=icon('cogs')),
                     value = 1),

            tabPanel(h5("Fixed catch"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Specify an MP that sets catch relative to last historical year", style='font-weight:bold'),
                            column(4,
                                   column(3,h5('MP Name:')),
                                   column(9,textInput("MS_FixC_Label",label=NULL, value = "CurC_100",
                                                      placeholder = "Name of management procedure")),
                                   sliderInput("MS_FixC_ratio", textOutput("MS_FixC_ratio_label"),
                                               min = 0, max = 5, value = 1, step = 0.05)
                            )
                     ),
                     actionButton("Build_MS_FixC","Build management procedure",style='color:red',icon=icon('cogs')),
                     value = 2),

            tabPanel(h5("Harvest control rule"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Design a custom harvest control rule by specifying the operational control points and output variables.",
                               style='font-weight:bold'),
                            column(4,
                                   column(3,h5('MP Name:')),
                                   column(9,textInput("MS_HCR_Label",label=NULL, value = "", placeholder = "Name of management procedure")),
                                   radioButtons("MS_Origin", "Origin of estimated variables",
                                                choiceNames=c("Perfect Information", "Stock Assessment", "Assessment Shortcut"),
                                                choiceValues=c("Perfect", "SCA_Pope", "Shortcut2"),inline=T),
                                   radioButtons("MS_DVar","Output type (dependent variable)",
                                                choiceNames=list(HTML("F / F<sub>MSY</sub>"),
                                                                 HTML("F / F<sub>0.1</sub>"),
                                                                 HTML("F / F<sub>max</sub>"),
                                                                 HTML("F / F<sub>SPR</sub>")),
                                                choiceValues=1:4,inline=T),
                                   radioButtons("MS_control","Number of operational control points (OCP)",choiceNames=c("None (Fixed F)", "2"),choiceValues=1:2,inline=T),
                                   conditionalPanel("input.MS_control != 1",
                                                    radioButtons("MS_IVar","OCP type (Harvest control rule independent variable)",
                                                                 choiceNames=list(HTML("SSB / SSB<sub>MSY</sub>"),
                                                                                  HTML("Depletion (SSB / Initial SSB<sub>0</sub>)"),
                                                                                  HTML("Dynamic depletion (SSB / Dynamic SSB<sub>0</sub>)"),
                                                                                  HTML("F / F<sub>MSY</sub>"),
                                                                                  HTML("F / F<sub>0.1</sub>"),
                                                                                  HTML("F / F<sub>SPR</sub>")),
                                                                 choiceValues=1:6,inline=FALSE)
                                   ),

                            ),
                            column(3,style='padding-top:30px; padding-left:0px',

                                   conditionalPanel("input.MS_control<5", # Not sketching
                                                    setSliderColor(rep("orange", 6), 1:6),
                                                    conditionalPanel("input.MS_IVar == 6",
                                                                     column(12, sliderInput("SPR_OCP", "Control point spawning potential ratio",min=0.1,max=0.7,value=0.4,step=0.05))
                                                    ),
                                                    conditionalPanel("input.MS_DVar == 4",
                                                                     column(12, sliderInput("SPR_targ", "Output spawning potential ratio",min=0.1,max=0.7,value=0.4,step=0.05))
                                                    ),

                                                    conditionalPanel("input.MS_control==1", # at least 2 control points,
                                                                     column(12,sliderInput('CP_yint',"Value of output",min=0,max=2,value=1,step=0.05))
                                                    ),
                                                    conditionalPanel("input.MS_control==2", # at least 2 control points,
                                                                     column(6,sliderInput('CP_1_x',"Control point 1: x",min=0,max=2,value=0.4,step=0.05)),
                                                                     column(6,sliderInput('CP_1_y',"y",min=0,max=2,value=0,step=0.05)),

                                                                     column(6,sliderInput('CP_2_x',"Control point 2: x",min=0,max=2,value=1,step=0.05)),
                                                                     column(6,sliderInput('CP_2_y',"y",min=0,max=2,value=1,step=0.05))
                                                    )
                                   )
                            ),

                            column(5,style='padding-top:0px',
                                   conditionalPanel("input.MS_control != 1",
                                                    plotOutput('HSplot')
                                   ),
                            ),
                     ),
                     actionButton("Build_MS","Build management procedure",style='color:red',icon=icon('cogs')),
                     value = 3),

            tabPanel(h5("Data-limited MPs"),
                     column(12, style="height:500px; padding-top:30px",
                            HTML("<h5 style='font-weight:bold'>Select from a suite of data-limited MPs from <a href='https://dlmtool.openmse.com/reference/index.html'>DLMtool</a>:</h5>"),
                            selectInput("MS_DLM", label = NULL,
                                        choices = MSEtool:::get_funcs("DLMtool", "MP", msg = FALSE),
                                        selectize = FALSE, size = 15),
                            p("Details of the MP can be found on the openMSE website:"), textOutput("DLM_URL", a)
                     ),
                     actionButton("Build_MS_DLM","Add management procedure",style='color:red',icon=icon('cogs')),
                     value = 4),

            tabPanel(h5("Import MP"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Import an MP (saved using saveRDS):", style='font-weight:bold'),
                            column(4,
                                   column(3,h5('MP Name:')),
                                   column(9,textInput("MS_Import_Label",label=NULL, value = "",
                                                      placeholder = "Name of management procedure")),
                                   fileInput("MS_Import_file", label = NULL)
                            )
                     ),
                     actionButton("Build_MS_import","Add management procedure",style='color:red',icon=icon('cogs')),
                     value = 5)

            )

