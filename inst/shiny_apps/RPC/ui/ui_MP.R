tabsetPanel(id="MP_select", selected = 1,

            tabPanel(h5("Overview"),
                     p("Step 3 provides the ability to develop various types of management procedures (MPs) for
                     projections. Two types of static MPs can be created here: fixed F (fishing mortality) and
                     fixed catch MPs, where the levels of F and catch, respectively, in the projection are
                     specified relative to the value in the last historical year."),

                     p("MPs which combine a model and harvest control rules can be developed by specifying (1)
                     the operational control points (OCPs); (2) the output variable used to generate the
                     corresponding catch recommendation; and (3) the model from which to obtain values of
                     the OCPs and output variables; and (4) the frequency at which the MP is applied (with
                     constant catch advice between updates)."),

                     p("Data-limited MPs can be selected from a list of those available in DLMtool."),

                     p("Custom MPs compatible with ", a("openMSE", href = "https://www.openmse.com/", target = "_blank"),
                       " can also be uploaded to RPC. This option may be useful for testing and fine-tuning case-specific
                       assessment models."),

                     p(tags$strong("No_Fishing"), "is always a selected MP."),

                     p("Closed-loop simulation is utilized for all MPs. The operating model generates data in
                     response to the each application of the MP (typically returning a catch recommendation,
                     but effort and size limits can also be specified in DLMtool MPs), with each successive
                     application of the MP forward in time using new data simulated during the projection period."),

                     p("At any time, the number of simulation replicates and projection years (for the future
                     projections) in the operating model can be adjusted in the 'Settings' tab in the upper right."),

                     p("MPs made in the App can also be downloaded to be used in openMSE."),
                     value = 1),

            tabPanel(h5("Fixed F"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Specify an MP that sets fishing mortality (F) relative to last historical year", style='font-weight:bold'),
                            column(4,
                                   column(3,h5('MP Name:')),
                                   column(9,textInput("MS_FixF_Label",label=NULL, value = "CurF_100",
                                                      placeholder = "Name of management procedure")),
                                   sliderInput("MS_FixF_ratio", textOutput("MS_FixF_ratio_label"),
                                               min = 0, max = 5, value = 1, step = 0.05)
                                   ),
                            column(8,
                                   plotOutput("MS_FixF_plot", height = 500)
                                   )
                            ),
                     actionButton("Build_MS_FixF","Build MP",style='color:red',icon=icon('cogs')),
                     downloadButton("Save_MS_FixF","Save MP",style='color:red'),
                     value = 2),

            tabPanel(h5("Fixed catch"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Specify an MP that sets catch relative to last historical year", style='font-weight:bold'),
                            column(4,
                                   column(3,h5('MP Name:')),
                                   column(9,textInput("MS_FixC_Label",label=NULL, value = "CurC_100",
                                                      placeholder = "Name of management procedure")),
                                   sliderInput("MS_FixC_ratio", textOutput("MS_FixC_ratio_label"),
                                               min = 0, max = 5, value = 1, step = 0.05)
                            ),
                            column(8,
                                   plotOutput("MS_FixC_plot", height = 500)
                                   )
                     ),
                     actionButton("Build_MS_FixC","Build MP",style='color:red',icon=icon('cogs')),
                     downloadButton("Save_MS_FixC","Save MP",style='color:red'),
                     value = 3),

            tabPanel(h5("Harvest control rule"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Design a custom harvest control rule by specifying the operational control points and output variables.",
                               style='font-weight:bold'),
                            column(4,
                                   div(style = "overflow-y:scroll; height:425px",
                                       column(3,h5('MP Name:')),
                                       column(9,textInput("MS_HCR_Label",label=NULL, value = "", placeholder = "Name of management procedure")),
                                       radioButtons("MS_Origin", "Origin of estimated variables",
                                                    choiceNames=c("Perfect Information", "Stock Assessment", "Assessment Shortcut"),
                                                    choiceValues=c("Perfect", "SCA_Pope", "Shortcut2"),inline=T),
                                       sliderInput("MS_interval", "Frequency of assessment (years)", min = 1, max = 10, value = 2, step = 1),
                                       radioButtons("MS_DVar","Output type (dependent variable)",
                                                    choiceNames=list(HTML("F / F<sub>MSY</sub>"),
                                                                     HTML("F / F<sub>0.1</sub>"),
                                                                     HTML("F / F<sub>max</sub>"),
                                                                     HTML("F / F<sub>SPR</sub>")),
                                                    choiceValues=list("FMSY", "F01", "Fmax", "FSPR"),
                                                    inline=TRUE),
                                       radioButtons("MS_control","Number of operational control points (OCP)",choiceNames=c("None (Fixed F)", "2"),choiceValues=1:2,inline=T),
                                       conditionalPanel("input.MS_control != 1",
                                                        radioButtons("MS_IVar","OCP type (Harvest control rule independent variable)",
                                                                     choiceNames=list(HTML("SSB / SSB<sub>MSY</sub>"),
                                                                                      HTML("Depletion (SSB / Initial SSB<sub>0</sub>)"),
                                                                                      HTML("Dynamic depletion (SSB / Dynamic SSB<sub>0</sub>)"),
                                                                                      HTML("F / F<sub>MSY</sub>"),
                                                                                      HTML("F / F<sub>0.1</sub>"),
                                                                                      HTML("F / F<sub>SPR</sub>")),
                                                                     choiceValues=list("SSB_SSBMSY", "SSB_SSB0", "SSB_dSSB0", "F_FMSY", "F_F01", "F_FSPR"),
                                                                     inline=FALSE)
                                       )
                                   )
                            ),
                            column(3,style='padding-top:30px; padding-left:0px',

                                   conditionalPanel("input.MS_control<5", # Not sketching
                                                    conditionalPanel("input.MS_DVar == 'FSPR'",
                                                                     column(12, sliderInput("SPR_targ", "Output spawning potential ratio",min=0.1,max=0.7,value=0.4,step=0.05))
                                                    ),
                                                    conditionalPanel("input.MS_control == 2 && input.MS_IVar == 'F_FSPR'",
                                                                     column(12, sliderInput("SPR_OCP", "Control point spawning potential ratio",min=0.1,max=0.7,value=0.4,step=0.05))
                                                    ),
                                                    conditionalPanel("input.MS_control==1", # No control points
                                                                     column(12,sliderInput('CP_yint',"Value of output",min=0,max=2,value=1,step=0.05))
                                                    ),
                                                    conditionalPanel("input.MS_control==2", # at least 2 control points,
                                                                     #setSliderColor("orange", 1:4),
                                                                     column(6,
                                                                            sliderInput('CP_1_x',"Control point 1:  x",min=0,max=2,value=0.4,step=0.05),
                                                                            sliderInput('CP_2_x',"Control point 2:  x",min=0,max=2,value=1,step=0.05)
                                                                            ),
                                                                     column(6,
                                                                            sliderInput('CP_1_y',"y",min=0,max=2,value=0,step=0.05),
                                                                            sliderInput('CP_2_y',"y",min=0,max=2,value=1,step=0.05)
                                                                            )
                                                    )
                                   )
                            ),

                            column(5,style='padding-top:0px',
                                   plotOutput('HSplot')
                            )
                     ),
                     actionButton("Build_MS","Build MP",style='color:red',icon=icon('cogs')),
                     downloadButton("Save_MS","Save MP",style='color:red'),
                     value = 4),

            tabPanel(h5("Data-limited MPs"),
                     column(12, style="height:500px; padding-top:30px",
                            HTML("<h5 style='font-weight:bold'>Select from a suite of data-limited MPs from <a target='_blank' href='https://dlmtool.openmse.com/reference/index.html'>DLMtool</a>:</h5>"),
                            column(3,
                                   selectInput("MS_DLM", label = NULL,
                                               choices = MSEtool:::get_funcs("DLMtool", "MP", msg = FALSE),
                                               selectize = FALSE, size = 15),
                                   sliderInput("MS_DLM_interval", "Frequency of MP update (years)", min = 1, max = 10, value = 2, step = 1)
                                   ),
                            column(9,
                                   uiOutput("DLM_iframe")
                                   )
                     ),
                     actionButton("Build_MS_DLM","Add MP",style='color:red',icon=icon('cogs')),
                     value = 5),

            tabPanel(h5("Import MP"),
                     column(12, style="height:500px; padding-top:30px",
                            h5("Import an MP (saved using saveRDS). Once a file is uploaded, provide the name and update frequency of the MP to add it.", style='font-weight:bold'),
                            column(4,
                                   column(3, h5('MP Name:')),
                                   column(9,
                                          textInput("MS_Import_Label",label=NULL, value = "", placeholder = "Name of management procedure")
                                   ),
                                   column(3, h5("Description:")),
                                   column(9,
                                          textInput("MS_Import_Description",label=NULL, value = "", placeholder = "Description (for summary)")
                                   )
                            ),
                            fileInput("MS_Import_file", label = NULL),
                            sliderInput("MS_Import_interval", "Frequency of MP update (years)", min = 1, max = 10, value = 2, step = 1)
                     ),
                     actionButton("Build_MS_import","Add MP",style='color:red',icon=icon('cogs')),
                     value = 6),

            tabPanel(h5("Summary"),
                     column(12, style="height:500px; padding-top:30px",
                            tableOutput("MS_summary")
                     ),
                     value = 7)

)

