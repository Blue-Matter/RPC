column(12,
       column(12,

              column(12,
                     h5("A. Select management procedure presets for exploration of outcomes",style='font-weight:bold'),

                     div(style="display: inline-block;vertical-align:top; width: 200px;",actionButton("MS_Frat",label = "Current F Ratios",style="color:red",width='200px',height='20px')),
                     div(style="display: inline-block;vertical-align:top; width: 200px;",actionButton("MS_Crat",label = "Current Catch Ratios",style="color:red",width='200px',height='20px')),
                     div(style="display: inline-block;vertical-align:top; width: 200px;",actionButton("MS_DFO",label = "DFO",style="color:red",width='200px',height='20px'))
              )
              #checkboxGroupButtons(inputId='MS_presets',label='B. Preset Management Strategies',choices=c("Current effort ratios","Current F ratios","Current Catch ratios"),status='danger'),
       ),

       column(12, style="height:470px",
              hr(),
              column(4,

                     h5("B. Design a custom management procedure",style='font-weight:bold'),
                     column(3,h5('Label:')),
                     column(9,textInput("MS_Label",label=NULL)),
                     radioButtons("MS_Origin", "Origin of estimated variables",
                                  choiceNames=c("Perfect Information", "Stock Assessment", "Assessment Shortcut"),
                                  choiceValues=c("Perfect", "SCA_Pope", "Shortcut2"),inline=T),
                     radioButtons("MS_IVar","Operational control point (HCR independent variable)",
                                  choiceNames=list(HTML("SSB / SSB<sub>MSY</sub>"),
                                                   HTML("Depletion (SSB / Initial SSB<sub>0</sub>)"),
                                                   HTML("Dynamic depletion (SSB / Dynamic SSB<sub>0</sub>)"),
                                                   HTML("F / F<sub>MSY</sub>"),
                                                   HTML("F / F<sub>0.1</sub>"),
                                                   HTML("F / F<sub>40% SPR</sub>")),
                                  choiceValues=1:6,inline=FALSE),
                     radioButtons("MS_DVar","Output type (dependent variable)",
                                  choiceNames=list(HTML("F / F<sub>MSY</sub>"),
                                                   HTML("F / F<sub>0.1</sub>"),
                                                   HTML("F / F<sub>max</sub>"),
                                                   HTML("F / F<sub>40% SPR</sub>")),
                                  choiceValues=1:4,inline=T),
                     radioButtons("MS_control","Number of control points",choiceNames=c("None (constant)","2"),choiceValues=1:2,inline=T)

              ),
              column(3,style='padding-top:30px; padding-left:0px',

                     conditionalPanel("input.MS_control<5", # Not sketching
                                      setSliderColor(rep("orange", 6), 1:6),
                                      conditionalPanel("input.MS_control==1", # at least 2 control points,
                                                       column(12,sliderInput('CP_yint',"Y intercept",min=0,max=2,value=1,step=0.02))
                                      ),
                                      conditionalPanel("input.MS_control==2", # at least 2 control points,
                                                       column(6,sliderInput('CP_1_x',"control point 1: x",min=0,max=2,value=0.4,step=0.02)),
                                                       column(6,sliderInput('CP_1_y',"y",min=0,max=2,value=0,step=0.02)),

                                                       column(6,sliderInput('CP_2_x',"control point 2: x",min=0,max=2,value=1,step=0.02)),
                                                       column(6,sliderInput('CP_2_y',"y",min=0,max=2,value=1,step=0.02))
                                      )

                     )

              ),

              column(5,style='padding-top:0px',
                     plotOutput('HSplot'),
                     column(7),
                     column(5,actionButton("Build_MS","Build management procedure",style='color:red',icon=icon('cogs')))
              )
       ),

       column(12, style="padding-left:0px; height:150px",
              hr(),
              #uiOutput("SelectedHS"),
              div(style="display: inline-block;vertical-align:top; width: 1100px;",selectInput("HS_sel",label="Selected Management procedures:",choices='',selected="",multiple=TRUE,width='1200px')),
              div(style="display: inline-block; width: 300px;", br(), actionButton("MS_Clear",label = "Clear",style="color:red;",width='200px',height='20px')),
       )
)

