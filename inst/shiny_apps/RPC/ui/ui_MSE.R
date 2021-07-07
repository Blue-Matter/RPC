tabsetPanel(id="Res", selected=1,

            tabPanel(h5("SSB0"),

                     tabsetPanel(id="SSB0", selected=1,
                                 tabPanel(h5("Median time series"),
                                          plotOutput("B_proj_plot",height=520),

                                          value=1),
                                 tabPanel(h5("Probability of exceeding RPs"),
                                          plotOutput("B_prob_plot",height=520),
                                          value=2),
                                 tabPanel(h5("Stochastic projection"),
                                          column(9,plotOutput("B_stoch_plot",height=500)),
                                          column(3,
                                                 selectInput("SMP1","",choices="",selected=""),
                                                 selectInput("SMP2","",choices="",selected="")
                                                 #selectInput("SMP3","",choices="",selected="")
                                          ),
                                          value=3),
                                 tabPanel(h5("By Simulation"),
                                          column(2,
                                                 selectInput("StochMP","",choices="",selected=""),
                                                 sliderInput('nsim_hist_SSB',"Number of simulations to plot:",min=1,max=3,value=1,step=1),
                                                 actionButton("StochB_resample","Resample")),
                                          column(10,plotOutput("plot_hist_SSB_sim",height=520)),
                                          value=4),
                                 tabPanel(h5("Table"),
                                          h5("COMING SOON: SSB relative to SSB0 datatable to go here",style = "color:black"),
                                          value=5)
                     ),
                     value=1),

            tabPanel(h5("BMSY"),

                     tabsetPanel(id="SSBMSY", selected=1,
                                 tabPanel(h5("Median time series"),
                                          h5("COMING SOON: Median SSBMSY SSB time series plots to go here",style = "color:black"),
                                          value=1),
                                 tabPanel(h5("Probability of exceeding RPs"),
                                          h5("COMING SOON: Probability of exceeding SSBMSY fractions to go here",style = "color:black"),
                                          value=2),
                                 tabPanel(h5("Stochastic projection"),
                                          h5("COMING SOON: SSB relative to SSBMSY Stochastic projection plot to go here",style = "color:black"),
                                          value=3),
                                 tabPanel(h5("By Simulation"),
                                          h5("COMING SOON:SSB relative to SSBMSY by simulation plot to go here",style = "color:black"),
                                          value=4),
                                 tabPanel(h5("Table"),
                                          h5("COMING SOON:SSB relative to SSBMSY datatable to go here",style = "color:black"),
                                          value=5)
                     ),
                     value=2),

            tabPanel(h5("FMSY"),
                     h5("COMING SOON: Tableset panel with outputs for F relative FMSY",style = "color:black"),
                     plotOutput("plot_FMSY",height=540),
                     value=3),

            tabPanel(h5("MSY"),
                     h5("COMING SOON: Tableset panel with catch relative to MSY",style = "color:black"),
                     plotOutput("plot_MSY",height=540),
                     value=4),

            tabPanel(h5("Summary"),
                     h5("COMING SOON: summary plots / Tables",style = "color:black"),
                     plotOutput("plot_summary",height=540),
                     value=5),

            tabPanel(h5("Trade-offs"),
                     h5("COMING SOON: SSB0 RP, SSBMSY RP, FMSY RP & MSY RP trade-offs",style = "color:black"),
                     plotOutput("plot_TO",height=540),
                     value=6)

) # tabsetpanel
