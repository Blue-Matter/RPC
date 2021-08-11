tabsetPanel(id="Res", selected=1,

            tabPanel(HTML("<h5>SSB<sub>0</sub></h5>"),

                     tabsetPanel(id="SSB0", selected=1,
                                 tabPanel(h5("Median time series"),
                                          plotOutput("B_proj_plot",height=520),
                                          value=1),
                                 tabPanel(h5("Stochastic projection"),
                                          column(12,
                                                 column(3,
                                                        selectInput("SMP1","First management procedure",choices="",selected=""),
                                                        selectInput("SMP2","Second management procedure",choices="",selected=""),
                                                        sliderInput("SSB0_MSE_quantile", "Quantile", min = 0.01, max = 0.99, value = 0.9, step = 0.01)
                                                 ),
                                                 column(9, plotOutput("B_stoch_plot",height=520),
                                                 )
                                          ),
                                          value=2),
                                 tabPanel(h5("Individual Simulations"),
                                          column(2,
                                                 selectInput("StochMP","Management procedure:",choices="",selected=""),
                                                 sliderInput('nsim_hist_SSB',"Number of simulations to plot:",min=1,max=3,value=1,step=1),
                                                 actionButton("StochB_resample","Resample")),
                                          column(10,plotOutput("plot_hist_SSB_sim",height=520)),
                                          value=3),

                                 tabPanel(h5("Probability"),
                                          column(12,
                                                 column(3,
                                                        sliderInput("SSB0_MSE_prob", HTML("SSB/SSB<sub>0</sub> threshold"), min = 0, max = 1, value = 0.4, step = 0.01),
                                                        sliderInput("SSB0_MSE_yrange", "Figure y-axis range", min = 0, max = 1.1, value = c(0, 1), step = 0.01),
                                                        sliderInput("SSB0_MSE_xrange", "Projection year range", min = 0, max = 0, value = c(0, 0), step = 1, sep = "")
                                                 ),
                                                 column(9, tabsetPanel(id = "SSB0_Prob", selected = 1,
                                                                       tabPanel("Figure",
                                                                                plotOutput("B_prob_plot",height=520),
                                                                                value = 1),
                                                                       tabPanel("Table",
                                                                                textOutput("B_prob_table_label"),
                                                                                tableOutput("B_prob_table"),
                                                                                value = 2)
                                                 )
                                                 )
                                          ),
                                          value=4)
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
