tabsetPanel(id="OM_hist", selected=1,

            tabPanel(h5("Stock dynamics"),
                     tabsetPanel(id="OM_hist_bio", selected=1,
                                 tabPanel(h5("Time series"),
                                          plotOutput("plot_hist_bio",height=540),
                                          value=1),
                                 tabPanel(h5("Growth Parameters"),
                                          plotOutput("plot_hist_growth_I",height=540),
                                          value=2),
                                 tabPanel(h5("Growth vs. Time"),
                                          plotOutput("plot_hist_growth_II",height=540),
                                          value=3),
                                 tabPanel(h5("Growth at Age"),
                                          plotOutput("plot_hist_growth_III",height=540),
                                          value=4),
                                 tabPanel(h5("Maturity"),
                                          plotOutput("plot_hist_maturity",height=540),
                                          value=5),
                                 tabPanel(h5("Natural mortality"),
                                          plotOutput("plot_hist_survival",height=540),
                                          value=6),
                                 tabPanel(h5("Spatial"),
                                          plotOutput("plot_hist_spatial",height=540),
                                          value=7)

                     ),
                     value=1),

            tabPanel(h5("Fishing dynamics"),
                     tabsetPanel(id = "OM_hist_exp", selected = 1,
                                 tabPanel(h5("Time series"),
                                          plotOutput("plot_hist_exp",height=540),
                                          value = 1),
                                 tabPanel(h5("Selectivity"),
                                          plotOutput("plot_hist_sel",height=540),
                                          value = 2)
                     ),
                     value=2),

            tabPanel(h5("Management Quantities"),
                     tabsetPanel(id="OM_hist_RP", selected=1,

                                 tabPanel(h5("SSB"),
                                          plotOutput("plot_hist_SSB",height=540),
                                          # add table of probabililites & slider
                                          value=1)

                     ),
                     #plotOutput("plot_hist_RPs",height=540),
                     value=3)

) # tabsetpanel
