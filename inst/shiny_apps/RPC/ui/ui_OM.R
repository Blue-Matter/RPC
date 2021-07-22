tabsetPanel(id="OM_hist", selected=1,

            tabPanel(h5("Stock dynamics"),
                     tabsetPanel(id="OM_hist_bio", selected=1,
                                 tabPanel(h5("Time series"),
                                          plotOutput("plot_hist_bio",height=540),
                                          value=1),
                                 tabPanel(h5("Age schedules"),
                                          column(12,
                                                 column(3,
                                                        radioButtons("bio_schedule", "Type", choiceNames = c("Length at age", "Weight at age", "Maturity", "Natural mortality"),
                                                                     choiceValues = c("Len_age", "Wt_age", "Mat_age", "M_ageArray"), inline = FALSE),
                                                        sliderInput("bio_schedule_sim", "Left figure: Simulation #", min = 0, max = 0, value = 0, step = 1),
                                                        sliderInput("bio_schedule_nage", "Left figure: Number of ages", min = 0, max = 0, value = 0, step = 1),
                                                        sliderInput("bio_schedule_year", textOutput("bio_year_text"), min = 0, max = 0, value = 0, step = 1, sep = "")
                                                 ),
                                                 column(9,
                                                        plotOutput("plot_hist_age_schedule", height = 540)
                                                 )
                                          ),
                                          value = 2),
                                 tabPanel(h5("Growth Parameters I"),
                                          plotOutput("plot_hist_growth_I",height=540),
                                          value=3),
                                 tabPanel(h5("Growth Parameters II"),
                                          plotOutput("plot_hist_growth_II",height=540),
                                          value=4),
                                 tabPanel(h5("Spatial"),
                                          plotOutput("plot_hist_spatial",height=540),
                                          value=5)

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
