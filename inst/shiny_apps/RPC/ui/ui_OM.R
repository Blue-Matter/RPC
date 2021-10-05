tabsetPanel(id="OM_hist", selected = 1,

            tabPanel(h5("Overview"),
                     p("This tab provides detailed figures on the specification of the operating model, including the historical and projection dynamics, including:"),
                     tags$ul(
                       tags$li("Age schedules provide the biological parameters, including growth, maturity, and natural mortality, by simulation, age, and year"),
                       tags$li("Selectivity figures compare fishery selectivity, retention (if modeled), and maturity"),
                       tags$li("Yield curves which calculate the equilibrium catch using any year combination of biological parameters and selectivity."),
                       tags$li("Recruitment deviations with the mean in normal and lognormal space"),
                       tags$li("Spatial dynamics if using a multiple-area model"),
                       tags$li("Options for changing the population dynamics in the projection period of the operating model")
                     ),
                     value = 1),

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

            tabPanel(h5("Selectivity"),
                     column(12,
                            column(3,
                                   sliderInput("sel_y", textOutput("sel_y_text"), min = 0, max = 0, value = c(0, 0), step = 1, sep = "")
                            ),
                            column(9,
                                   plotOutput("plot_hist_sel",height=520)
                            )
                     ),
                     value = 3),

            tabPanel(h5("Yield curve"),
                     column(12,
                            column(3,
                                   sliderInput("YC_Frange", "Fishing mortality range", min = 0, max = 3, value = c(0, 3), step = 0.01),
                                   sliderInput("YC_y_bio", textOutput("YC_bio_text"), min = 0, max = 0, value = 0, step = 1, sep = ""),
                                   sliderInput("YC_y_sel", "Year for selectivity", min = 0, max = 0, value = 0, step = 1, sep = "")
                            ),

                            column(9,
                                   plotOutput("hist_YC_plot", height = 520)
                            )
                     ),
                     value = 4),

            tabPanel(h5("Recruitment deviations"),
                     p("The top row of figure shows the median (with 50% and 90% confidence intervals) of historical and projected log-recruitment deviations from the stock-recruit relationship.
                       The bottom row plots the annual mean deviation (in normal space). Thus, a stationary mean around one implies average productivity defined by the stock-recruit relationship,
                       with greater or lower recruitment productivity using means greater or smaller, respectively, than one."),
                     plotOutput("plot_future_recruit", height = 540),
                     value = 5),

            #tabPanel(h5("Growth"),
            #         tabsetPanel(id = "OM_growth", selected = 1,
            #                     tabPanel(h5("Parameter Means"),
            #                              plotOutput("plot_hist_growth_I",height=540),
            #                              value = 1),
            #                     tabPanel(h5("Time-varying values"),
            #                              plotOutput("plot_hist_growth_II",height=540),
            #                              value = 2)
            #                     ),
            #         value = 5),

            tabPanel(h5("Spatial"),
                     p("Multiple-area spatial operating models can be used. A single-area (no spatial dynamics) model is effectively created with a 2-area model with all movement parameters set to 0.5."),
                     column(12,
                            column(3,
                                   sliderInput("spatial_year", textOutput("spatial_year_text"), min = 0, max = 0, value = 0, step = 1, sep = ""),
                                   sliderInput("spatial_age", "Age for matrix plot", min = 0, max = 0, value = 0, step = 1),
                                   sliderInput("spatial_quantile", "Quantile", min = 0, max = 1, value = 0.8, step = 0.01)
                            ),
                            column(9,
                                   tabsetPanel(id = "OM_spatial", selected = 1,
                                               tabPanel(h5("Matrix"),
                                                        plotOutput("plot_hist_spatial_matrix",height=540),
                                                        value = 1),

                                               tabPanel(h5("All movement"),
                                                        plotOutput("plot_hist_spatial_all", height = 540),
                                                        value = 2),

                                               tabPanel(h5("Parameters"),
                                                        plotOutput("plot_hist_spatial_par", height = 540),
                                                        value = 3)
                                   )
                            )
                     ),
                     value = 6),

            tabPanel(h5("Change projection dynamics"),
                     column(12,
                            column(3,
                                   radioButtons("change_bio", "Type", choiceNames = c("Weight at age", "Natural mortality", "Recruitment deviations"),
                                                choiceValues = c("Wt_age", "M_ageArray", "Perr_y"), inline = FALSE),
                                   conditionalPanel("input.change_bio == 'Perr_y'",
                                                    radioButtons("change_rec_dist", "Distribution", choices = c("Lognormal", "Pareto")),
                                                    conditionalPanel("input.change_rec_dist == 'Lognormal'",
                                                                     sliderInput("change_rec_sd", "Standard deviation", min = 0, max = 1.5, value = 0, step = 0.01),
                                                                     sliderInput("change_rec_AC", "Autocorrelation", min = -1, max = 1, value = 0, step = 0.01)
                                                    ),
                                                    conditionalPanel("input.change_rec_dist == 'Pareto'",
                                                                     sliderInput("change_rec_shape", "Shape parameter", min = 1, max = 4, value = 1.05, step = 0.01)
                                                    )
                                   ),
                                   conditionalPanel("input.change_bio != 'Perr_y'",
                                                    sliderInput("change_bio_slope", "Percent change year over year", min = -0.05, max = 0.05, value = 0, step = 0.001),
                                                    sliderInput("change_bio_mean", "Percent change in mean", min = -1, max = 1, value = 0, step = 0.01)
                                   )
                            ),
                            column(9,
                                   plotOutput("plot_change_bio", height = 540)
                            )
                     ),
                     actionButton("OM_change_bio", "Update operating model", icon = icon("cogs"), style = "color:red"),
                     value = 7)

) # tabsetpanel
