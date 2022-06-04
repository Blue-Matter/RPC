tabsetPanel(id="HistRes1", selected=1,

            tabPanel(h5("Overview"),
                     tabsetPanel(id = "Overview", selected = 1,
                                 tabPanel(h5("Table"),
                                          tableOutput("OM_name"),
                                          p("Step 2 provides summary figures describing the historical reconstruction of the stock in the operating model.
                                          Various methods for identifying reference points for management are also discussed. Values of reference points
                                          are usually related to some diagnostic, such as the yield curve, or some historical reference period,
                                          such as the lowest observed biomass. Along with annual values of abundance, biomass, fishing mortality,
                                          and recruitment, annual probabilities that the stock has exceeded a threshold, specified by the user,
                                          in the past are calculated."),
                                          p("These diagnostics can be used to provide justification for identifying a limit reference point (LRP)
                                            under the Fish Stocks Provisions, as well as a target reference point (TRP), upper stock reference (USR),
                                            and a reference removal (RR) rate. Not all methods are necessarily useful for setting reference points,
                                            for example, if the assumptions with a given method are not met."),
                                          p("Additional figures describing the specification of the operating model are available under the \"Detailed Operating Model Info\" tab,
                                          with some options for adjusting the dynamics of the projections for Steps 3 and 4."),
                                          HTML("<p>Further discussion of many of these reference points in the DFO context is in
                                               <a target = \"_blank\" href=\"https://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2002/2002_084-eng.htm\">Shelton and Rice 2002</a>.</p>")
                                          value = 1),
                                 tabPanel(h5("Figure"),
                                          plotOutput("plot_hist_bio", height = 540),
                                          value = 2)
                                 ),
                     value = 1),

            tabPanel(h5("Spawning biomass"),
                     tabsetPanel(id="SSB", selected=1,

                                 tabPanel(h5("Historical"),
                                          HTML("<p>Historical spawning biomass from a certain year may be used as a limit reference point if it reflects an undesirable low biomass state that should be avoided
                                                                    (e.g., <a target = \"_blank\" href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_070-eng.html\">Forrest et al. 2020</a>). <strong>B<sub>recover</sub></strong> is typically the lowest historical biomass at which the stock has seen a recovery. Also see text in the <strong>Stock-recruit</strong> and <strong>Surplus production</strong> panels for methods of identifying historical biomass levels using stock-recruit values or surplus production, respectively.</p>"),
                                          column(12,
                                                 column(3,
                                                        sliderInput("SSB_y", "Year for Historical Biomass", min = 0, max = 0, value = 0, step = 1, sep = "")
                                                 ),
                                                 column(9,
                                                        tabsetPanel(id = "SSBhist", selected = 1,
                                                                    tabPanel(h5("Time series"),
                                                                             plotOutput("hist_SSB_plot",height=520),
                                                                             value = 1),
                                                                    tabPanel(h5("Table"),
                                                                             p("Median and interquartile range of spawning biomass relative to that in historical year."),
                                                                             div(style = "overflow-y:scroll; height:520px",
                                                                                 tableOutput("hist_SSB_table")
                                                                             ),
                                                                             value = 2)
                                                        )
                                                 )
                                          ),
                                          value = 1),
                                 tabPanel(HTML("<h5>Equilibrium SSB<sub>0</sub></h5>"),
                                          HTML("<p>Spawning biomass relative to several definitions of unfished biomass (SSB<sub>0</sub>). <strong>Equilibrium SSB<sub>0</sub></strong> is the value calculated from year-specific &#120601<sub>0</sub> (unfished spawners per recruit) with stock recruit parameters &alpha; and &beta;. This is the expected SSB with no fishing if the year-specific biological parameters persist.</p>"),
                                          HTML("<p>Twenty to thirty percent (20-30%) SSB<sub>0</sub> has been suggested as a limit reference point that would avoid recruitment overfishing, with higher thresholds needed for lower productivity stocks (Beddington and Cooke 1983, as cited in <a target = \"_blank\" href=\"https://doi.org/10.1139/f94-013\">Mace (1994)</a>; <a target = \"_blank\" href=\"Sainsbury2008_refPts.pdf\">Sainsbury 2008</a>). These
                                                                    recommendations have ostensibly been developed in the absence of time-varying parameters, which would correspond to the asymptotic SSB<sub>0</sub> presented here.</p>"),
                                          HTML("<p>Altnerative stock-recruit parameters can be explored under the <strong>Detailed operating model</strong> tab.</p>"),
                                          plotOutput("hist_SSB0_eq_plot",height=520),
                                          value = 2),

                                 tabPanel(HTML("<h5>Initial SSB<sub>0</sub></h5>"),
                                          HTML("<p>Spawning biomass relative to several definitions of unfished biomass (SSB<sub>0</sub>). <strong>Initial SSB<sub>0</sub></strong> is the equilibrium SSB<sub>0</sub> in the first year of the operating model. Unfished biomass is based on initial conditions at the start of the operating model.</p>"),

                                          plotOutput("hist_SSB0_init_plot",height=520),
                                          value = 3),

                                 tabPanel(HTML("<h5>Dynamic SSB<sub>0</sub></h5>"),
                                          HTML("<p>Spawning biomass relative to several definitions of unfished biomass (SSB<sub>0</sub>). <strong>Dynamic SSB<sub>0</sub></strong> is the annual value calculated from reconstructing the model with zero catches and preserving the magnitude of the recruitment deviations (assuming they are independent of F).
                                               Thus, the time series of dynamic SSB<sub>0</sub> is intended to characterize the natural temporal variability of the population in the absence of fishing (<a target = \"_blank\" href=\"https://swfsc-publications.fisheries.noaa.gov/publications/CR/1985/8558.PDF\">MacCall et al. 1985</a>. SSB/Dynamic SSB<sub>0</sub> describes the impacts of fishing on the population (<a target = \"_blank\" href=\"https://doi.org/10.1016/j.fishres.2009.05.010\">Wang et al. 2009</a>).</p>"),
                                          plotOutput("hist_SSB0_dyn_plot",height=520),
                                          value = 4),

                                 tabPanel(HTML("<h5>SSB<sub>MSY</sub></h5>"),
                                          HTML("<p>Time series of SSB and SSB<sub>MSY</sub>. When there are time-varying parameters (biological parameters and/or selectivity),
                                               annual SSB<sub>MSY</sub> is calculated from constant stock-recruit &alpha; and &beta; with annual unfished spawners per recruit. Ensure that there is an optimum in the yield curve under the <strong>Detailed Operating Model Info</strong> tab. MSY may not be appropriate with high productivity (e.g., high M or high steepness) or high selectivity relative to maturity, which results in very high values of FMSY or the lack of an optimum in the yield curve.</p>"),
                                          HTML("<p>Use of MSY-based reference points implies the social choice of optimal yield as a management objective (<a target = \"_blank\" href=\"Sainsbury2008_refPts.pdf\">Sainsbury 2008</a>). Typically,
                                                                    the limit reference point is set below SSB<sub>MSY</sub>, e.g., 0.5 SSB<sub>MSY</sub> or (1-M) SSB<sub>MSY</sub> in recognition that the population will naturally fluctuate above and below
                                                                    SSB<sub>MSY</sub> when fishing at F<sub>MSY</sub> (<a target = \"_blank\" href=\"https://doi.org/10.1006/jmsc.1999.0546\">Restrepo and Powers 1999</a>).</p>"),
                                          HTML("<p>Altnerative stock-recruit parameters can be explored under the <strong>Detailed operating model</strong> tab.</p>"),
                                          plotOutput("hist_SSBMSY_plot", height = 520),
                                          value = 5),

                                 tabPanel(h5("Probability"),
                                          column(12,
                                                 column(3,
                                                        radioButtons("SSB_prob_type", "SSB type",
                                                                     choiceNames = list("Historical SSB", HTML("SSB<sub>0</sub> (equilibrium)"),
                                                                                        HTML("SSB<sub>0</sub> (initial)"), HTML("SSB<sub>0</sub> (dynamic)"),
                                                                                        HTML("SSB<sub>MSY</sub>")),
                                                                     choiceValues = c("hist", "equilibrium", "initial", "dynamic", "MSY")),
                                                        sliderInput("SSB_prob", uiOutput("SSB_threshold_label"), min = 0, max = 2, value = 1, step = 0.01),
                                                        sliderInput("SSB_yrange", "Figure y-axis range", min = 0, max = 1, value = c(0, 1), step = 0.01)
                                                 ),
                                                 column(9,
                                                        tabsetPanel(id = "SSB_prob_output", selected = 1,
                                                                    tabPanel(h5("Figure"),
                                                                             plotOutput("hist_SSB_prob", height = 520),
                                                                             value = 1),
                                                                    tabPanel(h5("Table"),
                                                                             div(style = "overflow-y:scroll; height:520px",
                                                                                 uiOutput("hist_SSB_prob_table_label"),
                                                                                 tableOutput("hist_SSB_prob_table")
                                                                             ),
                                                                             value = 2)
                                                        )
                                                 )
                                          ),
                                          value = 6)
                     ),
                     value = 2),

            tabPanel(h5("Exploitation"),
                     tabsetPanel(id = "Exploit", selected = 1,
                                 tabPanel(HTML("<h5>F<sub>MSY</sub></h5>"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              HTML("<p>Time series of catches (landings and discards, or total removals only if no discards are modeled) with instantaneous fishing mortality (F) and F<sub>MSY</sub>. When there are time-varying parameters (biological and selectivity parameters),
                                               annual F<sub>MSY</sub> is calculated from constant stock-recruit parameters &alpha; and &beta; with annual unfished spawners per recruit (&#120601<sub>0</sub>). F<sub>MSY</sub> near zero implies that fishing has little impact on the the stock, which could crash in the absence of fishing
                                                   (&#120601<sub>0</sub> is very low). On the other hand, large values of F<sub>MSY</sub> may imply that there is no optimum in the yield curve. MSY may not be a suitable concept for stocks at these extremes. Double-check the yield curve under the <strong>Detailed Operating Model Info</strong> tab."),
                                              HTML("<p>Use of MSY-based reference points implies the social choice of optimal yield as a management objective (<a target = \"_blank\" href=\"Sainsbury2008_refPts.pdf\">Sainsbury 2008</a>). Typically,
                                                    a conservative limit reference point for fishing mortality should not exceed F<sub>MSY</sub> (<a target = \"_blank\" href=\"https://doi.org/10.1006/jmsc.1999.0546\">Restrepo and Powers 1999</a>).</p>"),
                                              HTML("<p>Altnerative stock-recruit parameters can be explored under the <strong>Detailed operating model</strong> tab.</p>"),
                                              tabsetPanel(id = "exp_hist", selected = 1,
                                                          tabPanel(h5("Figure"),
                                                                   plotOutput("hist_exp",height=540),
                                                                   value = 1),
                                                          tabPanel(h5("Table"),
                                                                   tableOutput("hist_exp2"),
                                                                   value = 2)
                                              ),
                                          ),
                                          value = 1),

                                 tabPanel(HTML("<h5>F<sub>med</sub></h5>"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              HTML("<p><a target = \"_blank\" href=\"https://doi.org/10.1139/f87-110\">Sissenwine and Shepherd (1987)</a> introduced the concept of average replacement when the stock-recruit relationship is indeterminate, for example, the stock-recruit pairs imply a linear relationship with no compensation in recruitment.
                                              Low fishing mortality generates high recruits per spawner (R/S) which tends to increase the population (high replacement), while high F generates low R/S and decrease the stock (low replacement). The average replacement then should sustain the population to the historical average."),
                                              HTML("<p>Here, the fishing mortality corresponding to this replacement line is F<sub>med</sub>, which produces the recruits per spawner (R/S) corresponding to the median historical value. When there are time-varying parameters (biological and selectivity parameters), annual F<sub>med</sub> is calculated from the median historical R/S and annual unfished spawners per recruit."),
                                              HTML("<p>See also the <strong>Stock-recruit</strong> panel that plots the median historical R/S.</p>"),
                                              HTML("The utility of F<sub>med</sub> is dependent on the fishing history. If the fishing mortality has been high, then the lack of recruitment compensation could imply that the stock is near the origin of the stock-recruit relationship. Thus, F<sub>med</sub> could be a proxy for F<sub>crash</sub> (<a target = \"_blank\" href=\"Mace_Sissenwine_1993_HowMuchSPR.pdf\">Mace and Sissenwine 1993</a>), in which case a limit reference point should be at some value less than F<sub>med</sub>.
                                                   Otherwise, F<sub>med</sub> should be more conservative than F<sub>crash</sub>. If the stock has been near MSY, then F<sub>med</sub> could be reflective of F<sub>MSY</sub> (<a target = \"_blank\" href=\"https://doi.org/10.1006/jmsc.1999.0546\">Restrepo and Powers 1999</a>)."),
                                              HTML("The year-specific value of F<sub>med</sub> is calculated from the corresponding year-specific values of selectivity and biological parameters."),
                                              tabsetPanel(id = "exp_Fmed", selected = 1,
                                                          tabPanel(h5("Figure"),
                                                                   plotOutput("hist_Fmed",height=540),
                                                                   value = 1),
                                                          tabPanel(h5("Table"),
                                                                   tableOutput("hist_Fmed2"),
                                                                   value = 2)
                                              ),
                                          ),
                                          value = 2),

                                 tabPanel(h5("Spawning potential ratio (SPR)"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              p("It can be difficult to adequately define a single annual value for fishing mortality in complex operating models
                                          (i.e., multiple areas, multiple fleets with very different selectivity at age). In such cases, spawning potential ratio (SPR),
                                          the reduction in spawning output relative to unfished conditions, is more robust in accounting for all these processes."),
                                              HTML("<p>SPR is also frequently used when it is desirable to avoid MSY-based reference point, for example, when there is high uncertainty about the stock-recruit relationship.
                                                Typically, SPR reference points have been established between 0.3 - 0.6, with higher values used for lower productivity stocks (<a target = \"_blank\" href=\"https://doi.org/10.1006/jmsc.1999.0546\">Restrepo and Powers 1999</a>; <a target = \"_blank\" href=\"Sainsbury2008_refPts.pdf\">Sainsbury 2008</a>).</p>"),
                                              HTML("Several SPR time series are reported here:
                                          <ul>
                                          <li><strong>Equilibrium SPR</strong> (top left) is the SPR calculated from year-specific biological parameters and fishing pressure. Equilibrium SPR = 1 implies no fishing in that year.</li>
                                          <li><strong>Dynamic SPR</strong> (bottom left) is the SPR calculated using the cumulative survival experienced by the cohorts present in that year. Whereas equilibrium SPR is instantaneous, dynamic SPR accounts
                                          for prior fishing mortality in previous years. Thus, dynamic SPR will lag equilibrium SPR (<a target = \"_blank\" href=\"https://doi.org/10.1016/j.fishres.2014.12.018\">Hordyk et al. 2015</a>).</li>
                                          <li><strong>SPR<sub>crash</sub></strong> (top right) is the SPR associated with the stock-recruit &alpha; parameter (also see Stock-recruit panel). This value is constant With a fixed stock-recruit curve.
                                          SPR<sub>crash</sub> is the threshold for long-term extinction, i.e., fishing pressure in the long-term that reduces equilibrium SPR below SPR<sub>crash</sub> is expected to lead to extinction.
                                          Thus, serious harm is implied to occur at some SPR above SPR<sub>crash</sub> (<a target = \"_blank\" href=\"https://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2002/2002_084-eng.htm\">Shelton and Rice 2002</a>). </li>
                                          <li><strong>(1 - SPR<sub>eq</sub>)/(1 - SPR<sub>crash</sub>)</strong> (bottom right) is a relative SPR metric used to compare equilibrium SPR relative to SPR<sub>crash</sub>. The relative SPR is transformed such that
                                          long-term extinction is implied when the y-axis is greater than 1.</li>
                                               </ul>"),
                                              tabsetPanel(id = "SPR_hist", selected = 1,
                                                          tabPanel(h5("Figure"),
                                                                   plotOutput("hist_SPR", height = 520),
                                                                   value = 1),
                                                          tabPanel(h5("Table"),
                                                                   tableOutput("hist_SPR2"),
                                                                   value = 2)
                                              ),
                                          ),
                                          value = 3),
                                 tabPanel(h5("Probability"),
                                          conditionalPanel("input.exp_type == 'FMSY'",
                                                           HTML("<p>Report the annual probability that the fishing mortality does not exceed some percentage of F<sub>MSY</sub>.</p>")
                                          ),
                                          conditionalPanel("input.exp_type == 'Fmed'",
                                                           HTML("<p>Report the annual probability that the fishing mortality does not exceed some percentage of F<sub>med</sub>.</p>")
                                          ),
                                          conditionalPanel("input.exp_type == 'SPR'",
                                                           p("Report the annual probability that the SPR exceeds some proportion.")
                                          ),
                                          column(12,
                                                 column(3,
                                                        radioButtons("exp_type", "Exploitation type", choiceNames = list(HTML("F<sub>MSY</sub>"), HTML("F<sub>med</sub>"), "Equilibrium SPR"),
                                                                     choiceValues = c("FMSY", "Fmed", "SPR")),
                                                        conditionalPanel("input.exp_type == 'F'",
                                                                         sliderInput("FMSY_prob", HTML("F/F<sub>MSY</sub> threshold"), min = 0, max = 1.25, value = 1, step = 0.01)
                                                        ),
                                                        conditionalPanel("input.exp_type == 'F'",
                                                                         sliderInput("Fmed_prob", HTML("F/F<sub>med</sub> threshold"), min = 0, max = 2, value = 1, step = 0.01)
                                                        ),
                                                        conditionalPanel("input.exp_type == 'SPR'",
                                                                         sliderInput("SPR_prob", "SPR threshold", min = 0, max = 1, value = 0.4, step = 0.01)
                                                        ),
                                                        sliderInput("exp_yrange", "Figure y-axis range", min = 0, max = 1, value = c(0, 1), step = 0.01)
                                                        ),
                                                 column(9,
                                                        tabsetPanel(id = "exp_histprob", selected = 1,
                                                                    tabPanel(h5("Figure"),
                                                                             plotOutput("hist_exp_prob", height = 520),
                                                                             value = 1),
                                                                    tabPanel(h5("Table"),
                                                                             div(style = "overflow-y:scroll; height:520px",
                                                                                 textOutput("hist_exp_table_label"),
                                                                                 tableOutput("hist_exp_table")
                                                                             ),
                                                                             value = 2)
                                                        )
                                                 )
                                          ),
                                          value = 4)
                     ),
                     value = 3),

            tabPanel(h5("Recruitment"),
                     tabsetPanel(id="Recruithist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Recruitment deviations (top row) and corresponding recruitment (bottom row), with respect to time (left column) and spawning biomass (right column)."),
                                          HTML("<p>Poor recruitment relative to a fitted stock-recruit relationship could be indicative of serious harm (<a target = \"_blank\" href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_009-eng.html\">Kronlund et al. 2018</a>).
                                                                               With this technique, one searches for a biomass threshold below which negative recruitment deviations are seen more often than not (bottom row figure).</p>"),
                                          plotOutput("hist_R_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              tableOutput("hist_R_table")
                                          ),
                                          value = 2)
                     ),
                     value = 4),

            tabPanel(h5("Stock-recruit"),
                     div(style = "overflow-y:scroll; height:520px",
                         HTML("<p>Scatterplot of stock-recruit pairs overlaid on top of the predicted stock-recruit curve. Dashed lines indicate the recruits per spawner associated with maximum recruits per spawner (stock recruit &alpha; parameter; red), median historical recruits per spawner (black), and unfished recruits per spawner (blue).</p>"),

                         HTML("<p>The ICES guide to reference points defines a biomass limit below which a stock is considered to have reduced reproductive capacity (<a target = \"_blank\" href=\"https://doi.org/10.17895/ices.advice.7891\">ICES 2021</a>).
                                                                          Often, this is determined using the stock-recruit plot (top right figure). The LRP is chosen depending on the observed pattern in stock-recruit points:
                                                                          <ul>
                                                                          <li><strong>Type 1:</strong> Spasmodic stocks with occasional large year classes. The LRP can be the lowest observed SSB that has generated a spasmodic recruitment event, unless F has been low, in which case, lowest observed SSB (regardless of recruitment) is a potential candidate</li>
                                                                          <li><strong>Type 2:</strong> Wide range in SSB with evidence of impaired recruitment at low SSB. The LRP can be change point from a segmented regression of a hockey-stick stock-recruit curve.</li>
                                                                          <li><strong>Type 3:</strong> Same as Type 2, but no clear asymptote in recruitment at high SSB. If F has been high, the LRP could be highest SSB observed</li>
                                                                          <li><strong>Type 4:</strong> Wide range in SSB with increased recruitment at low SSB. No clear LRP can be discerned using this method</li>
                                                                          <li><strong>Type 5:</strong> Wide range in SSB no clear S-R relationship. The LRP could be lowest observed SSB</li>
                                                                          <li><strong>Type 6:</strong> Narrow range in SSB with no evidence of impaired recruitment. No clear guidance on the LRP here, potentially the lowest observed SSB</li>
                                                                          </ul>
                                                                          </p>"),

                         column(12,
                                column(3,
                                       checkboxGroupInput("SR_plot_options", "Plot options", selected = 1:3,
                                                          choiceNames = c("Scatter plot", "Stock-recruit curve", "Recruits per spawner (R/S)"),
                                                          choiceValues = 1:3),
                                       sliderInput("SR_yrange", "Recruitment range", min = 0, max = 0, value = c(0, 0), step = 0.1),
                                       sliderInput("SR_xrange", "Spawning biomass range", min = 0, max = 0, value = c(0, 0), step = 0.1),
                                       sliderInput("SR_y_RPS0", "Year of unfished R/S", min = 0, max = 0, value = 0, step = 1, sep = "")
                                ),

                                column(9,
                                       plotOutput("hist_SR_plot", height = 520)
                                )
                         )
                     ),
                     value = 5),

            tabPanel(h5("Surplus production"),
                     HTML("<p>Surplus production is the predicted annual change in stock biomass in the absence of fishing, calculated as the sum of the annual catch and change in biomass from one year to the next (<a target = \"_blank\" href=\"https://doi.org/10.1139/f01-018\">Hilborn 2001</a>, <a target = \"_blank\" href=\"https://doi.org/10.1139/F08-170\">Walters et al. 2008</a>). In equilibrium, the predicted surplus production function is the yield curve.</p>"),
                     HTML("<p><a target = \"_blank\" href=\"https://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2004/2004_039-eng.htm\">Mohn and Chouinard (2004)</a> and <a target = \"_blank\" href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_009-eng.html\">Kronlund et al. (2018)</a> used surplus production as a metric for identifying potential serious harm. Biomass thresholds below which there was poor surplus production, or biomass during a time when poor surplus production is observed, are obvious candidates for limit reference points. These shifts
                                               could be evidence of depensation at low stock size. This method may always be pertinent, for example, high surplus production can be observed at lowest stock sizes.</p>"),
                     HTML("<p>By default, total stock biomass is used as the unit of stock abundance, with an option to use vulnerable biomass instead. Spawning biomass is another alternative and may be suitable if the fishery selects only mature animals.</p>"),

                     column(12,
                            column(3,
                                   radioButtons("hist_SP_Bunit", label = "Stock units", choiceNames = c("Total biomass", "Vulnerable biomass", "Spawning biomass"),
                                                choiceValues = c("B", "VB", "SSB"))
                                   ),
                            column(9,
                                   tabsetPanel(id="SPhist", selected=1,
                                               tabPanel(h5("Time series"),
                                                        plotOutput("hist_SP_plot",height=520),
                                                        value=1),
                                               tabPanel(h5("Phase plot"),
                                                        plotOutput("hist_SP_plot_phase",height=520),
                                                        value=2),
                                               tabPanel(h5("Table"),
                                                        div(style = "overflow-y:scroll; height:520px",
                                                            tableOutput("hist_SP_table")
                                                        ),
                                                        value=3)
                                   )
                            )
                     ),
                     value = 6),

            tabPanel(h5("Recruits per spawner"),
                     tabsetPanel(id="RpShist", selected=1,
                                 tabPanel(h5("Time series"),
                                          HTML("<p>Recruits per spawner (R/S; top row) and the corresponding logarithm of R/S (bottom row). The left column shows historical values over time, while in the right column scatter plots show historical values with lines indicating values predicted from the stock-recruit relationship. Total surplus production (the sum of the annual catch and change in total biomass; top row) and per capita surplus production (per unit biomass; bottom row).
                                                                            Poor R/S relative to values predicted by the stock-recruit relationship can be a criterion for establishing limit reference points. Reduction in R/S at low stock sizes would be indicative of depensation (see Figure 3 of <a target = \"_blank\" href=\"https://doi.org/10.1126/science.269.5227.1106\">Myers et al. (1995)</a></p>"),
                                          plotOutput("hist_RPS_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              tableOutput("hist_RPS_table")
                                              ),
                                          value=2)
                     ),
                     value = 7),

            tabPanel(HTML("<h5>50% R<sub>max</sub></h5>"),
                     tabsetPanel(id="Rmaxhist", selected=1,
                                 tabPanel(h5("Time Series"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              HTML("<p><a target = \"_blank\" href=\"https://doi.org/10.1139/f94-013\">Mace (1994)</a> proposed that the SSB (SSB<sub>50%Rmax</sub>) corresponding to 50% of maximum recruitment from the stock-recruit function as a limit reference point for recruitment overfishing.
                                                                          However, this diagnostic can be indeterminate if the stock has not been observed to be below SSB<sub>50%Rmax</sub>."),
                                              HTML("<p><a target = \"_blank\" href=\"https://doi.org/10.1006/jmsc.1994.1020\">Myers et al. (1994)</a> developed a diagnostic procedure to determine the appropriateness of SSB<sub>50%Rmax</sub>, and potentially any candidate LRP (see their Figure 2 reproduced below). The slope of
                                                                          a regression line of log(R) vs. log(SSB) with points above the LRP should be lower than from points below. If the slope-above is steeper than the slope-below, then the candidate LRP may not be high enough.
                                                                          On the other hand, if the slope-above and slope-below are both negative, then the candidate LRP may be overly cautious.</p>"),
                                              HTML("<p>The left figure below shows the stock-recruit relationship (lines) along with historical value scatter points (median in black points). The vertical lines indicate SSB<sub>50%Rmax</sub>.</p>"),
                                              HTML("<p>Altnerative stock-recruit parameters can be explored under the <strong>Detailed operating model</strong> tab.</p>"),

                                              plotOutput("hist_Rmax_plot",height=520),
                                              p("Image below taken from Myers et al. (1994)."),
                                              img(src = "Myers_figure2.png")
                                              ),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_Rmax_table"),
                                          value=2),

                                 tabPanel(h5("Probability"),
                                          column(12,
                                                 column(3,
                                                        sliderInput("Rmax_prob", HTML("SSB<sub>50% Rmax</sub> threshold"), min = 0, max = 1, value = 1, step = 0.01),
                                                        sliderInput("Rmax_yrange", "Figure y-axis range", min = 0, max = 1, value = c(0, 1), step = 0.01)
                                                 ),
                                                 column(9,
                                                        tabsetPanel(id = "Rmax_prob_output", selected = 1,
                                                                    tabPanel(h5("Figure"),
                                                                             plotOutput("hist_Rmax_prob", height = 520),
                                                                             value = 1),
                                                                    tabPanel(h5("Table"),
                                                                             div(style = "overflow-y:scroll; height:520px",
                                                                                 uiOutput("hist_Rmax_prob_table_label"),
                                                                                 tableOutput("hist_Rmax_prob_table")
                                                                             ),
                                                                             value = 2)
                                                        )
                                                 )
                                          ),
                                          value = 3)
                     ),
                     value = 8),

            tabPanel(h5("90% R/S"),
                     tabsetPanel(id="RpS90hist", selected=1,
                                 tabPanel(h5("Time Series"),
                                          HTML("<a target = \"_blank\" href=\"https://archive.nafo.int/open/studies/s16/serebrya.pdf\">Serebryakov (1991)</a> proposed an LRP to be the SSB (left figure) at the intersection of the 90th percentile of observed recruitment and 90th percentile of recruits per spawner (right figure). The idea is that at this biomass, SSB<sub>90%ile R/S</sub> relatively good recruitment has still been observed in light of high fishing mortality. Median values of the 90th percentile R/S, R, and SSB are in dark dashed lines, while values from individual simulations are shown in light, transparent colors.
                                               The stock-recruit relationship is plotted here for reference but is not considered in this LRP. See <a target = \"_blank\" href=\"https://doi.org/10.1006/jmsc.1994.1020\">Myers et al. (1994)</a> for more discussion."),
                                          plotOutput("hist_RpS90_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_RpS90_table"),
                                          value=2),

                                 tabPanel(h5("Probability"),
                                          column(12,
                                                 column(3,
                                                        sliderInput("RpS90_prob", HTML("SSB<sub>50% Rmax</sub> threshold"), min = 0, max = 1, value = 1, step = 0.01),
                                                        sliderInput("RpS90_yrange", "Figure y-axis range", min = 0, max = 1, value = c(0, 1), step = 0.01)
                                                 ),
                                                 column(9,
                                                        tabsetPanel(id = "RpS90_prob_output", selected = 1,
                                                                    tabPanel(h5("Figure"),
                                                                             plotOutput("hist_RpS90_prob", height = 520),
                                                                             value = 1),
                                                                    tabPanel(h5("Table"),
                                                                             div(style = "overflow-y:scroll; height:520px",
                                                                                 uiOutput("hist_RpS90_prob_table_label"),
                                                                                 tableOutput("hist_RpS90_prob_table")
                                                                             ),
                                                                             value = 2)
                                                        )
                                                 )
                                          ),
                                          value = 3)
                     ),
                     value = 9)

) # tabsetpanel
