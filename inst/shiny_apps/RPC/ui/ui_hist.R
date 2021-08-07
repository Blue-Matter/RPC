tabsetPanel(id="HistRes1", selected=1,

            tabPanel(HTML("<h5>SSB<sub>0</sub></h5>"),
                     tabsetPanel(id="SSBhist", selected=1,
                                 tabPanel(h5("Time series"),
                                          HTML("<p>Spawning biomass relative to several definitions of unfished biomass (SSB<sub>0</sub>):
                                                                               <ul>
                                                                               <li><strong>Asymptotic SSB<sub>0</sub></strong> is the value calculated from year-specific &#120601<sub>0</sub> (unfished spawners per recruit) with constant steepness and R</sub>0</sub>.</li>
                                                                               <li><strong>Initial SSB<sub>0</sub></strong> is the asymptotic SSB<sub>0</sub> in the first year</li>
                                                                               <li><strong>Dynamic SSB<sub>0</sub></strong> is the annual value calculated from reconstructing the model with zero catches, preserving the magnitude of the recruitment deviations.</li>
                                                                               </ul>
                                                                               </p>"),
                                          plotOutput("hist_SSBref_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Probability"),
                                          HTML("<p>Plot the annual probability that the spawning biomass has exceeded a given percentage of SSB<sub>0</sub>.</p>"),
                                          column(12,
                                                 column(3,
                                                        sliderInput("SSB0_prob", HTML("SSB/SSB<sub>0</sub> threshold"), min = 0, max = 1, value = 0.4, step = 0.01),
                                                        sliderInput("SSB0_yrange", "Figure y-axis range", min = 0, max = 1, value = c(0, 1), step = 0.01)
                                                        ),

                                                 column(9,
                                                        plotOutput("hist_SSBref_prob", height = 520)
                                                 )
                                          ),
                                          value = 2),

                                 tabPanel(h5("Table"),
                                          p("Median values of SSB."),
                                          div(style = "overflow-y:scroll; height:520px",
                                              tableOutput("hist_SSBref_table")
                                              ),
                                          value = 3)
                     ),
                     value = 1),

            tabPanel(h5("Recruitment"),
                     tabsetPanel(id="Recruithist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Recruitment deviations (top row) and corresponding recruitment (bottom row), with respect to time (left column) and spawning biomass (right column)."),
                                          HTML("<p>Poor recruitment relative to a fitted stock-recruit relationship could be indicative of serious harm (<a href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_009-eng.html\">Kronlund et al. 2018</a>).
                                                                               With this technique, one searches for a biomass threshold below which negative recruitment deviations are seen more often than not (bottom row figure).</p>"),
                                          plotOutput("hist_R_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              tableOutput("hist_R_table")
                                              ),
                                          value = 2)
                     ),
                     value = 2),

            tabPanel(h5("Exploitation"),
                     tabsetPanel(id = "Exploit", selected = 1,
                                 tabPanel(h5("Spawning potential ratio (SPR)"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              p("It can be difficult to adequately define a single annual value for fishing mortality in complex operating models
                                          (i.e., multiple areas, multiple fleets with very different selectivity at age). In such cases, spawning potential ratio (SPR),
                                          the reduction in spawning output relative to unfished conditions, is more robust in accounting for all these processes."),

                                              HTML("Several SPR time series are reported here:
                                          <ul>
                                          <li><strong>Equilibrium SPR</strong> (top left) is the SPR calculated from year-specific biological parameters and fishing pressure. Equilibrium SPR = 1 implies no fishing in that year.</li>
                                          <li><strong>Dynamic SPR</strong> (bottom left) is the SPR calculated using the cumulative survival experienced by the cohorts present in that year. Whereas equilibrium SPR is instantaneous, dynamic SPR accounts
                                          for prior fishing mortality in previous years. Thus, dynamic SPR will lag equilibrium SPR (<a href=\"https://doi.org/10.1016/j.fishres.2014.12.018\">Hordyk et al. 2015</a>).</li>
                                          <li><strong>SPR<sub>crash</sub></strong> (top right) is the SPR associated with the stock-recruit &lpha; parameter (also see Stock-recruit panel). This value is constant With a fixed stock-recruit curve.
                                          SPR<sub>crash</sub> is the threshold for long-term extinction, i.e., fishing pressure in the long-term that reduces equilibrium SPR below SPR<sub>crash</sub> is expected to lead to extinction.
                                          Thus, serious harm is implied to occur at some SPR above SPR<sub>crash</sub> (<a href=\"https://www.dfo-mpo.gc.ca/csas-sccs/publications/resdocs-docrech/2002/2002_084-eng.htm\">Shelton and Rice 2002</a>). </li>
                                          <li><strong>(1 - SPR<sub>eq</sub>)/(1 - SPR<sub>crash</sub>)</strong> (bottom right) is a relative SPR metric used to compare equilibrium SPR relative to SPR<sub>crash</sub>. The relative SPR is transformed such that
                                          long-term extinction is implied when the y-axis is greater than 1.</li>
                                               </ul>"),
                                              plotOutput("hist_SPR", height = 520)
                                              ),
                                          value = 1),
                                 tabPanel(h5("Yield curve"),
                                          column(12,
                                                 column(3,
                                                        sliderInput("YC_Frange", "Fishing mortality range", min = 0, max = 3, value = c(0, 3), step = 0.01),
                                                        sliderInput("YC_y_bio", "Year for biological parameters", min = 0, max = 0, value = 0, step = 1, sep = ""),
                                                        sliderInput("YC_y_sel", "Year for selectivity", min = 0, max = 0, value = 0, step = 1, sep = ""),
                                                        radioButtons("YC_exp_type", "Fishing pressure metric",
                                                                     choiceNames = c("Instantaneous F", "Spawning potential ratio"),
                                                                     choiceValues = c("F", "SPR")),
                                                        radioButtons("YC_calc", "Calculation type",
                                                                     choiceNames = list(HTML("Fixed R<sub>0</sub>, h"), HTML("Fixed stock-recruit &alpha;, &beta;")),
                                                                     choiceValues = 1:2)
                                                 ),

                                                 column(9,
                                                        plotOutput("hist_YC_plot", height = 520)
                                                 )
                                          ),
                                          value = 2),
                                 tabPanel(h5("Probability"),
                                          p("Additional figures and tables TBD"),
                                          value = 3)
                     ),
                     value = 3),

            tabPanel(h5("Stock-recruit"),
                     div(style = "overflow-y:scroll; height:520px",
                         HTML("<p>Scatterplot of stock-recruit pairs overlaid on top of the predicted stock-recruit curve. Dashed lines indicate the recruits per spawner associated with maximum recruits per spawner (stock recruit &alpha; parameter; red), median historical recruits per spawner (black), and unfished recruits per spawner (blue).</p>"),

                         HTML("<p>The ICES guide to reference points defines a biomass limit below which a stock is considered to have reduced reproductive capacity (<a href=\"https://doi.org/10.17895/ices.advice.7891\">ICES 2021</a>).
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
                     value = 4),

            tabPanel(h5("Surplus production"),
                     tabsetPanel(id="SPhist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Total surplus production (the sum of the annual catch and change in total biomass; top row) and per capita surplus production (per unit biomass; bottom row) with respect to time (left column) and biomass (right column)."),
                                          HTML("<a href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_009-eng.html\">Kronlund et al. (2018)</a> used surplus production as a metric for identifying potential serious harm. Biomass thresholds below which there was poor surplus production, or biomass during a time when poor surplus production is observed, are obvious candidates for limit reference points."),
                                          plotOutput("hist_BvsSP_plot",height=520),
                                          value=1),
                                 tabPanel(h5("Table"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              tableOutput("hist_BvsSP_table")
                                              ),
                                          value=2)

                     ),
                     value = 4),

            tabPanel(h5("Recruits per spawner"),
                     tabsetPanel(id="RpShist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Recruits per spawner (R/S; top row) and the corresponding logarithm of R/S (bottom row). The left column shows historical values over time, while in the right column scatter plots show hisotrical values with lines indicating values predicted from the stock-recruit relationship. Total surplus production (the sum of the annual catch and change in total biomass; top row) and per capita surplus production (per unit biomass; bottom row).
                                                                            Poor R/S relative to values predicted by the stock-recruit relationship can be a criterion for establishing limit reference points."),
                                          plotOutput("hist_RpS_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              tableOutput("hist_RpS_table")
                                              ),
                                          value=2)
                     ),
                     value = 5),

            tabPanel(HTML("<h5>50% R<sub>max</sub></h5>"),
                     tabsetPanel(id="Rmaxhist", selected=1,
                                 tabPanel(h5("Diagnostic"),
                                          div(style = "overflow-y:scroll; height:520px",
                                              HTML("<p><a href=\"https://doi.org/10.1139/f94-013\">Mace (1994)</a> proposed that the SSB (SSB<sub>50%Rmax</sub>) corresponding to 50% of maximum recruitment from the stock-recruit function as a limit reference point for recruitment overfishing.
                                                                          However, this diagnostic can be indeterminate if the stock has not been observed to be below SSB<sub>50%Rmax</sub>."),
                                              HTML("<p><a href=\"https://doi.org/10.1006/jmsc.1994.1020\">Myers et al. (1994)</a> developed a diagnostic procedure to determine the appropriateness of SSB<sub>50%Rmax</sub>, and potentially any candidate LRP (see their Figure 2 reproduced below). The slope of
                                                                          a regression line of log(R) vs. log(SSB) with points above the LRP should be lower than from points below. If the slope-above is steeper than the slope-below, then the candidate LRP may not be high enough.
                                                                          On the other hand, if the slope-above and slope-below are both negative, then the candidate LRP may be overly cautious.</p>"),
                                              HTML("<p>The left figure below shows the stock-recruit relationship (lines) along with historical value scatter points (median in black points). The right shows the log(R) and log(SSB) with the regression lines above and below SSB<sub>50%Rmax</sub>.
                                                                          For both, vertical lines indicate SSB<sub>50%Rmax</sub>.</p>"),
                                              plotOutput("hist_Rmax_plot",height=520),
                                              p("Image below taken from Myers et al. (1994)."),
                                              img(src = "Myers_figure2.png")
                                              ),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_Rmax_table"),
                                          value=2)
                     ),
                     value = 6),

            tabPanel(h5("90% R/S"),
                     tabsetPanel(id="RpS90hist", selected=1,
                                 tabPanel(h5("Diagnostic"),
                                          HTML("<a href=\"https://doi.org/10.1006/jmsc.1994.1020\">Myers et al. (1994)</a> proposed an LRP to be the SSB (left figure) at the intersection of the 90th percentile of observed recruitment and 90th percentile of recruits per spawner (right figure). The idea is that at this biomass, relatively good recruitment has still been observed in light of high fishing mortality."),
                                          plotOutput("hist_RpS90_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_RpS90_table"),
                                          value=2)
                     ),
                     value = 7)

) # tabsetpanel
