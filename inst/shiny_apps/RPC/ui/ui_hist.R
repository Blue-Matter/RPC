tabsetPanel(id="HistRes1", selected=1,

            tabPanel(h5("Spawning biomass"),
                     tabsetPanel(id="SSBhist", selected=1,
                                 tabPanel(h5("Time series"),
                                          HTML("<p>Spawning biomass in absolute magnitude and relative to several definitions of unfished biomass (SSB<sub>0</sub>):
                                                                               <ul>
                                                                               <li><strong>Aysmptotic SSB<sub>0</sub></strong> is the value calculated from year-specific biological parameters (constant steepness and R</sub>0</sub>)</li>
                                                                               <li><strong>Initial SSB<sub>0</sub></strong> is the asymptotic SSB<sub>0</sub> in the first year</li>
                                                                               <li><strong>Dynamic SSB<sub>0</sub></strong> is the annual value calculated from reconstructing the model with zero catches, preserving the magnitude of the recruitment deviations.</li>
                                                                               </ul>
                                                                               Annual probabilities that the SSB has exceeded below 10% and 40% of these quantities are also shown.</p>"),
                                          plotOutput("hist_SSBref_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          p("Median values of SSB."),
                                          tableOutput("hist_SSBref_table"),
                                          value=2)
                     ),
                     value=1),

            tabPanel(h5("Recruitment"),
                     tabsetPanel(id="Recruithist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Recruitment (top row) and recruitment deviations (bottom row), with respect to time (left column) and spawning biomass (right column). In top right, scatter plots show historical values and lines show values predicted from stock-recruit relationship.
                                                                            In bottom right, lines show median trajectory with a scatterplot of values from all simulations."),
                                          p("Two potential methods can be used to determine a limit reference point (LRP) here."),
                                          HTML("<p>First, the ICES guide to reference points defines a biomass limit below which a stock is considered to have reduced reproductive capacity (<a href=\"https://doi.org/10.17895/ices.advice.7891\">ICES 2021</a>).
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
                                          HTML("<p>Second, poor recruitment relative to a fitted stock-recruit relationship could be indicative of serious harm (<a href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_009-eng.html\">Kronlund et al. 2018</a>).
                                                                               With this technique, one searches for a biomass threshold below which negative recruitment deviations are seen more often than not (bottom row figure).</p>"),
                                          plotOutput("hist_R_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_R_table"),
                                          value=2)
                     ),
                     value=2),

            tabPanel(h5("Surplus production"),
                     tabsetPanel(id="SPhist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Total surplus production (the sum of the annual catch and change in total biomass; top row) and per capita surplus production (per unit biomass; bottom row) with respect to time (left column) and biomass (right column)."),
                                          HTML("<a href=\"https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_009-eng.html\">Kronlund et al. (2018)</a> used surplus production as a metric for identifying potential serious harm. Biomass thresholds below which there was poor surplus production, or biomass during a time when poor surplus production is observed, are obvious candidates for limit reference points."),
                                          plotOutput("hist_BvsSP_plot",height=520),
                                          value=1),
                                 tabPanel(h5("Table"),
                                          tableOutput("hist_BvsSP_table"),
                                          value=2)

                     ),
                     value=3),

            tabPanel(h5("Recruits per spawner"),
                     tabsetPanel(id="RpShist", selected=1,
                                 tabPanel(h5("Time series"),
                                          p("Recruits per spawner (R/S; top row) and the corresponding logarithm of R/S (bottom row). The left column shows historical values over time, while in the right column scatter plots show hisotrical values with lines indicating values predicted from the stock-recruit relationship. Total surplus production (the sum of the annual catch and change in total biomass; top row) and per capita surplus production (per unit biomass; bottom row).
                                                                            Poor R/S relative to values predicted by the stock-recruit relationship can be a criterion for establishing limit reference points."),
                                          plotOutput("hist_RpS_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_RpS_table"),
                                          value=2)
                     ),
                     value=4),

            tabPanel(HTML("<h5>50% R<sub>max</sub></h5>"),
                     tabsetPanel(id="Rmaxhist", selected=1,
                                 tabPanel(h5("Diagnostic"),
                                          HTML("<p><a href=\"https://doi.org/10.1139/f94-013\">Mace (1994)</a> proposed that the SSB (SSB<sub>50%Rmax</sub>) corresponding to 50% of maximum recruitment from the stock-recruit function as a limit reference point for recruitment overfishing.
                                                                          However, this diagnostic can be indeterminate if the stock has not been observed to be below SSB<sub>50%Rmax</sub>."),
                                          HTML("<p><a href=\"https://doi.org/10.1006/jmsc.1994.1020\">Myers et al. (1994)</a> developed a diagnostic procedure to determine the appropriateness of SSB<sub>50%Rmax</sub>, and potentially any candidate LRP (see their Figure 2 reproduced below). The slope of
                                                                          a regression line of log(R) vs. log(SSB) with points above the LRP should be lower than from points below. If the slope-above is steeper than the slope-below, then the candidate LRP may not be high enough.
                                                                          On the other hand, if the slope-above and slope-below are both negative, then the candidate LRP may be overly cautious.</p>"),
                                          HTML("<p>The left figure below shows the stock-recruit relationship (lines) along with historical value scatter points (median in black points). The right shows the log(R) and log(SSB) with the regression lines above and below SSB<sub>50%Rmax</sub>.
                                                                          For both, vertical lines indicate SSB<sub>50%Rmax</sub>.</p>"),
                                          plotOutput("hist_Rmax_plot",height=520),
                                          p("Image below taken from Myers et al. (1994)."),
                                          img(src = "Myers_figure2.png"),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_Rmax_table"),
                                          value=2)
                     ),
                     value=5),

            tabPanel(h5("90% R/S"),
                     tabsetPanel(id="RpS90hist", selected=1,
                                 tabPanel(h5("Diagnostic"),
                                          HTML("Myers et al. (1994) proposed an LRP to be the SSB (left figure) at the intersection of the 90th percentile of observed recruitment and 90th percentile of recruits per spawner (right figure). The idea is that at this biomass, relatively good recruitment has still been observed in light of high fishing mortality."),
                                          plotOutput("hist_RpS90_plot",height=520),
                                          value=1),

                                 tabPanel(h5("Table"),
                                          tableOutput("hist_RpS90_table"),
                                          value=2)
                     ),
                     value=6)

) # tabsetpanel
