

Fishery_UI <- function(id, label="Fishery"){

  ns <- NS(id)
  tagList(

    tabsetPanel(id=ns("OM_input"), selected=1,

      tabPanel(h5("Select"),
         h5("Select and example operating model",style = "color:black"),
         tipify(
           selectInput(ns("SelectOMDD"),choices=OMs,
                     label=NULL, selected=OMs[1]),
           title="tipfy test"
         ),

         actionButton(ns("SelectOM"),label = "Select"),

         value=1),

      tabPanel( h5("Load"),
         h5("Load an operating model from file",style = "color:black"),
         tipify(fileInput(ns("Load_OM"),label=NULL),title="tipfy test)"),
         value=2),

      tabPanel( h5("Sketch"),
         h5("Construct a fully specified operating model"),
         fluidRow(

           column(12,style="padding-top:30px;padding-left:50px;padding-right:50px;padding-bottom:30px;",
                  fluidRow(

                    column(width = 4, style="height:360px",
                           tabsetPanel( id = ns("tabs1"),selected=1,



                                        tabPanel(h4(ns("Fishery"),style = "color:black"),

                                                 conditionalPanel(width=4,condition="output.Fpanel==undefined|output.Fpanel==0",

                                                                  HTML("<br>"),
                                                                  h5("The Fishery panel contains a set of questions about the characteristics of the fish population and its fishery.",style="color:grey"),
                                                                  h5("These questions specify: ",style="color:grey"),
                                                                  h5(" - productivity and resilience of the population",style="color:grey"),
                                                                  h5(" - historical characteristics of its fishery",style="color:grey"),
                                                                  h5(" - vulnerability to fishing of various size classes",style="color:grey"),
                                                                  h5(""),
                                                                  h5("More detailed help on the Fishery questions can be found in the
                                         MERA User Guide: ", a("Section 2.1.", href="https://dlmtool.github.io/DLMtool/MERA/MERA_User_Guide_v6.html#21_fishery_questions", target="_blank"),style="color:grey")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==1",#|output.Fpanel==undefined",
                                                                  fluidRow(
                                                                    column(width=12,h5("1. Fishery description")),
                                                                    column(width=4,style="height:40px;padding:19px",
                                                                           h5("Name:",style="font-weight:bold")),
                                                                    column(width=8,style="height:40px",
                                                                           textInput(ns("Name"), "", "e.g. Atlantic swordfish")),
                                                                    column(width=4,style="height:40px;padding:19px",
                                                                           h5("Species:",style="font-weight:bold")),
                                                                    column(width=8,style="height:40px",
                                                                           textInput(ns("Species"),"","e.g. Xiphias gladius")),
                                                                    column(width=4,style="height:40px;padding:19px",
                                                                           h5("Location:",style="font-weight:bold")),
                                                                    column(width=8,style="height:40px",
                                                                           textInput(ns("Region"),"","e.g. North Atlantic")),
                                                                    column(width=4,style="height:40px;padding:19px",
                                                                           h5("Agency:",style="font-weight:bold")),
                                                                    column(width=8,style="height:40px",
                                                                           textInput(ns("Agency"),"","e.g. ICCAT")),
                                                                    column(width=4,style="height:40px;padding:19px",
                                                                           h5("Fishery start:",style="font-weight:bold")),
                                                                    column(width=3,style="height:40px",
                                                                           numericInput(ns("Syear"), "", 1951,min=1800,max=Current_Year - 2,step=1)),
                                                                    column(width=2,style="height:40px;padding:19px",
                                                                           h5("End:",style="font-weight:bold")),
                                                                    column(width=3,style="height:40px",
                                                                           numericInput(ns("Lyear"), "", Current_Year,min=Current_Year,max=Current_Year,step=1)),
                                                                    column(width=4,style="height:40px;padding:19px",
                                                                           h5("Author:",style="font-weight:bold")),
                                                                    column(width=8,style="height:40px",
                                                                           textInput(ns("Author"), "","Alex Doe (a.doe@gmail.com)"))
                                                                  )
                                                 ),

                                                 conditionalPanel(width=4,condition="output.Fpanel==2",#|output.Fpanel==undefined",
                                                                  checkboxGroupInput(ns("M"), label = h5("2. Longevity",style="color:black"),
                                                                                     choices = M_list, selected = M_list),
                                                                  actionLink(ns("All_M"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==3",
                                                                  checkboxGroupInput("D", label = h5("3. Stock depletion",style="color:black"),
                                                                                     choices = D_list, selected = D_list),
                                                                  actionLink(ns("All_D"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==4",
                                                                  checkboxGroupInput("h", label = h5("4. Resilience",style="color:black"),
                                                                                     choices = h_list, selected = h_list),
                                                                  actionLink(ns("All_h"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==5",
                                                                  column(12,style="padding-left:0px",
                                                                         h5("5. Historical effort pattern",style="color:black"),
                                                                         h5("Click on the plot to sketch the historical pattern of relative fishing effort"),
                                                                         #plotOutput("effort_plot", click = "plot_click", hover = "plot_hover",height=220),
                                                                         HTML("<br>"),
                                                                         column(6),
                                                                         column(6,h5("Entry coordinates:")),
                                                                         column(6,actionButton(ns("new_series"), "New"),
                                                                                actionButton(ns("undo_last"), "Undo"),
                                                                                actionButton(ns("reset_plot"), "Clear")),
                                                                         column(6,verbatimTextOutput(ns("info")))

                                                                  )
                                                 ),

                                                 conditionalPanel(width=4,condition="output.Fpanel==6",
                                                                  checkboxGroupInput(ns("F"), label = h5("6. Inter-annual variability in historical effort",style="color:black"),
                                                                                     choices = F_list, selected = F_list),
                                                                  actionLink(ns("All_F"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==7",
                                                                  checkboxGroupInput(ns("qh"), label = h5("7. Historical fishing efficiency changes",style="color:black"),
                                                                                     choices = qh_list, selected = qh_list),
                                                                  actionLink(ns("All_qh"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==8",
                                                                  checkboxGroupInput(ns("q"), label = h5("8. Future fishing efficiency changes",style="color:black"),
                                                                                     choices = q_list, selected = q_list),
                                                                  actionLink(ns("All_q"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==9",
                                                                  checkboxGroupInput(ns("LM"), label = h5("9. Length at maturity",style="color:black"),
                                                                                     choices = LM_list, selected = LM_list),
                                                                  actionLink(ns("All_LM"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==10",
                                                                  checkboxGroupInput(ns("sel"), label = h5("10. Selectivity of small fish",style="color:black"),
                                                                                     choices = sel_list, selected = sel_list),
                                                                  actionLink(ns("All_sel"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==11",
                                                                  checkboxGroupInput(ns("dome"), label = h5("11. Selectivity of large fish",style="color:black"),
                                                                                     choices = dome_list, selected = dome_list),
                                                                  actionLink(ns("All_dome"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==12",
                                                                  checkboxGroupInput(ns("DR"), label = h5("12. Discard rate",style="color:black"),
                                                                                     choices = DR_list, selected = DR_list),
                                                                  actionLink(ns("All_DR"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==13",
                                                                  checkboxGroupInput(ns("PRM"), label = h5("13. Post-release mortality rate",style="color:black"),
                                                                                     choices = PRM_list, selected = PRM_list),
                                                                  actionLink(ns("All_PRM"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==14",
                                                                  checkboxGroupInput(ns("sigR"), label = h5("14. Recruitment variability",style="color:black"),
                                                                                     choices = sigR_list, selected = sigR_list),
                                                                  actionLink(ns("All_sigR"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==15",
                                                                  checkboxGroupInput(ns("Ah"), label = h5("15. Size of existing spatial closures",style="color:black"),
                                                                                     choices = Ah_list, selected = Ah_list[[1]]),
                                                                  actionLink(ns("All_Ah"),"DEFAULT")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==16",
                                                                  checkboxGroupInput(ns("Vh"), label = h5("16. Spatial mixing (movement) in/out of existing spatial closures",style="color:black"),
                                                                                     choices = Vh_list, selected = Vh_list[[length(Vh_list)]]),
                                                                  actionLink(ns("All_Vh"),"DEFAULT")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==17",
                                                                  checkboxGroupInput(ns("A"), label = h5("17. Size of future potential spatial closure",style="color:black"),
                                                                                     choices = A_list, selected = A_list),
                                                                  actionLink(ns("All_A"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==18",
                                                                  checkboxGroupInput(ns("V"), label = h5("18. Spatial mixing (movement) in/out of future potential spatial closure",style="color:black"),
                                                                                     choices = V_list, selected = V_list),
                                                                  actionLink(ns("All_V"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Fpanel==19",
                                                                  checkboxGroupInput(ns("Dh"), label = h5("19. Initial stock depletion",style="color:black"),
                                                                                     choices = Dh_list, selected = Dh_list[[5]]),
                                                                  actionLink(ns("All_Dh"),"DEFAULT")),

                                                 value=1),


                                        tabPanel(h4(ns("Management"),style = "color:black"),

                                                 conditionalPanel(width=4,condition="output.Mpanel==undefined|output.Mpanel==0",

                                                                  HTML("<br>"),
                                                                  h5("The Management panel contains a set of questions about what fishery management options are available and how well management advice is followed.",style="color:grey"),
                                                                  h5("These questions are used to: ",style="color:grey"),
                                                                  h5(" - identify what management procedures are feasible given the types of management measures.",style="color:grey"),
                                                                  h5(" - determine the relative success of management procedures that provide various types of advice.",style="color:grey"),
                                                                  h5(""),
                                                                  h5("More detailed help on the Management questions can be found in the MERA manual
                                         : ", a("Section 2.2.", href="https://dlmtool.github.io/DLMtool/MERA/MERA_User_Guide_v6.html#22_management_questions", target="_blank"),style="color:grey")),


                                                 conditionalPanel(width=4,condition="output.Mpanel==1",
                                                                  checkboxGroupInput(ns("M1"), label = h5("1. Types of fishery management that are possible",style="color:black"),
                                                                                     choices = M1_list, selected = M1_list),
                                                                  actionLink(ns("All_M1"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Mpanel==2",
                                                                  checkboxGroupInput(ns("IB"), label = h5("2. TAC offset: consistent overages/underages",style="color:black"),
                                                                                     choices = IB_list, selected = IB_list),
                                                                  actionLink(ns("All_IB"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Mpanel==3",
                                                                  checkboxGroupInput(ns("IV"), label = h5("3. TAC implementation variability",style="color:black"),
                                                                                     choices = IV_list, selected = IV_list),
                                                                  actionLink(ns("All_IV"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Mpanel==4",
                                                                  checkboxGroupInput(ns("IBE"), label = h5("4. TAE offset: consistent overages/underages",style="color:black"),
                                                                                     choices = IBE_list, selected = IBE_list),
                                                                  actionLink(ns("All_IBE"),"MATCH TAC IMPLEMENTATION")),

                                                 conditionalPanel(width=4,condition="output.Mpanel==5",
                                                                  checkboxGroupInput(ns("IVE"), label = h5("5. TAE implementation variability",style="color:black"),
                                                                                     choices = IVE_list, selected = IVE_list),
                                                                  actionLink(ns("All_IVE"),"MATCH TAC IMPLEMENTATION")),

                                                 conditionalPanel(width=4,condition="output.Mpanel==6",
                                                                  checkboxGroupInput(ns("IBSL"), label = h5("6. Size limit offset: catching consistently smaller/larger than min. size",style="color:black"),
                                                                                     choices = IBSL_list, selected = IBSL_list),
                                                                  actionLink(ns("All_IBSL"),"MATCH TAC IMPLEMENTATION")),

                                                 conditionalPanel(width=4,condition="output.Mpanel==7",
                                                                  checkboxGroupInput(ns("IVSL"), label = h5("7. Size limit implementation variability",style="color:black"),
                                                                                     choices = IVSL_list, selected = IVSL_list),
                                                                  actionLink(ns("All_IVSL"),"MATCH TAC IMPLEMENTATION")),

                                                 value=2),

                                        tabPanel(h4(ns("Data"),style = "color:black"),

                                                 conditionalPanel(width=4,condition="output.Dpanel==undefined|output.Dpanel==0",

                                                                  HTML("<br>"),
                                                                  h5("The Data panel provides the option to load fishery data and specify the quality of the data that are available.",style="color:grey"),
                                                                  h5("These questions are used to: ",style="color:grey"),
                                                                  h5(" - identify what management procedures are feasible given the types of data available.",style="color:grey"),
                                                                  h5(" - determine the relative success of the management approaches that rely on differing types of data.",style="color:grey"),
                                                                  h5(""),
                                                                  h5("More detailed help on the data questions can be found in the MERA manual
                                         : ", a("Section 2.3.", href="https://dlmtool.github.io/DLMtool/MERA/MERA_User_Guide_v6.html#23_data_questions", target="_blank"),style="color:grey")),

                                                 conditionalPanel(width=4,condition="output.Dpanel==1",

                                                                  h5("1. Load fishery data (optional)",style="color:black"),
                                                                  column(12,style="padding-left:27px",

                                                                         HTML("<br>"),
                                                                         fileInput(ns("Load_Data"),"Load available data  (.csv .xlsx .rda)"),
                                                                         conditionalPanel(width=4,condition="output.Data==1",
                                                                                          h5("Data Report",style="font-weight:bold"),
                                                                                          downloadButton(ns("Build_Data")," ")
                                                                         )
                                                                  )),

                                                 conditionalPanel(width=4,condition="output.Dpanel==2",
                                                                  checkboxGroupInput(ns("CB"), label = h5("2. Catch reporting bias",style="color:black"),
                                                                                     choices = CB_list, selected = CB_list),
                                                                  actionLink(ns("All_CB"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Dpanel==3",
                                                                  checkboxGroupInput(ns("Beta"), label = h5("3. Hyperstability in indices",style="color:black"),
                                                                                     choices = Beta_list, selected = Beta_list),
                                                                  actionLink(ns("All_Beta"),"UNKNOWN")),

                                                 conditionalPanel(width=4,condition="output.Dpanel==4",
                                                                  radioButtons(ns("Err"), label = h5("4. Overall data quality",style="color:black"),
                                                                               choices = Err_list, selected = "Err_bad"),
                                                                  actionLink(ns("All_Err"),"DEFAULT")),
                                                 value=3)

                           )
                    ),

                    column(width = 7,style="height:360px",

                           HTML("<br><br><br>"),
                           #hr(),

                           # --------- Fishery panel guides ---------------------------------------------------------------------------------------------------------------

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==0",
                                            h5("",style = "color:grey")
                           ),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==1",

                                            column(12,
                                                   h5("Describe the fishery you are modelling and identify yourself and the relevant management agency.",style = "color:grey"),
                                                   h5("'Fishery start' specifies the first year of exploitation and 'End' is the last year for the operating model. If users upload data they must match these years.
                               Uploaded data indexed after the 'End' year will be used as indicator data in the Management Performance mode",style = "color:grey"),
                                                   h5("To provide futher context for this analysis, please include additional introductory details or background references in the text box below.",style = "color:grey")

                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==2",
                                            column(6,plotOutput(ns("plotM"),height=240)),

                                            column(6,
                                                   h5("How long-lived is the fish species? This is a critical input determining stock productivity.",style = "color:grey"),
                                                   h5("The parameter M is the instantaneous natural mortality rate. For a review of data-limited methods of estimating M see",style = "color:grey"),
                                                   h5(a("Kenchington (2014).", href="http://onlinelibrary.wiley.com/doi/10.1111/faf.12027/abstract", target="_blank"),style = "color:grey"),

                                                   h5("The plot to the left shows survival rates at age for the longevity scenarios you have selected.",style = "color:grey"),
                                                   h5("The range in the maximum age (age at 2% survival) is plotted as vertical dashed lines.",style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==3",
                                            column(6,plotOutput(ns("plotD"),height=240)),
                                            column(6,
                                                   h5("Depletion (D), refers to current spawning stock biomass relative to the unfished level.",style = "color:grey"),
                                                   h5("Since depletion is a data-rich quantity it may not be readily quantified and it may be necessary to specify a wide range of uncertainty for this input to identify MPs that are suitably robust.",style = "color:grey"),
                                                   h5("In a data-limited situation, coarse information regarding depletion may be obtained from examining length compositions, historical versus current catch rates, or by use of so-called Robin-Hood approaches.",style = "color:grey"),
                                                   h5("For further information see ", a("Carruthers et al. (2014)", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081", target="_blank"),style = "color:grey"),
                                                   h5("and ", a("Punt et al (2011)", href="https://academic.oup.com/icesjms/article/68/5/972/653125/Among-stock-comparisons-for-improving-stock", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==4",
                                            column(6,plotOutput(ns("ploth"),height=240)),
                                            column(6,
                                                   h5("How resilient to exploitation is the stock?",style = "color:grey"),
                                                   h5("This question controls recruitment compensation - the extent to which recruitment is reduced from unfished levels (R0) as the spawning stock becomes increasingly depleted below unfished levels (SSB0).",style = "color:grey"),
                                                   h5("Resilence is expressed in terms of steepness (h), which is the fraction of unfished recruitment at 1/5 of unfished spawning biomass.",style = "color:grey"),
                                                   h5("For a useful review of compensatory density dependence in fish populations see ", a("Rose et al. (2001).", href="http://hqczhzkgch48vzcc4czubj6v.wpengine.netdna-cdn.com/files/2012/07/Rose_etal_FishFisheries.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==5",

                                            column(6,plotOutput(ns("effort_plot"), click = "plot_click", hover = "plot_hover",height=240)),
                                            column(6,
                                                   h5("What temporal pattern best describes the trend in historical annual fishing effort (e.g. boat-days per year, number of trips per year)?",style = "color:grey"),
                                                   h5("If more than one effort time series is specified, historical fishing will be simulated by sampling all series with equal probability.",style = "color:grey"),
                                                   h5("This question specifies the possible range of mean trends, you will have an opportunity to adjust the extent of inter-annual variability and changes in fishing efficiency (catchability) in the following questions.",style = "color:grey"),
                                                   h5("Here is an introduction to fishing effort courtesy of the ", a("UN FAO.", href="http://www.fao.org/docrep/x5685e/x5685e04.htm", target="_blank"),style = "color:grey")
                                            )),


                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==6",
                                            column(6,plotOutput(ns("plotF"),height=240)),
                                            column(6,
                                                   h5("The extent of interannual variability in historical exploitation rates around the mean trend(s) specified in Fishery question #5.",style = "color:grey"),
                                                   h5("Again, here is the introduction to effort and exploitation rate by the ", a("UN FAO.", href="http://www.fao.org/docrep/x5685e/x5685e04.htm", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==7",
                                            column(6,plotOutput(ns("plotqh"),height=240)),
                                            column(6,
                                                   h5("The annual percentage increase or decrease in historical fishing efficiency. In targeted fisheries gear efficiency may improve over time given techological improvements in the gear, changes in fishing behavior, fish distribution and information sharing among fishers, among other things. Conversely, non-target or bycatch species may be subject to declining fishing efficiency due to regulations or avoidance behaviors. The catchability (q) is the fraction of available fish caught per unit of effort. For example, a 2% per annum increase in fishing efficiency means that after 35 years twice as many fish will be caught for the same effort as today.",style = "color:grey"),
                                                   h5("The introduction to fishing efficiency by the FAO provides a ", a("basic summary.", href="http://www.fao.org/docrep/008/y6982e/y6982e06.htm", target="_blank"),style = "color:grey"),
                                                   h5(a("Arrenguin-Sanchez", href="https://drive.google.com/open?id=1ZrHbhotCJ5Vjw4JNloUSY94BVoM0iyfI", target="_blank")," provides a more comprehensive review of catchability.",style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==8",
                                            column(6,plotOutput(ns("plotq"),height=240)),
                                            column(6,
                                                   h5("The annual percentage increase or decrease in future fishing efficiency. In targeted fisheries gear efficiency may improve over time given techological improvements in the gear, changes in fishing behavior, fish distribution and information sharing among fishers, among other things. Conversely, non-target or bycatch species may be subject to declining fishing efficiency due to regulations or avoidance behaviors. The catchability (q) is the fraction of available fish caught per unit of effort. For example, a 2% per annum increase in fishing efficiency means that after 35 years twice as many fish will be caught for the same effort as today.",style = "color:grey"),
                                                   h5("The introduction to fishing efficiency by the FAO provides a ", a("basic summary.", href="http://www.fao.org/docrep/008/y6982e/y6982e06.htm", target="_blank"),style = "color:grey"),
                                                   h5(a("Arrenguin-Sanchez", href="https://drive.google.com/open?id=1ZrHbhotCJ5Vjw4JNloUSY94BVoM0iyfI", target="_blank")," provides a more comprehensive review of catchability.",style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==9",
                                            column(6,plotOutput(ns("plotLM"),height=240)),
                                            column(6,
                                                   h5("Size a maturity relative to asymptotic length (LM).",style = "color:grey"),
                                                   h5("Note 1: 'maturity' as used by this model (and most fish population dynamics models) is not really whether a fish has fully developed gonads, but rather the fraction of maximum spawning potential per weight. For example, some fishes mature early, but at small sizes they spawn infrequently and their recruits have poor survival (low spawning fraction).",style = "color:grey"),
                                                   h5("Note 2: asymptotic length is not the maximum length observed but rather the mean expected size of fish at their maximum age under unfished conditions",style = "color:grey"),
                                                   h5("An ICES workshop report provides ", a("an overview of maturity estimation.", href="http://www.ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2008/WKMOG/WKMOG08.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==10",
                                            column(6,plotOutput(ns("plotsel"),height=240)),
                                            column(6,
                                                   h5("Fishing gear selectivity relative to asymptotic length (S) (ascending limb selectivity). For example, if 50% of 40cm fish are caught and maximum length is 100cm, S = 0.4.",style = "color:grey"),
                                                   h5("The UN FAO provides an ", a("introduction to gear selectivity and how it may be quantified.", href="http://www.fao.org/docrep/w5449e/w5449e08.htm", target="_blank"),style = "color:grey"),
                                                   h5("For a more involved discussion on selectivity see the ", a("IATTC CAPAM workshop report.", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==11",
                                            column(6,plotOutput(ns("plotdome"),height=240)),
                                            column(6,
                                                   h5("Fishing gear selectivity of the largest individuals (SL). For example, if only 20% of the longest fish are caught by the gear, SL = 0.2.",style = "color:grey"),
                                                   h5("Again, here is the UN FAO introduction to fishing gear selectivity ", a("introductory document.", href="http://www.fao.org/docrep/w5449e/w5449e08.htm", target="_blank"),style = "color:grey"),
                                                   h5("and here is the ", a("IATTC CAPAM workshop report.", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==12",
                                            column(6,plotOutput(ns("plotDR"),height=240)),
                                            column(6,
                                                   h5("Discard rate (DR) is the fraction of fish that discarded both dead and alive",style = "color:grey"),
                                                   h5("The US National Marine Fisheries Service has a general guide to ", a("Understanding Fish Bycatch Discard and Escapee Mortality.", href="https://www.afsc.noaa.gov/quarterly/jfm03/featurejfm03.pdf", target="_blank"),style = "color:grey"),
                                                   h5("and one of the authors of that guide, Michael Davis also has a useful article: ", a("Key principles for understanding fish bycatch discard mortality.", href="https://drive.google.com/open?id=1ZtqB_MHapyagplv_enJ0o-_t4UKF5chh", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==13",
                                            column(6,plotOutput(ns("plotPRM"),height=240)),
                                            column(6,
                                                   h5("The post-release mortality rate (PRM) is the fraction of discarded fish that die after release",style = "color:grey"),
                                                   h5("The US National Marine Fisheries Service have a general guide to ", a("Understanding Fish Bycatch Discard and Escapee Mortality.", href="https://www.afsc.noaa.gov/quarterly/jfm03/featurejfm03.pdf", target="_blank"),style = "color:grey"),
                                                   h5("and one of the authors of that guide, Michael Davis also has a useful article: ", a("Key principles for understanding fish bycatch discard mortality.", href="https://drive.google.com/open?id=1ZtqB_MHapyagplv_enJ0o-_t4UKF5chh", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==14",
                                            column(6,plotOutput(ns("plotsigR"),height=240)),
                                            column(6,
                                                   h5("The interannual variability in recruitment is expressed here as the maximum inter-annual change. Recruitment is expected to change among years in response to spawning biomass levels. Additional variability may be driven by many factors including varying ocean conditions, amount of spawning habitat, food availability and predation.",style = "color:grey"),
                                                   h5("Recruitment variation is commonly described by the coefficient of variation in log-normal recruitment deviations (sigma R). An approximate rule of thumb is that 95% of recruitments fall in a range that is twice the sigma R. So given a sigma R of 10%, 95% of recruitments will fall within an interannual change of 20% of the mean recruitment predicted from spawning biomass.",style = "color:grey"),
                                                   h5("Edward Houde has authored a ", a("Comprehensive Review of Recruitment and Sources of Variability.", href="https://drive.google.com/open?id=19q_ct4Xd954H2jjotX3YKy0BJ-v53bt2", target="_blank"),style = "color:grey"),
                                                   h5("See also ", a("Chambers and Trippel (1997).", href="https://drive.google.com/open?id=1KhwP5URGPzW6ViJPiwprfY2tE2uucpDR", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==15",
                                            column(6,plotOutput(ns("plotAh"),height=240)),
                                            column(6,
                                                   h5("The size of a existing spatial closure (e.g. Marine Protected Area, MPA). The size A, is the % of habitat that is protected (the same fraction closed is applied to the habitats of all life stages, for example spawning and rearing grounds).",style = "color:grey"),
                                                   h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==16",
                                            column(6,plotOutput(ns("plotVh"),height=240)),
                                            column(6,
                                                   h5("Stock mixing in/out of existing spatial closure. The degree of the spatial mixing of the fish stock is represented as the probability (P) of a fish leaving the spatial closure (i.e. the marine protected area, MPA) between years",style = "color:grey"),
                                                   h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==17",
                                            column(6,plotOutput(ns("plotA"),height=240)),
                                            column(6,
                                                   h5("The size of a potential future spatial closure (Marine Protected Area, MPA). The size A, is the % of habitat that is protected (the same fraction closed is applied to the habitats of all life stages, for example spawning and rearing grounds).",style = "color:grey"),
                                                   h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==18",
                                            column(6,plotOutput(ns("plotV"),height=240)),
                                            column(6,
                                                   h5("Stock mixing in/out of a future spatial closure. The degree of the spatial mixing of the fish stock is represented as the probability (P) of a fish leaving the closed area (i.e. the marine protected area, MPA) between years",style = "color:grey"),
                                                   h5("The FAO provides a comprehensive ", a("review of spatial closures and Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==1&output.Fpanel==19",
                                column(6,plotOutput(ns("plotDh"),height=240)),
                                column(6,
                                       h5("Initial depletion of the stock relative to asymptotic unfished levels (D1: spawning stock biomass in year 1 relative to equilibrium unfished conditions).",style = "color:grey"),
                                       h5("Many fisheries undertake large fluctuations in productivity. In some of these cases, a fishery may have began at a time when the stock was naturally low. This question provides an opportunity to specify this initial depletion. The default however is that the stock was at asymptotic unfished levels in the first year of the fishery.",style = "color:grey"),
                                       h5("For further information see ", a("Carruthers et al. (2014)", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081", target="_blank"),style = "color:grey"),
                                       h5("and ", a("Punt et al (2011)", href="https://academic.oup.com/icesjms/article/68/5/972/653125/Among-stock-comparisons-for-improving-stock", target="_blank"),style = "color:grey")
                                )),


                           # -------- Management panel guides --------------------------------------------------------------------------------------------------------------

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==0",
                                h5("",style = "color:grey")
                           ),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==1",
                                # h5("Possible modes of fishery management",style = "color:black"),
                                h5("Here you indicate which MPs are feasible given the management options that are available.", style = "color:grey"),
                                h5("Management procedures can provide management advice in terms of:",style="color:grey"),
                                h5(" - Total Allowable Catch (TAC, e.g. 20,000 metric tonnes).",style="color:grey"),
                                h5(" - Total Allowable Effort (TAE, e.g. 800 trap days per year).",style="color:grey"),
                                h5(" - Size limits (e.g. minimum size of 45cm).",style="color:grey"),
                                h5(" - Time-area closures (e.g. a permanent marine protected area or seasonal closure).",style="color:grey"),
                                h5(""),
                                h5("For more information see the ", a("UN FAO guide to fishery management types.", href="http://www.fao.org/docrep/005/y3427e/y3427e06.htm", target="_blank"),style = "color:grey"),
                                h5(""),
                                h5("Steffanson and Rosenberg describe and discuss fishery management types in ", a("their 2005 paper.", href="https://drive.google.com/open?id=1V5aMNf3raitNC515qyFfITDivgbXkU4X", target="_blank"),style = "color:grey")

                           ),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==2",
                                column(7,plotOutput(ns("plotIB"),height=280)),
                                column(5,
                                       h5("What is the possible extent to which fishing operations may exceed (overages) or fall short (underages)
                                          of the specified Total Allowable Catch (TAC)? For example, given a TAC of 1000 tonnes a 10% offset (overage) would on average lead to 1100 tonnes of fish taken.",style = "color:grey"),
                                       h5(""),
                                       h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey"),
                                       h5(""),
                                       h5("Fulton et al. also provide a discussion of implementation error in their ",a("2011 paper.", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h", target="_blank"),style = "color:grey")
                                )),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==3",
                                            column(7,plotOutput(ns("plotIV"),height=280)),
                                            column(5,
                                                   h5("In the previous question you specified the range of the possible TAC offset (mean overage or underage).
                                   In this question you add the variability (V) in the implementation of TACs among years. For example, if on average there
                                   is no TAC offset, a V of 10% leads to annual overages/underages within 20% of the annual TAC recommendation (the black line in the figure opposite)
                                   for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest
                                   (solid line) levels of overages/underages specified in the previous question.",style = "color:grey"),
                                                   h5(""),
                                                   h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==4",
                                            column(7,plotOutput(ns("plotIB_E"),height=280)),
                                            column(5,
                                                   h5("What is the possible extent to which fishing operations may exceed (overages) or fall short (underages)
                                of the specified Total Allowable Effort (TAE)? For example, given a TAE of 2000 boat-days of fishing a 10% overage would on average lead to 2200 boat days of effort.",style = "color:grey"),
                                                   h5(""),
                                                   h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey"),
                                                   h5(""),
                                                   h5("Fulton et al. also provide a discussion of implementation error in their ",a("2011 paper.", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==5",
                                            column(7,plotOutput(ns("plotIV_E"),height=280)),
                                            column(5,
                                                   h5("In the previous question you specified the range of possible TAE offset (mean overages/underages).
                                   In this question you add the variability (V) in the implementation of TAEs among years. For example, if on average there
                                   is no TAE offset, a V of 20% leads to annual TAE overages/underages within 40% of the annual TAE recommendation (the black line in the figure opposite)
                                   for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest
                                   (solid line) levels of overages/underages specified in the previous question.",style = "color:grey"),
                                                   h5(""),
                                                   h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==6",
                                            column(7,plotOutput(ns("plotIB_SL"),height=280)),
                                            column(5,
                                                   h5("What is the possible extent to which fishing operations may exceed (catch larger) or fall short (catch smaller)
                                fish than the specified minimum size limit? For example, given a size limit of 20cm (e.g. escape hole size of a trap), a value of 20% would lead to a mean minimum size in the catch of 24cm.",style = "color:grey"),
                                                   h5(""),
                                                   h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey"),
                                                   h5(""),
                                                   h5("Fulton et al. also provide a discussion of implementation error in their ",a("2011 paper.", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h", target="_blank"),style = "color:grey")
                                            )),

                           conditionalPanel(condition="input.tabs1==2&output.Mpanel==7",
                                            column(7,plotOutput(ns("plotIV_SL"),height=280)),
                                            column(5,
                                                   h5("In the previous question you specified the range of possible mean violations of a minimum size limit.
                                   In this question you add variability (V) in size limit implementation among years. For example, a size limit of 90cm is exceeded by an average of 10cm, a value of 5% leads to minimum catch sizes of between 90cm and 110cm (the black line in the figure opposite)
                                   for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest
                                   (solid line) offset in size limit specified in the previous question.",style = "color:grey"),
                                                   h5(""),
                                                   h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey")
                                            )),


                           # -------- Data panel guides --------------------------------------------------------------------------------------------------------------------

                           conditionalPanel(condition="input.tabs1==3&output.Dpanel==0",
                                h5("",style = "color:grey")
                           ),

                           conditionalPanel(condition="input.tabs1==3&output.Dpanel==1",
                                h5("Users have the option of loading fishery data to unlock various MERA features",style = "color:grey"),
                                h5("When formatted into a DLMtool/MSEtool csv data file, fishery data can be used to:",style = "color:grey"),
                                h5(" - condition operating models",style = "color:grey"),
                                h5(" - determine feasible MPs (Management Planning mode)", style = "color:grey"),
                                h5(" - assess the fishery status (Status Determination mode)", style = "color:grey"),
                                h5(" - test for exceptional circumstances (Management Performance mode).",style = "color:grey"),
                                h5("A description of the data object can be found ",a("here", href="https://dlmtool.github.io/DLMtool/cheat_sheets/Data", target="_blank"),style = "color:grey"),
                                h5("A comprehensive guide to data formatting for MERA is avaialble ",a("here", href="https://dlmtool.github.io/DLMtool/Data/Data.html", target="_blank"),style = "color:grey")
                           ),

                           conditionalPanel(condition="input.tabs1==3&output.Dpanel==2",
                                  column(7,plotOutput(ns("plotCB"),height=280)),
                                  column(5,
                                         h5("Catch reporting bias includes a chronic misreporting of the catch over time.",style = "color:grey"),
                                         h5("In some data-limited fisheries, incomplete monitoring of fishing operations may lead to under-reporting (and to a lesser extent over-reporting) of annual catches.",style = "color:grey"),
                                         h5(""),
                                         h5("For further discussion of catch under reporting see",a("Agnew et al. (2009).", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2646833/", target="_blank"),style = "color:grey")
                                  )

                           ),

                           conditionalPanel(condition="input.tabs1==3&output.Dpanel==3",
                                  column(7,plotOutput(ns("plotBeta"),height=280)),
                                  column(5,
                                         h5("Is the primary index of relative abundance proportional to real biomass? Indices of relative abundance derived from fishery
                                   catch-per-unit effort (CPUE) may decline faster than real abundance (hyperdepletion) in cases where, for example, the
                                   species is being avoided or there has been attrition of high-density sub-population structure during early commericial
                                   fishing. Conversely CPUE data may respond slower than real biomass changes (hyperstability) if the species is being targeted,
                                   there is range contraction of fishing toward high density areas as the stock declines or the population naturally forms aggregations.
                                   For example, purse-seine fisheries are often strongly hyperstable since the fish per aggregation may remain high even at low stock sizes.
                                   It may be generally assumed that a well designed fishery-independent survey is proportional to abundance but there are notable exceptions.",style = "color:grey"),
                                                   #h5(""),
                                                   #tagList("See ",a("Hilborn and Walters. (1992)", href="https://books.google.ca/books?id=Y0EGCAAAQBAJ&pg=PA190&lpg=PA190&dq=hyperstability+fisheries&source=bl&ots=v3jjRE1mwh&sig=XBbO2o7JvBqEwISAdQE83xMU5v0&hl=en&sa=X&ved=0ahUKEwiA__KW8-zZAhUJ3WMKHeL3CQ4Q6AEISjAF#v=onepage&q=hyperstability%20fisheries&f=false")),
                                                   h5("See ",a("Erisman et al. (1992)", href="https://drive.google.com/open?id=1jwhIGfTmXewKWGSTNyyjoo4TefW2JiNR", target="_blank"),style = "color:grey"),
                                                   h5("or ",a("Maunder et al. (2006)", href="https://drive.google.com/open?id=1chNF72tCB_fjTjbbhZk7EyxtRU810EIc", target="_blank"),style = "color:grey")
                                  )
                           ),

                           conditionalPanel(condition="input.tabs1==3&output.Dpanel==4",
                                            #column(7,plotOutput("plotErr",height=280)),
                                column(12,
                                       h5("What is the overall quality of data that are available?",style = "color:grey"),
                                       h5("Perfect Information: an unrealistic and idealized observation model for testing the theoretical performance of MPs.",style = "color:grey"),
                                       h5("Good quality: annual catches and abundance indices are observed with low error (<20% CV) and length/age composition data are numerous (~100 independent observations per year).",style = "color:grey"),
                                       h5("Data moderate: annual catches and abundance indices are observed with greater error (<30% CV) and length/age composition data are fewer (~40 independent samples per year).",style = "color:grey"),
                                       h5("Data poor: annual catches and abundance indices are imprecisely observed (<50% CV) and length/age composition data are sparse (~15 independent samples per year).",style = "color:grey"),
                                       h5(""),
                                       h5("A description of the observation error model is included in ",a("Carruthers et al (2013)", href="https://drive.google.com/open?id=1EX6mu9HOb4jwlQF-Co6DQ-FJzwTcO7JP", target="_blank"),style = "color:grey"),
                                       h5(" and a similar model was used by ",a("Carruthers et al. (2015).", href="https://drive.google.com/open?id=1xvNxp_3oUOhSaLARY_mI2cAiG2qFAgqN", target="_blank"),style = "color:grey")
                                )
                           ),

                           # ----- Extra panel guides

                           conditionalPanel(condition="input.tabs1==4&output.Opanel==0",
                                      h5("",style = "color:grey")
                           ),


                           conditionalPanel(condition="input.tabs1==4&output.Opanel==1",
                                column(12,
                                       HTML("<br>"),
                                       HTML("<br>"),
                                       h5("Users have the option to specify bio-economic models that control the response of fishing effort in addition to management advice set by MPs", style = "color:grey"),
                                       h5("There are five parameters of the simple response model:
                                           (1) the current cost of a unit of fishing effort,
                                           (2) the current revenue of a unit of catch,
                                           (3) the % change in effort given the current level of profit,
                                           (4) the expected % change in future annual cost per unit of effort,
                                           (5) the % change in future annual revenue per catch.", style = "color:grey"),
                                                           h5("The Simple Response model is relatively simple and models fishing effort increases according to expected profit: effort next year = (effort this year) * (1+response) * (revenue catch) - (cost effort)", style = "color:grey"),
                                       h5("For further information here is a ", a("guide to the bioeconomic model.", href="https://dlmtool.github.io/DLMtool/MERA/BioEco.html", target="_blank"),style = "color:grey")

                                )
                           ),

                           # ---- Other panel guides

                           conditionalPanel(condition="output.Fpanel==0&output.Dpanel==0&output.Mpanel==0&output.Opanel==0", #(input.tabs1==1 & (output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))|(input.tabs1==2&(output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))|(input.tabs1==3&(output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))|(input.tabs1==4&(output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))",
                                column(12,
                                       h5("The Fishery, Management and Data questions specify the range of operating model simulations used in the closed-loop testing of management procedures (MPs).",style = "color:grey"),
                                       h5("The questions are presented in order of general importance and default to maxmum uncertainty.",style = "color:grey"),
                                       h5("At any stage you can select an analysis type and press 'CALCULATE'.",style = "color:grey"),
                                       h5("As you work through the questions in the Fishery, Management and Data panels, you can narrow the range of simulated fisheries but you should provide justification for each selection in the justification box.",style = "color:grey"),
                                       h5("The Extra panel includes extensions to the questionnaire that allow for operating model customization where necessary.",style = "color:grey")
                                )
                           ),

                           conditionalPanel(condition="input.tabs1==5 & (output.Dpanel>0 | output.Fpanel>0 | output.Mpanel>0)",
                                  column(12,

                                       h5("Users can also determine the total number of simulations, the number of projected years and the management update interval (years between management recommendations in the projection).
                                         The burn-in is intended to represent a duration over which an MP has already been used. Burn-in is also the number of initial projected years correponding to some stock status performance indicators. ",style = "color:grey"),
                                       h5("Users can also choose to exclude reference management procedures (e.g. zero catches, fishing at FMSY), activate parallel computation if more than 48 simulations are specified (which is much faster but there is no MSE progress bar).",style = "color:grey"),
                                       h5("The Application step requires the selection of a single MP. Other options include the loading of custom DLMtool/MSEtool code (MPs, performance metrics and MSE controls)",style = "color:grey"),
                                       h5(""),
                                       h5("A more detailed guide to these options can be found in the MERA manual ",a("Section 7.", href="www.datalimitedtoolkit.org", target="_blank"),style = "color:grey"),
                                       h5(""),
                                       h5("NOTE: a few features are currently not available such as the ability to specify Low Trophic Level (LTL) species for an alternative
                                         performance evaluation, the ability to upload indicator data and select variables for power analysis.",style = "color:grey")

                                 )
                           )

                    )

                  )
           ),


           column(12,style="padding-top:30px;padding-left:50px;padding-right:50px;padding-bottom:30px;",
                  fluidRow(

                    column(6,style="height:80px",
                           fluidRow(

                             column(width = 2,
                                    conditionalPanel(condition="(input.tabs1==1 & output.Fpanel>0)|(input.tabs1==2 & output.Mpanel>0)|(input.tabs1==3 & output.Dpanel>0)|(input.tabs1==4 & output.Opanel>0)",
                                            actionButton(ns("Fback"),"< Back")
                                    ),
                                    conditionalPanel(condition="!((input.tabs1==1 & output.Fpanel>0)|(input.tabs1==2 & output.Mpanel>0)|(input.tabs1==3 & output.Dpanel>0)|(input.tabs1==4 & output.Opanel>0))",
                                            actionButton(ns("FbackD"),"< Back",style="color: #CFCFCF;  border-color: #CFCFCF") #background-color: #CFCFCF;
                                    )

                             ),

                             column(width = 2,
                                    conditionalPanel(condition="!((input.tabs1==1 & output.Fpanel<19)|(input.tabs1==2 & output.Mpanel<7)|(input.tabs1==3 & output.Dpanel<4)|(input.tabs1==4 & output.Opanel<2))",
                                            actionButton(ns("FcontD"),"Next >",style="color: #CFCFCF;  border-color: #CFCFCF") #background-color: #CFCFCF;
                                    ),
                                    conditionalPanel(condition="output.Fpanel==0 & output.Mpanel==0 & output.Dpanel==0 & output.Opanel==0",
                                            actionButton(ns("Fcont_red"),"Next >",style="color:red; border-color:red")
                                    ),
                                    conditionalPanel(condition="((input.tabs1==1 & output.Fpanel<19)|(input.tabs1==2 & output.Mpanel<7)|(input.tabs1==3 & output.Dpanel<4)|(input.tabs1==4 & output.Opanel<2)) & !(output.Fpanel==0 & output.Mpanel==0 & output.Dpanel==0 & output.Opanel==0)",
                                            actionButton(ns("Fcont"),"Next >")
                                    )


                             ),

                             column(width=3,#style="height:180px",
                                    conditionalPanel(condition="output.Fpanel>0|output.Ppanel>0|output.Dpanel>0|output.Fpanel!=undefined|output.Mpanel!=undefined|output.Dpanel!=undefined",
                                           textOutput(ns("Dpanelout")),
                                           textOutput(ns("Fpanelout")),
                                           textOutput(ns("Mpanelout"))
                                    )
                             )

                           )
                    )
                  )
           )
         ), # end of Step 1 fluid row

         value=3)

    ),
    verbatimTextOutput(ns("Intro"))
  )
}

Fishery_Server <- function(id) {
  moduleServer(id,
     function(input, output, session) {


     }
  )
}
