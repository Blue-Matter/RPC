library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(shinyWidgets)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(MSEtool)
library(SAMtool)
library(DLMtool)
library(MSEextra)

#for (fl in list.files("./Source/UI")) source(file.path("./Source/UI", fl), local = TRUE)
for (fl in list.files("./Source/RPCfuncs")) source(file.path("./Source/RPCfuncs", fl), local = TRUE)
for (fl in list.files("./Data/OMs/")){
  OM<-readRDS(file.path("./Data/OMs", fl))
  assign(OM@Name,OM)
}
remove(OM)

# Shared variables

Current_Year<<-as.integer(substr(Sys.time(),start=1,stop=4))
CurrentYr<<-2021 # as.integer(input$Lyear) #as.integer(substr(as.character(Sys.time()),1,4))
Syear<<-1951
Lyear<<-2018
nsim<<-24 # for alpha testing, maybe 92 or 148 for real application
OMs<<-avail('OM')[avail('OM')!='testOM']


# App design

# ----- Fishery answers -----------------------------------------------------------------------------
#F2
M_list<<-list("Very short-lived (5 < maximum age < 7)" = "M_60", "Short-lived (7 < maximum age < 10)" = "M_40_60",
              "Moderate life span  (10 < maximum age < 20)" = "M_20_40", "Moderately long-lived (20 < maximum age < 40)" = "M_10_20",
              "Long-lived          (40 < maximum age < 80)" = "M_05_10","Very long-lived    (80 < maximum age < 160)" = "M_05")

M_mins<- c( 0.56,    0.39,      0.2,      0.098,      0.049,    0.02445)
M_maxes<-c( 0.78,    0.56,      0.39,      0.2,      0.098,     0.049)

#F3
D_list<<-list("Crashed (D < 0.05)" = "D_05", "Very depleted (0.05 < D < 0.1)" = "D_05_10",
              "Depleted (0.1 < D < 0.15)" = "D_10_15", "Moderately depleted (0.15 < D < 0.3)" = "D_15_30",
              "Healthy (0.3 < D < 0.5)" = "D_30_50","Underexploited (0.5 < D)" = "D_50")

D_mins<- c( 0.01,  0.05,      0.1,      0.15,      0.3,     0.5)
D_maxes<-c( 0.05,   0.1,      0.15,     0.3,       0.5,     0.8)

#F4
h_list<<-list("Not resilient (steepness < 0.3)" = "h_30", "Low resilience (0.3 < steepness < 0.5)" = "h_30_50",
              "Moderate resilence (0.5 < steepness < 0.7)" = "h_50_70", "Resilient (0.7 < steepness < 0.9)" = "h_70_90",
              "Very Resilient (0.9 < steepness)" = "h_90")

h_mins<- c( 0.25,   0.3,      0.5,      0.7,      0.9)
h_maxes<-c( 0.3,    0.5,      0.7,      0.9,      0.99)

#F5
FP_list<<-list("Stable" = "FP_s","Two-phase" = "FP_gr" ,"Boom-bust" = "FP_bb","Gradual increases"="FP_gi",
               "Stable, recent increases"="FP_ri", "Stable, recent declines" = "FP_rd")

M1s<-   c(0.2,    0.32,   0.2,    0.15,   0.2,     0.2)
M2s<-   c(1.2,    0.79,   0.7,    1.4,    1.2,     0.7)
sd1s<-  c(0.075,  0.13,   0.1,    0.05,   0.075,   0.075)
sd2s<-  c(0.1,    0.15,   0.18,   0.4,    0.2,     0.2)
h2s<-   c(0,      0,     3,      8,      2.5,     0)
bms<-   c(F,      T,     F,      F,      F,       F)

#F6
F_list<<-list("Not variable (less than 20% inter-annual change (IAC))" = "F_10", "Variable (maximum IAC between 20% to 50%)" = "F_10_25",
              "Highly variable (maximum IAC between 50% and 100%)"="F_25_50")

F_mins<-   c(0.05,    0.1,    0.25)
F_maxes<-   c(0.1,     0.25,   0.5)

#F7
qh_list<<-list("Declining by 2-3% pa (halves every 25-35 years)"="qh_d3_d2","Declining by 1-2% pa (halves every 35-70 years)"="qh_d2_d1",
              "Stable -1% to 1% pa (may halve/double every 70 years)"="qh_d1_1","Increasing by 1-2% pa (doubles every 35-70 years)"="qh_1_2",
              "Increasing by 2-3% pa (doubles every 25-35 years)"="qh_2_3")

qh_mins<- c(-3,       -2,       -1,      1,      2)
qh_maxes<-c(-2,       -1,       1,       2,      3)

#F8
q_list<<-list("Declining by 2-3% pa (halves every 25-35 years)"="q_d3_d2","Declining by 1-2% pa (halves every 35-70 years)"="q_d2_d1",
              "Stable -1% to 1% pa (may halve/double every 70 years)"="q_d1_1","Increasing by 1-2% pa (doubles every 35-70 years)"="q_1_2",
              "Increasing by 2-3% pa (doubles every 25-35 years)"="q_2_3")

q_mins<- c(-3,       -2,       -1,      1,      2)
q_maxes<-c(-2,       -1,       1,       2,      3)

#F9
LM_list<<-list("Very small (0.4 < LM < 0.5)" = "LM_40_50","Small (0.5 < LM < 0.6)"="LM_50_60",
                "Moderate (0.6 < LM < 0.7)" = "LM_60_70",
                "Moderate to large (0.7 < LM < 0.8)" = "LM_70_80","Large (0.8 < LM < 0.9)"="LM_80_90")

LM_mins<- (4:8)/10
LM_maxes<-(5:9)/10

#F10
sel_list<<-list("Very small (0.1 < S < 0.2)" = "sel_10_20","Small (0.2 < S < 0.4)"="sel_20_40",
                "Half asymptotic length (0.4 < S < 0.6)" = "sel_40_60",
                "Large (0.6 < S < 0.8)" = "sel_60_80","Very large (0.8 < S < 0.9)"="sel_80_90")

sel_mins<- c( 0.1,   0.2,         0.4,      0.6,  0.8)
sel_maxes<-c( 0.2,   0.4,         0.6,      0.8,  0.9)

#F11
dome_list<<-list("Asymptotic selectivity (SL = 1)" = "dome_100", "Declining selectivity with length (0.75 < SL < 1)"="dome_75_100",
                 "Dome-shaped selectivity (0.25 < SL < 0.75)" = "dome_25_75", "Strong dome-shaped selectivity (SL < 0.25)" = "dome_25")

dome_mins<- c(0.98,       0.75,         0.25,        0.05)
dome_maxes<-c(1,          1,            0.75,        0.25)

#F12
DR_list<<-list("Low (DR < 1%)"="DR_1","Low - moderate (1% < DR < 10%)"="DR_1_10","Moderate (10% < DR < 30%)"="DR_10_30",
               "Moderate - high (30% < DR < 50%)"="DR_30_50", "High (50% < DR < 70%)"="DR_50_70")

DR_mins<- c(  0,     0.01,    0.1,      0.3,      0.5)
DR_maxes<-c(  0.01,  0.1,     0.3,      0.5,      0.7)

#F13
PRM_list<<-list("Low (PRM < 5%)"="PRM_5","Low - moderate (5% < PRM < 25%)"="PRM_5_25","Moderate (25% < PRM < 50%)"="PRM_25_50",
               "Moderate - high (50% < PRM < 75%)"="PRM_50_75", "High (75% < PRM < 95%)"="PRM_75_95", "Almost all die (95% < PRM < 100%)"="PRM_95_100")

PRM_mins<- c(  0,     0.05,    0.25,      0.5,      0.75,  0.95)
PRM_maxes<-c(  0.05,  0.25,     0.5,      0.75,      0.95, 1)

#F14
sigR_list<<-list("Very low (less than 20% inter-annual changes (IAC))"="sigR_10","Low (max IAC of between 20% and 60%)"="sigR_10_30",
                 "Moderate (max IAC of between 60% and 120%)"="sigR_30_60", "High (max IAC of between 120% and 180%)"="sigR_60_90",
                 "Very high (max IAC greater than 180%)"="sigR_90")

sigR_mins<- c(  0.05,     0.1,          0.3,         0.6,        0.9)
sigR_maxes<-c(  0.1,      0.3,          0.6,         0.9,        1.2)

#F15
Ah_list<<-list("None"="A_1","Small (A < 5%)"="A_1_5", "Small-moderate (5% < A < 10%)" = "A_5_10", "Moderate (10% < A < 20%)" = "A_10_20",
               "Large (20% < A < 30%)"="A_20_30","Very large (30% < A < 40%)" = "A_30_40", "Huge (40% < A < 50%)"="A_40_50")

Ah_mins<- c(0.005,  0.01,     0.05,    0.1,        0.2,       0.3,       0.4)
Ah_maxes<-c(0.01,  0.05,     0.1,     0.2,        0.3,       0.4,       0.5)

#F16
Vh_list<<-list("Very low (P < 1%)" = "P_1", "Low (1% < P < 5%)" = "P_1_5", "Moderate (5% < P < 10%)" = "P_5_10",
               "High (10% < P < 20%)" = "P_10_20", "Fully mixed" = "P_20")

Vh_mins<- c(0.005, 0.01,     0.05,    0.1,        0.2)
Vh_maxes<-c(0.01,  0.05,     0.1,     0.2,        0.5)

#F17
A_list<<-list("None"="A_1","Small (A < 5%)"="A_1_5", "Small-moderate (5% < A < 10%)" = "A_5_10", "Moderate (10% < A < 20%)" = "A_10_20",
              "Large (20% < A < 30%)"="A_20_30","Very large (30% < A < 40%)" = "A_30_40", "Huge (40% < A < 50%)"="A_40_50")

A_mins<- c(0.005,  0.01,     0.05,    0.1,        0.2,       0.3,       0.4)
A_maxes<-c(0.01,  0.05,     0.1,     0.2,        0.3,       0.4,       0.5)

#F18
V_list<<-list("Very low (P < 1%)" = "P_1", "Low (1% < P < 5%)" = "P_1_5", "Moderate (5% < P < 10%)" = "P_5_10",
              "High (10% < P < 20%)" = "P_10_20", "Fully mixed" = "P_20")

V_mins<- c(0.005, 0.01,     0.05,    0.1,        0.2)
V_maxes<-c(0.01,  0.05,     0.1,     0.2,        0.5)

#F19
Dh_list<<-list("Very low (0.1 < D1 < 0.15)" = "D1_10_15", "Low (0.15 < D1 < 0.3)" = "D1_15_30",
              "Moderate (0.3 < D < 0.5)" = "D1_30_50", "High (0.5 < D1)" = "D1_50",
              "Asymptotic unfished levels (D1 = 1)" = "D1_100_100")

Dh_mins<- c( 0.1,      0.15,      0.3,     0.5, 1)
Dh_maxes<-c( 0.15,     0.3,       0.5,     1,   1)


# ----- Management answers -----------------------------------------------------------------------------

M1_list<<-list("TAC" = "TAC", "TAE" = "TAE",
               "Size limit" = "size_limit", "Time-area closures" = "time_area_close")

IB_list<<-list("Large underages (40% - 70% of recommended)" = "IB_n30", "Underages (70% - 90% of recommended)" = "IB_n30_n10","Slight underages (90% - 100% of recommended)" = "IB_n10_0",
               "Taken exactly (95% - 105% of recommended)"="IB_n5_5","Slight overages (100% - 110% of recommended)"="IB_0_10","Overages (110% - 150% of recommended)"="IB_10_30","Large overages (150% - 200% of recommended)"="IB_30")

IB_mins<- c( 0.4,  0.7,      0.9,     0.95,      1,     1.1, 1.5)
IB_maxes<-c( 0.7,  0.9,      1,       1.05,      1.1,   1.5, 2)


IV_list<<-list("Constant (V < 1%)" = "IV_1", "Not variable (1% < V < 5%)" = "IV_1_5","Low variability (5% < V < 10%)" = "IV_5_10",
               "Variable (10% < V < 20%)"="IV_10_20","Highly variable (20% < V < 40%)"="IV_20_40")

IV_mins<-c(0.005,0.01,0.05,0.1,0.2)
IV_maxes<-c(0.01,0.05,0.1,0.2,0.4)


IBE_list<<-list("Large underages (40% - 70% of recommended)" = "IB_n30", "Underages (70% - 90% of recommended)" = "IB_n30_n10","Slight underages (90% - 100% of recommended)" = "IB_n10_0",
               "Taken exactly (95% - 105% of recommended)"="IB_n5_5","Slight overages (100% - 110% of recommended)"="IB_0_10","Overages (110% - 150% of recommended)"="IB_10_30","Large overages (150% - 200% of recommended)"="IB_30")

IBE_mins<- c( 0.4,  0.7,      0.9,     0.95,      1,     1.1, 1.5)
IBE_maxes<-c( 0.7,  0.9,      1,       1.05,      1.1,   1.5, 2)


IVE_list<<-list("Constant (V < 1%)" = "IV_1", "Not variable (1% < V < 5%)" = "IV_1_5","Low variability (5% < V < 10%)" = "IV_5_10",
               "Variable (10% < V < 20%)"="IV_10_20","Highly variable (20% < V < 40%)"="IV_20_40")

IVE_mins<-c(0.005,0.01,0.05,0.1,0.2)
IVE_maxes<-c(0.01,0.05,0.1,0.2,0.4)

IBSL_list<<-list("Much smaller (40% - 70% of recommended)" = "IB_n30", "Smaller (70% - 90% of recommended)" = "IB_n30_n10","Slightly smaller (90% - 100% of recommended)" = "IB_n10_0",
               "Taken exactly (95% - 105% of recommended)"="IB_n5_5","Slightly larger (100% - 110% of recommended)"="IB_0_10","Larger (110% - 150% of recommended)"="IB_10_30","Much larger (150% - 200% of recommended)"="IB_30")

IBSL_mins<- c( 0.4,  0.7,      0.9,     0.95,      1,     1.1, 1.5)
IBSL_maxes<-c( 0.7,  0.9,      1,       1.05,      1.1,   1.5, 2)


IVSL_list<<-list("Constant (V < 1%)" = "IV_1", "Not variable (1% < V < 5%)" = "IV_1_5","Low variability (5% < V < 10%)" = "IV_5_10",
               "Variable (10% < V < 20%)"="IV_10_20","Highly variable (20% < V < 40%)"="IV_20_40")

IVSL_mins<-c(0.005,0.01,0.05,0.1,0.2)
IVSL_maxes<-c(0.01,0.05,0.1,0.2,0.4)




# ----- Data answers -----------------------------------------------------------------------------

D1_list<<-list("Historical annual catches (from unfished)" = "ann_cat","Recent annual catches (at least 5 recent years)" = "ann_cat_R", "Historical relative abundance index (from unfished)"= "ind",
               "Recent relative abundance index (at least 5 recent years)"= "ind_R",
               "Fishing effort" = "fis_eff","Size composition (length samples)" = "siz_com","Age composition (age samples)" = "age_com", "Growth (growth parameters)" = "growth",
               "Absolute biomass survey"="cur_bio_sur")

CB_list<<-list("Strong under-reporting (30% - 50%)" = "CB_n50_n30", "Under-reporting (10% - 30%)" = "CB_n30_n10","Slight under-reporting (0% - 10%)" = "CB_n10_0",
               "Reported accurately (+/- 5%)" = "CB_n5_5","Slight over-reporting (less than 10%)" = "CB_0_10")

CB_mins<- c( 0.5,   0.7,      0.9,     0.95,      1)
CB_maxes<-c( 0.7,   0.9,      1,       1.05,      1.1)

Beta_list<<-list("Strong hyperdepletion (2 < Beta < 3)" = "Beta_200_300", "Hyperdepletion (1.25 < Beta < 2)" = "Beta_125_200","Proportional (0.8 < Beta < 1.25)" = "Beta_80_125",
                 "Hyperstability (0.5 < Beta < 0.8)" = "Beta_50_80","Strong hyperstability (0.33 < Beta < 0.5)"="Beta_33_50")

Beta_mins<- c( 2,                1.25,          0.8,           0.50,        0.33)
Beta_maxes<-c( 3,                2,             1.25,          0.8,         0.50)

Err_list<<-list("Perfect" = "Err_perf","Good (accurate and precise)" = "Err_good","Data moderate (some what inaccurate and imprecise)"="Err_Mod",
                "Data poor (inaccurate and imprecise)" = "Err_bad")





