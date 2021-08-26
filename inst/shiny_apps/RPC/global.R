library(dplyr)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(MSEtool)
library(SAMtool)
library(DLMtool)
library(RPC)
if(!requireNamespace("MSEextra", quietly = TRUE)) MSEtool::MSEextra()
library(MSEextra)
library(shinyalert)



A_DFO_DEMO1<-readRDS("./data/OMs/DFO_DEMO1.rda")
A_DFO_DEMO2<-readRDS("./data/OMs/DFO_DEMO2.rda")
DFO_Arrowtooth_Flounder_2014 <- readRDS("./data/OMs/Arrowtooth_Flounder_2014.rda")
DFO_Inside_YE_Rockfish<-readRDS("./data/OMs/DFO_IYRF.rds")
DFO_BoF_Herring<-readRDS("./data/OMs/Fit.rda")@OM
DFO_HG_Herring<-readRDS("./data/OMs/HG_herring.rda")
DFO_SoG_Herring<-readRDS("./data/OMs/SoG_herring.rda")
DFO_WCVI_Herring<-readRDS("./data/OMs/WCVI_herring.rda")
DFO_Inside_QB_Rockfish<-readRDS("./data/OMs/insQB_prelim.rds")
DFO_Pacific_Cod<-readRDS("./data/OMs/pcod_5ABCD.rds")
#DFO_Pacific_Hake<-readRDS("./data/OMs/Hake.rda")

No_Fishing <- MSEtool::NFref
MPdesc$No_Fishing <- "Set F = 0"
MPinterval$No_Fishing <- expression(OBJs$MSEhist@OM@proyears + 1)

OMs<-unique(avail('OM', msg = FALSE)[avail('OM', msg = FALSE)!='testOM'])

nsim<<-24 # Default value to start, user can adjust with sliders in app

#for (fl in list.files("./Source/UI")) source(file.path("./Source/UI", fl), local = TRUE)
for (fl in list.files("./Source/RPCfuncs")) source(file.path("./Source/RPCfuncs", fl))

#for (fl in list.files("./Data/OMs/")){
# OM<<-readRDS(file.path("./Data/OMs", fl))
#assign(OM@Name,OM,envir=globalenv())
#}
#remove(OM)

# Load MERA stuff
source("global_MERA.R", local = TRUE)




