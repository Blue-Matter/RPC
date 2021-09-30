library(dplyr)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(MSEtool)
library(SAMtool)
library(DLMtool)
library(RPC)
library(MSEextra)
library(shinyalert)

No_Fishing <- MSEtool::NFref
MPdesc$No_Fishing <- "Set F = 0"
MPinterval$No_Fishing <- expression(OBJs$MSEhist@OM@proyears + 1)

OMs <- local({
  RPC_OM <- avail("OM", package = "RPC", msg = FALSE)
  DEMO_OM <- RPC_OM[grepl("DEMO", RPC_OM)]
  DFO_OM <- RPC_OM[!grepl("DEMO", RPC_OM)]

  c(DEMO_OM, DFO_OM, avail("OM", package = "MSEextra", msg = FALSE))
})

nsim<<-24 # Default value to start, user can adjust with sliders in app

#for (fl in list.files("./Source/UI")) source(file.path("./Source/UI", fl), local = TRUE)
for (fl in list.files("./Source/RPCfuncs")) source(file.path("./Source/RPCfuncs", fl))

# Load MERA stuff
source("global_MERA.R", local = TRUE)




