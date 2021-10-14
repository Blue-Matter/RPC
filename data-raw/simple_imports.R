

library(MSEtool)

# Demo - these rds files no longer exist
#DFO_DEMO_increaseM <- readRDS("data-raw/DFO_DEMO1.rda")
#DFO_DEMO_increaseM@Name <- "Example OM with increasing M over time"
#DFO_DEMO_increaseM@Source <- ""
#usethis::use_data(DFO_DEMO_increaseM, overwrite = TRUE)
#
#DFO_DEMO_decreaseM <- readRDS("data-raw/DFO_DEMO2.rda")
#DFO_DEMO_decreaseM@Name <- "Example OM with decreasing M over time"
#DFO_DEMO_decreaseM@Source <- ""
#usethis::use_data(DFO_DEMO_decreaseM, overwrite = TRUE)

# Quillback rockfish
DFO_Inside_QB_Rockfish_2020 <- readRDS("data-raw/insQB_prelim.rds")
DFO_Inside_QB_Rockfish_2020@Name <- "Preliminary operating model for inside Quillback Rockfish"
DFO_Inside_QB_Rockfish_2020@Agency <- "DFO"
DFO_Inside_QB_Rockfish_2020@Source <- ""
DFO_Inside_QB_Rockfish_2020@Common_Name <- "Quillback Rockfish"
DFO_Inside_QB_Rockfish_2020@Species <- "Sebastes maliger"

usethis::use_data(DFO_Inside_QB_Rockfish_2020, overwrite = TRUE)

# Pcod
dir <- "G:\\Shared drives\\BM shared\\1. Projects\\RPC\\Operating models"
set.seed(43)
DFO_Pacific_Cod_2020 <- MSEtool::iSCAM2OM(file.path(dir, 'pcod/0_1a_5ABCD_BASE_2020'), nsim = 250, mcmc = TRUE)

DFO_Pacific_Cod_2020@Name <- "Pacific Cod 5ABCD, 2020 update (MCMC from iSCAM assessment)"
DFO_Pacific_Cod_2020@Agency <- "DFO"
DFO_Pacific_Cod_2020@Source <-
  c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_070-eng.html",
    SR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2021/2021_002-eng.html")
DFO_Pacific_Cod_2020@Common_Name <- "Pacific Cod"
DFO_Pacific_Cod_2020@Species <- "Gadus macrocephalus"

usethis::use_data(DFO_Pacific_Cod_2020, overwrite = TRUE)

# Pacific herring WCVI
dir <- "G:\\Shared drives\\BM shared\\1. Projects\\Herring MICE\\Data\\Herring_iscam"
library(gfiscamutils)

mcmc <- gfiscamutils::read.mcmc(file.path(dir, "WCVI"))
DFO_WCVI_Herring_2019 <- MSEtool::iSCAM2OM(file.path(dir, "WCVI"), nsim = 250, mcmc = mcmc)
DFO_WCVI_Herring_2019@Name <- "Pacific Herring, West Coast Vancouver Island, 2019 update (MCMC from iSCAM assessment)"
DFO_WCVI_Herring_2019@Agency <- "DFO"
DFO_WCVI_Herring_2019@Source <-
  c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_028-eng.html",
    SR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-eng.html")
DFO_WCVI_Herring_2019@Common_Name <- "Pacific Herring"
DFO_WCVI_Herring_2019@Species <- "Clupea pallasii"

usethis::use_data(DFO_WCVI_Herring_2019, overwrite = TRUE)




# Pacific herring SoG
mcmc <- gfiscamutils::read.mcmc(file.path(dir, "SoG"))
DFO_SoG_Herring_2019 <- MSEtool::iSCAM2OM(file.path(dir, "SoG"), nsim = 250, mcmc = mcmc)
DFO_SoG_Herring_2019@Name <- "Pacific Herring, Strait of Georgia, 2019 update (MCMC from iSCAM assessment)"
DFO_SoG_Herring_2019@Agency <- "DFO"
DFO_SoG_Herring_2019@Source <-
  c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_028-eng.html",
    SR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-eng.html")
DFO_SoG_Herring_2019@Common_Name <- "Pacific Herring"
DFO_SoG_Herring_2019@Species <- "Clupea pallasii"

usethis::use_data(DFO_SoG_Herring_2019, overwrite = TRUE)



# Pacific herring HG
mcmc <- gfiscamutils::read.mcmc(file.path(dir, "HG"))
DFO_HG_Herring_2019 <- MSEtool::iSCAM2OM(file.path(dir, "HG"), nsim = 250, mcmc = mcmc)
DFO_HG_Herring_2019@Name <- "Pacific Herring, Haida Gwaii, 2019 update (MCMC from iSCAM assessment)"
DFO_HG_Herring_2019@Agency <- "DFO"
DFO_HG_Herring_2019@Source <-
  c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_028-eng.html",
    SR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-eng.html")
DFO_HG_Herring_2019@Common_Name <- "Pacific Herring"
DFO_HG_Herring_2019@Species <- "Clupea pallasii"

usethis::use_data(DFO_HG_Herring_2019, overwrite = TRUE)

# Hake
DFO_Pacific_Hake_2019 <- MSEtool::SS2OM('hake-model-2019', nsim = 250, model_discards = FALSE)
DFO_Pacific_Hake_2019@Name <- "Pacific Hake, 2019 assessment (MPD from SS3)"
DFO_Pacific_Hake_2019@Agency <- "DFO/NOAA"
DFO_Pacific_Hake_2019@Source <- c(ResDoc = "https://www.fisheries.noaa.gov/resource/document/2020-pacific-hake-whiting-stock-assessment")
DFO_Pacific_Hake_2019@Common_Name <- "Pacific Hake"
DFO_Pacific_Hake_2019@Species <- "Merluccius productus"

usethis::use_data(DFO_Pacific_Hake_2019, overwrite = TRUE)

# Inside Yelloweye Rockfish
DFO_Inside_YE_Rockfish_2019 <- readRDS("data-raw/DFO_IYRF.rds")
DFO_Inside_YE_Rockfish_2019@Name <- "Inside Yelloweye Rockfish, 2019 operating model"
DFO_Inside_YE_Rockfish_2019@Agency <- "DFO"
DFO_Inside_YE_Rockfish_2019@Source <- c(SAR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2020/2020_056-eng.html")
DFO_Inside_YE_Rockfish_2019@Common_Name <- "Yelloweye Rockfish"
DFO_Inside_YE_Rockfish_2019@Species <- "Sebastes ruberrimus"

usethis::use_data(DFO_Inside_YE_Rockfish_2019, overwrite = TRUE)

# Atlantic herring
RCM <- readRDS("data-raw/Fit.rda")
DFO_BoF_Herring_2018 <- RCM@OM
DFO_BoF_Herring_2018@Name <- "Atlantic Herring, Bay of Fundy, 2018 operating model"
DFO_BoF_Herring_2018@Agency <- "DFO"
DFO_BoF_Herring_2018@Source <- ""
DFO_BoF_Herring_2018@Common_Name <- "Atlantic Herring"
DFO_BoF_Herring_2018@Species <- "Clupea harengus"

usethis::use_data(DFO_BoF_Herring_2018, overwrite = TRUE)

# Arrowtooth flounder
DFO_Arrowtooth_Flounder_2014 <-
  MSEtool::iSCAM2OM("G:\\Shared drives\\BM shared\\1. Projects\\RPC\\Operating models\\arrowtooth_flounder_2014", nsim = 250, mcmc = TRUE)
DFO_Arrowtooth_Flounder_2014@Name <- "BC Arrowtooth Flounder, 2014 assessment (MCMC from iSCAM model)"
DFO_Arrowtooth_Flounder_2014@Agency <- "DFO"
DFO_Arrowtooth_Flounder_2014@Source <-
  c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2017/2017_025-eng.html",
    SAR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2015/2015_055-eng.html")
DFO_Arrowtooth_Flounder_2014@Common_Name <- "Arrowtooth Flounder"
DFO_Arrowtooth_Flounder_2014@Species <- "Atheresthes stomas"

usethis::use_data(DFO_Arrowtooth_Flounder_2014, overwrite = TRUE)


# Pacific Ocean Perch BC DFO
DFO_POP_2016 <- Awatea2OM('G:\\Shared drives\\BM shared\\1. Projects\\RPC\\Operating models\\POP\\Awatea', nsim = 250)
DFO_POP_2016@Name <- "BC Pacific Ocean Perch (Queen Charlotte Sound), MCMC from Awatea model"
DFO_POP_2016@Common_Name <- "Pacific Ocean Perch"
DFO_POP_2016@Species <- "Sebastes alutus"
DFO_POP_2016@Agency <- "DFO"
DFO_POP_2016@Region <- "Queen Charlotte Sound, B.C."
DFO_POP_2016@Source <-
  c(ResDoc = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_038-eng.html",
    SAR = "https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2017/2017_043-eng.html")

usethis::use_data(DFO_POP_2016, overwrite = TRUE)
