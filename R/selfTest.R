## Self Test per https://github.com/James-Thorson-NOAA/VAST/wiki/Simulator
## Needs to be >= V4.4
## Using run from 23 Jan 2020
# remotes::install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")
# remotes::install_github( "James-Thorson-NOAA/VAST", ref="3.4.0" )
require(TMB)
require(VAST)
## load stuff from run of interest
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/TmbData.Rdata")
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/TmbList.Rdata")
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Save_original.RData")
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Record.RData")
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Spatial_List.Rdata")
## Pull out fitted 


DateFile <- paste0(getwd(),"/runs/",Sys.Date(),"selfTest_23Jan/")
if (!exists(DateFile)) dir.create(DateFile)
## try full re-run


Sim = Obj$simulate( complete=TRUE )



Obj <- Save$Obj
Opt <- Save$Opt
Sim = Obj$simulate( complete=TRUE )
