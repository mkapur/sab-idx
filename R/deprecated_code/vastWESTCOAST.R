## Survey Data 
# NWFSC shelf-slope survey
# 1. Length Comps
# 2. Conditional Age-at-length Comps
# 3. ***Index of Abundance (VAST) (for NWFSC shelfslope only)
# b) pre-recruit index (Owen will get)
# c) DO NOT UPDATE Triennial survey or CPUE indices


rm(list = ls())
basedir <- paste0(getwd(),"/runs/sabWCVAST"); if(!exists(basedir)) dir.create(basedir)
setwd(basedir)
# devtools::install_github("nwfsc-assess/VASTWestCoast")
library(VASTWestCoast)
# devtools::install_github("James-Thorson-NOAA/VAST@2034b7442f31bbe0e79b521652a58ed99e025e49")
# library(VAST)


# Currently, the package is not up to date with VAST. Please use the following code:
library(devtools)
devtools::install_github("kellijohnson-NOAA/FishStatsUtils")
devtools::install_github("James-Thorson-NOAA/VAST@2034b7442f31bbe0e79b521652a58ed99e025e49")
devtools::install_github("nwfsc-assess/VASTWestCoast")


require(maps)
# Q_Config <- FALSE
for(i in c('lognormal','gamma','poislink')[2]){
  downloaddir <- paste0(getwd(),"/VAST_",i)
  if(i == 'lognormal'){
    ObsModelcondition_temp <- c(1, 0)
  } else if (i == 'gamma'){
    ObsModelcondition_temp <- c(2, 0)
  }else{
    ObsModelcondition_temp <- c(2, 1)
  }
  Sim_Settings <- list(
    "Species" = "WCGBTS_Anoplopoma_fimbria",
    "ObsModelcondition" = ObsModelcondition_temp,
    "nknots" = 50,
    "strata" = data.frame("STRATA" = "All_areas"),
    "depth" = c("no", "linear", "squared")[1],
    "Passcondition" = TRUE)

  test <- VAST_condition(
    conditiondir = downloaddir,
    settings = Sim_Settings,
    spp = Sim_Settings$Species,
    datadir = downloaddir,
    overdispersion = NULL)
  load(paste0(downloaddir,"/Save.Rdata"))
  cat(downloaddir,"\n")
  if(Opt$Convergence_check != "There is no evidence that the model is not converged") stop()

}


# g10 <- list.dirs(basedir)[grep('VAST_', list.dirs(basedir))]
# g1 <- g10[!grepl('dep',g10)]
# mapply(VAST_diagnostics,  g1[!grepl('WCGBTS',g1)])

## Kelli's figure updates -- force update -- hopefully will call this over Jim's
devtools::install_github("https://github.com/kellijohnson-NOAA/FishStatsUtils")
library(FishStatsUtils)
# source("https://raw.githubusercontent.com/kellijohnson-NOAA/FishStatsUtils/master/R/plot_maps.r") ## overwrite plot_maps
dir <- paste0("C:/Users/maia kapur/Dropbox/UW/assessments/widow_2019_update/VAST_gamma/")
load(paste0(dir,"setup.Rdata"))
load(paste0(dir,"save.Rdata"))

VASTWestCoast::VAST_diagnostics(dir)
require(maps)

pch = 1
source("https://raw.githubusercontent.com/kellijohnson-NOAA/FishStatsUtils/master/R/plot_data.R")
plot_data(
  Extrapolation_List = info$Extrapolation_List,
  Spatial_List = info$Spatial_List, 
  Data_Geostat = Database,
  PlotDir = paste0("C:/Users/maia kapur/Dropbox/UW/assessments/widow_2019_update/VAST_lognormal/", .Platform$file.sep))

years <- Database$Year
Year_Set <- seq(min(years), max(years))
Years2Include <- which(Year_Set %in% sort(unique(years)))

FishStatsUtils::plot_encounter_diagnostic(Report = Report,
  Data_Geostat = Database,
  DirName =  dir)

Q <- FishStatsUtils::plot_quantile_diagnostic(
  TmbData = TmbData,
  Report = Report,
  FileName_PP = "Posterior_Predictive",
  FileName_Phist = "Posterior_Predictive-Histogram",
  FileName_QQ = "Q-Q_plot",
  FileName_Qhist = "Q-Q_hist",
  DateFile = dir)

source("https://raw.githubusercontent.com/kellijohnson-NOAA/FishStatsUtils/master/R/make_map_info.R")
info$Spatial_List$fine_scale <- FALSE
MapDetails_List <- make_map_info(
  "Region" = info$region,
  spatial_list = info$Spatial_List,
  "NN_Extrap" = info$Spatial_List$NN_Extrap,
  "Extrapolation_List" = info$Extrapolation_List)


source("https://raw.githubusercontent.com/kellijohnson-NOAA/FishStatsUtils/master/R/plot_residuals.R")
plot_residuals(
  Lat_i = Database$Lat,
  Lon_i = Database$Lon,
  TmbData = TmbData,
  Report = Report,
  Q = Q, savedir = dir, FileName = paste0(dir, .Platform$path.sep),
  MappingDetails = MapDetails_List[["MappingDetails"]],
  PlotDF = MapDetails_List[["PlotDF"]],
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  Year_Set = unique(Database[,'Year'])[order(unique(Database[,'Year']))], Years2Include = Years2Include,
  Rotate = MapDetails_List[["Rotate"]],
  Cex = MapDetails_List[["Cex"]], Legend = MapDetails_List[["Legend"]],
  zone = MapDetails_List[["Zone"]],
  mar = c(0, 0, 2, 0), oma = c(3.5, 3.5 ,0, 0), cex = 0.8)
## Vis final