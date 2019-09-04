## Take 5 -- actually paste the whole thing & replace

library(devtools)

################
# Settings
################
#devtools::install_github("james-thorson/FishStatsUtils", ref="development")
#devtools::install_github("james-thorson/VAST", ref="development")
#devtools::install_github("james-thorson/FishData")
#devtools::install_github('ices-tools-prod/icesDatras', ref="1.1-1")
#devtools::install_github("ha0ye/FishData") #removed specific version check for icesDatras version 1.1.1 bc its now icesDatras 1.2.0
#install_github( "james-thorson/FishStatsUtils", ref="test" )


gc()



## create_extrapolation_grids_forMaia.R [modified] ----
# devtools::install_github("james-thorson/VAST", dependencies = F)
library(TMB)
library(dplyr)
library(VAST)
library(FishData)
library(TMBhelper)
library(ThorsonUtilities)
library(FishStatsUtils)
# working_directory <- getwd()
# grid2 <- readRDS(file = paste0(working_directory,"/make_extrapolation_grids/WBS/grid2.rds"))

zone <- 31

# Extract strata boundaries by region for WBS
# WBS_boundaries = c(range(grid2$Lon), range(grid2$Lat))
# grid2$STRATA <- "WBS"

# grid2 <- data.frame(Lon=grid2$Lon,Lat=grid2$Lat,Area_km2=grid2$Area_km2,STRATA = grid2$X1.nrow.grid1.) #,REGION = grid2$STRATA, STRATA = grid2$X1.nrow.grid1.




# 

# strata.limits <- data.frame('STRATA'="All_areas")
# load("./data/Data_Set.rds")

# Data_Geostat <- data.frame( "Catch_KG"= Data_Set$Sablefish_n3, 
#                             "Year"=  Data_Set$YEAR, 
#                             "Vessel"= rep("IPHC", nrow(Data_Set)), 
#                             "AreaSwept_km2"= rep(1,nrow(Data_Set)), #*100 (i think this actually put it in hectares if * 100)
#                             "Lat"= Data_Set$LATITUDE, 
#                             "Lon"= Data_Set$LONGITUDE, 
#                             "Pass"=0, 
#                             "Stratum"= Data_Set$REG_BIG,
#                             "Survey" = Data_Set$Station)
strata.limits <- data.frame(
  'STRATA' = c("R3","R4","R5"),
  'west_border' = c(-180, -145, -115),
  'east_border' = c(-145, -115, -110),
  'north_border' = c(63, 63, 63),
  'south_border' = c(50, 50, 50) )


# #western bering sea                                   
# Other_extrap <- Prepare_User_Extrapolation_Data_Fn(Region=Region,
#                                                    strata.limits=strata.limits,
#                                                    observations_LL = Data_Set[,c("LONGITUDE","LATITUDE")],
#                                                    flip_around_dateline=TRUE,
#                                                    input_grid = data.frame('Lon' = Data_Geostat$Lon,
#                                                                            'Lat' = Data_Geostat$Lat,
#                                                                            'Area_km2' = rep(4, nrow(Data_Geostat))))

# Reduce to one column
# Other_extrap$a_el$All_areas <- rep(4, nrow(Other_extrap$a_el))
# Other_extrap$a_el <- cbind( "All"=Other_extrap$a_el[,1], 
#                                  "R3"=c(Other_extrap$a_el[,1],rep(0,nrow(Other_extrap$a_el)),rep(0,nrow(Other_extrap$a_el))), 
#                                  "R4"=c(rep(0,nrow(Other_extrap$a_el)),Other_extrap$a_el[,1],rep(0,nrow(Other_extrap$a_el))), 
#                                  "R5" =c(rep(0,nrow(Other_extrap$a_el)),rep(0,nrow(Other_extrap$a_el)),Other_extrap$a_el[,1]) )

# colSums( Extrapolation_List$a_el ) #check area sums
print()
# saveRDS(Other_extrap, file = "./data/r345_extrap.rds")

## Make R3/R4/R5 (AK ONLY) ---- 
## you will need to have obs in Data_Geostat in each region otherwise it'll be zero.
# strata.limits <- data.frame(
#   'STRATA' = c("R5","R4", "R3"),
#   'west_border' = c(-180, -145, -143),
#   'east_border' = c(-145, -144, -110),
#   'north_border' = c(63, 63, 63),
#   'south_border' = c(50, 50, 50) )
strata.limits <- data.frame(
  'STRATA' = c("R5","R4", "R3"),
  'west_border' = c(-180, -145, -135),
  'east_border' = c(-145, -135, -110),
  'north_border' = c(63, 63, 63),
  'south_border' = c(50, 50, 50) )
# strata.limits <- data.frame(
#   'STRATA' = c("All areas","WGOA","CGOA"),
#   'west_border' = c(-159.62, -159.62, -154.98),
#   'east_border' = c(-149.92, -154.15, -149.92),
#   'north_border' = c(58.73, 57.02, 58.73),
#   'south_border' = c(54.59, 54.59, 56.31) )

# print(strata.limits)

# R345_Extrap <- Prepare_User_Extrapolation_Data_Fn(
#   Region = Region,
#   strata.limits = strata.limits,
#   observations_LL = Data_Set[, c("LONGITUDE", "LATITUDE")],
#   flip_around_dateline =
#     TRUE,
#   input_grid = data.frame(
#     'Lon' = Data_Geostat$Lon,
#     'Lat' = Data_Geostat$Lat,
#     'Area_km2' = rep(4, nrow(Data_Geostat))
#   )
# )
# R345_Extrap <-
#   make_extrapolation_info(
#     Region = 'User',
#     strata.limits = strata.limits,
#     input_grid = data.frame(
#       'Lon' = Data_Geostat$Lon,
#       'Lat' = Data_Geostat$Lat,
#       'Area_km2' = rep(4, nrow(Data_Geostat))
#     )
#   )
# # Reduce to one column
# # Other_extrap$a_el$All_areas <- rep(4, nrow(Other_extrap$a_el))
# R345_Extrap$a_el <- cbind( "All"=Other_extrap$a_el[,1],
#                                  "R3"=c(Other_extrap$a_el[,1],rep(0,nrow(Other_extrap$a_el)),rep(0,nrow(Other_extrap$a_el))),
#                                  "R4"=c(rep(0,nrow(Other_extrap$a_el)),Other_extrap$a_el[,1],rep(0,nrow(Other_extrap$a_el))),
#                                  "R5" =c(rep(0,nrow(Other_extrap$a_el)),rep(0,nrow(Other_extrap$a_el)),Other_extrap$a_el[,1]) )

# colSums( R345_Extrap$a_el ) #check area sums
# print()
# saveRDS(R345_Extrap, file = "./data/RDSFILES/r345_extrap.rds"); rm(R345_Extrap)



#load Data_Geostat & Extrapolation list
working_directory <- getwd()

Extrapolation_List <-  readRDS("./data/RDSFILES/r345_extrap.rds")
# Extrapolation_List <-  readRDS("./data/RDSFILES/r345_extrap.rds") ## made in VAST_dataprep -- only has AK in three regions

Data_Geostat <-  readRDS("./data/AK/Data_Geostat_AK.rds") ## made in 

# Extrapolation_List <-  readRDS(file = paste0(working_directory,"/Extrapolation_List.rds"))

RootDir <- paste0(getwd(),"/runs")
DataDir <- paste0( getwd(), "/data/" )
Data2Dir <- paste0( getwd(), "/data/" )
TmbDir <- system.file("executables", package <- "VAST")

Species <- "Anoplopoma fimbria"
n_x <- 100   # Specify number of stations (a.k.a. "knots")

DateFile <- paste0(RootDir,"/",Sys.Date(),"_",Species,"_nx=",n_x,"/")
dir.create(DateFile)

Version <- "VAST_v8_0_0" # get_latest_version( package<-"VAST" )
Method <- c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km <- 25

FieldConfig <- matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
RhoConfig <- c("Beta1"=0, "Beta2"=0, "Epsilon1"=4, "Epsilon2"=4)
#FieldConfig = matrix( c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0), nrow=3, byrow=TRUE )
#RhoConfig = c("Beta1"=1, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)

OverdispersionConfig <- c("Eta1"=0, "Eta2"=0)
ObsModel <- list(c(2,0),c(2,3))[[1]]

#Covariate = c("None", "Bottom", "BotVar", "Cold_pool")[4]
Aniso <- FALSE
fine_scale <- FALSE
Options =  c("Calculate_Range"=FALSE, "Calculate_effective_area"=FALSE, "SD_site_logdensity"=FALSE)

# Default
# strata.limits <- data.frame('STRATA'="All_areas")

# Run directory
RunDir <-
  paste0(
    DateFile,
    "Obs=",
    paste0(ObsModel, collapse = ""),
    "_Field=",
    paste0(FieldConfig, collapse = ""),
    "_Rho=",
    paste0(RhoConfig, collapse = ""),
    "/"
  )
dir.create(RunDir)

#"Covariate",
Record <- ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Aniso","fine_scale","Options") )
save( Record, file=file.path(RunDir,"Record.RData"))
capture.output( Record, file=paste0(RunDir,"Record.txt"))

################
# Prepare data
################

# Build
Spatial_List <- FishStatsUtils::make_spatial_info( grid_size_km=grid_size_km, 
                                                  n_x=n_x, 
                                                  fine_scale=fine_scale, 
                                                  Method=Method, 
                                                  Lon_i = Data_Geostat[,'Lon'], 
                                                  Lat_i =Data_Geostat[,'Lat'], 
                                                  LON_intensity=Extrapolation_List$Data_Extrap[which(Extrapolation_List$Data_Extrap[,'Include']==TRUE),'Lon'], 
                                                  LAT_intensity=Extrapolation_List$Data_Extrap[which(Extrapolation_List$Data_Extrap[,'Include']==TRUE),'Lat'],
                                                  Extrapolation_List=Extrapolation_List, DirPath=RunDir, Save_Results=T )


################
# Run without bias correction
################
Run2Dir <- paste0(RunDir,"_","_BiasCorr=TRUE/")
dir.create(Run2Dir)


TmbData <- VAST::make_data(#"X_itp"=X_itp, 
  #"X_gtp"=X_gtp, 
  #"Xconfig_zcp"=Xconfig_zcp, 
  "Version"=Version, 
  "Aniso"=Aniso, 
  "FieldConfig"=FieldConfig, 
  "OverdispersionConfig"=OverdispersionConfig, 
  "RhoConfig"=RhoConfig, 
  "ObsModel"=ObsModel, 
  "c_i"=rep(0,nrow(Data_Geostat)), 
  "b_i"=Data_Geostat[,'Catch_KG'], 
  "a_i"=Data_Geostat[,'AreaSwept_km2'], 
  "v_i"=as.numeric(Data_Geostat[,'Vessel']),#-1, 
  "t_i"=Data_Geostat[,'Year'], 
  "spatial_list"=Spatial_List, 
  "Options"=Options )

## ensure that VAST vXXXX is in the directory specified in TmbDir
TmbList <-  VAST::make_model("build_model"=TRUE, "TmbData"=TmbData, "RunDir"=getwd(), "Version"=Version, 
                      "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
Obj <- TmbList[["Obj"]]
Opt <- TMBhelper::fit_tmb( obj=Obj, getsd=TRUE, savedir=Run2Dir, getReportCovariance=FALSE, bias.correct=FALSE,
                          newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl") )
Report <- Obj$report()
Save <-
  list(
    "Opt" = Opt,
    "Report" = Report,
    "ParHat" = Obj$env$parList(Opt$par),
    "SDReport" = TMB::sdreport(Obj),
    "TmbData" = TmbData
  )
save(Save, file=paste0(Run2Dir,"Save.RData")) ## save it in subdir
# save(TmbData, file=paste0(Run2Dir,"DatabaseSave.Rdata")) ## save it in subdir

load(paste0(Run2Dir,"parameter_estimates.Rdata")) ## save it in subdir

FishStatsUtils::plot_biomass_index(TmbData,
                                   Sdreport = Save$SDReport,
                                   DirName = Run2Dir)


VAST::check_fit(parameter_estimates = parameter_estimates, quiet = FALSE) == F ##Did an automated check find an obvious problem code (TRUE is bad; FALSE is good)


## run and save diagnostics
require(maps)


years <- Data_Geostat$Year
Year_Set <- seq(min(years), max(years))
Years2Include <- which(Year_Set %in% sort(unique(years)))

FishStatsUtils::plot_encounter_diagnostic(Report = Report,
                                          Data_Geostat = Data_Geostat,
                                          DirName =  Run2Dir)

Q <- FishStatsUtils::plot_quantile_diagnostic(
  TmbData = TmbData,
  Report = Report,
  FileName_PP = "Posterior_Predictive",
  FileName_Phist = "Posterior_Predictive-Histogram",
  FileName_QQ = "Q-Q_plot",
  FileName_Qhist = "Q-Q_hist",
  DateFile = Run2Dir)


make_map()
# 
# source("https://raw.githubusercontent.com/kellijohnson-NOAA/FishStatsUtils/master/R/make_map_info.R")
Spatial_List$fine_scale <- FALSE
MapDetails_List <- make_map_info(
  "Region" = 'user',
  spatial_list = Spatial_List,
  "NN_Extrap" = Spatial_List$NN_Extrap,
  "Extrapolation_List" = Extrapolation_List)


# source("https://raw.githubusercontent.com/kellijohnson-NOAA/FishStatsUtils/master/R/plot_residuals.R")
plot_residuals(
  Lat_i = Data_Geostat$Lat,
  Lon_i = Data_Geostat$Lon,
  TmbData = TmbData,
  Report = Report,
  Q = Q, savedir = Run2Dir, FileName = paste0(Run2Dir, .Platform$path.sep),
  MappingDetails = MapDetails_List[["MappingDetails"]],
  PlotDF = MapDetails_List[["PlotDF"]],
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  Year_Set = unique(Data_Geostat[,'Year'])[order(unique(Data_Geostat[,'Year']))], Years2Include = Years2Include,
  Rotate = MapDetails_List[["Rotate"]],
  Cex = MapDetails_List[["Cex"]], Legend = MapDetails_List[["Legend"]],
  zone = MapDetails_List[["Zone"]],
  mar = c(0, 0, 2, 0), oma = c(3.5, 3.5 ,0, 0), cex = 0.8)
## Vis final
## export outcomes at given strata
