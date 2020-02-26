library(TMB)
library(dplyr)
library(VAST)
library(FishData)
library(TMBhelper)
library(ThorsonUtilities)
library(FishStatsUtils)
library(ggplot2)
source("./R/build_Extrap.R")

## Load Data & Extrapolation grids ----
## load everything in directory
Data_Geostat <- list.files("./data/data_geostat",full.names = T) %>% 
  purrr::map_df(.,readRDS) 
  
## Build and/or load extraps
strata.limits.input <- paste0("R",3:5) ## gets used later to name file

# Extrapolation_List <- readRDS(paste0("./data/extraps/",paste0(strata.limits.input,collapse = "" ),"_extrap.rds"))

Extrapolation_List <- build_Extrap(Data_Geostat = Data_Geostat,
             strata.limits.input = strata.limits.input)

## Prep model and save inputs ----
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

# Run directory
RunDir <-
  paste0(
    DateFile,
    paste0(strata.limits.input,collapse = "" ),
    "_Obs=",
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
bias.correction <- TRUE
Run2Dir <- paste0(RunDir,"BiasCorr=",bias.correction,"/")
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
Opt <- TMBhelper::fit_tmb( obj=Obj, getsd=TRUE, savedir=Run2Dir, getReportCovariance=FALSE, bias.correct=bias.correction,
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

## run default plot to gen table
FishStatsUtils::plot_biomass_index(TmbData,
                                   Sdreport = Save$SDReport,
                                   DirName = Run2Dir,
                                   strata_names = paste0('Region ',substr(strata.limits.input,2,2)))

