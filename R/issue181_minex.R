library(TMB)
library(dplyr)
library(VAST)
library(FishData)
library(TMBhelper)
library(ThorsonUtilities)
library(FishStatsUtils)

Data_Geostat <-  readRDS("./data/AK/Data_Geostat_AK.rds") 


strata.limits <- data.frame(
  'STRATA' = c("R5","R4", "R3"),
  'west_border' = c(-180, -145, -143),
  'east_border' = c(-145, -144, -110),
  'north_border' = c(63, 63, 63),
  'south_border' = c(50, 50, 50) )

R345_Extrap <-
  make_extrapolation_info(
    Region = 'User',
    strata.limits = strata.limits,
    input_grid = data.frame(
      'Lon' = Data_Geostat$Lon,
      'Lat' = Data_Geostat$Lat,
      'Area_km2' = rep(4, nrow(Data_Geostat))
    )
  )

 ## made in VAST_dataprep -- all_areas
# Extrapolation_List <-  readRDS("./data/RDSFILES/r345_extrap.rds") ## made in VAST_dataprep -- only has AK in three regions



RootDir <- paste0(getwd(),"/runs")
DataDir <- paste0( getwd(), "/data/" )
Data2Dir <- paste0( getwd(), "/data/" )
TmbDir <- system.file("executables", package <- "VAST")

Species <- "Anoplopoma fimbria"
n_x <- 50   # Specify number of stations (a.k.a. "knots")

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
    "_fine=",
    fine_scale,
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


