# devtools::install_github("james-thorson/VAST", dependencies = T)
library(TMB)
library(dplyr)
library(VAST)
library(TMBhelper)
library(ThorsonUtilities)
library(FishStatsUtils)


Extrapolation_List <-  readRDS("./data/Other_extrap.rds") ## made in VAST_dataprep

#Data
Data_Geostat = data.frame( "Catch_KG"=all_strata[,'WEIGHT'], 
                           "Year"=all_strata[,'YEAR'], 
                           "Vessel"=all_strata[,'VESSEL'], 
                           "AreaSwept_km2"=all_strata[,'AreaSwept_km2'], #*100 (i think this actually put it in hectares if * 100)
                           "Lat"=all_strata[,'START_LATITUDE'], 
                           "Lon"=all_strata[,'START_LONGITUDE'], 
                           "Pass"=0, 
                           "Stratum"=all_strata[,'unique_stratum'],
                           "Survey" = all_strata[,'Survey'])

RootDir = getwd()
DataDir = paste0( RootDir, "data/" )
Data2Dir = paste0( RootDir, "data/" )
TmbDir = system.file("executables", package = "VAST")

Species = c("Gadus chalcogrammus", "Gadus macrocephalus")[1]
n_x = 100   # Specify number of stations (a.k.a. "knots")

Date = Sys.Date()
DateFile = paste0(RootDir,Date,"_",Species,"_nx=",n_x,"/")
dir.create(DateFile)

Version = "VAST_v8_0_0" # get_latest_version( package="VAST" )
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25

FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=4, "Epsilon2"=4)
#FieldConfig = matrix( c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0), nrow=3, byrow=TRUE )
#RhoConfig = c("Beta1"=1, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)

OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
if( Species=="Gadus chalcogrammus" ) ObsModel = c(2,0)
if( Species=="Gadus macrocephalus" ) ObsModel = c(2,3)
#Covariate = c("None", "Bottom", "BotVar", "Cold_pool")[4]
Aniso = FALSE
fine_scale = FALSE
Options =  c("Calculate_Range"=FALSE, "Calculate_effective_area"=FALSE, "SD_site_logdensity"=FALSE)

# Default
strata.limits <- data.frame('STRATA'="All_areas")


# Run directory
RunDir = paste0(DateFile,"Obs=",paste0(ObsModel,collapse=""),"_Field=",paste0(FieldConfig,collapse=""),"_Rho=",paste0(RhoConfig,collapse=""),"_fine=",fine_scale,"/")
dir.create(RunDir)

#"Covariate",
Record = ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Aniso","fine_scale","Options") )
save( Record, file=file.path(RunDir,"Record.RData"))
capture.output( Record, file=paste0(RunDir,"Record.txt"))

################


# CC METHOD
Spatial_List = FishStatsUtils::make_spatial_info( grid_size_km=grid_size_km, 
                                                  n_x=n_x, 
                                                  fine_scale=fine_scale, 
                                                  Method=Method, 
                                                  Lon_i = Data_Geostat[,'Lon'], 
                                                  Lat_i =Data_Geostat[,'Lat'], 
                                                  LON_intensity=Extrapolation_List$Data_Extrap[which(Extrapolation_List$Data_Extrap[,'Include']==TRUE),'Lon'], 
                                                  LAT_intensity=Extrapolation_List$Data_Extrap[which(Extrapolation_List$Data_Extrap[,'Include']==TRUE),'Lat'],
                                                  Extrapolation_List=Extrapolation_List, DirPath=RunDir, Save_Results=TRUE )



## MY METHOD
# Spatial_List <- FishStatsUtils::make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method,
#                                                    Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'],
#                                                    Extrapolation_List= Other_extrap, randomseed=Kmeans_Config[["randomseed"]],
#                                                    nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]],
#                                                    DirPath=DateFile, Save_Results=FALSE )

Run2Dir = paste0(RunDir,"_","_BiasCorr=FALSE/")
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

TmbList <- make_model("build_model"=TRUE, "TmbData"=TmbData, "RunDir"=RootDir, "Version"=Version, "RhoConfig"=RhoConfig, "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=TmbDir)
Obj <- TmbList[["Obj"]]
Opt <- TMBhelper::fit_tmb( obj=Obj, getsd=TRUE, savedir=Run2Dir, getReportCovariance=FALSE, bias.correct=FALSE,
                          newtonsteps=1, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=1, vars_to_correct="Index_cyl") )
Report <- Obj$report()
Save <- list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(Run2Dir,"Save.RData"))


