############################################################################################################################################################
#
### Example for running VAST with inputs for spatially-explicit categorical and continuous covariates
### Last updated: 7/10/2019 Dave McGowan, UW SAFS
#
############################################################################################################################################################

library(TMB)
library(VAST)
library(TMBhelper)
library(ThorsonUtilities)

############################################################################################################################################################
### Define Model structure
##########################
rm(list=ls())
HomeDir <- getwd()

### Input my data - *** DEMO: capelin data from AFSC Gulf of Alaska bottom trawl survey for 2001-2017 (odd years only), with in situ bottom temperatures ***
Data_Set <- read.csv("vast-examples/GOA BT survey 2001-2017_capelin.csv", header=TRUE)
Data_descrip = "goaBT2001-2017_capelin" #data description

### Define VAST version
#Version = get_latest_version(package = 'VAST') #demo is based on "VAST_v7_0_0"
Version = "VAST_v7_0_0"
  
##### *** A. Spatial settings: define the spatial resolution for the model, and whether to use a grid or mesh approximation ***
Method = c("Grid", "Mesh", "Spherical_mesh")[2]                        #mesh is default recommendation, number of knots need to be specified
grid_size_km = 25                                                      #Value only matters if Method="Grid"
n_x = 100                                                              #Number of "knots" used when Method="Mesh"
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )   #do not modify Kmeans set up

##### *** B. Model settings for each component of the model: ***
# FieldConfig: define if there is spatial (Omega) and spatio-temporal (Epsilon) variation
FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1)    #on, single factor 
# RhoConfig: determine if there is autocorrelation across time for intecepts (Beta) and spatio-temporal variation (Epsilon)
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)        #off
# OverdispersionConfig: define if there overdispersion due to vessel effects
OverdispersionConfig = c("Delta1"=0, "Delta2"=0)                       #off
# bias.correct: implement epsilon bias correction estimator for nonlinear transformation of random effects
bias.correct = FALSE
# ObsModel: define distribution for errors and which model to run
ObsModel = c(2,0) #Conventional delta-model using clog-log link for encounter prob. & gamma distribution w/ log link for positive catches

##### *** C. Specified outputs calculated after model runs - what reports to create? ***
Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1, "Calculate_evenness"=0,
             "Calculate_effective_area"=1, "Calculate_Cov_SE"=1, 'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

### Save files setting
setwd(HomeDir)  #Make sure that the working directory is back where it started
# Create folder to store records
(DateFile <- paste0(getwd(),'/VAST_output_',Data_descrip,'_Dist=',ObsModel[1],'_Link=',ObsModel[2],'_',Version,'/')) # 
dir.create(DateFile)

# Save all settings
Record = ThorsonUtilities::bundlelist( c("Data_Set","Version","Method","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=paste0(DateFile,"Record.txt"))

############################################################################################################################################################
### Preparing data for VAST
###########################
Data_Geostat = data.frame(Catch_KG = Data_Set$WTCPUE,            
                          AreaSwept_km2 = rep(1,nrow(Data_Set)), #set to 1 since CPUEs already standardized
                          Year = Data_Set$YEAR,
                          Year_Num = as.factor(Data_Set$YEAR),
                          Lat = Data_Set$LATITUDE,
                          Lon = Data_Set$LONGITUDE,
                          Station = Data_Set$STATION,
                          Vessel = Data_Set$VESSEL,      
                          fRegion = Data_Set$fRegion,     #Categorical factor for GOA subregions
                          BtmDepth = Data_Set$BOT_DEPTH,  #Continuous covariate
                          fBtmDepth = Data_Set$fBtmDepth, #Categorical factor for bottom depth
                          BtmTemp = Data_Set$BOT_TEMP     #Continuous covariate
                          )
levels(Data_Geostat$Year_Num) <- c(1:length(unique(Data_Set$YEAR)))

##### *** D. Extrapolation grid ***
# Region that tells software which grid to use
Region = "Gulf_of_Alaska"

# *** DEMO: how to include multiple strata within the extrapolation grid ***
# Extract strata boundaries by region - for 
GOA.sl = c(range(Data_Set$LONGITUDE), range(Data_Set$LATITUDE))
WGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="WGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="WGOA"]))
CGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="CGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="CGOA"]))
NGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="NGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="NGOA"]))
EGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="EGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="EGOA"]))
# Specify strata limits for GOA survey domain and 4 GOA subregions
strata.limits <- data.frame(
  'STRATA' = c("GOA","WGOA","CGOA","NGOA","EGOA"),
  'west_border' = c(GOA.sl[1], WGOA.sl[1], CGOA.sl[1], NGOA.sl[1], EGOA.sl[1]),
  'east_border' = c(GOA.sl[2], WGOA.sl[2], CGOA.sl[2], NGOA.sl[2], EGOA.sl[2]),
  'north_border' = c(GOA.sl[4], WGOA.sl[4], CGOA.sl[4], NGOA.sl[4], EGOA.sl[4]),
  'south_border' = c(GOA.sl[3], WGOA.sl[3], CGOA.sl[3], NGOA.sl[3], EGOA.sl[3]) )

Extrapolation_List = FishStatsUtils::make_extrapolation_info( Region=Region, strata.limits=strata.limits,
                                                              observations_LL = Data_Geostat[,c("Lat","Lon")] )
# If you want to limit extrapolation to a specified area around each knot:
#Extrapolation_List = FishStatsUtils::make_extrapolation_info( Region = "Other", strata.limits = strata.limits,
#                                                              observations_LL = Data_Geostat[,c("Lat","Lon")],
#                                                              maximum_distance_from_sample = 10) #specifies a 10 km radius around each knot

# Derived objects for spatio-temporal estimation
Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, 
                                                         Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'],
                                                         Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], 
                                                         nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]],
                                                         DirPath=DateFile, Save_Results=FALSE )

############################################################################################################################################################
### Formatting Habitat Covariate Data for VAST - constructing 'X_xtp'
###################################################################
# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, knot_x = Spatial_List$knot_i )
#Data_Geostat$aTmp = (Data_Geostat$BtmTemp - mean(Data_Geostat$BtmTemp)) / sd(Data_Geostat$BtmTemp)

### Organize covariate inputs
# Indices
knot.ls <- c(as.numeric(unique(Data_Geostat$knot_x)))
year.ls <- unique(Data_Geostat$Year)
n_t <- as.numeric(length(year.ls))   #number of years in series
n_p = 5 #Specify number of habitat covariates: 1 categorical (e.g. fBtmDepth) & 4 continuous (e.g. BtmTemp & BtmTemp^2, standardized BtmTemp & BtmTemp^2)

# Set up an empty array indexed by knot, year, & covariate. Will have 2 sets of covariates, one each for presence/absence & density submodels
X_xtp <- array(NA,dim=c(n_x,n_t,n_p))   

# Loop through Covariate matrices to assign covariate values to each spatial knot by year
for(t in 1:n_t){
  x.data <- Data_Geostat[Data_Geostat$Year==year.ls[t],]
  # Create matrix of covariate values averaged by spatial location (i.e. knot)
  Covar_x <- data.frame(knot_x = row.names(tapply(x.data$BtmTemp, x.data$knot_x, mean)), #index for spatial locations (i.e. knots)
                        BtmDepth_x = tapply(x.data$BtmDepth, x.data$knot_x, mean),       #mean bottom depth by knot - to create bottom depth category
                        BtmTemp_x = tapply(x.data$BtmTemp, x.data$knot_x, mean)          #mean bottom temp by knot
                        )
  # Categorize bottom depth for 2 levels
  Covar_x$fBtmDepth = ifelse(Covar_x$BtmDepth_x<100, 0, 1) #Bottom depth factor: 0 = <100m, 1 = >=100m
  ### Assign covariate values by spatial location (i.e. knot)
  for(x in 1:n_x){
    if(knot.ls[x]%in%unique(x.data$knot_x)){
      # Continuous covariates at all locations
      X_xtp[knot.ls[x],t,1] <- Covar_x$fBtmDepth[Covar_x$knot_x==knot.ls[x]]   #Bottom depth factor: 0 = <100m, 1 = >=100m
      X_xtp[knot.ls[x],t,2] <- Covar_x$BtmTemp_x[Covar_x$knot_x==knot.ls[x]]   #Bottom temp
      X_xtp[knot.ls[x],t,3] <- Covar_x$BtmTemp_x[Covar_x$knot_x==knot.ls[x]]^2 #Bottom temp squared (i.e. quadratic term)
      X_xtp[knot.ls[x],t,4] <- NA                                              #empty slot for standardized Bottom Temp
      X_xtp[knot.ls[x],t,5] <- NA                                              #empty slot for standardized Bottom Temp^2
    }
  }
  rm(x.data, Covar_x)
}

# Standarize bottom temp values
X_xtp[,,4] <- (X_xtp[,,2] - mean(X_xtp[,,2],na.rm=T)) / sd(X_xtp[,,2],na.rm=T)    #standardized Bottom Temp
X_xtp[,,5] <- (X_xtp[,,4]^2)                                                      #standardized Bottom Temp^2

############################################################################################################################################################
### Declare Model Inputs
###########################
### Specify Covariate array, either by excluding covariates from X_xtp or selecting covariates to turn off using Map
# Select covariates from X_xtp (here we select all of them so we can turn some off):
F.covar <- c(1,2,3,4,5)  #1=fBtmDepth; 2=BtmTemp; 3=BtmTemp^2; 4=sBtmTemp; 5=sBtmTemp^2
Map.covar1 <- c(2,3)     #Select covariates from F.covar based on vector element # to turn off in Encounter probability model - 0=all covars on
Map.covar2 <- c(2,3,4,5) #Select covariates from F.covar based on vector element # to turn off in Positive density model - 0=all covars on
# Subset X_xtp (in this example, we retain all covariates)
X_xtp.x <- X_xtp[,,F.covar,drop=FALSE]
# Convert NAs to 0 - these samples will not be included in likelihood estimate
X_xtp.x = ifelse( is.na(X_xtp.x), 0, X_xtp.x)

##### *** E. Generate base TMB data inputs ***
rm(TmbList, Obj, Opt, Report, Params, Map, Map.x) #Delete objects & outputs from previous model runs

Aniso = 0 #Specify either 0=assume isotropy or 1=assume geometric anisotropy
TmbData = VAST::make_data("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, "RhoConfig"=RhoConfig,
                          "ObsModel"=ObsModel, "c_iz"=rep(0,nrow(Data_Geostat)), "b_i"=Data_Geostat$Catch_KG, "a_i"=Data_Geostat$AreaSwept_km2,
                          "s_i"=Data_Geostat$knot_x - 1, "t_iz"=as.numeric(Data_Geostat$Year_Num), "a_xl"=Spatial_List$a_xl, "MeshList"=Spatial_List$MeshList, 
                          "GridList"=Spatial_List$GridList, "Method"=Spatial_List$Method, "Options"=Options, "X_xtp"=X_xtp.x, Aniso=Aniso ) 
names(TmbData)
### Manually generate Parameter & Map lists *** Necessary if you are turning off (i.e. Mapping) covariates in X_xtp
# Generate Parameter list
Params = VAST::Param_Fn( Version=Version, DataList=TmbData, RhoConfig=RhoConfig )
#set initial parameter values to 0 for covariates that will be turned off (identified by Map.covar1 & Map.covar2)
Params$gamma1_ctp[,,Map.covar1] <- 0 
Params$gamma2_ctp[,,Map.covar2] <- 0 
# Generate Map list
Map = VAST::Make_Map( "DataList"=TmbData, "TmbParams"=Params, "RhoConfig"=RhoConfig)
# Make a copy of Map
Map.x <- Map
# Turn off parameters identified by Map.covar1 & Map.covar2
# Single-species model
if(length(unique(TmbData$c_iz))==1){ 
  if(min(Map.covar1)!=0) levels(Map.x[["gamma1_ctp"]])[Map.covar1] = factor(NA); print(Map.x[["gamma1_ctp"]]);
  if(min(Map.covar2)!=0) levels(Map.x[["gamma2_ctp"]])[Map.covar2] = factor(NA); print(Map.x[["gamma2_ctp"]]);
}
# Multicategory model with 2 or more categories
if(length(unique(TmbData$c_iz))>1){ 
  if(min(Map.covar1)!=0) levels(Map[["gamma1_ctp"]])[c(TmbData$n_c*Map.covar1 - 1, TmbData$n_c*Map.covar1)] = factor(NA); print(Map.x[["gamma1_ctp"]]);
  if(min(Map.covar2)!=0) levels(Map.x[["gamma2_ctp"]])[c(TmbData$n_c**Map.covar2 - 1, TmbData$n_c*Map.covar2)] = factor(NA); print(Map.x[["gamma2_ctp"]]);
}

##### *** F. Make & Optimize model ***
# Specify if using restricted max. likelihood
USE_REML = FALSE 
### Build TMB object
TmbList = VAST::make_model("TmbData"=TmbData, "Version"=Version, "RhoConfig"=RhoConfig, "Method"=Method, Use_REML = USE_REML,
                           "loc_x"=Spatial_List$loc_x, "Parameters"=Params, "Map"=Map.x,  "RunDir"=DateFile)
Obj = TmbList[["Obj"]]

### Run optimizer with Newton steps to improve convergence
Opt <- TMBhelper::fit_tmb ( "obj"=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], "getsd"=TRUE, "newtonsteps"=1, "savedir"=DateFile, "bias.correct"=FALSE )                                                         

############################################################################################################################################################
### Model Outputs
##############################
### Preliminary Diagnostics
#Check to make sure this doesn't equal NaN
Obj$report()$jnll
# Check convergence via gradient (should be TRUE)
( Conv.grad <- all( abs(Opt$diagnostics[,'final_gradient'])<1e-6 ) )
# Check convergence via Hessian (should be TRUE)
( Conv.Hess <- all( eigen(Opt$SD$cov.fixed)$values>0 ) )

# Create the report
Report <- Obj$report()

# Save model outputs
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"_Save.RData"))
capture.output(Opt, file=paste0(DateFile,"_Param_est.txt"))

### Parameter Outputs
Par = Opt$par
StdErr = summary(Opt$SD)[(1+length(summary(Opt$SD))/2):((length(summary(Opt$SD))/2)+Opt$number_of_coefficients[2])]
Wald.Ztest <- (1-pnorm(abs(Par/StdErr),0,1))*2  #Z-test version - assumes normality -> pnorm(q,mean=0,sd=1)
round(cbind(Par, StdErr, Wald.Ztest), digits=4)
print(Opt$AIC)
# Calculate geostatistical range
round( sqrt(8) / exp(as.numeric(Opt$par['logkappa1'])), digits=1 ) #Submodel #1 = 325.6 km
round( sqrt(8) / exp(as.numeric(Opt$par['logkappa2'])), digits=1 ) #Submodel #2 = 134.3 km

############################################################################################################################################################

