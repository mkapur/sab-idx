# devtools::install_github("james-thorson/VAST", dependencies = F)
library(TMB)
library(VAST)
library(TMBhelper)
library(ThorsonUtilities)
library(FishStatsUtils)
############################################################################################################################################################
### Define Model structure
##########################
rm(list=ls())
# setwd("C:/Users/McGowanD/Desktop/R examples/VAST inputs demo/")
# HomeDir <- getwd()

### Input my data - *** DEMO: capelin data from AFSC Gulf of Alaska bottom trawl survey for 2001-2017 (odd years only), with in situ bottom temperatures ***
Data_Set <-  read.csv("./data/manual_compile_2019-05-13.csv", na.strings = "#N/A") %>% sample_n(., 1000)
Data_Set <- Data_Set[Data_Set$Longitude != "" & Data_Set$Latitude != ""  ,]
## reformat lat-lon
Data_Set$LATITUDE <- as.numeric(gsub("\\s", ".", gsub("[.]","", gsub("'","",Data_Set$Latitude))))
Data_Set$LONGITUDE <- as.numeric(gsub("\\s", ".", gsub("[.]","", gsub("'","",Data_Set$Longitude))))

Data_Set$LONGITUDE[Data_Set$LONGITUDE > 0] <- Data_Set$LONGITUDE[Data_Set$LONGITUDE > 0]*-1

## Make a dummy column on Data_Set that can talk to the pre-set Extrapolation Grids
Data_Set$REG_BIG <- with(Data_Set, ifelse(LATITUDE < 49, 'WC', 'AK'))
# with(Data_Set, plot(Lat2 ~ Lon2))
Data_descrip = "IPHC_Survey_NPUE" #data description

Data_Set <- Data_Set %>% filter(LONGITUDE >= -180 & LONGITUDE <= 180)
### Define VAST version
#Version = get_latest_version(package = 'VAST') #demo is based on "VAST_v7_0_0"
Version = "VAST_v7_0_0"
  
##### *** A. Spatial settings: define the spatial resolution for the model, and whether to use a grid or mesh approximation ***
Method = c("Grid", "Mesh", "Spherical_mesh")[2]                        #mesh is default recommendation, number of knots need to be specified
grid_size_km = 25                                                      #Value only matters if Method="Grid"
n_x = 50                                                              #Number of "knots" used when Method="Mesh"
Kmeans_Config = list( "randomseed"=1, "nstart"=50, "iter.max"=1e3 )   #do not modify Kmeans set up

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
  ObsModel = c(1,0) #Conventional delta-model using clog-log link for encounter prob. & gamma distribution w/ log link for positive catches

##### *** C. Specified outputs calculated after model runs - what reports to create? ***
Options =  c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1, "Calculate_evenness"=0,
             "Calculate_effective_area"=1, "Calculate_Cov_SE"=1, 'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

## Save files setting
# setwd(HomeDir)  #Make sure that the working directory is back where it started
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
Data_Geostat = data.frame(Catch_KG = Data_Set$Sablefish_n3,            
                          AreaSwept_km2 = rep(1,nrow(Data_Set)), #set to 1 since CPUEs already standardized
                          Year = Data_Set$YEAR,
                          Year_Num = as.factor(Data_Set$YEAR),
                          Lat = Data_Set$LATITUDE,
                          Lon = Data_Set$LONGITUDE,
                          Station = Data_Set$Station,
                          Vessel = rep("IPHC", nrow(Data_Set)),      
                          fRegion = Data_Set$REG_AREA,     #Categorical factor for GOA subregions
                          BtmDepth = Data_Set$Depth..F.,  #Continuous covariate
                          fBtmDepth = Data_Set$Depth..F., #Categorical factor for bottom depth
                          BtmTemp = rep(NA, nrow(Data_Set))     #Continuous covariate
                          )
levels(Data_Geostat$Year_Num) <- c(1:length(unique(Data_Set$YEAR)))

##### *** D. Extrapolation grid *** -- MAKE CUSTOM
# Region that tells software which grid to use
## thanks Jim
comblist <- list(); strata.limits <- data.frame('STRATA'="All_areas")
# if('AK' %in% Data_Set$REG_BIG){
  # Extract strata boundaries by region - for 
  # GOA.sl = c(range(Data_Set$LONGITUDE), range(Data_Set$LATITUDE))
  # WGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="WGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="WGOA"]))
  # CGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="CGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="CGOA"]))
  # NGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="NGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="NGOA"]))
  # EGOA.sl = c(range(Data_Set$LONGITUDE[Data_Set$fRegion=="EGOA"]), range(Data_Set$LATITUDE[Data_Set$fRegion=="EGOA"]))
  # Specify strata limits for GOA survey domain and 4 GOA subregions
  # strata.limits <- data.frame(
  #   'STRATA' = c("GOA","WGOA","CGOA","NGOA","EGOA"),
  #   'west_border' = c(GOA.sl[1], WGOA.sl[1], CGOA.sl[1], NGOA.sl[1], EGOA.sl[1]),
  #   'east_border' = c(GOA.sl[2], WGOA.sl[2], CGOA.sl[2], NGOA.sl[2], EGOA.sl[2]),
  #   'north_border' = c(GOA.sl[4], WGOA.sl[4], CGOA.sl[4], NGOA.sl[4], EGOA.sl[4]),
  #   'south_border' = c(GOA.sl[3], WGOA.sl[3], CGOA.sl[3], NGOA.sl[3], EGOA.sl[3]) )
  strata.limits <- data.frame(
    'STRATA' = c('NEP'),
    'west_border' = c(155),
    'east_border' = c( 115),
    'north_border' = c(60),
    'south_border' = c(30) )
#   EBS_extrap = make_extrapolation_info(
#     Region = "Eastern_Bering_Sea",
#     strata.limits = strata.limits,
#     zone = 32,
#     flip_around_dateline = F
#   )
#   append(comblist, EBS_extrap)
# }
# if('BC' %in% Data_Set$REG_BIG){
#   BC_extrap = make_extrapolation_info(
#     Region = "british_columbia",
#     strata.limits = NA,
#     zone = 32,
#     flip_around_dateline = F
#   )
#   BC_extrap$Data_Extrap$Area_km2 <- BC_extrap$Area_km2_x
#   names(BC_extrap$a_el) <- "All_areas"
# }
# if('WC' %in% Data_Set$REG_BIG){
#   CC_extrap = make_extrapolation_info(
#     Region = "california_current",
#     strata.limits = strata.limits,
#     zone = 32,
#     flip_around_dateline = F
#   )
#   CC_extrap$Data_Extrap$Area_km2 <- CC_extrap$Area_km2_x
# }
# 
# if('WC' %in% REG & 'BC' %in% REG & 'AK' %in% REG){
#   Extrapolation_List <- combine_extrapolation_info("EBS" = EBS_extrap,
#                                                    "NBS" = NBS_extrap,
#                                                    "GOA" = GOA_extrap,
#                                                    "BC" = BC_extrap,
#                                                    "CC" = CC_extrap)
# }
# if('WC' %in% REG & 'AK' %in% REG){
#   Extrapolation_List <- combine_extrapolation_info(
#     "AK" = EBS_extrap,
#     "CC" = CC_extrap)
# }
# 
# if('AK' %in% REG){
#   Extrapolation_List <- EBS_extrap                                          
# }
# 
# if('WC' %in% REG){
#   Extrapolation_List <- CC_extrap
# }
# 
# if('BC' %in% REG){
#   Extrapolation_List <- BC_extrap
# }
  
  # EBS_extrap = make_extrapolation_info( Region = "Eastern_Bering_Sea", strata.limits = strata.limits, zone = 32, flip_around_dateline = F )
  # NBS_extrap = make_extrapolation_info( Region = "Northern_Bering_Sea", strata.limits = strata.limits, zone = 32, flip_around_dateline = F )
  # GOA_extrap = make_extrapolation_info( Region = "gulf_of_alaska", strata.limits = strata.limits, zone = 32, flip_around_dateline = T )
  # BC_extrap = make_extrapolation_info( Region = "british_columbia", strata.limits = strata.limits, zone = 32, flip_around_dateline = F )
  # BC_extrap$Data_Extrap$Area_km2 <- BC_extrap$Area_km2_x
  # names(BC_extrap$a_el) <- "All_areas"
  # CC_extrap = make_extrapolation_info( Region = "california_current", 
                                       # strata.limits = strata.limits, zone = 32, flip_around_dateline = TRUE )
  # CC_extrap$Data_Extrap$Area_km2 <- CC_extrap$Area_km2_x
  
  obsLL <- data.frame('Lon' = Data_Set$LONGITUDE, 'Lat' = Data_Set$LATITUDE) 
  
    Other_extrap <- make_extrapolation_info(
      Region = "other",
      strata.limits = strata.limits,
      observations_LL = obsLL,
      grid_dim_ll = c(0.1,0.1),
      # zone = 32,
      flip_around_dateline = F
    )
  
  
  # Extrapolation_List = combine_extrapolation_info(
  #   "EBS" = EBS_extrap,
  #   "NBS" = NBS_extrap,
  #   "GOA" = GOA_extrap,
  #  "BC" = BC_extrap,
  #   "CC" = CC_extrap
  # )
  # rm(EBS_extrap);rm(NBS_extrap);rm(GOA_extrap);rm(CC_extrap);rm(BC_extrap); rm(Data_Set)
  # rm(northern_bering_sea_grid); rm(eastern_bering_sea_grid); rm(gulf_of_alaska_grid); rm(gulf_of_mexico_grid); rm(california_current_grid)
# Region = "Gulf_of_Alaska"

# Extrapolation_List <- combine_extrapolation_info("EBS" = EBS_extrap,
#                                                  "NBS" = NBS_extrap,
#                                                  "GOA" = GOA_extrap,
#                                                  "BC" = BC_extrap,
#                                                  "CC" = CC_extrap)

# *** DEMO: how to include multiple strata within the extrapolation grid ***


# Extrapolation_List = FishStatsUtils::make_extrapolation_info( Region=c('eastern_bering_sea','california_current'),
#                                                               strata.limits=strata.limits,
#                                                               observations_LL = Data_Geostat[,c("LATITUDE","LONGITUDE")] )
# If you want to limit extrapolation to a specified area around each knot:
#Extrapolation_List = FishStatsUtils::make_extrapolation_info( Region = "Other", strata.limits = strata.limits,
#                                                              observations_LL = Data_Geostat[,c("Lat","Lon")],
#                                                              maximum_distance_from_sample = 10) #specifies a 10 km radius around each knot

# Derived objects for spatio-temporal estimation
# gc() ## from cecilia to deal w memory issues

Spatial_List <- FishStatsUtils::make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method,
                                                         Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'],
                                                         Extrapolation_List= Other_extrap, randomseed=Kmeans_Config[["randomseed"]],
                                                         nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]],
                                                         DirPath=DateFile, Save_Results=FALSE )

############################################################################################################################################################
### Formatting Habitat Covariate Data for VAST - constructing 'X_xtp'
###################################################################
# Add knots to Data_Geostat
Data_Geostat = cbind( Data_Geostat, knot_x = Spatial_List$knot_i )%>% mutate(BtmTemp = 0, BtmDepth = ifelse(is.na(BtmDepth),0, BtmDepth))
#Data_Geostat$aTmp = (Data_Geostat$BtmTemp - mean(Data_Geostat$BtmTemp)) / sd(Data_Geostat$BtmTemp)

# ### Organize covariate inputs
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
#   # Categorize bottom depth for 2 levels
  Covar_x$fBtmDepth = ifelse(Covar_x$BtmDepth_x<100, 0, 1) #Bottom depth factor: 0 = <100m, 1 = >=100m
  ## Assign covariate values by spatial location (i.e. knot)
  for(x in 1:n_x){
    if(knot.ls[x]%in%unique(x.data$knot_x)){
#       # Continuous covariates at all locations
      X_xtp[knot.ls[x],t,1] <- Covar_x$fBtmDepth[Covar_x$knot_x==knot.ls[x]]   #Bottom depth factor: 0 = <100m, 1 = >=100m
      X_xtp[knot.ls[x],t,2] <- Covar_x$BtmTemp_x[Covar_x$knot_x==knot.ls[x]]   #Bottom temp
      X_xtp[knot.ls[x],t,3] <- Covar_x$BtmTemp_x[Covar_x$knot_x==knot.ls[x]]^2 #Bottom temp squared (i.e. quadratic term)
      X_xtp[knot.ls[x],t,4] <- NA                                              #empty slot for standardized Bottom Temp
      X_xtp[knot.ls[x],t,5] <- NA                                              #empty slot for standardized Bottom Temp^2
    }
  }
  rm(x.data, Covar_x)
}
# 
# # Standarize bottom temp values
X_xtp[,,4] <- (X_xtp[,,2] - mean(X_xtp[,,2],na.rm=T)) / sd(X_xtp[,,2],na.rm=T)    #standardized Bottom Temp
X_xtp[,,5] <- (X_xtp[,,4]^2)                                                      #standardized Bottom Temp^2

############################################################################################################################################################
### Declare Model Inputs
###########################
### Specify Covariate array, either by excluding covariates from X_xtp or selecting covariates to turn off using Map
# Select covariates from X_xtp (here we select all of them so we can turn some off):
F.covar <- c(1,2,3,4,5)  #1=fBtmDepth; 2=BtmTemp; 3=BtmTemp^2; 4=sBtmTemp; 5=sBtmTemp^2
Map.covar1 <- c(2,3)     #Select covariates from F.covar based on vector element # to turn off in Encounter probability model - 0=all covars on
Map.covar2 <- c(2,3) #Select covariates from F.covar based on vector element # to turn off in Positive density model - 0=all covars on
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
                          "GridList"=Spatial_List$GridList, X_xtp = X_xtp.x, "Method"=Spatial_List$Method, "Options"=Options,  Aniso=Aniso ) 

### Manually generate Parameter & Map lists *** Necessary if you are turning off (i.e. Mapping) covariates in X_xtp
Params = VAST::Param_Fn( Version=Version, DataList=TmbData, RhoConfig=RhoConfig )
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
### Build TMB object ##MAKE_MODEL is new version
TmbList = VAST::make_model("TmbData"=TmbData, "Version"=Version, "RhoConfig"=RhoConfig, "Method"=Method, Use_REML = USE_REML,
                           "loc_x"=Spatial_List$loc_x, "Parameters"=Params, "Map"=Map.x,  "RunDir"=DateFile)
Obj = TmbList[["Obj"]]

### Run optimizer with Newton steps to improve convergence
Opt <- TMBhelper::fit_tmb ( "obj"=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], "getsd"=TRUE, "newtonsteps"=3, "savedir"=DateFile, "bias.correct"=FALSE )                                                         

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
## Plot data

plot_data(Extrapolation_List=Other_extrap, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )


## Convergence
pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 

## Diagnostics for encounter-probability component

Enc_prob = plot_encounter_diagnostic( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)


## Diagnostics for positive-catch-rate component

Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, FileName_PP="Posterior_Predictive",
                              FileName_Phist="Posterior_Predictive-Histogram", 
                              FileName_QQ="Q-Q_plot", FileName_Qhist="Q-Q_hist", DateFile=DateFile )

## Diagnostics for plotting residuals on a map

# Get region-specific settings for plots
MapDetails_List = make_map_info( "Region"=Region,
                                "NN_Extrap"=Spatial_List$PolygonList$NN_Extra,
                                 "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)


## Model selection

# Model output


## Direction of "geometric anisotropy"
plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

## Density surface for each year

Dens_xt = plot_maps(plot_set=c(2), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )



## Index of abundance


Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, 
                            Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, 
                            use_biascorr=TRUE )
pander::pandoc.table( Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] ) 

## Center of gravity and range expansion/contraction

plot_range_index(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)
