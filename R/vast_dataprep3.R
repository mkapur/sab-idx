# devtools::install_github("james-thorson/VAST", dependencies = F)
library(TMB)
library(dplyr)
library(VAST)
library(TMBhelper)
library(ThorsonUtilities)
library(FishStatsUtils)

# 
### Input my data - IPHC manual compilation with NPUE
# Data_Set <-  read.csv("./data/manual_compile_2019-05-13.csv", na.strings = "#N/A") %>% sample_n(., 1000)
# Data_Set <- Data_Set[Data_Set$Longitude != "" & Data_Set$Latitude != ""  ,]
# ## reformat lat-lon
# Data_Set$LATITUDE <- as.numeric(gsub("\\s", ".", gsub("[.]","", gsub("'","",Data_Set$Latitude))))
# Data_Set$LONGITUDE <- as.numeric(gsub("\\s", ".", gsub("[.]","", gsub("'","",Data_Set$Longitude))))
# 
# Data_Set$LONGITUDE[Data_Set$LONGITUDE > 0] <- Data_Set$LONGITUDE[Data_Set$LONGITUDE > 0]*-1
# 
# ## Make a dummy column on Data_Set that can talk to the pre-set Extrapolation Grids
# Data_Set$REG_BIG <- with(Data_Set, ifelse(LATITUDE < 49, 'WC', 'AK'))
# # with(Data_Set, plot(Lat2 ~ Lon2))
# Data_descrip = "IPHC_Survey_NPUE" #data description
# 
# Data_Set <- Data_Set %>% filter(LONGITUDE >= -180 & LONGITUDE <= 180)
# save(Data_Set, file = "./data/Data_Set.rds")

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

## CC METHOD
Data_Geostat <- data.frame( "Catch_KG"= Data_Set$Sablefish_n3, 
                           "Year"=  Data_Set$YEAR, 
                           "Vessel"= rep("IPHC", nrow(Data_Set)), 
                           "AreaSwept_km2"= rep(1,nrow(Data_Set)), #*100 (i think this actually put it in hectares if * 100)
                           "Lat"= Data_Set$LATITUDE, 
                           "Lon"= Data_Set$LONGITUDE, 
                           "Pass"=0, 
                           "Stratum"= Data_Set$REG_BIG,
                           "Survey" = Data_Set$Station)

## MY METHOD
# Data_Geostat = data.frame(Catch_KG = Data_Set$Sablefish_n3,            
#                           AreaSwept_km2 = rep(1,nrow(Data_Set)), #set to 1 since CPUEs already standardized
#                           Year = Data_Set$YEAR,
#                           Year_Num = as.factor(Data_Set$YEAR),
#                           Lat = Data_Set$LATITUDE,
#                           Lon = Data_Set$LONGITUDE,
#                           Station = Data_Set$Station,
#                           Vessel = rep("IPHC", nrow(Data_Set)),      
#                           fRegion = Data_Set$REG_AREA,     #Categorical factor for GOA subregions
#                           BtmDepth = Data_Set$Depth..F.,  #Continuous covariate
#                           fBtmDepth = Data_Set$Depth..F., #Categorical factor for bottom depth
#                           BtmTemp = rep(NA, nrow(Data_Set))     #Continuous covariate
# )
# levels(Data_Geostat$Year_Num) <- c(1:length(unique(Data_Set$YEAR)))

##### *** D. Extrapolation grid *** -- MAKE CUSTOM
strata.limits <- data.frame(
  'STRATA' = c('NEP'),
  'west_border' = c(155),
  'east_border' = c(115),
  'north_border' = c(60),
  'south_border' = c(30)
)
gc()
obsLL <- data.frame('Lon' = Data_Geostat$Lon, 'Lat' = Data_Geostat$Lat, 
                    'Area_km2' = rep(4, nrow(Data_Geostat)), 'OBS' = Data_Geostat$Catch_KG) 
Other_extrap <- make_extrapolation_info(
  Region = "user",
  strata.limits = strata.limits,
  observations_LL = obsLL,
  grid_dim_ll = c(0.1,0.1),
  input_grid = obsLL,
  # zone = 32,
  flip_around_dateline = F,
  Area_km2_x = rep(1, length(obsLL))
)

Other_extrap$a_el = cbind(
  "All" = Other_extrap$a_el[, 1],
  "EBS" = c(EBS_extrap$a_el[, 1], rep(0, nrow(NBS_extrap$a_el)), rep(0, nrow(WBS_extrap$a_el))),
  "NBS" = c(rep(0, nrow(EBS_extrap$a_el)), NBS_extrap$a_el[, 1], rep(0, nrow(WBS_extrap$a_el))),
  "WBS" = c(rep(0, nrow(EBS_extrap$a_el)), rep(0, nrow(NBS_extrap$a_el)), WBS_extrap$a_el[, 1])
)


# Reduce to one column
colSums( Other_extrap$a_el ) #check area sums
saveRDS(Other_extrap, file = paste0("./data/Other_extrap.rds"))





