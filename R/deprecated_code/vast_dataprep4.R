# devtools::install_github("james-thorson/VAST", dependencies = F)
library(TMB)
library(dplyr)
library(VAST)
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
## Input my data - IPHC manual compilation with NPUE
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




Region="User"
# wbs.strata.limits <- data.frame(
#   'STRATA' = c("All_areas"), #c("WBS"), 
#   'west_border' = c(WBS_boundaries[1]),
#   'east_border' = c(WBS_boundaries[2]),
#   'north_border' = c(WBS_boundaries[4]),
#   'south_border' = c(WBS_boundaries[3]) )

strata.limits <- data.frame(
  'STRATA' = c('All_areas'),
  'west_border' = c(155),
  'east_border' = c(115),
  'north_border' = c(60),
  'south_border' = c(30)
)

# strata.limits <- data.frame('STRATA'="All_areas")
load("./data/Data_Set.rds")

Data_Geostat <- data.frame( "Catch_KG"= Data_Set$Sablefish_n3, 
                            "Year"=  Data_Set$YEAR, 
                            "Vessel"= rep("IPHC", nrow(Data_Set)), 
                            "AreaSwept_km2"= rep(1,nrow(Data_Set)), #*100 (i think this actually put it in hectares if * 100)
                            "Lat"= Data_Set$LATITUDE, 
                            "Lon"= Data_Set$LONGITUDE, 
                            "Pass"=0, 
                            "Stratum"= Data_Set$REG_BIG,
                            "Survey" = Data_Set$Station)
#western bering sea                                   
Other_extrap <- Prepare_User_Extrapolation_Data_Fn(Region=Region,
                                                strata.limits=strata.limits,
                                                observations_LL = Data_Set[,c("LONGITUDE","LATITUDE")],
                                                flip_around_dateline=TRUE,
                                                input_grid = data.frame('Lon' = Data_Geostat$Lon, 
                                                                        'Lat' = Data_Geostat$Lat, 
                                                                        'Area_km2' = rep(4, nrow(Data_Geostat))))
#eastern bering sea
# EBS_extrap = FishStatsUtils::make_extrapolation_info( Region="Eastern_Bering_Sea", 
#                                                       zone = zone,
#                                                       strata.limits=strata.limits  )
#northern bering sea
# NBS_extrap = FishStatsUtils::make_extrapolation_info( Region="Northern_Bering_Sea", 
#                                                       flip_around_dateline=TRUE,
#                                                       zone = zone, 
#                                                       strata.limits=strata.limits )
#comibine EBS,NBS,WBS
# Extrapolation_List = FishStatsUtils::combine_extrapolation_info( "EBS"=EBS_extrap, "NBS"=NBS_extrap, "WBS" = WBS_extrap)

# Add strata
# WBS_extrap$a_el = cbind( "All"=Extrapolation_List$a_el[,1], 
#                                  "EBS"=c(EBS_extrap$a_el[,1],rep(0,nrow(NBS_extrap$a_el)),rep(0,nrow(WBS_extrap$a_el))), 
#                                  "NBS"=c(rep(0,nrow(EBS_extrap$a_el)),NBS_extrap$a_el[,1],rep(0,nrow(WBS_extrap$a_el))), 
#                                  "WBS" =c(rep(0,nrow(EBS_extrap$a_el)),rep(0,nrow(NBS_extrap$a_el)),WBS_extrap$a_el[,1]) )

# Reduce to one column
Other_extrap$a_el$All_areas <- rep(4, nrow(Other_extrap$a_el))
colSums( Other_extrap$a_el ) #check area sums
saveRDS(Other_extrap, file = "./data/Other_extrap.rds")


