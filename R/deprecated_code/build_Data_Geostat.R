require(dplyr)


## Input my data - IPHC manual compilation with NPUE ----
# Data_Set <-  read.csv("./data/IPHC/manual_compile_2019-05-13.csv", na.strings = "#N/A") %>% sample_n(., 1000)
# Data_Set <- Data_Set[Data_Set$Longitude != "" & Data_Set$Latitude != ""  ,]
# ## reformat lat-lon
# Data_Set$LATITUDE <- as.numeric(gsub("\\s", ".", gsub("[.]","", gsub("'","",Data_Set$Latitude))))
# Data_Set$LONGITUDE <- as.numeric(gsub("\\s", ".", gsub("[.]","", gsub("'","",Data_Set$Longitude))))
# Data_Set$LONGITUDE[Data_Set$LONGITUDE > 0] <- Data_Set$LONGITUDE[Data_Set$LONGITUDE > 0]*-1
# ## Make a dummy column on Data_Set that can talk to the pre-set Extrapolation Grids
# Data_Set$REG_BIG <- with(Data_Set, ifelse(LATITUDE < 49, 'WC', 'AK'))
# # with(Data_Set, plot(Lat2 ~ Lon2))
# Data_descrip = "IPHC_Survey_NPUE" #data description
# Data_Set <- Data_Set %>% filter(LONGITUDE >= -180 & LONGITUDE <= 180)
# Data_Geostat <- data.frame( "Catch_KG"= Data_Set$Sablefish_n3, 
#                             "Year"=  Data_Set$YEAR, 
#                             "Vessel"= rep("IPHC", nrow(Data_Set)), 
#                             "AreaSwept_km2"= rep(1,nrow(Data_Set)), #*100 (i think this actually put it in hectares if * 100)
#                             "Lat"= Data_Set$LATITUDE, 
#                             "Lon"= Data_Set$LONGITUDE, 
#                             "Pass"=0, 
#                             "Stratum"= Data_Set$REG_BIG,
#                             "Survey" = Data_Set$Station)


## AK Race CPUE by HAUL ----
AKDS <- read.csv("./data/AK/race_cpue_by_haul.csv")  %>% filter( Starting.Longitude..dd. <=  0 & !is.na(Effort..km2.))
Data_Geostat <- data.frame( "Catch_KG"= AKDS$Weight..kg.,
                            "Year"=  AKDS$Year,
                            "Vessel"= AKDS$Vessel.Number,
                            "AreaSwept_km2"=  AKDS$Effort..km2., #*100 (i think this actually put it in hectares if * 100)
                            "Lat"= AKDS$Starting.Latitude..dd.,
                            "Lon"= AKDS$Starting.Longitude..dd.,
                            "Pass"=0,
                            "Stratum"= AKDS$Stratum.INPFC.Area,
                            "Survey" = AKDS$Survey,
                            "State" = rep('Alaska',nrow(AKDS)))
rm(AKDS)
saveRDS(Data_Geostat, file = "./data/data_geostat/Data_Geostat_AK.rds")


## AK RACE CPUE and BC StRs ----
# AKDS <- read.csv("./data/AK/race_cpue_by_haul.csv")  %>% filter(Starting.Longitude..dd. <=  0 & !is.na(Effort..km2.))
BCDS <- read.csv("./data/BC/BC_sable_survey_data.Aug262019.csv")  %>% filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & is.na(CPUE_SABLE_WEIGHT))
Data_Geostat <- data.frame( "Catch_KG"= BCDS$CPUE_SABLE_WEIGHT,
                            "Year"=  BCDS$SET_YEAR,
                            "Vessel"= BCDS$VESSEL_ID,
                            "AreaSwept_km2"=  BCDS$CPUE_TRAPS, #*100 (i think this actually put it in hectares if * 100)
                            "Lat"= BCDS$START_LATITUDE,
                            "Lon"= BCDS$START_LONGITUDE,
                            "Pass"=0,
                            "Stratum"= BCDS$SABLE_AREA_GROUP,
                            "Survey" = BCDS$SABLE_SET_TYPE,
                            "State" = rep('BritishColumbia',nrow(BCDS)))
rm(BCDS)
saveRDS(Data_Geostat, file = "./data/data_geostat/Data_Geostat_BC.rds")
