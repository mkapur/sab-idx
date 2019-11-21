## MKAPUR MOD FROM J THORSON "RUN_COMBINED_MODEL_2019_09_04"
# Use development branch
# remotes::install_github("james-thorson/FishStatsUtils", ref="development")
# remotes::install_github("james-thorson/VAST", ref="development")
## dev @ Sep 19 2019
# remotes::install_github("james-thorson/VAST",
#                         ref="15c7a0ba843bd199670afe71c6cb9907078cf431")


#Libraries
library(VAST)
library(TMB)
library(dplyr)
library(tidyr)
# library(plyr)
library(reshape)
library(mapdata)
library(ggplot2)

# Directories ----
RootFile <- paste0(getwd(),"/runs/") 
DataFile  <- paste0( getwd(), "/data/" ) #paste0( RootFile,"Data/")

# Resolution
n_x <- 250 # Number of stations

# Choose species
Species <- "Anoplopoma fimbria"
Species_code <- 'SAB' # switch(Species, "arrowtooth flounder"="arrow", "Pacific ocean perch"="POP")
Surveys_to_include = c("Triennial", "WCGBTS", "BCs", "BCt", "GOA", "EBS") [c(1,2,4,5,6)] #This will only work for years after 2003


# Date
Date <- Sys.Date()
DateFile <- paste0(RootFile,Date,"_nx=",n_x,"_Species=",Species_code,"_", 
                   paste0(Surveys_to_include, collapse = "_"),"/")
dir.create(DateFile)


## Settings & Options ----
# if( file.exists(paste0(DateFile,"Record.RData")) ){
#   load( file=paste0(DateFile,"Record.RData"))
#   attach( Record )
# }else{
  # Inputs
  # Surveys_to_include = c("WCGBTS", "Triennial", "BC", "GOA", "EBS")[c(1:5)] #This will only work for years after 2003
  
  # settings
  
  FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, 
                          "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
  Aniso = FALSE
  Version = "VAST_v8_0_0" # get_latest_version( package="VAST" )
  OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
  # FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence
  # ObsModel = c(1,0) ## lognormal for catch, pos only for enctr  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  ObsModel <- c(2,0) ## gamma for catch, pos only for enctr  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  Spatial_Smoother = c("Index", "Smoothed_Index", "Spatiotemporal", "Spatiotemporal_AR")[3]
  BC_catchability = c("Separate")[1]
  BiasCorr = c(FALSE,TRUE)[2]
  Zone = 5
  # Options =  c("Calculate_Range"=1, "Calculate_evenness"=1, "Calculate_effective_area"=1)
  Options =  c("Calculate_Range"=FALSE, "Calculate_effective_area"=FALSE, "SD_site_logdensity"=FALSE)
  BaseQ = c("GOA_late", "WCGBTS")[1]
  Use_REML = TRUE
  fine_scale = TRUE
  create_strata_per_region = TRUE
  Method = c("Grid", "Mesh", "Spherical_mesh")[2]
  grid_size_km = 25
  # Year_Range = c(2004, 2018)
  Year_Range = c(1975, 2018)
  
  #Year_Range = c(-Inf, Inf) #c(1983, 2014)
  
  # Derived - NOTE THAT RhoConfig[1:2] must be 0 when using ObsModel[2]=3:  Other options are not coded to work together
  if( Spatial_Smoother=="Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)
  if( Spatial_Smoother=="Smoothed_Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=2, "Epsilon2"=2)
  if( Spatial_Smoother=="Spatiotemporal" ) RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2) # Pointwise random walk (epsilon is RW, Beta1 is constant for all years)
  if( Spatial_Smoother=="Spatiotemporal_AR" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=3, "Epsilon2"=3) # Pointwise autocorrelation (beta is freely estimated in each year)
  
  # Save options for future records
  # Record = ThorsonUtilities::bundlelist( c("Surveys_to_include","n_x","FieldConfig","RhoConfig","ObsModel",
  #                                          "Spatial_Smoother","BC_catchability","BiasCorr","Zone","Options","BaseQ","Use_REML","fine_scale",
  #                                          "Year_Range","create_strata_per_region") )
  # capture.output( Record, file=paste0(DateFile,"Record.txt"))
  Record <- ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","BC_catchability","BaseQ","Use_REML","fine_scale",
                                            "FieldConfig","RhoConfig","OverdispersionConfig", "Year_Range",
                                            "ObsModel","Aniso","fine_scale","Options", "create_strata_per_region") )
  
  save( Record, file=paste0(DateFile,"Record.RData"))
# }

## Build Data_CPUE ----
Data_CPUE <- NULL
ZeroMethod <- "Fast"

# Load shelf-slope
## this also appears to already have zeros in catch wt
if( "WCGBTS" %in% Surveys_to_include ){
  WCGBTS <-  read.csv( paste0(DataFile,"Catch__NWFSC.Combo_2019-03-15.csv")) #read.csv( paste0(DataFile,"SurveyHaulAndCatchData03To14--HaulFishCatchData03To14.csv"), skip=8, header=TRUE)
  # WCGBTS_b <-  read.csv( paste0(DataFile,"Catch__NWFSC.Combo_2019-03-15.csv")) #read.csv( paste0(DataFile,"SurveyHaulAndCatchData03To14--HaulData03To14.csv"), skip=8, header=TRUE)
  # WCGBTS_b <-  cbind( WCGBTS_b, "AreaSwept_km2"=WCGBTS_b[,"Area_Swept_ha"]*0.01) #WCGBTS_b[,'Distance.Trawled..km.']*WCGBTS_b[,'Trawl.Net.Width..m.']/1000)
  # WCGBTS <-  cbind( WCGBTS, WCGBTS_b[match(WCGBTS[,'Trawl_id'],WCGBTS_b[,'Trawl_id']),
  # c('Vessel','Latitude_dd','Longitude_dd','AreaSwept_km2')] )
  WCGBTS$AreaSwept_km2 <- WCGBTS[,"Area_Swept_ha"]*0.01
  # WCGBTS[,'Project']  <-   as.numeric( substr(WCGBTS[,'Project'],start=7,stop=11))
  Data1 <- WCGBTS[,c('Trawl_id','Year','Latitude_dd','Longitude_dd','AreaSwept_km2','total_catch_wt_kg')]
  ## for some reason this is replacing all values with zero.
  # Data1 <-   FishData::add_missing_zeros( data_frame=WCGBTS, Method=ZeroMethod,
  #                                         if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'),
  #                                         unique_sample_ID_colname='Trawl_id', sample_colname='total_catch_wt_kg',
  #                                         species_subset=Species, species_colname="Common_name")
  # 
  # Data1 %>% sample_n(.,250) %>% 
  #   complete(total_catch_wt_kg, nesting(Trawl_id), 
  #            fill = list(total_catch_wt_kg = 0.0))
  rm(WCGBTS)
  Data1 <-  ThorsonUtilities::rename_columns( Data1[,c('Year','Latitude_dd','Longitude_dd','AreaSwept_km2','total_catch_wt_kg')], 
                                              newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data1 <-  cbind("Survey"="WCGBTS", "Region"="CC", Data1)
  
  # Data1 %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat)))
  Data1 %>%
  mutate(CZERO = Catch_KG == 0) %>%
    ggplot(., aes(x = Lat, y = Lon, color = CZERO))  + geom_point() + facet_wrap(~Year)
  
  Data_CPUE <- rbind( Data_CPUE, Data1 )
  rm(Data1)
}

# Load triennial
# Has some problem with multiple replicated samples
## There ARE zeroes here already...
if( "Triennial" %in% Surveys_to_include ){
  # ThorsonUtilities::LoadFn( paste0(DataFile,"Tri_Shelf_and_AFSC_Slope_survey_bio.All.Sp 9 June 2015.dmp"))
  ThorsonUtilities::LoadFn( paste0(DataFile,"Catch__Triennial_2019-03-15.Rda"))
  Data2 <- Out
  rm(Out)
  # Triennial <- Out %>% select("Trawl_id",'Year','Latitude_dd','Longitude_dd','Area_Swept_ha','total_catch_wt_kg',"Common_name")
  # Triennial <- Data$Lengths #Tri_Shelf_and_AFSC_Slope_survey_bio.All.Sp$Lengths
  # Data2 <- FishData::add_missing_zeros( data_frame=Triennial, Method=ZeroMethod, if_multiple_records='Combine',
  #                                      unique_sample_ID_colname='Trawl_id', sample_colname='total_catch_wt_kg',
  #                                      species_subset=Species, species_colname="Common_name" )
  Data2 <- cbind(Data2, "AreaSwept_km2"= Data2[,"Area_Swept_ha"]*0.01) #Data2[,'DISTANCE_FISHED']*Data2[,'NET_WIDTH']/1000 )
  Data2 <- ThorsonUtilities::rename_columns( Data2[,c('Year','Latitude_dd','Longitude_dd','AreaSwept_km2','total_catch_wt_kg')], newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  # Data2 <- cbind("Survey"="Triennial", "Region"="CC", Data2)
  Data2 %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat)))
  Data2 %>% 
    mutate(CZERO = Catch_KG == 0) %>%
    ggplot(., aes(x = Lat, y = Lon, color = CZERO)) + geom_point() + facet_wrap(~Year)
  
  ## Create timeblocking
  Triennial_late <- subset(Data2, Year > 1995) %>% mutate(Survey = 'Triennial_late', Region = "CC")
  Triennial_early <- subset(Data2, Year <= 1995) %>% mutate(Survey = 'Triennial_early', Region = "CC")
  
  
  Data_CPUE = rbind( Data_CPUE, Triennial_late,  Triennial_early)
  rm(Data2)
}

# Load BC catches
if( "BCs" %in% Surveys_to_include ){
  # Exclude PCOD monitoring survey, which is non-random
  # SpeciesCode = switch( Species, "arrowtooth flounder"='ARF_KG', "Pacific ocean perch"='POP_KG' )
  BCs <- read.csv(paste0(DataFile,"/BC/BC_sable_survey_data.Aug262019.csv"))  %>% 
    filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & !is.na(TOTAL_SABLE_WEIGHT) & 
             SABLE_SET_TYPE == 'StRS') %>%
    mutate(AreaSwept_km2=CPUE_TRAPS,
           TRIP_ID2 = paste(SET_YEAR,START_LATITUDE, START_LONGITUDE))

  
  Data3 <- ThorsonUtilities::rename_columns( BCs[,c("TRIP_ID2", 'SABLE_SET_TYPE','SET_YEAR','START_LATITUDE','START_LONGITUDE','AreaSwept_km2',"TOTAL_SABLE_WEIGHT")],
                                             newname=c("TRIP_ID", 'Survey','Year','Lat','Lon','AreaSwept_km2','Catch_KG')) %>%
    complete(Year,nesting(TRIP_ID,Lat,Lon),
             fill = list(Catch_KG = 0.0)) %>%
    mutate(Survey = 'BC_StRS', Region = 'BC') %>% select(-TRIP_ID)
  
  
  # Data3 %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat)))
  
  
  # Data3 %>% 
  #   mutate(CZERO = (Catch_KG == 0))%>%
  #   ggplot(., aes(x = Lat, y = Lon, color = CZERO)) + geom_point() + facet_wrap(~Year)
  
  # Data3[,'Survey'] = 'BCs' #droplevels(Data3[,'Survey'])
  # Data3 <- cbind(Data3, "Region"="BC")  
  # Data3$Lon = -1*Data3$Lon ## I think this was wrong before but no longer. Should be negative.
  Data_CPUE <- rbind( Data_CPUE, Data3 )
  rm(Data3);  rm(BCs)
}

if( "BCt" %in% Surveys_to_include ){
  # Exclude PCOD monitoring survey, which is non-random
  # SpeciesCode = switch( Species, "arrowtooth flounder"='ARF_KG', "Pacific ocean perch"='POP_KG' )
  BCt <- read.csv(paste0(DataFile,"/BC/BC_trawl_survey_sable_data.Oct312019.csv"))  %>% 
    filter(LONGITUDE <= 0 & !is.na(TOW_LENGTH_M) & !is.na(CATCH_WEIGHT) ) %>%
    mutate("AreaSwept_km2"=as.numeric(as.character(TOW_LENGTH_M))/1000)
  # BCt <- cbind( BCt,  #/1e6) ## to scale effort
  # BCt <- BCt[-which(BCt[,'ACTIVITY_DESC']=="HECATE STRAIT PCOD MONITORING TRAWL SURVEY"),]

  Data3a <- ThorsonUtilities::rename_columns( BCt[,c("SURVEY_ID", 'SURVEY_DESC','YEAR','LATITUDE','LONGITUDE',
                                                     'AreaSwept_km2',"CATCH_WEIGHT")], 
                                              newname=c("TRIP_ID", 'Survey','Year','Lat','Lon','AreaSwept_km2','Catch_KG')) %>% 
    mutate(Survey = 'BC_TRAWL', Region = 'BC')%>% select(-TRIP_ID)
  rm(BCt)
  
  Data3a %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat)))
  
  Data3a %>%
    mutate(CZERO = (Catch_KG == 0))%>%
    ggplot(., aes(x = Lat, y = Lon, color = CZERO)) + geom_point() + facet_wrap(~Year)
  
  
  # Data3a[,'Survey'] <- 'BC_TRAWL' #droplevels(Data3a[,'Survey'])
  # Data3a <- cbind(Data3a, "Region"="BC")  
  # Data3a$Lon = -1*Data3a$Lon ## I think this was wrong before but no longer. Should be negative.
  Data_CPUE = rbind( Data_CPUE, Data3a )
  rm(Data3a)
}

# Load GOA trawl -- these already have zeros
if( "GOA" %in% Surveys_to_include ){
  
  ## DH indicated to use <700m and drop 1984, 1987 and split at 1993
  GOA <-  read.csv( paste0(DataFile,"AK/race_cpue_by_haul.csv"), header=TRUE ) %>% 
    # filter(Survey == 'GOA'  | Survey == 'AI') 
    filter(Survey == 'GOA' & Gear.Depth <= 700 & !(Year %in% c(1984,1987)) ) 
  names(GOA) <- toupper(names(GOA))
  GOA <-  cbind( GOA, "TowID"=paste(GOA[,'YEAR'],GOA[,'STARTING.LATITUDE..DD.'],GOA[,'STARTING.LONGITUDE..DD.'],sep="_"), "AreaSwept_km2"=0.01) #0.01
  Data4 <- GOA %>% select(c('YEAR','STARTING.LATITUDE..DD.','STARTING.LONGITUDE..DD.','AreaSwept_km2','WEIGHT..KG.'))
  # Data4 = FishData::add_missing_zeros( data_frame=GOA, Method=ZeroMethod, if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'),
  # unique_sample_ID_colname="TowID", sample_colname="WEIGHT..KG.", species_subset=Species, species_colname="COMMON.NAME" )
  rm(GOA)
  Data4 <- ThorsonUtilities::rename_columns( Data4[,c('YEAR','STARTING.LATITUDE..DD.','STARTING.LONGITUDE..DD.','AreaSwept_km2','WEIGHT..KG.')],
                                             newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data4$Lon <- ifelse(  Data4$Lon > 0,   Data4$Lon*-1,  Data4$Lon) ## otherwise things in japan
  
  # Data4 %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat)))
  Data4 %>%
    mutate(ZERO_CATCH = (Catch_KG == 0))%>%
    ggplot(., aes(y = Lat, x = Lon, color = ZERO_CATCH)) + geom_point(alpha = 0.2) + facet_wrap(~Year) +
    labs(title = 'AK_trawl_GOA', subtitle="Subset race_cpue_by_haul.csv to GOA, Depth < 700 and drop 1984, 1987")
  
  
  ## Create timeblocking
  GOA_late <- subset(Data4, Year > 1993) %>% mutate(Survey = 'GOA_late', Region = "GOA")
  GOA_early <- subset(Data4, Year <= 1993) %>% mutate(Survey = 'GOA_early', Region = "GOA")
  
  Data_CPUE = rbind( Data_CPUE, GOA_early, GOA_late )
  rm(Data4)
}
# Load GOA -- these already have zeros, 
## Survey_catch_analysis.csv has the catch weight by station/haul as estimated 
## by l-w relationship. It only has this for the domestic survey and will have to be
##  crossreferenced to station locations from one of the other data sources.
if( "AK_DOM_LL" %in% Surveys_to_include ){
  
  ## slow to make the merge; did once and save.
  AK_DOM_LL_Loc <-  read.csv( paste0(DataFile,"AK/LLData/catch_summary_view_with_nulls.csv"), header=TRUE,skip = 6) %>%
    filter(Year > 1989)
  ## these have a small fudge factor leading to many dupes. get a mean survey location for each.
  AK_DOM_LL0 <-  read.csv( paste0(DataFile,"AK/LLData/SurveyCatchAnalysis.csv"), header = TRUE)
  names(AK_DOM_LL0)[1] <- 'Year'
  
  # ## get station numbers from dom LL
  AK_DOM_LL <- AK_DOM_LL_Loc %>% group_by(Station.Number,Year) %>% dplyr::summarise(meanLat = mean(Start.Latitude..DD.),
                                                                                    meanLon = mean(Start.Longitude..DD.)) %>%
    select(Station.Number,Year, meanLat, meanLon) %>%
    merge(AK_DOM_LL0,.,
          by = c("Year",'Station.Number'),  all.y = TRUE)
  # ## overwrite NA weights to zero
  AK_DOM_LL$Total.Weight..kg.[is.na( AK_DOM_LL$Total.Weight..kg.)] <- 0
  write.csv(AK_DOM_LL, file = paste0(DataFile,"AK/LLData/merged_AK_DOM_LL.csv") )
  
  Data5 <- read.csv(paste0(DataFile,"AK/LLData/merged_AK_DOM_LL.csv") )%>% 
    mutate(AreaSwept_km2 = 0.01) %>%
    select(c('Year','meanLat','meanLon','AreaSwept_km2','Total.Weight..kg.'))
  
  # Data5 = FishData::add_missing_zeros( data_frame=GOA, Method=ZeroMethod, if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'),
  # unique_sample_ID_colname="TowID", sample_colname="WEIGHT..KG.", species_subset=Species, species_colname="COMMON.NAME" )
  Data5 <- ThorsonUtilities::rename_columns( Data5[,c('Year','meanLat','meanLon','AreaSwept_km2','Total.Weight..kg.')], 
                                             newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))  %>%
    mutate(Survey = 'AK_DOM_LL', Region = 'AK')
  # Data5  cbind("Survey"="GOA", "Region"="GOA", Data5)  
  Data5$Lon <- ifelse(  Data5$Lon > 0,   Data5$Lon*-1,  Data5$Lon) ## otherwise things in japan
  
  # Data5 %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat))) %>% tail(10)
  
  Data5 %>%
    mutate(ZERO_CATCH = (Catch_KG == 0))%>%
    ggplot(., aes(y = Lat, x = Lon, color = ZERO_CATCH)) + geom_point(alpha = 0.2) + facet_wrap(~Year) +
    labs(title = 'AK_DOM_LL', subtitle="Station Numbers from catch_summary_view_with_nulls")
  
  # AK_DOM_late <- subset(Data5, YEAR > 1993) %>% mutate(Survey = 'AK_DOM_late', "Region = AK")
  # AK_DOM_early <- subset(Data5, YEAR <= 1993) %>% mutate(Survey = 'AK_DOM_early', "Region = AK")
  
  Data_CPUE <- rbind( Data_CPUE, Data5) #AK_DOM_early, AK_DOM_late )
  rm(Data5)
  
  # ggplot(Data4, aes(x = Lon, y = Lat, col = Catch_KG)) + geom_point() + facet_wrap(~Year)
}

# Load EBS -- these already have zeros
if( "EBS" %in% Surveys_to_include ){
  EBS <- read.csv( paste0(DataFile,"AK/race_cpue_by_haul.csv"), header=TRUE ) %>% 
    filter(Survey == 'EBS_SHELF'  | Survey == 'EBS_SLOPE' | Survey == 'AI')
  names(EBS) <- toupper(names(EBS))
  # EBS$WEIGHT..KG. <-  EBS$WEIGHT..KG.*10
  
  EBS <- EBS %>%
    mutate(TowID=paste(EBS[,'YEAR'],EBS[,'STARTING.LATITUDE..DD.'],EBS[,'STARTING.LONGITUDE..DD.'],sep=""),
                AreaSwept_km2=EFFORT..KM2.) %>% 
    select("TowID", "SURVEY", "COMMON.NAME", 'YEAR','STARTING.LATITUDE..DD.','STARTING.LONGITUDE..DD.','AreaSwept_km2','WEIGHT..KG.')#0.01
  # Data6 <- EBS 
  # Data6 <- FishData::add_missing_zeros( data_frame=EBS, Method='Slow', if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'),
  # unique_sample_ID_colname="TowID", sample_colname="WEIGHT..KG.", species_subset=Species, species_colname="COMMON.NAME" )
  Data6 <- ThorsonUtilities::rename_columns( EBS[,c('TowID',"SURVEY", 'YEAR','STARTING.LATITUDE..DD.','STARTING.LONGITUDE..DD.','AreaSwept_km2','WEIGHT..KG.')],
                                             newname=c('TowID',"Survey", 'Year','Lat','Lon','AreaSwept_km2','Catch_KG')) #%>%
  #   complete(TowID,nesting(Survey, Year,Lat,Lon,AreaSwept_km2),
  #            fill = list(Catch_KG = 0.0)) %>%
  #   select(-TowID)
  Data6$Lon <- ifelse(  Data6$Lon > 0,   Data6$Lon*-1,  Data6$Lon) ## otherwise things in japan
  
  
  # Data6 %>% group_by(Year) %>% summarise(sum(Catch_KG ==0), n(), length(unique(Lat)))
  #   Data6 %>%
  # mutate(ZERO_CATCH = (Catch_KG == 0))%>%
  # ggplot(., aes(y = Lat, x = Lon, color = ZERO_CATCH)) + geom_point(alpha = 0.2) + facet_wrap(~Year) +
  #     labs(title = 'AK_TRAWL_EBS', subtitle = "Subset to EBS_SHELF, EBS_SLOPE & AI")
  
  
  ## Create timeblocking
  EBS_late <- subset(Data6, Year > 1993) %>% mutate(Survey = 'EBS_late', Region = "EBS") %>% select(-TowID)
  EBS_early <- subset(Data6, Year <= 1993) %>% mutate(Survey = 'EBS_early', Region = "EBS") %>% select(-TowID)
  
  Data_CPUE <- rbind( Data_CPUE, EBS_late, EBS_early  )
}

# Memory management
gc()

# Restrict years
Data_Geostat <- Data_CPUE[which(Data_CPUE$Year>=Year_Range[1] & Data_CPUE$Year<=Year_Range[2]),]
save(Data_Geostat, file = paste0(DateFile,"/Data_Geostat.Rdata"))
Data_Geostat %>% group_by(Survey) %>% dplyr::summarise(min(Year))
Data_Geostat <- na.omit( Data_Geostat )
Data_Geostat %>% group_by(Survey) %>% dplyr::summarise(min(Catch_KG), max(Catch_KG)) ## should be 0 to pos
Data_Geostat %>% group_by(Survey) %>% dplyr::summarise(min(AreaSwept_km2), max(AreaSwept_km2)) ## should be positive
Data_Geostat %>% group_by(Survey) %>% dplyr::summarise(min(Year)) ## should be > Year_Range[1]

##  Define regions ----
# Including two different approaches
Region <- NULL
if( TRUE ){
  if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
  if("BCs" %in% Surveys_to_include | "BCt" %in% Surveys_to_include) Region = c( Region, "British_Columbia" )
  if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
  if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
  Extrapolation_List <- make_extrapolation_info( Region=Region, strata_to_use=c('SOG','WCVI','QCS','HS','WCHG'),
                                                 zone=Zone, create_strata_per_region=create_strata_per_region )
}else{
  if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
  if("BCs" %in% Surveys_to_include | "BCt" %in% Surveys_to_include) Region = c( Region, "British_Columbia" )
  if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
  if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
  observations_LL <- Data_Geostat[ which(Data_Geostat[,'Region']=="BC"), c('Lat','Lon') ]
  Extrapolation_List <-  make_extrapolation_info( Region=Region,
                                                  observations_LL=observations_LL, zone=Zone, create_strata_per_region=create_strata_per_region )
}

## Make spatial list ----
Spatial_List <- make_spatial_info( n_x=n_x, Lon=Data_Geostat[,'Lon'], 
                                   Lat=Data_Geostat[,'Lat'], 
                                   Extrapolation_List=Extrapolation_List,
                                   DirPath=DateFile, Save_Results=FALSE, 
                                   "knot_method"="grid", refine=FALSE, 
                                   fine_scale=fine_scale )


# Plot details
MapDetails_List <- make_map_info( "Region"="Other", 
                                  "spatial_list"=Spatial_List, 
                                  "Extrapolation_List"=Extrapolation_List )
Year_Set <- min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])

# Exclude surveys without any encounters
EncNum_k <- tapply( Data_Geostat[,'Catch_KG'], INDEX=Data_Geostat[,'Survey'], 
                    FUN=function(vec){sum(vec>0)} )
if( any(EncNum_k==0) ){
  Which = which( EncNum_k==0 )
  Which2Remove = which( Data_Geostat[,'Survey'] %in% names(Which) )
  Data_Geostat = Data_Geostat[-Which2Remove,]
  Spatial_List$loc_i = Spatial_List$loc_i[-Which2Remove,]
  Spatial_List$knot_i = Spatial_List$knot_i[-Which2Remove]
  Data_Geostat[,'Survey'] = droplevels( Data_Geostat[,'Survey'] )
}

# Make catchability matrix (Q_i) -- this should be where the BC one gets separated. The resulting Q_ik will have n-1 columns, with baseQ excluded.
if( length(unique(Data_Geostat[,'Survey']))==1 ){
  Q_ik <- matrix(0, ncol=1, nrow=nrow(Data_Geostat))
}else{
  Q_ik <- ThorsonUtilities::vector_to_design_matrix( Data_Geostat[,'Survey'] )
  if( !(BaseQ %in% colnames(Q_ik)) ) stop("Problem with Q_ik")
  Q_ik <- Q_ik[,-which(colnames(Q_ik)==BaseQ),drop=FALSE]
}

# Plot location of data
png(paste0(DateFile,"/Extrapolation_List.png"), width = 8, height = 6, units = 'in', res = 520)
plot( Extrapolation_List )
dev.off()
png(paste0(DateFile,"/Spatial_List.png"), width = 8, height = 6, units = 'in', res = 520)
plot( Spatial_List ) 
dev.off()

# Make and Run TMB model ----
# (THIS WILL BE SIMILAR FOR EVERY DATA SET)


# # Make TMB data list
# TmbData <-
#   make_data(
#     "FieldConfig" = FieldConfig,
#     "RhoConfig" = RhoConfig,
#     "ObsModel" = ObsModel,
#     "c_i" = rep(0, nrow(Data_Geostat)),
#     "b_i" = Data_Geostat[, 'Catch_KG'],
#     "a_i" = Data_Geostat[, 'AreaSwept_km2'],
#     "t_i" = Data_Geostat[, 'Year'],
#     "spatial_list" = Spatial_List,
#     "Q_ik" = Q_ik
#   )

## from CC version
TmbData <- VAST::make_data(#"X_itp"=X_itp, 
  #"X_gtp"=X_gtp, 
  #"Xconfig_zcp"=Xconfig_zcp, 
  "Version"=Version,
  "Aniso"=Aniso, 
  "FieldConfig"=FieldConfig, 
  "OverdispersionConfig" = c("Eta1"=0, "Eta2"=0), 
  "RhoConfig"=RhoConfig, 
  "ObsModel"= ObsModel, 
  "c_i"=rep(0,nrow(Data_Geostat)), 
  "b_i"=Data_Geostat[,'Catch_KG'], 
  "a_i"=Data_Geostat[,'AreaSwept_km2'], 
  # "v_i"=as.numeric(Data_Geostat[,'Vessel']),#-1, 
  "t_i"=Data_Geostat[,'Year'], 
  "Q_ik" = Q_ik,
  "spatial_list"=Spatial_List, 
  "Options"=Options )

# Make TMB object
# TmbList <- make_model( "TmbData"=TmbData, "RhoConfig"=RhoConfig, 
#                        "Use_REML"=Use_REML, "RunDir"=DateFile, 
#                        
#                        "Version"=get_latest_version() )
TmbList <- make_model("build_model"=TRUE, "TmbData"=TmbData, "RunDir"=DateFile, 
                      "Version"=Version, "RhoConfig"=RhoConfig, 
                      "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=getwd())

# ## this is what gets called within make_model
# plist <- make_parameters("DataList"=TmbData, "RhoConfig"=RhoConfig,
#             Version=get_latest_version())
# # 
# plist$lambda1_k
# Run model ----
Obj <- TmbList[["Obj"]]
Obj$par['lambda2_k'] ## should not be NA
Obj$par['lambda1_k'] 
# Obj$par['gamma1_k'] 

Opt <- TMBhelper::fit_tmb(
    obj = Obj,
    lower = TmbList[["Lower"]],
    upper = TmbList[["Upper"]],
    newtonsteps = 1,
    getsd = TRUE,
    bias.correct = TRUE,
    bias.correct.control = list(vars_to_correct = "Index_cyl"),
    savedir = DateFile
  )  # , rel.tol=1e-20


Report <- TmbList$Obj$report()
ParHat <- TmbList$Obj$env$parList()

# Save stuff [NOTE OBJ IS INSIDE SAVE]
Save <- list("Opt"=Opt, "Report"=Report,
             "ParHat"=TmbList$Obj$env$parList(Opt$par),
             'Obj' = Obj)
save(Save, file=paste0(DateFile,"Save_original.RData"))





# strata.limits <- subset(strata.limits.full, STRATA %in% as.factor(strata.limits.input))

# 
# Obj$report()
# 
# Obj$report(Obj$env$last.par.best) ## all derived quantities
# 


################
# Make diagnostic plots ----

# Plot locations
plot_data( Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, 
           Data_Geostat=Data_Geostat, PlotDir=DateFile, 
           Plot1_name="Data_and_knots.png", Plot2_name="Data_by_year.png", col="red")

# Plot index
Index <- plot_biomass_index( DirName=DateFile, 
                             TmbData=TmbData, 
                             use_biascorr = TRUE,
                             Sdreport=Opt$SD, 
                             Year_Set=Year_Set, 
                             strata_names=c("All",Region), 
                             plot_log=TRUE, width=6, height=6 ) # , total_area_km2=sum(a_xl[,1])



plot_range_index( Sdreport=Opt$SD, Report=Report, Year_Set=Year_Set, TmbData=TmbData, 
                  Znames=colnames(TmbData$Z_xm), PlotDir=DateFile )

# Plot Anisotropy
plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report )

# Plot encounter rate diagnostics
# plot_quantile_diagnostic( Report=Report, TmbData=TmbData, DateFile=DateFile)

# Positive catch rate diagnostics
Q <- plot_quantile_diagnostic( TmbData=TmbData, Report=Report, DateFile=DateFile ) # SpatialDeltaGLMM::

# Pearson residuals diagnostics
plot_residuals( Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], 
                extrapolation_list = Extrapolation_List,
                TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, spatial_list=Spatial_List )

# Plot density
plot_maps( plot_set=3, Report=Report, PlotDF=MapDetails_List[["PlotDF"]], 
           working_dir=DateFile, Year_Set=Year_Set )


file.rename(paste0(DateFile,"/Table_for_SS3.csv"), 
            to = paste0(DateFile,"/Table_for_SS3_original.csv"))


file.rename(paste0(DateFile,"/parameter_estimates.txt"), 
            to = paste0(DateFile,"/parameter_estimates_original.txt"))

file.rename(paste0(DateFile,"/Index-Biomass.png"), 
            to = paste0(DateFile,"/Index-Biomass-original.png"))

file.rename(paste0(DateFile,"/density.png"), 
            to = paste0(DateFile,"/density-original.png"))

file.rename(paste0(DateFile,"/aniso.png"), 
            to = paste0(DateFile,"/aniso-original.png"))

file.rename(paste0(DateFile,"/Record.Rdata"), 
            to = paste0(DateFile,"/Record-Original.Rdata"))

# source("./R/reVAST_newstrata.R")
