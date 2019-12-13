
# Use development branch
remotes::install_github("james-thorson/FishStatsUtils", ref="development")
remotes::install_github("james-thorson/VAST", ref="development")

#Libraries
library(VAST)
library(TMB)
library(dplyr)
library(plyr)
library(reshape)
library(mapdata)

# Directories
RootFile = "C:/Users/James.Thorson/Desktop/Work files/Collaborations/2015 -- Combined GOA-BC-CC arrowtooth/"
DataFile = paste0( RootFile,"Data/")

# Resolution
n_x = 500 # Number of stations

# Choose species
Species = "arrowtooth flounder"
 Species_code = switch(Species, "arrowtooth flounder"="arrow", "Pacific ocean perch"="POP")

# Date
Date = Sys.Date()
  DateFile = paste0(RootFile,Date,"_nx=",n_x,"_Exclude=none_Species=",Species_code,"Base=GOA/")#    -without_omega_epsilonAR
  dir.create(DateFile)

###############
# Settings
###############

if( file.exists(paste0(DateFile,"Record.RData")) ){
  load( file=paste0(DateFile,"Record.RData"))
  attach( Record )
}else{
  # Inputs
  Surveys_to_include = c("WCGBTS", "Triennial", "BC", "GOA", "EBS")[c(1:5)] #This will only work for years after 2003

  # settings
  FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence
  ObsModel = c(1,1)  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  Spatial_Smoother = c("Index", "Smoothed_Index", "Spatiotemporal", "Spatiotemporal_AR")[3]
  BC_catchability = c("Separate")[1]
  BiasCorr = FALSE #TRUE #
  Zone = 5
  Options =  c("Calculate_Range"=1, "Calculate_evenness"=1, "Calculate_effective_area"=1)
  BaseQ = c("GOA", "WCGBTS")[1]
  Use_REML = TRUE
  fine_scale = TRUE
  create_strata_per_region = TRUE

  Year_Range = c(1984, 2014)
  #Year_Range = c(1995, 2014)

  #Year_Range = c(-Inf, Inf) #c(1983, 2014)

  # Derived
  if( Spatial_Smoother=="Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)
  if( Spatial_Smoother=="Smoothed_Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=2, "Epsilon2"=2)
  if( Spatial_Smoother=="Spatiotemporal" ) RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2) # Pointwise random walk (epsilon is RW, Beta1 is constant for all years)
  if( Spatial_Smoother=="Spatiotemporal_AR" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=3, "Epsilon2"=3) # Pointwise autocorrelation (beta is freely estimated in each year)

  # Save options for future records
  Record = ThorsonUtilities::bundlelist( c("Surveys_to_include","n_x","FieldConfig","RhoConfig","ObsModel",
     "Spatial_Smoother","BC_catchability","BiasCorr","Zone","Options","BaseQ","Use_REML","fine_scale",
     "Year_Range","create_strata_per_region") )
  capture.output( Record, file=paste0(DateFile,"Record.txt"))
  save( Record, file=paste0(DateFile,"Record.RData"))
}

##################
# Load data
##################
Data_CPUE = NULL
ZeroMethod = "Fast"

# Load shelf-slope
if( "WCGBTS" %in% Surveys_to_include ){
  WCGBTS = read.csv( paste0(DataFile,"SurveyHaulAndCatchData03To14--HaulFishCatchData03To14.csv"), skip=8, header=TRUE)
  WCGBTS_b = read.csv( paste0(DataFile,"SurveyHaulAndCatchData03To14--HaulData03To14.csv"), skip=8, header=TRUE)
  WCGBTS_b = cbind( WCGBTS_b, "AreaSwept_km2"=WCGBTS_b[,'Distance.Trawled..km.']*WCGBTS_b[,'Trawl.Net.Width..m.']/1000)
  WCGBTS = cbind( WCGBTS, WCGBTS_b[match(WCGBTS[,'Trawl.Identifier'],WCGBTS_b[,'Trawl.Identifier']),c('Vessel','Vessel.Latitude.at.Trawl.Net.Touchdown..Start..dd.','Vessel.Longitude.at.Trawl.Net.Touchdown..Start..dd.','AreaSwept_km2')] )
  WCGBTS[,'Survey.Cycle'] = as.numeric( substr(WCGBTS[,'Survey.Cycle'],start=7,stop=11))
  Data1 = FishData::add_missing_zeros( data_frame=WCGBTS, Method=ZeroMethod, if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'), unique_sample_ID_colname='Trawl.Identifier', sample_colname='Species.Haul.Weight..kg.', species_subset=Species, species_colname="Species.Common.Name")
  Data1 = ThorsonUtilities::rename_columns( Data1[,c('Survey.Cycle','Vessel.Latitude.at.Trawl.Net.Touchdown..Start..dd.','Vessel.Longitude.at.Trawl.Net.Touchdown..Start..dd.','AreaSwept_km2','Species.Haul.Weight..kg.')], newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data1 = cbind("Survey"="WCGBTS", "Region"="CC", Data1)
  Data_CPUE = rbind( Data_CPUE, Data1 )
}

# Load triennial
  # Has some problem with multiple replicated samples
if( "Triennial" %in% Surveys_to_include ){
  ThorsonUtilities::LoadFn( paste0(DataFile,"Tri_Shelf_and_AFSC_Slope_survey_bio.All.Sp 9 June 2015.dmp"))
  Triennial = Tri_Shelf_and_AFSC_Slope_survey_bio.All.Sp$Lengths
  Data2 = FishData::add_missing_zeros( data_frame=Triennial, Method=ZeroMethod, if_multiple_records='Combine', unique_sample_ID_colname='HAULJOIN', sample_colname='SP_TOW_WGHT_KG', species_subset=Species, species_colname="CommonName" )
  Data2 = cbind(Data2, "AreaSwept_km2"=Data2[,'DISTANCE_FISHED']*Data2[,'NET_WIDTH']/1000 )
  Data2 = ThorsonUtilities::rename_columns( Data2[,c('YEAR','START_LATITUDE','START_LONGITUDE','AreaSwept_km2','SP_TOW_WGHT_KG')], newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data2 = cbind("Survey"="Triennial", "Region"="CC", Data2)
  Data_CPUE = rbind( Data_CPUE, Data2 )
}

# Load BC catches
if( "BC" %in% Surveys_to_include ){
  # Exclude PCOD monitoring survey, which is non-random
  SpeciesCode = switch( Species, "arrowtooth flounder"='ARF_KG', "Pacific ocean perch"='POP_KG' )
  BC = read.csv( paste0(DataFile,"spera1_sets_with_temp_depth_catches_Jan2016.csv"), header=TRUE)
  BC = cbind( BC, "AreaSwept_km2"=BC[,'AREA_SWEPT']/1e6)
  BC = BC[-which(BC[,'ACTIVITY_DESC']=="HECATE STRAIT PCOD MONITORING TRAWL SURVEY"),]
  Data3 = ThorsonUtilities::rename_columns( BC[,c('ACTIVITY_DESC','YEAR','BEST_LAT','BEST_LONG','AreaSwept_km2',SpeciesCode)], newname=c('Survey','Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data3[,'Survey'] = droplevels(Data3[,'Survey'])
  Data3 = cbind(Data3, "Region"="BC")
  Data3$Lon = -1*Data3$Lon
  Data_CPUE = rbind( Data_CPUE, Data3 )
}

# Load GOA
if( "GOA" %in% Surveys_to_include ){
  GOA = read.csv( paste0(DataFile,"GOA_combined.csv"), header=TRUE )
  GOA = cbind( GOA, "TowID"=paste(GOA[,'YEAR'],GOA[,'LATITUDE'],GOA[,'LONGITUDE'],sep="_"), "AreaSwept_km2"=0.01) #0.01
  Data4 = FishData::add_missing_zeros( data_frame=GOA, Method=ZeroMethod, if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'), unique_sample_ID_colname="TowID", sample_colname="WTCPUE", species_subset=Species, species_colname="COMMON" )
  Data4 = ThorsonUtilities::rename_columns( Data4[,c('YEAR','LATITUDE','LONGITUDE','AreaSwept_km2','WTCPUE')], newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data4 = cbind("Survey"="GOA", "Region"="GOA", Data4)
  Data_CPUE = rbind( Data_CPUE, Data4 )
}

# Load EBS
if( "EBS" %in% Surveys_to_include ){
  EBS = read.csv( paste0(DataFile,"allebs.csv"), header=TRUE)
  EBS = cbind( EBS, "TowID"=paste(EBS[,'YEAR'],EBS[,'LATITUDE'],EBS[,'LONGITUDE'],sep="_"), "AreaSwept_km2"=0.01)
  Data5 = FishData::add_missing_zeros( data_frame=EBS, Method=ZeroMethod, if_multiple_records=ifelse(ZeroMethod=="Slow",'Error','Combine'), unique_sample_ID_colname="TowID", sample_colname="WTCPUE", species_subset=Species, species_colname="COMMON" )
  Data5 = ThorsonUtilities::rename_columns( Data5[,c('YEAR','LATITUDE','LONGITUDE','AreaSwept_km2','WTCPUE')], newname=c('Year','Lat','Lon','AreaSwept_km2','Catch_KG'))
  Data5 = cbind("Survey"="EBS", "Region"="EBS", Data5)
  Data_CPUE = rbind( Data_CPUE, Data5 )
}

# Memory management
remove( list=c("Tri_Shelf_and_AFSC_Slope_survey_bio.All.Sp","WCGBTS","WCGBTS_b","Triennial","BC","GOA") )
gc()

# Restrict years
Data_Geostat = Data_CPUE[which(Data_CPUE$Year>=Year_Range[1] & Data_CPUE$Year<=Year_Range[2]),]
Data_Geostat = na.omit( Data_Geostat )

# Define regions
# Including two different approaches
Region = NULL
if( TRUE ){
  if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
  if("BC" %in% Surveys_to_include) Region = c( Region, "British_Columbia" )
  if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
  if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
  Extrapolation_List = make_extrapolation_info( Region=Region, strata_to_use=c('SOG','WCVI','QCS','HS','WCHG'),
    zone=Zone, create_strata_per_region=create_strata_per_region )
}else{
  if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
  if("BC" %in% Surveys_to_include) Region = c( Region, "Other" )
  if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
  if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
  observations_LL = Data_Geostat[ which(Data_Geostat[,'Region']=="BC"), c('Lat','Lon') ]
  Extrapolation_List = make_extrapolation_info( Region=Region,
    observations_LL=observations_LL, zone=Zone, create_strata_per_region=create_strata_per_region )
}

# Make spatial list
Spatial_List = make_spatial_info( n_x=n_x, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List,
  DirPath=DateFile, Save_Results=FALSE, "knot_method"="grid", refine=FALSE, fine_scale=fine_scale )

# Plot details
MapDetails_List = make_map_info( "Region"="Other", "spatial_list"=Spatial_List, "Extrapolation_List"=Extrapolation_List )
Year_Set = min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])

# Exclude surveys without any encounters
EncNum_k = tapply( Data_Geostat[,'Catch_KG'], INDEX=Data_Geostat[,'Survey'], FUN=function(vec){sum(vec>0)} )
if( any(EncNum_k==0) ){
  Which = which( EncNum_k==0 )
  Which2Remove = which( Data_Geostat[,'Survey'] %in% names(Which) )
  Data_Geostat = Data_Geostat[-Which2Remove,]
  Spatial_List$loc_i = Spatial_List$loc_i[-Which2Remove,]
  Spatial_List$knot_i = Spatial_List$knot_i[-Which2Remove]
  Data_Geostat[,'Survey'] = droplevels( Data_Geostat[,'Survey'] )
}

# Make catchability matrix (Q_i)
if( length(unique(Data_Geostat[,'Survey']))==1 ){
  Q_ik = matrix(0, ncol=1, nrow=nrow(Data_Geostat))
}else{
  Q_ik = ThorsonUtilities::vector_to_design_matrix( Data_Geostat[,'Survey'] )
  if( !(BaseQ %in% colnames(Q_ik)) ) stop("Problem with Q_ik")
  Q_ik = Q_ik[,-which(colnames(Q_ik)==BaseQ),drop=FALSE]
}

# Plot location of data
plot( Extrapolation_List )
plot( Spatial_List )

################
# Make and Run TMB model
# (THIS WILL BE SIMILAR FOR EVERY DATA SET)
################

# Make TMB data list
TmbData = make_data("FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig, "ObsModel"=ObsModel,
  "b_i"=Data_Geostat[,'Catch_KG'], "a_i"=Data_Geostat[,'AreaSwept_km2'],
  "t_i"=Data_Geostat[,'Year'], "spatial_list"=Spatial_List, "Q_ik"=Q_ik )

# Make TMB object
TmbList = make_model( "TmbData"=TmbData, "RhoConfig"=RhoConfig, "Use_REML"=Use_REML, "RunDir"=DateFile, "Version"=get_latest_version() )

# Run model
Opt = TMBhelper::fit_tmb( obj=TmbList[["Obj"]], lower=TmbList[["Lower"]], upper=TmbList[["Upper"]],
  newtonsteps=1, getsd=TRUE, bias.correct=BiasCorr, bias.correct.control=list(vars_to_correct="Index_cyl"),
  savedir=DateFile )  # , rel.tol=1e-20

# Reports
Report = TmbList$Obj$report()
ParHat = TmbList$Obj$env$parList()

# Save stuff
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=TmbList$Obj$env$parList(Opt$par))
save(Save, file=paste0(DateFile,"Save.RData"))

################
# Make diagnostic plots
################

# Plot locations
plot_data( Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile, Plot1_name="Data_and_knots.png", Plot2_name="Data_by_year.png", col="red")

# Plot index
Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=Year_Set, strata_names=c("All",Region), plot_log=TRUE, width=6, height=6 ) # , total_area_km2=sum(a_xl[,1])

# Plot center of gravity
plot_range_index( Sdreport=Opt$SD, Report=Report, Year_Set=Year_Set, 
                  TmbData=TmbData, Znames=colnames(TmbData$Z_xm),
                  PlotDir=DateFile )

# Plot Anisotropy
plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report )

# Plot encounter rate diagnostics
plot_quantile_diagnostic( Report=Report, TmbData=TmbData, DateFile=DateFile)

# Positive catch rate diagnostics
#Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, DateFile=DateFile ) # SpatialDeltaGLMM::

# Pearson residuals diagnostics
#plot_residuals( Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, spatial_list=Spatial_List )

# Plot density
plot_maps( plot_set=3, Report=Report, PlotDF=MapDetails_List[["PlotDF"]], working_dir=DateFile, Year_Set=Year_Set,  )


