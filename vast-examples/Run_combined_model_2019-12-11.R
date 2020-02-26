
# Install packages

# Must use local version due to NOAA IT issues
# if( FALSE ){
#   devtools::install_local("C:/Users/James.Thorson/Desktop/Git/FishData", force=TRUE, dep=FALSE )
#   devtools::install_local("C:/Users/James.Thorson/Desktop/Git/FishStatsUtils", force=TRUE, dep=FALSE )
#   devtools::install_local("C:/Users/James.Thorson/Desktop/Git/VAST", force=TRUE, dep=FALSE )
# }
# 
# # Other users can use github as per normal
# if( FALSE ){
#   devtools::install_github("james-thorson/FishData")
#   devtools::install_local("C:/Users/James.Thorson/Desktop/Git/FishStatsUtils", ref="development" )
#   devtools::install_local("C:/Users/James.Thorson/Desktop/Git/VAST", ref="development" )
# }
#Libraries
library(VAST)
library(FishData)

# Directories
# RootFile = "C:/Users/James.Thorson/Work/Collaborations/2019 -- Basinwide model/"
# DataFile = paste0( RootFile, "/Data/" )
comp.name <- c("mkapur",'maia kapur')[2]
RootFile <- paste0( "C:/Users/",comp.name ,"/Dropbox/UW/sab-idx/runs/") 
DataFile  <- paste0( "C:/Users/",comp.name ,"/Dropbox/UW/sab-idx/data/" ) #paste0( RootFile,"Data/")

# Resolution
n_x = 500 # Number of stations

# Base catchability
BaseQ = c("GOA", "WCGBTS")[1]

# Structure
Spatial_Smoother = c("Index", "Smoothed_Index", "Spatiotemporal", "Spatiotemporal_AR")[4]

# Choose species
Species = "sablefish"
 Species_code = switch(Species, "sablefish" = "sab", "arrowtooth flounder"="arrow", "Pacific ocean perch"="POP")
 Sci_name = switch(Species, "arrowtooth flounder"="Atheresthes stomias","sablefish" = "Anoplopoma fimbria" )

# Date
Date = Sys.Date()
  DateFile = paste0(RootFile,Date,"A_nx=",n_x,"_Exclude=none_Species=",Species_code,"Base=",BaseQ,"_",Spatial_Smoother,"/")#    -without_omega_epsilonAR
  dir.create(DateFile)

###############
# Settings
###############

if( file.exists(paste0(DateFile,"Record.RData")) ){
  load( file=paste0(DateFile,"Record.RData"))
  attach( Record )
}else{
  # Inputs
  #Surveys_to_include = c("WCGBTS", "Triennial", "BC", "GOA", "EBS")[c(1:5)] #This will only work for years after 2003
  Surveys_to_include = c("WCGBTS", "Triennial", "BC", "GOA", "EBS", "AI")[c(1,2,4,6)] #This will only work for years after 2003

  # settings
  FieldConfig = c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) # 1=Presence-absence; 2=Density given presence
  ObsModel = c(2,1)  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
  BC_catchability = c("Separate")[1]
  BiasCorr = FALSE #TRUE #
  #Zone = 4
  Options =  c("Calculate_Range"=TRUE, "Calculate_effective_area"=TRUE)
  Use_REML = TRUE
  fine_scale = TRUE
  create_strata_per_region = TRUE
  knot_method = "grid"
  projargs = "+proj=moll +lon_0=-160 +datum=WGS84 +units=km"

  #Year_Range = c(1984, 2014)
  #Year_Range = c(1995, 2014)
  Year_Range = c(2001, 2018)

  #Year_Range = c(-Inf, Inf) #c(1983, 2014)

  # Derived
  if( Spatial_Smoother=="Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)
  if( Spatial_Smoother=="Smoothed_Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=2, "Epsilon2"=2)
  if( Spatial_Smoother=="Spatiotemporal" ) RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2) # Pointwise random walk (epsilon is RW, Beta1 is constant for all years)
  if( Spatial_Smoother=="Spatiotemporal_AR" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=4, "Epsilon2"=4) # Pointwise autocorrelation (beta is freely estimated in each year)

  # Save options for future records
  Record = ThorsonUtilities::bundlelist( c("Surveys_to_include","FieldConfig","RhoConfig","ObsModel",
     "BC_catchability","BiasCorr","projargs","Options","Use_REML","fine_scale",
     "Year_Range","create_strata_per_region","knot_method","projargs") )
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
  # survey="WCGBTS"; species_set=Sci_name; localdir=DataFile
  # add_zeros=TRUE; error_tol=1e-12; measurement_type="biomass"
  WCGBTS = download_catch_rates( survey="WCGBTS", species_set=Sci_name, localdir=DataFile )
  Data1 = data.frame("Survey"="WCGBTS", "Region"="CC", "Lat"=WCGBTS[,'Lat'], 
                     "Lon"=WCGBTS[,'Long'], "Year"=WCGBTS[,'Year'], "Catch_KG"=WCGBTS[,'Wt'], "AreaSwept_km2"=0.01*WCGBTS[,'AreaSwept_ha'] )
  Data_CPUE = rbind( Data_CPUE, Data1 )
}

# Load triennial
  # Has some problem with multiple replicated samples
if( "Triennial" %in% Surveys_to_include ){
  Triennial = download_catch_rates( survey="Triennial", species_set=Sci_name, localdir=DataFile )
  Data1 = cbind("Survey"="Triennial", "Region"="CC", "Lat"=Triennial[,'Lat'], "Lon"=Triennial[,'Long'], "Year"=Triennial[,'Year'], "Catch_KG"=Triennial[,'Wt'], "AreaSwept_km2"=0.01*WCGBTS[,'AreaSwept_ha'] )
  Data_CPUE = rbind( Data_CPUE, Data2 )
}

# Load BC catches
if( "BC" %in% Surveys_to_include ){
  BC = readRDS( file=paste0(DataFile,"pbs-synoptic-2019-10-17.rds") )
  BC = BC[ which(BC[,'species_science_name'] %in% tolower(Sci_name)), ]
  Data3 = data.frame( "Survey"="BCsynpotic", "Region"="BC", "Lat"=BC[,'latitude'], "Lon"=BC[,'longitude'], "Year"=BC[,'year'], "Catch_KG"=BC[,'density_kgpm2']*1e4, "AreaSwept_km2"=0.01 )
  Data_CPUE = rbind( Data_CPUE, Data3 )
}

# Load GOA
if( "GOA" %in% Surveys_to_include ){
  GOA = download_catch_rates( survey="Gulf_of_Alaska", species_set=Sci_name, localdir=DataFile )
  Data4 = data.frame( "Survey"="GOA", "Region"="GOA", "Lat"=GOA[,'Lat'], "Lon"=GOA[,'Long'], "Year"=GOA[,'Year'], "Catch_KG"=GOA[,'Wt'], "AreaSwept_km2"=0.01*GOA[,'AreaSwept_ha'] )
  Data_CPUE = rbind( Data_CPUE, Data4 )
}

# Load EBS
if( "EBS" %in% Surveys_to_include ){
  EBS = download_catch_rates( survey="Eastern_Bering_Sea", species_set=Sci_name, localdir=DataFile )
  Data5 = data.frame( "Survey"="EBS", "Region"="EBS", "Lat"=EBS[,'Lat'], "Lon"=EBS[,'Long'], "Year"=EBS[,'Year'], "Catch_KG"=EBS[,'Wt'], "AreaSwept_km2"=0.01*EBS[,'AreaSwept_ha'] )
  Data_CPUE = rbind( Data_CPUE, Data5 )
}

# Load Aluetian Islands
if( "AI" %in% Surveys_to_include ){
  AI = download_catch_rates( survey="Aleutian_Islands", species_set=Sci_name, localdir=DataFile )
  Data6 = data.frame( "Survey"="AI", "Region"="AI", "Lat"=AI[,'Lat'], "Lon"=AI[,'Long'], "Year"=AI[,'Year'], "Catch_KG"=AI[,'Wt'], "AreaSwept_km2"=0.01*AI[,'AreaSwept_ha'] )
  Data_CPUE = rbind( Data_CPUE, Data6 )
}

# Restrict years
Data_Geostat = Data_CPUE[which(Data_CPUE$Year>=Year_Range[1] & Data_CPUE$Year<=Year_Range[2]),]
Data_Geostat = na.omit( Data_Geostat )

# Check years
tapply( Data_Geostat[,'Catch_KG'], INDEX=list(Data_Geostat[,'Region'],Data_Geostat[,'Year']), FUN=function(vec){length(vec)} )

# Define regions
# Including two different approaches
Region = NULL
if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
if("BC" %in% Surveys_to_include) Region = c( Region, "British_Columbia" )
if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
if("AI" %in% Surveys_to_include) Region = c( Region, "Aleutian_Islands" )

# Extrapolation area
Extrapolation_List = make_extrapolation_info( Region=Region, strata_to_use=c('SOG','WCVI','QCS','HS','WCHG'),
  create_strata_per_region=create_strata_per_region, projargs=projargs )

# Make spatial list
Spatial_List = make_spatial_info( n_x=n_x, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List,
  DirPath=DateFile, Save_Results=FALSE, "knot_method"=knot_method, refine=FALSE, fine_scale=fine_scale )

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
  "t_i"=Data_Geostat[,'Year'], "spatial_list"=Spatial_List, "Q_ik"=Q_ik, "Options"=Options )

# Make TMB object
TmbList = make_model( "TmbData"=TmbData, "RhoConfig"=RhoConfig, "Use_REML"=Use_REML, "RunDir"=DateFile,
  "Version"=get_latest_version() )

# Run model
Opt = TMBhelper::fit_tmb( obj=TmbList[["Obj"]], lower=TmbList[["Lower"]], upper=TmbList[["Upper"]],
  newtonsteps=1, getsd=TRUE, bias.correct=BiasCorr, bias.correct.control=list(vars_to_correct="Index_cyl"),
  savedir=DateFile, getJointPrecision=TRUE )  # , rel.tol=1e-20

# Reports
Report = TmbList$Obj$report()
ParHat = TmbList$Obj$env$parList()

# Lambda_hat
lambda_hat = summary(Opt$SD,"random")
lambda_hat = lambda_hat[ grep("lambda",rownames(lambda_hat)), ]
capture.output( lambda_hat, file=paste0(DateFile,"lambda_hat.txt") )
lambdasum_hat = lambda_hat[ grep("lambda1",rownames(lambda_hat)), ] + lambda_hat[ grep("lambda2",rownames(lambda_hat)), ]
lambdasum_hat[,'Std. Error'] = sqrt(lambda_hat[ grep("lambda1",rownames(lambda_hat)), 'Std. Error']^2 + lambda_hat[ grep("lambda2",rownames(lambda_hat)), 'Std. Error']^2)
lambdasum_hat = cbind( lambdasum_hat, "Ratio"=exp(lambdasum_hat[,'Estimate']) )
capture.output( lambda_hat, file=paste0(DateFile,"lambdasum_hat.txt") )

# Save stuff
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=TmbList$Obj$env$parList(Opt$par), "lambda_hat"=lambda_hat)
save(Save, file=paste0(DateFile,"Save.RData"))

################
# Make diagnostic plots
################

# Plot projection
projargs_plot = "+proj=utm +datum=WGS84 +units=km +zone=3"
#projargs_plot = "+proj=moll +lon_0=-150 +datum=WGS84 +units=km"

# Plot locations
plot_data( Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile, Plot1_name="Data_and_knots.png", Plot2_name="Data_by_year.png", col="red")

# Plot index
Index = plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=Year_Set,
  strata_names=c("All",Region), plot_log=FALSE, width=6, height=6 )

# Plot center of gravity
plot_range_index( Sdreport=Opt$SD, Report=Report, Year_Set=Year_Set, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile )

# Plot Anisotropy
plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report )

# Plot density
plot_maps( plot_set=3, Report=Report, PlotDF=MapDetails_List[["PlotDF"]], working_dir=DateFile,
  Year_Set=Year_Set, projargs=projargs_plot, country=c("united states of america","canada","mexico","russia") )

# Range edges
plot_range_edge(Sdreport=Opt$SD, Obj=TmbList$Obj, Year_Set=Year_Set,
  working_dir=DateFile, quantiles=c(0.05, 0.95),
  n_samples=100, interval_width=1 )

# Custom map
ThorsonUtilities::save_fig( file=paste0(DateFile,"Density_examples"), width=6, height=6 )
  year_index = c(1,9,18)
  par( mfrow=c(2,2), mar=c(2,2,1,0), mgp=c(2,0.5,0), tck=-0.02 )
  Y_gs = log(Report$D_gcy[,1,year_index])
  Y_gs = ifelse( Y_gs<(max(Y_gs)/1e3), NA, Y_gs )
  for(tI in seq_along(year_index) ){
    plot_variable( Y_gt=Y_gs[,tI], zlim=range(Y_gs,na.rm=TRUE), map_list=MapDetails_List, land_color=rgb(0,0,0,alpha=0),
      add=TRUE, Format="", projargs=projargs_plot, country=c("united states of america","canada","mexico","russia") )
    mtext( side=3, text=Year_Set[year_index[tI]] )
    axis(1); axis(2)
  }
dev.off()

if( FALSE ){
  # Plot encounter rate diagnostics
  plot_quantile_diagnostic( Report=Report, TmbData=TmbData, DateFile=DateFile)

  # Positive catch rate diagnostics
  Q = plot_quantile_diagnostic( TmbData=TmbData, Report=Report, DateFile=DateFile ) # SpatialDeltaGLMM::

  # Pearson residuals diagnostics
  plot_residuals( Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, spatial_list=Spatial_List )
}
