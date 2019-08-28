# Download release number 3.0.0; its useful for reproducibility to use a specific release number
devtools::install_github("james-thorson/FishStatsUtils")
devtools::install_github("james-thorson/VAST")

# Set local working directory (change for your machine)
# setwd( "D:/UW Hideaway (SyncBackFree)/AFSC/2019-03 -- Making helper functions for VAST" )
# setwd("C:/merrill/VAST_examples/output_demo")
working_dir <- getwd()

# Load packages
library(TMB)               
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" )

## limit data for this example so it runs faster
example$sampling_data <- subset(example$sampling_data, Year > 2005)

###################
## Settings
###################

## Option 1: Wrapper function
# settings = make_settings( n_x=100, Region=example$Region, purpose="index", 
#   strata.limits=example$strata.limits, bias.correct=FALSE )

## Option 2: Manually go through steps to create settings (not using wrapper function)
  ## arguments in make_settings
  n_x = 100
  Region = example$Region
  strata.limits = example$strata.limits
  bias.correct = FALSE

  ## default for all purposes
  Version = FishStatsUtils::get_latest_version()  
  grid_size_km = 25
  Method = "Mesh"
  use_anisotropy = TRUE
  fine_scale = TRUE

  ## default when purpose = "index"
  FieldConfig = c("Omega1"="IID", "Epsilon1"="IID", "Omega2"="IID", "Epsilon2"="IID")
  RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)
  VamConfig = c("Method"=0, "Rank"=0, "Timing"=0)
  OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
  ObsModel = c(1,1)
  bias.correct = FALSE
  treat_nonencounter_as_zero = TRUE
  Options =  c("SD_site_logdensity"=FALSE, "Calculate_Range"=TRUE, "Calculate_effective_area"=TRUE, "treat_nonencounter_as_zero"=treat_nonencounter_as_zero )
  vars_to_correct = c( "Index_cyl" )

  # Bundle and export
  settings = list("Version"=Version, "n_x"=n_x, "Region"=Region, "strata.limits"=strata.limits, "zone"=NA, "FieldConfig"=FieldConfig, "RhoConfig"=RhoConfig,
    "VamConfig"=VamConfig, "OverdispersionConfig"=OverdispersionConfig, "ObsModel"=ObsModel, "vars_to_correct"=vars_to_correct,
    "Options"=Options, "grid_size_km"=grid_size_km,
    "Method"=Method, "use_anisotropy"=use_anisotropy, "fine_scale"=fine_scale, "bias.correct"=bias.correct )

###################
## Fit model
###################

## Option 1: Wrapper function
# fit = fit_model( "settings"=settings, "Lat_i"=example$sampling_data[,'Lat'], 
#   "Lon_i"=example$sampling_data[,'Lon'], "t_i"=example$sampling_data[,'Year'], 
#   "c_i"=rep(0,nrow(example$sampling_data)), "b_i"=example$sampling_data[,'Catch_KG'], 
#   "a_i"=example$sampling_data[,'AreaSwept_km2'], "v_i"=example$sampling_data[,'Vessel'] )

## Option 2: Manually go through steps to run model
  # Assemble inputs
  data_frame = data.frame( "Lat_i"=example$sampling_data[,'Lat'], "Lon_i"=example$sampling_data[,'Lon'], "a_i"=example$sampling_data[,'AreaSwept_km2'], "v_i"=example$sampling_data[,'Vessel'], "b_i"=example$sampling_data[,'Catch_KG'])
  # Decide which years to plot
  year_labels = seq( min(example$sampling_data[,'Year']), max(example$sampling_data[,'Year']) )
  years_to_plot = which( unique(example$sampling_data[,'Year']) %in% sort(unique(example$sampling_data[,'Year'])))

  # Save record
  dir.create(working_dir, showWarnings=FALSE, recursive=TRUE)
  save( settings, file=file.path(working_dir,"Record.RData"))
  capture.output( settings, file=file.path(working_dir,"Record.txt"))

  # Build extrapolation grid
  extrapolation_list = FishStatsUtils::make_extrapolation_info(Region=settings$Region, strata.limits=settings$strata.limits, zone=settings$zone)

  # Build information regarding spatial location and correlation
  spatial_list = FishStatsUtils::make_spatial_info(grid_size_km=settings$grid_size_km, n_x=settings$n_x, Method=settings$Method, Lon_i=data_frame$Lon_i, Lat_i=data_frame$Lat_i,
    Extrapolation_List=extrapolation_list, DirPath=working_dir, Save_Results=TRUE, fine_scale=settings$fine_scale)

  # Build data
  data_list = VAST::make_data("Version"=settings$Version, "FieldConfig"=settings$FieldConfig, "OverdispersionConfig"=settings$OverdispersionConfig,
    "RhoConfig"=settings$RhoConfig, "ObsModel"=settings$ObsModel, "c_iz"=rep(0, nrow(example$sampling_data)), "b_i"=data_frame$b_i, "a_i"=data_frame$a_i, "v_i"=data_frame$v_i,
    "s_i"=spatial_list$knot_i-1, "t_iz"=example$sampling_data[,'Year'], "spatial_list"=spatial_list, "Options"=settings$Options, "Aniso"=settings$use_anisotropy,
    Xconfig_zcp=NULL, X_gtp=NULL, X_itp=NULL, Q_ik=NULL)

  # Build object
  tmb_list = VAST::make_model("TmbData"=data_list, "RunDir"=working_dir, "Version"=settings$Version,
    "RhoConfig"=settings$RhoConfig, "loc_x"=spatial_list$loc_x, "Method"=spatial_list$Method)

  # Optimize object
  parameter_estimates = TMBhelper::fit_tmb(obj=tmb_list$Obj, lower=tmb_list$Lower, upper=tmb_list$Upper,
    savedir=working_dir, getsd=TRUE, newtonsteps=0, bias.correct=FALSE, quiet=TRUE,
    control=list(eval.max=10000,iter.max=10000,trace=1), loopnum=2)

  # Extract standard outputs
  Report = tmb_list$Obj$report()
  ParHat = tmb_list$Obj$env$parList( parameter_estimates$par )

  # Build and output
  fit = list("data_frame"=data_frame, "extrapolation_list"=extrapolation_list, "spatial_list"=spatial_list,
    "data_list"=data_list, "tmb_list"=tmb_list, "parameter_estimates"=parameter_estimates, "Report"=Report,
    "ParHat"=ParHat, "year_labels"=year_labels, "years_to_plot"=years_to_plot, "settings"=settings)
  save(fit, file=file.path(working_dir, "fit.RData"))

###################
## Fit model
###################

load(file.path(working_dir, "fit.RData"))
## Option 1: Wrapper function
## to working directory, plots:
## 1. Aniso
## 2. Center of gravity
## 3. Density
## 4. Encounter probability diagnostic
## 5. Effective area occupied
## 6. biomass index
## 7. QQ plots and posterior predictive distributions
# plot_results( fit, settings, category_names="EBS Pollock" )

## Option 2: Manually plot all figures included in FishStatsUtils
  ## map information
  # PLot settings
  map_list = make_map_info( "Region"=settings$Region, "spatial_list"=fit$spatial_list, "Extrapolation_List"=fit$extrapolation_list )

  # Plot diagnostic for encounter probability
  plot_encounter_diagnostic( Report=fit$Report, Data_Geostat=cbind("Catch_KG"=fit$data_frame[,'b_i']), DirName=working_dir)

  # Plot anisotropy
  plot_anisotropy( FileName=file.path(working_dir,"Aniso.png"), Report=fit$Report, TmbData=fit$data_list )

  # Plot index
  plot_biomass_index( DirName=working_dir, TmbData=fit$data_list, Sdreport=fit$parameter_estimates$SD, Year_Set=year_labels,
    Years2Include=years_to_plot, use_biascorr=TRUE)

  # Plot range indices
  plot_range_index(Report=fit$Report, TmbData=fit$data_list, Sdreport=fit$parameter_estimates$SD, Znames=colnames(fit$data_list$Z_xm),
    PlotDir=working_dir, Year_Set=year_labels, use_biascorr=TRUE )

  # Plot densities
  plot_maps(plot_set=3, MappingDetails=map_list[["MappingDetails"]], Report=fit$Report, Sdreport=fit$parameter_estimates$SD,
    PlotDF=map_list[["PlotDF"]], MapSizeRatio=map_list[["MapSizeRatio"]], Xlim=map_list[["Xlim"]], Ylim=map_list[["Ylim"]], 
    Year_Set=year_labels, Years2Include=years_to_plot, Rotate=map_list[["Rotate"]], Cex=map_list[["Cex"]], Legend=map_list[["Legend"]],
    zone=map_list[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE, TmbData=fit$data_list)

  # Plot quantile-quantile plot
  Q = plot_quantile_diagnostic( TmbData=fit$data_list, Report=fit$Report, FileName_PP="Posterior_Predictive",
    FileName_Phist="Posterior_Predictive-Histogram", FileName_QQ="Q-Q_plot", FileName_Qhist="Q-Q_hist", save_dir=working_dir )


## Option 3: Plotting package
devtools::install_github("merrillrudd/VASTPlotUtils")


VASTPlotUtils::plot_biomass_index(TmbData = fit$data_list, Sdreport=fit$parameter_estimates$SD, PlotName="Index_v2", category_names = 'EBS Pollock')
VASTPlotUtils::plot_maps(plot_set=3, Report=fit$Report, Sdreport=fit$parameter_estimates$SD, TmbData = fit$data_list, spatial_list = fit$spatial_list, DirName=working_dir, PlotName="v2", category_names="EBS Pollock", cex=0.7)

