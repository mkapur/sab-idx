## Run at NEW STRATA
## Requires a pre-executed mod with MLEs already discovered.
## Re
sourceFile <- DateFile
  
  # "C:/Users/maia kapur/Dropbox/UW/sab-idx/runs/2019-10-24_nx=100_Species=SAB_Triennial_WCGBTS_BC_GOA_EBS"

### EXPERIMENT W NEW STRATA OUTPUT----
## COLE:  I do know if you rebuild the model with your new extrapolation settings 
## and then start it from the MLE it should just stop and then the prediction stuff will work out. 
## Because if I understand right the data won't change. 
## It may take a few steps but should be a fraction of the run time.

strata.limits.full <- data.frame(
  'STRATA' = c("R5","R4", "R3","R2","R1"),
  'west_border' = c(-180, -145, -135,-135,-135),
  'east_border' = c(-145, -135, -110,-110,-110),
  'north_border' = c(65, 65, 65,50,36),
  'south_border' = c(50, 50, 50, 36,26 ))

Region <- strata.limits.full$STRATA

## load and reclassify regions
# Data_Geostat <- load(paste0(sourceFile, "/Data_Geostat.Rdata"))
# class(Data_Geostat[i, 'Lat']) <- 'numeric'
# class(Data_Geostat[i, 'Lon']) <- 'numeric'

for (i in 1:nrow(Data_Geostat)) {
  Data_Geostat[i, 'Region'] <-
    paste(strata.limits.full$STRATA[which(
      Data_Geostat[i, 'Lat'] >= strata.limits.full$south_border &
        Data_Geostat[i, 'Lat'] <= strata.limits.full$north_border &
        Data_Geostat[i, 'Lon'] >= strata.limits.full$west_border &
        Data_Geostat[i, 'Lon'] <= strata.limits.full$east_border
    )])
  
  
}


tempExtrap <-
  make_extrapolation_info(
    Region = 'User',
    strata.limits = strata.limits.full,
    create_strata_per_region=TRUE,
    zone = 5,
    input_grid = data.frame(
      'Lon' = Data_Geostat$Lon,
      'Lat' = Data_Geostat$Lat,
      'Area_km2' = rep(4, nrow(Data_Geostat))
    )
  )


##  Define regions ----
# Including two different approaches
# if( TRUE ){
#   if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
#   if("BC" %in% Surveys_to_include) Region = c( Region, "British_Columbia" )
#   if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
#   if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
#   Extrapolation_List <- make_extrapolation_info( Region=Region, strata_to_use=c('SOG','WCVI','QCS','HS','WCHG'),
#                                                  zone=Zone, create_strata_per_region=create_strata_per_region )
# }else{
#   if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
#   if("BC" %in% Surveys_to_include) Region = c( Region, "Other" )
#   if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
#   if("EBS" %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
#   observations_LL <- Data_Geostat[ which(Data_Geostat[,'Region']=="BC"), c('Lat','Lon') ]
#   Extrapolation_List <-  make_extrapolation_info( Region=Region,
#                                                   observations_LL=observations_LL, zone=Zone, create_strata_per_region=create_strata_per_region )
# }


Spatial_List <-
  make_spatial_info(
    n_x = n_x,
    Lon = Data_Geostat[, 'Lon'],
    Lat = Data_Geostat[, 'Lat'],
    Extrapolation_List = tempExtrap,
    DirPath = DateFile,
    Save_Results = FALSE,
    "knot_method" = "grid",
    refine = FALSE,
    fine_scale = fine_scale
  )



  ## from CC version
  TmbData <- VAST::make_data(#"X_itp"=X_itp, 
    #"X_gtp"=X_gtp, 
    #"Xconfig_zcp"=Xconfig_zcp, 
    "Version"=Version,
    "Aniso"=Aniso, 
    "FieldConfig"=FieldConfig, 
    "OverdispersionConfig" = c("Eta1"=0, "Eta2"=0), 
    "RhoConfig"=RhoConfig, 
    "ObsModel"=ObsModel, 
    "c_i"=rep(0,nrow(Data_Geostat)), 
    "b_i"=Data_Geostat[,'Catch_KG'], 
    "a_i"=Data_Geostat[,'AreaSwept_km2'], 
    # "v_i"=as.numeric(Data_Geostat[,'Vessel']),#-1, 
    "t_i"=Data_Geostat[,'Year'], 
    "Q_ik" = Q_ik,
    "spatial_list"= Spatial_List,
    "Options"=Options )


mle_params <- Obj$env$parList() ## need this from previous model run; it is inside Save.Rdata
## OBJ derives from TmbList which is a make_model object.

# new_obj <- MakeADFun(data, parameters = mle_params) ## will rerun thru derived quantities AT mle, no need to refit
new_obj <-  make_model(
  Parameters = mle_params,
  "build_model" = TRUE,
  "TmbData" = TmbData,
  "RunDir" = DateFile,
  "Version" = Version,
  "RhoConfig" = RhoConfig,
  "loc_x" = Spatial_List$loc_x,
  "Method" = Method,
  "TmbDir" = getwd()
)
# mle_params_change <- mle_params
# mle_params_change[1] <- 5
# quantitites_changes <- Obj$env(mle_params_change)


Obj2 <- new_obj[["Obj"]]
Obj2$par['lambda2_k'] ## should not be NA or zero
Obj2$par['lambda1_k'] 
# Obj$par['gamma1_k'] 

Opt2 <-
  TMBhelper::fit_tmb(
    obj = Obj2,
    lower = new_obj[["Lower"]],
    upper = new_obj[["Upper"]],
    newtonsteps = 1,
    getsd = TRUE,
    bias.correct = BiasCorr,
    bias.correct.control = list(vars_to_correct = "Index_cyl"),
    savedir = DateFile
  )  # , rel.tol=1e-20

Report <- new_obj$Obj$report()
ParHat <- new_obj$Obj$env$parList()

# Save stuff
Save <- list("Opt"=Opt2, "Report"=Report, "ParHat"=TmbList$Obj$env$parList(Opt2$par),'Obj' = Obj2)
save(Save, file=paste0(DateFile,"Save_growthStrata.RData"))

Index <- plot_biomass_index( DirName=DateFile, 
                             TmbData=TmbData, 
                             Sdreport=Opt2$SD, 
                             Year_Set=Year_Set, 
                             PlotName = 'newstrata_index',
                             strata_names=rev(Region), 
                             plot_log=TRUE, width=6, height=6 ) # , tot


# Positive catch rate diagnostics
# Q <- plot_quantile_diagnostic( TmbData=TmbData, Report=Report, DateFile=DateFile, FileName_PP = NULL, FileName_QQ = NULL,
#                                FileName_Phist = NULL, FileName_Qhist = NULL) # SpatialDeltaGLMM::

# Pearson residuals diagnostics
# plot_residuals( Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], 
#                 extrapolation_list = Extrapolation_List,
#                 TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, spatial_list=Spatial_List )

# Plot density
# plot_maps( plot_set=3, Report=Report, PlotDF=MapDetails_List[["PlotDF"]], 
#            working_dir=DateFile, Year_Set=Year_Set )
