library(VAST)
library(TMB)
library(dplyr)
library(tidyr)
library(reshape)
library(mapdata)
library(ggplot2)

# Directories ----
comp.name <- c("mkapur",'maia kapur')[2]
RootFile <- paste0( "C:/Users/",comp.name ,"/Dropbox/UW/sab-idx/runs/") 

## Specify model run this is based on
sourceDate <- "/2020-01-23"
sourceDir <- list.dirs(RootFile, recursive = FALSE)[grepl(sourceDate,list.dirs(RootFile, recursive = FALSE))]
## load rdata from there
list.files(sourceDir, pattern = paste(c("*.Rdata","*.RData"), collapse = "|"), full.names = TRUE) %>% lapply(.,load,.GlobalEnv)
DateFile <- paste0(RootFile,Sys.Date(),"newStrata_",basename(sourceDir)); dir.create(DateFile)

n_x = 500
## Custom Strata ----
strata.limits.full <- data.frame(
  'STRATA' = c("R5","R4", "R3","R2","R1"),
  'west_border' = c(-180, -145, -135,-135,-135),
  'east_border' = c(-145, -135, -110,-110,-110),
  'north_border' = c(65, 65, 65,50,36),
  'south_border' = c(50, 50, 50, 36,26 ))

Region <- strata.limits.full$STRATA

Data_Geostat <- Data_Geostat %>% filter(Lon < -50) ## drop wonky AK values (very few)
levels(Data_Geostat$Region) <- factor(strata.limits.full$STRATA)
for (i in 1:nrow(Data_Geostat)) {
  Data_Geostat[i, 'Region'] <-
    paste(strata.limits.full$STRATA[which(
      Data_Geostat[i, 'Lat'] >= strata.limits.full$south_border &
        Data_Geostat[i, 'Lat'] <= strata.limits.full$north_border &
        Data_Geostat[i, 'Lon'] >= strata.limits.full$west_border &
        Data_Geostat[i, 'Lon'] <= strata.limits.full$east_border
    )])
}

save(Data_Geostat, file = paste0(DateFile, "/Data_Geostat.Rdata"))

Extrapolation_List <-
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

png(paste0(DateFile,"/Extrapolation_List.png"), width = 8, height = 6, units = 'in', res = 520)
plot( Extrapolation_List )
dev.off()


## Make spatial list ----
Spatial_List <- make_spatial_info( n_x=500, Lon=Data_Geostat[,'Lon'], 
                                   Lat=Data_Geostat[,'Lat'], 
                                   Extrapolation_List=Extrapolation_List,
                                   DirPath=DateFile, Save_Results=FALSE, 
                                   "knot_method"="grid", refine=FALSE, 
                                   fine_scale=FALSE )

png(paste0(DateFile,"/Spatial_List.png"), width = 8, height = 6, units = 'in', res = 520)
plot( Spatial_List ) 
dev.off()


# Plot details
MapDetails_List <- make_map_info( "Region"="Other",
                                  "spatial_list"=Spatial_List,
                                  "Extrapolation_List"=Extrapolation_List )
Year_Set <- min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])


## no EnumK here because no 0 encounter

# Make catchability matrix (Q_i) ----
# The resulting Q_ik will have n-1 columns, with baseQ excluded.
if( length(unique(Data_Geostat[,'Survey']))==1  |
    length(unique(Data_Geostat[,'Region'])) == 1){
  Q_ik <- matrix(0, ncol=1, nrow=nrow(Data_Geostat))
}else{
  Q_ik <- ThorsonUtilities::vector_to_design_matrix( Data_Geostat[,'Survey'] )
  if( !(BaseQ %in% colnames(Q_ik)) ) stop("Problem with Q_ik")
  Q_ik <- Q_ik[,-which(colnames(Q_ik)==BaseQ),drop=FALSE]
}
head(Q_ik) ## should have ncol == fleets-1

# Plot location of data

source("./R/vastOptions.R")
## from CC version
TmbData <- VAST::make_data(
  #"X_itp"=X_itp, 
  #"X_gtp"=X_gtp, 
  #"Xconfig_zcp"=Xconfig_zcp, 
  "Version"="VAST_v8_0_0",
  "Aniso"=Aniso, 
  "FieldConfig"=FieldConfig, 
  "OverdispersionConfig" = c("Eta1"=0, "Eta2"=0), 
  "RhoConfig"=RhoConfig, 
  "ObsModel"= ObsModel, 
  "c_i"=rep(0,nrow(Data_Geostat)), 
  "b_i"=Data_Geostat[,'Catch_KG'], 
  "a_i"=Data_Geostat[,'AreaSwept_km2'], 
  "v_i"= as.numeric(as.factor(Data_Geostat[,'Vessel'])),#-1,
  "t_i"= Data_Geostat[,'Year'], 
  "Q_ik" = Q_ik,
  "spatial_list"=Spatial_List, 
  "Options"= Options)

save(TmbData, file = paste0(DateFile,"/TmbData.Rdata"))


Obj <- TmbList$Obj ## loaded from previous

mle_params <- Obj$env$parList() ## need this from previous model run; it is inside Save.Rdata
## OBJ derives from TmbList which is a make_model object.

# new_obj <- MakeADFun(data, parameters = mle_params) ## will rerun thru derived quantities AT mle, no need to refit
new_obj <-  make_model(
  # Parameters = mle_params,
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

# 
# # Make TMB object
# TmbList <- make_model("build_model"=TRUE, "TmbData"=TmbData, "RunDir"=DateFile, 
#                       "Version"=Version, "RhoConfig"=RhoConfig, 
#                       "loc_x"=Spatial_List$loc_x, "Method"=Method, "TmbDir"=getwd())
# 
# save(TmbList, file = paste0(DateFile,"/TmbList.Rdata"))
# 
# # Run model ----
# Obj <- TmbList[["Obj"]]
# Obj$par['lambda2_k'] ## should not be NA UNLESS length(fleet) == 1/QIK = 1
# Obj$par['lambda1_k'] 
# # Obj$par['gamma1_k'] 
# 
# Opt <- TMBhelper::fit_tmb(
#   obj = Obj,
#   lower = TmbList[["Lower"]],
#   upper = TmbList[["Upper"]],
#   newtonsteps = 1,
#   getsd = TRUE,
#   bias.correct = TRUE,
#   bias.correct.control = list(vars_to_correct = "Index_cyl"),
#   savedir = DateFile
# )  # , rel.tol=1e-20
# 
# 
# Report <- TmbList$Obj$report()
# ParHat <- TmbList$Obj$env$parList()
# 
# # Save stuff [NOTE OBJ IS INSIDE SAVE]
# Save <- list("Opt"=Opt, 
#              "Report"=Report,
#              "ParHat"=TmbList$Obj$env$parList(Opt$par),
#              'Obj' = Obj)
# save(Save, file=paste0(DateFile,"Save_original.RData"))
# 
# plot_data( Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, 
#            Data_Geostat=Data_Geostat, PlotDir=DateFile, 
#            Plot1_name="Data_and_knots.png", Plot2_name="Data_by_year.png", col="red")

# Plot index
Index <- plot_biomass_index( DirName=DateFile, 
                             TmbData=TmbData, 
                             use_biascorr = BiasCorr,
                             Sdreport=Opt$SD, 
                             Year_Set=Year_Set, 
                             strata_names=c('AllAreas',Region), 
                             plot_log=TRUE, width=6, height=6 ) # , total_area_km2=sum(a_xl[,1])


# load(paste0(DateFile,"Save_original.Rdata"))
Opt <- Save$Opt
Report <- Save$Report

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

## plot easting-northing shifts
#To plot effective area occupied, please re-run with Options['Calculate_effective_area']=1
plot_range_index( Sdreport=Save$Opt$SD, Report=Save$Report, Year_Set=Year_Set, 
                  TmbData=TmbData, Znames=colnames(TmbData$Z_xm),
                  PlotDir=DateFile 
)
# source("https://raw.githubusercontent.com/nwfsc-assess/VASTWestCoast/2473eb0ca2c25aa780e39ff1a94e7252d0d335bc/R/summary_nwfsc.R")
source("./R/summary_nwfscMK.r")
TableC <- summary_nwfscMK(obj = Save$Obj, 
                          sdreport = Save$Opt$SD, 
                          savedir = DateFile)[[3]]

TableC %>% data.frame() %>% 
  exp() %>% round(.,2) %>% 
  mutate('PAR'=row.names(TableC)) %>%
  write.csv(.,file = paste0(DateFile,'tableC_mod.csv'))
