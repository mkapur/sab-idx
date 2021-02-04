## Jan 2021 update
## trying to minimize repeated process
## kapurm@uw.edu

## I downloaded R v4.0.03 on desktop 01 Feb 2021, and updated Rtools
## Updating all VAST, FishStatsUtils here to start fresh.

## load pre-made datasets
require(here)
require(VAST)
require(dplyr)

## loading datasets made with draft3_v3_2019; last saved Jan 30
list.files(here('input'), full.names = T) %>% lapply(.,load, .GlobalEnv)

Record$BaseQ <- c("GOA_late", "AK_DOM_LL","WCGBTS")[2]
Record$n_x <- 500
Record$Version <- "VAST_v12_0_0"

attach(Record)

outfile <- here('runs',paste0(Sys.Date(),"_BaseQ=",BaseQ,"_nx=",n_x,"_",Version))
if(!exists(outfile)) dir.create(outfile)

save(Record,file = paste0(outfile, "/Record.Rdata"))

## build Q_ik accordingly
# source("https://raw.githubusercontent.com/James-Thorson/utilities/9ea779ab14361957eb1021fe896971490805d313/R/vector_to_design_matrix.R")
if( length(unique(Data_Geostat[,'Survey']))==1  | 
    length(unique(Data_Geostat[,'Region'])) == 1){
  Q_ik <- matrix(0, ncol=1, nrow=nrow(Data_Geostat))
}else{
  Q_ik <- ThorsonUtilities::vector_to_design_matrix( Data_Geostat[,'Survey'] )
  if( !(BaseQ %in% colnames(Q_ik)) ) stop("Problem with Q_ik")
  Q_ik <- Q_ik[,-which(colnames(Q_ik)==BaseQ),drop=FALSE]
}
ncol(Q_ik) == (length(unique(Data_Geostat$Survey))-1) ## should have ncol == fleets-1


TmbData <- VAST::make_data(
  #"X_itp"=X_itp, 
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
  "v_i"= as.numeric(as.factor(Data_Geostat[,'Vessel'])),#-1,
  "t_i"= Data_Geostat[,'Year'], 
  "Q_ik" = Q_ik,
  "spatial_list"=Spatial_List, 
  "Options"=Options )

save(TmbData, file = paste0(outfile,"/TmbData.Rdata"))

# Make TMB object #https://github.com/kaskr/adcomp/issues/321
TmbList <-
  make_model(
    "build_model" = TRUE,
    "TmbData" = TmbData,
    "RunDir" = outfile,
    "Version" =Version, ## paste from documents/r/winlibrary/vast/
    "RhoConfig" = RhoConfig,
    "loc_x" = Spatial_List$loc_x,
    "Method" = Method,
    "TmbDir" = here()
  )
save(TmbList, file = paste0(outfile,"/TmbList.Rdata"))

Obj <- TmbList[["Obj"]]

Opt <- TMBhelper::fit_tmb(
  obj = Obj,
  lower = TmbList[["Lower"]],
  upper = TmbList[["Upper"]],
  newtonsteps = 1,
  getsd = TRUE,
  getJointPrecision = FALSE, ## required for SIMULATOR
  bias.correct = FALSE, ## could try false
  bias.correct.control = list(vars_to_correct = "Index_cyl"),
  savedir = outfile
)  


Report <- TmbList$Obj$report()
ParHat <- TmbList$Obj$env$parList()

# Save stuff [NOTE OBJ IS INSIDE SAVE]
Save <- list("Opt"=Opt, 
             "Report"=Report,
             "ParHat"=TmbList$Obj$env$parList(Opt$par),
             'Obj' = Obj)
save(Save, file=paste0(outfile,"/Save_original.RData"))

source(here('R','idxPlots.R')) ## automates all plots
