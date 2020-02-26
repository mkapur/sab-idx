# Choose species
Species <- "Anoplopoma fimbria"
Species_code <- 'SAB' # switch(Species, "arrowtooth flounder"="arrow", "Pacific ocean perch"="POP")
Surveys_to_include <- c("Triennial", "WCGBTS", "BCs", "BCo",
                        "BCt", "AK_DOM_LL", "GOA", "EBS")[c(1:4,6:7)] #This will only work for years after 2003


# Date
Date <- Sys.Date()
BaseQ <- c("GOA_late","AK_DOM_late", "AK_DOM_LL","WCGBTS")[3]
Year_Range = c(1980, 2018)

FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, 
                        "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
Aniso = FALSE
Version = "VAST_v8_0_0" # get_latest_version( package="VAST" )
OverdispersionConfig = c("Eta1"=0, "Eta2"=0)
ObsModel <- c(2,0) ## gamma for catch, pos only for enctr  # 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
Spatial_Smoother = c("Index", "Smoothed_Index", "Spatiotemporal", "Spatiotemporal_AR")[3]
BC_catchability = c("Separate")[1]
BiasCorr = c(FALSE,TRUE)[1]
Zone = 5
Options =  c("Calculate_Range"=TRUE, "Calculate_effective_area"=TRUE,
             "SD_site_logdensity"=FALSE)

Use_REML = TRUE
fine_scale = TRUE
create_strata_per_region = TRUE
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25


# Derived - NOTE THAT RhoConfig[1:2] must be 0 when using ObsModel[2]=3:  Other options are not coded to work together
if( Spatial_Smoother=="Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0)
if( Spatial_Smoother=="Smoothed_Index" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=2, "Epsilon2"=2)
if( Spatial_Smoother=="Spatiotemporal" ) RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2) # Pointwise random walk (epsilon is RW, Beta1 is constant for all years)
if( Spatial_Smoother=="Spatiotemporal_AR" ) RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=3, "Epsilon2"=3) # Pointwise autocorrelation (beta is freely estimated in each year)

# Save options for future records
Record <- ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","BC_catchability","BaseQ","Use_REML","fine_scale",
                                          "FieldConfig","RhoConfig","OverdispersionConfig", "Year_Range",
                                          "ObsModel","Aniso","fine_scale","Options", "create_strata_per_region") )

save( Record, file=paste0(DateFile,"/Record.RData"))