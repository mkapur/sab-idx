# Load package
library(VAST)
library(here)
library(dplyr)
library(ggplot2)

packageVersion('VAST')
packageVersion('FishStatsUtils')
Species <- "Anoplopoma fimbria"
# Load the data for VAST
Data_Geostat <- readRDS(file =  here('data','2022-01-14inputVast.rds')) %>% 
  filter(Survey %in% c('NWFSC_Combo',"Triennial_late" , "Triennial_early"))

## from v3
FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, 
                        "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2) 

strata.limits = data.frame('STRATA' = c('C1','C2'), 'north_border' = c(36,50), 'south_border' = c(25,36))
# Make settings 
settings <- make_settings( Version = "VAST_v12_0_0",
                          n_x = 500,#1000, 
                          Region =  "california_current",
                          purpose = "index2", #index 2 is recommended BUT doesn't provide annual estimates
                          fine_scale = TRUE, 
                          ObsModel= c(2,0), #c(2,1), #c(1,1) #c(10,2)
                          strata.limits=strata.limits,
                          treat_nonencounter_as_zero =FALSE,
                          RhoConfig = RhoConfig,
                          FieldConfig = FieldConfig,
                          knot_method = "grid", 
                          bias.correct = TRUE,
                          vars_to_correct = list(vars_to_correct = "Index_cyl"),
                          use_anisotropy = TRUE)
gc()


wkdir <-  here('runs',paste0(Sys.Date(),"-WC_500-nonEncounter-v12/"))
dir.create(wkdir)
# Run model
fit <- fit_model( "settings"=settings, 
                 "Lat_i"=Data_Geostat[,'Lat'], 
                 "Lon_i"=Data_Geostat[,'Lon'], 
                 "t_i"=Data_Geostat[,'Year'], 
                 "b_i"=Data_Geostat[,'Catch_KG'], 
                 "a_i"=Data_Geostat[,'AreaSwept'], 
                 "v_i"=Data_Geostat[,'Vessel'], 
                 # "input_grid"=input_grid, 
                 optimize_args=list("lower"=-Inf,"upper"=Inf),
                 "working_dir" =wkdir)


# Plot results
plot( fit )

# save the VAST model
saveRDS(fit,file = paste0(wkdir,"fit.RDS"))

