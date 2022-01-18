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
  filter(Survey %in% c('GOA_LATE','GOA_EARLY') )

FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1,
                        "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2)
# Import extrapolation grid. I made this in buildExtrap.R
input_grid <- readRDS(here('data',"user_region.rds")) %>%
  filter(Region_Name %in% c('A3','A4'))

strata.limits = data.frame('STRATA' = c('A4','A3'), 'west_border' = c(-Inf,-145), 'east_border' = c(-145,-130))

# Make settings 
settings <- make_settings( Version = "VAST_v13_1_0",
                           n_x = 500,#1000, 
                           Region =  "gulf_of_alaska",
                           purpose = "index2", 
                           fine_scale = TRUE, 
                           # ObsModel= c(2,0), #c(2,1), #c(1,1) #c(10,2)
                           strata.limits=strata.limits,
                           treat_nonencounter_as_zero =FALSE,
                           knot_method = "grid", 
                           RhoConfig = RhoConfig,
                           FieldConfig = FieldConfig,
                           # bias.correct = TRUE,
                           # bias.correct.control = list(vars_to_correct = "Index_cyl"),
                           use_anisotropy = TRUE) 
gc()


wkdir <-  here('runs',paste0(Sys.Date(),"-AK_500nonEncounter/"))
dir.create(wkdir)
# Run model
fit <- fit_model( "settings"=settings, 
                  "Lat_i"=Data_Geostat[,'Lat'], 
                  "Lon_i"=Data_Geostat[,'Lon'], 
                  "t_i"=Data_Geostat[,'Year'], ## maybe force this to be seq(1990,2020,1)
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

