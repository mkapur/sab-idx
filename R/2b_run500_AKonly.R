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

FieldConfig = matrix( c("IID","IID","IID","IID","IID","IID"), ncol=2, nrow=3, dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")) )
RhoConfig  = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

strata.limits = data.frame('STRATA' = c('A4','A3'), 'west_border' = c(-Inf,-146), 'east_border' = c(-146,-130))

# Make settings 
settings <- make_settings( Version = "VAST_v13_1_0",
                           n_x = 500,#1000, 
                           Region =  "gulf_of_alaska",
                           purpose = "index2", 
                           fine_scale = TRUE, 
                           # ObsModel= c(2,0), #c(2,1), #c(1,1) #c(10,2)
                           strata.limits=strata.limits,
                           treat_nonencounter_as_zero =T,
                           knot_method = "grid", 
                           RhoConfig = RhoConfig,
                           FieldConfig = FieldConfig,
                           # bias.correct = TRUE,
                           # bias.correct.control = list(vars_to_correct = "Index_cyl"),
                           use_anisotropy = TRUE) 
gc()


wkdir <-  here('runs',paste0(Sys.Date(),"-AK_500-146/"))
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

