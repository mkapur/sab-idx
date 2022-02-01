# Load package
library(VAST)
library(here)
library(dplyr)
library(ggplot2)

packageVersion('VAST')
packageVersion('FishStatsUtils') 
# Load the data for VAST
Data_Geostat <- readRDS(file =  here('data','2022-02-01inputVast.rds')) %>%  
  filter(Survey %in% c('GOA_LATE','GOA_EARLY') & Year < 2021) 
# Data_Geostat$Survey[Data_Geostat$Year < 1990] <- "Dropped_Years"
# Data_Geostat$Survey[Data_Geostat$Year < 1990] <- "Dropped_Years"


FieldConfig = matrix( c("IID","IID","IID","IID","IID","IID"), ncol=2, nrow=3, 
                      dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")) )
RhoConfig  = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

strata.limits = data.frame('STRATA' = c('A4','A3'), 'west_border' = c(-Inf,-146), 'east_border' = c(-146,-130))

# Make settings 
settings <- make_settings( Version = "VAST_v12_0_0",
                           n_x = 500, 
                           Region =  "gulf_of_alaska",
                           purpose = "index2", 
                           fine_scale = TRUE, 
                           strata.limits=strata.limits,
                           treat_nonencounter_as_zero = TRUE,
                           knot_method = "grid", 
                           ObsModel= c(2,1), #c(1,1)
                           RhoConfig = RhoConfig,
                           FieldConfig = FieldConfig,
                           use_anisotropy = TRUE) 
gc()


wkdir <-  here('runs',paste0(Sys.Date(),"-AK_500-146-v12-500m/"))
dir.create(wkdir)
# Run model
fit <- fit_model( "settings"=settings, 
                  "Lat_i"=Data_Geostat[,'Lat'], 
                  "Lon_i"=Data_Geostat[,'Lon'], 
                  "t_i"=Data_Geostat[,'Year'], ## maybe force this to be seq(1990,2020,1)
                  "b_i"=Data_Geostat[,'Catch_KG'], 
                  "a_i"=Data_Geostat[,'AreaSwept'], 
                  "v_i"=Data_Geostat[,'Vessel'], 
                  optimize_args=list("lower"=-Inf,"upper"=Inf),
                  "working_dir" =paste0(wkdir,"/"))
# Plot results
plot( fit )

# save the VAST model
saveRDS(fit,file = paste0(wkdir,"fit.RDS"))

