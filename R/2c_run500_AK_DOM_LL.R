# Load package
library(VAST)
library(here)
library(dplyr)
library(ggplot2)

packageVersion('VAST')
packageVersion('FishStatsUtils') 
# Load the data for VAST
# Data_Geostat <- readRDS(file =  here('data','2022-02-01inputVast.rds')) %>%  
#   filter(Survey %in% c('GOA_LATE','GOA_EARLY') & Year >= 1990) 
# Data_Geostat$Survey[Data_Geostat$Year < 1990] <- "Dropped_Years"
# Data_Geostat$Survey[Data_Geostat$Year < 1990] <- "Dropped_Years"
Data_Geostat <- readRDS("C:/Users/mkapur/Dropbox/UW/sab-idx/data/2023-03-24ak_LL.rds")

FieldConfig = matrix( c("IID","IID","IID","IID","IID","IID"), ncol=2, nrow=3,
                      dimnames=list(c("Omega","Epsilon","Beta"),
                                    c("Component_1","Component_2")) )
FieldConfig[2,1]=0 ## got a gradient warning

## FROM THORSON:
# The simplest way to interpolate among years is use `RhoConfig[c("Beta1","Beta2")] = 2 or 3 or 4` and
# `treat_nonencounter_as_zero = FALSE` ...
# this then ensures that intercepts follow something resembling a Brownian bridge (mean reverting random walk) 
# to nearest years with data.  If doing this, it also typically makes sense to specify
# `RhoConfig[c("Epsilon1","Epsilon2)] = 2 or 3 or 4` as well, so that spatio-temporal 
# hotspots are interpolated.
RhoConfig  = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)
# RhoConfig  = c("Beta1" = 2, "Beta2" = 2, "Epsilon1" = 2, "Epsilon2" = 2)
strata.limits = data.frame('STRATA' = c('A4','A3'), 'west_border' = c(-Inf,-145), 
                           'east_border' = c(-145,-130))

# Make settings 
# user_region <- readRDS(here('data','user_region.rds') ) %>% filter(Region_Name %in% c('A3','A4'))
settings <- spatial_args_default <- make_settings( Version = "VAST_v13_1_0",
                           n_x = 500,
                           Region =  "gulf_of_alaska",
                           purpose = "index2", 
                           fine_scale = TRUE,
                           strata.limits=strata.limits,
                           treat_nonencounter_as_zero = TRUE,
                           knot_method = "grid",
                           ObsModel= c(2,4), ## to deal with 100% catch rates 
                           RhoConfig = RhoConfig,
                           FieldConfig = FieldConfig,
                           use_anisotropy = TRUE)

gc()
wkdir <-  here('runs',paste0(Sys.Date(),"-AK_500-145-v13-1-DOMLL-OBS2-4_Region=GOA/"))
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


