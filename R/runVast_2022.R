# Load package
library(VAST)
library(here)
library(dplyr)
library(ggplot2)

packageVersion('VAST')
packageVersion('FishStatsUtils')
Species <- "Anoplopoma fimbria"
# Load the data for VAST
Data_Geostat <- readRDS(file =  here('data','2022-01-14inputVast.rds'))

# Define strata
# strata.limits <- data.frame(STRATA = as.factor('All_areas'))
# strata.limits <- data.frame('STRATA' = "west_of_145W", 'west_border' = -Inf, 'east_border' = -145 )

## from flathead
# FieldConfig = matrix( c("IID","IID","IID","IID","IID","IID"), ncol=2, nrow=3,
#                       dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")) )
## from v3
FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, 
                        "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
RhoConfig  = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 0, "Epsilon2" = 0)

# Make settings 
settings <- make_settings( Version = "VAST_v12_0_0",
                          n_x = 750,#1000, 
                          Region = "User", #"gulf_of_alaska",
                          purpose = "index2", 
                          fine_scale = TRUE, 
                          ObsModel= c(2,0), #c(2,1), #c(1,1) #c(10,2)
                          # strata.limits=strata.limits, 
                          knot_method = "grid", 
                          bias.correct = TRUE,
                          use_anisotropy = TRUE)

# Import extrapolation grid. I made this in buildExtrap.R
input_grid <- readRDS(here('data',"user_region.rds"))

# input_grid=cbind(Lat=mygrid$Lat,
#                  Lon=mygrid$Lon,
#                  Area_km2=mygrid$Shape_Area)  # Extrapolation grid already in km2
gc()


wkdir <-  here('runs',paste0(Sys.Date(),"-AKWC_750"))
dir.create(wkdir)
# Run model
fit <- fit_model( "settings"=settings, 
                 "Lat_i"=Data_Geostat[,'Lat'], 
                 "Lon_i"=Data_Geostat[,'Lon'], 
                 "t_i"=Data_Geostat[,'Year'], 
                 "b_i"=Data_Geostat[,'Catch_KG'], 
                 "a_i"=Data_Geostat[,'AreaSwept'], 
                 "v_i"=Data_Geostat[,'Vessel'], #### ##was ok to leave in because it's all "missing" or zero, so no vessel effects
                 "input_grid"=input_grid, 
                 optimize_args=list("lower"=-Inf,"upper"=Inf),
                 "working_dir" =wkdir)

# user_region <- readRDS('user_region.rds')
# fit <- fit_model(settings=settings,
#                  Lat_i=dat$Lat, Lon_i=dat$Lon,
#                  t_i=dat$Year, b_i=dat$Catch_KG,
#                  a_i=dat$AreaSwept_km2,
#                  input_grid=user_region)

# Plot results
plot( fit )

# save the VAST model
saveRDS(fit,file = here('runs',paste0(Sys.Date(),"-VASTfit.RDS")))

