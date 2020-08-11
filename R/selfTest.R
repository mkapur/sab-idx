## Self Test per https://github.com/James-Thorson-NOAA/VAST/wiki/Simulator
## Needs to be >= V4.4
## Using run from 23 Jan 2020



## needs to be a fresh run; got singularity issues on updated versions
source("./R/draft3_v3.R")
 ## this is from dev FishStatsUtils -- if I update from v2.5 my original model no longer works
## so just sourcing the function instead

source("https://raw.githubusercontent.com/James-Thorson-NOAA/FishStatsUtils/f5b7f29096165ee330652b3afdf76a7c60148a5a/R/simulate_data.R")

Sim <- Obj$simulate( complete=TRUE )

# fit_orig <- Opt
# Data_sim <- simulate_data( fit_orig, type=3 )

Extrapolation_List <-
  make_extrapolation_info(
    Region = Region,
    strata_to_use = c('SOG', 'WCVI', 'QCS', 'HS', 'WCHG'),
    zone = Zone, 
    observations_LL = Data_Geostat[,c('Lat','Lon')],
    create_strata_per_region = create_strata_per_region
  )


settings = make_settings( n_x=n_x, 
                          Region=Data_Geostat$Region, 
                          purpose="index",
                          strata.limits = Extrapolation_List,
              
                          # strata.limits=example$strata.limits,
                          bias.correct=FALSE, 
                          ObsModel=ObsModel,
                          RhoConfig=RhoConfig )
# Run EM on simulated data
fit_sim = fit_model(
  "observations_LL" = Data_Geostat[,c('Lat','Lon')],
  "grid_dim_km" = c(100,100),## error about memory crashes
  "settings" = settings, ## settings_em
  "Lat_i" = Data_Geostat[, 'Lat'],#example$sampling_data[, 'Lat']
  "Lon_i" = Data_Geostat[, 'Lon'],
  "t_i" = Data_Geostat[, 'Year'],
  "c_i" = rep(0, nrow(Data_Geostat)),
  "b_i" = Sim$b_i, #Data_sim$b_i,
  "a_i" = Data_Geostat[, 'AreaSwept_km2'],
  "v_i" = Data_Geostat[, 'Vessel'],
  "working_dir" =here::here("runs","selfTest_06May_basedOn02May"),
  "newtonsteps" = 0
)
