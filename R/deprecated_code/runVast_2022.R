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
   filter(Survey %in% c('GOA_LATE','GOA_EARLY') )

# Define strata
# strata.limits <- data.frame(STRATA = as.factor('All_areas'))
# strata.limits <- data.frame('STRATA' = c('A4','A3','B3','B2','C2','C1'), 
#                             'north_border' = c(65.5,), 
#                             'south_border' = c(), 
#                             'west_border' = c(), 
#                             'east_border' = c())
# strata.limits <- data.frame('STRATA' = c('C1','C2','B2','B3','A3','A4'))
## from flathead
# FieldConfig = matrix( c("IID","IID","IID","IID","IID","IID"), ncol=2, nrow=3,
#                       dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")) )
## from v3
FieldConfig = matrix( c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, 
                        "Epsilon2"=1, "Beta1"="IID", "Beta2"="IID"), nrow=3, byrow=TRUE )
RhoConfig = c("Beta1"=3, "Beta2"=3, "Epsilon1"=2, "Epsilon2"=2) 
# Import extrapolation grid. I made this in buildExtrap.R
input_grid <- readRDS(here('data',"user_region.rds")) %>%
  filter(Region_Name %in% c('A3','A4'))
strata.limits <- input_grid %>%
  group_by(Region_Name) %>% 
  summarise(west_border=min(Lon),east_border = max(Lon), north_border = min(Lat),south_border = max(Lat)) %>%
  select(STRATA = Region_Name, everything()) %>%
  filter(STRATA %in% unique(input_grid$Region_Name)) %>%
  data.frame() 
  # select(STRATA,north_border,south_border) %>% 

# strata.limits = data.frame('STRATA' = c('A4','A3'), 'west_border' = c(-Inf,-145), 'east_border' = c(-145,-130))
strata.limits = data.frame('STRATA' = c('C1','C2'), 'north_border' = c(36,50), 'south_border' = c(25,36))

# strata.limits <- data.frame('STRATA' = "west_of_140W", 'west_border' = -Inf, 'east_border' = -140 )
# Make settings 
settings <- make_settings( Version = "VAST_v13_1_0",
                          n_x = 500,#1000, 
                          Region =  "california_current",
                          purpose = "index2", 
                          fine_scale = TRUE, 
                          ObsModel= c(2,0), #c(2,1), #c(1,1) #c(10,2)
                          strata.limits=strata.limits,
                          knot_method = "grid", 
                          bias.correct = TRUE,
                          use_anisotropy = TRUE)


# input_grid=cbind(Lat=mygrid$Lat,
#                  Lon=mygrid$Lon,
#                  Area_km2=mygrid$Shape_Area)  # Extrapolation grid already in km2
gc()


wkdir <-  here('runs',paste0(Sys.Date(),"-WC_500/"))
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
# fit <- fit_model(settings=settings,
#                  Lat_i=Data_Geostat$Lat, Lon_i=Data_Geostat$Lon,
#                  t_i=Data_Geostat$Year, b_i=Data_Geostat$Catch_KG,
#                  a_i=Data_Geostat$AreaSwept,
#                  input_grid=input_grid,
#                  working_dir = wkdir)
 # user_region <- readRDS('user_region.rds')
# fit <- fit_model(settings=settings,
#                  Lat_i=dat$Lat, Lon_i=dat$Lon,
#                  t_i=dat$Year, b_i=dat$Catch_KG,
#                  a_i=dat$AreaSwept_km2,
#                  input_grid=user_region)

# Plot results
plot( fit )

# save the VAST model
saveRDS(fit,file = paste0(wkdir,"fit.RDS"))

