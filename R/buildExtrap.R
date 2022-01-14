# build extrapolation grid
## my understanding from here is that i can export a shapefile with my 6 areas and fill in the estimates
# check out https://github.com/James-Thorson-NOAA/VAST/wiki/Creating-an-extrapolation-grid method 2

require(sf)
load(here('data',"sub_area_clips_50N.Rdata") ) ## as used for MSE plotting; see make_spdf.R in sab-mse

## MANUALLY RENAME THE CLIPS
clips[[1]]$Region_Name <- 'A4'
clips[[2]]$Region_Name <- 'A3'
clips[[3]]$Region_Name <- 'B3'
clips[[4]]$Region_Name <- 'B2'
clips[[5]]$Region_Name <- 'C2'
clips[[6]]$Region_Name <- 'C1'


allsubareas <- do.call(rbind, clips)
spdf <- sf::as_Spatial(allsubareas)
sps <- spTransform(spdf, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
# plot(sps) ## sanity check, shouldn't be split up
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
# utmzone <- floor((lon + 180)/6)+1
utmzone = 5 ## from v3
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 4000
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL
## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, 
                             # Id =NA,
                             factor(Region_Name),
                             Area_km2=( (cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, !is.na(Region_Name))
region$Lon[region$Lon>0] <- region$Lon[region$Lon>0]*-1 ## flip around dateline
## This is the final file needed.
str(region)
## > 'data.frame':	106654 obs. of  5 variables:
##  $ Lon     : num  -166 -166 -166 -166 -166 ...
##  $ Lat     : num  53.9 53.9 54 53.9 53.9 ...
##  $ Id      : Factor w/ 1 level "all": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Area_km2: num  4 4 4 4 4 4 4 4 4 4 ...
##  $ row     : int  401 402 975 976 977 978 1549 1550 1551 1552 ...

### Save it to be read in and passed to VAST later.
saveRDS(region, file = here('data',"user_region.rds"))
### End of creating user extrapolation region object


## Plot to confirm it looks good and correctly labeled
ggplot(region, aes(x = Lon, y = Lat, col = factor(Region_Name),fill = factor(Region_Name))) +
  ggsidekick::theme_sleek() +
  geom_point() +
  scale_color_manual(values = rev(c("#015b58" ,"#2c6184", "#1f455e", "#ba7999" ,"#984e73" ,"#a8bbcc" )))+
  scale_fill_manual(values = rev(c("#015b58" ,"#2c6184", "#1f455e", "#ba7999" ,"#984e73" ,"#a8bbcc" )))

ggsave(last_plot(), 
       file = here('figures','user_region.png'),
       height = 7, width = 7, unit = 'in',dpi = 400)  


### Show how to run it in VAST
library(VAST)
dat <- load_example(data_set='EBS_pollock')$sampling_data
dat <- subset(dat, Year==2000)

settings <- make_settings(n_x=200, Region='User',
                          purpose="index2", bias.correct=FALSE,
                          knot_method='grid')
settings$FieldConfig[2,] <- 0 ## turn off temporal components
user_region <- readRDS('user_region.rds')
fit <- fit_model(settings=settings,
                 Lat_i=dat$Lat, Lon_i=dat$Lon,
                 t_i=dat$Year, b_i=dat$Catch_KG,
                 a_i=dat$AreaSwept_km2,
                 input_grid=user_region)
plot_results(fit, plot_set=3)