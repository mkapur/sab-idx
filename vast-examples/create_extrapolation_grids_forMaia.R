#devtools::install_github("ceciliaOLearySBU/FishStatsUtils")
# devtools::install_github("james-thorson/FishStatsUtils", ref="development")
require(FishStatsUtils)
working_directory <- getwd()
grid2 <- readRDS(file = paste0(working_directory,"/make_extrapolation_grids/WBS/grid2.rds"))

zone <- 31

# Extract strata boundaries by region for WBS
WBS_boundaries = c(range(grid2$Lon), range(grid2$Lat))
grid2$STRATA <- "WBS"

grid2 <- data.frame(Lon=grid2$Lon,Lat=grid2$Lat,Area_km2=grid2$Area_km2,STRATA = grid2$X1.nrow.grid1.) #,REGION = grid2$STRATA, STRATA = grid2$X1.nrow.grid1.

Region="User"
wbs.strata.limits <- data.frame(
  'STRATA' = c("All_areas"), #c("WBS"), 
  'west_border' = c(WBS_boundaries[1]),
  'east_border' = c(WBS_boundaries[2]),
  'north_border' = c(WBS_boundaries[4]),
  'south_border' = c(WBS_boundaries[3]) )
strata.limits <- data.frame(
  'STRATA' = c("All areas","WGOA","CGOA"),
  'west_border' = c(-159.62, -159.62, -154.98),
  'east_border' = c(-149.92, -154.15, -149.92),
  'north_border' = c(58.73, 57.02, 58.73),
  'south_border' = c(54.59, 54.59, 56.31) )


# strata.limits <- data.frame('STRATA'="All_areas")
     
#western bering sea                                   
WBS_extrap = Prepare_User_Extrapolation_Data_Fn(Region=Region,
                                  flip_around_dateline=TRUE,
                                  zone = 31)
#eastern bering sea
EBS_extrap = FishStatsUtils::make_extrapolation_info( Region="Eastern_Bering_Sea", 
                                                      zone = zone,
                                                      strata.limits=strata.limits  )
#northern bering sea
NBS_extrap = FishStatsUtils::make_extrapolation_info( Region="Northern_Bering_Sea", 
                                                      flip_around_dateline=TRUE,
                                                      zone = zone, 
                                                      strata.limits=strata.limits )
#comibine EBS,NBS,WBS
Extrapolation_List = FishStatsUtils::combine_extrapolation_info( "EBS"=EBS_extrap, "NBS"=NBS_extrap#,
                                                                 "WBS" = WBS_extrap)

# Add strata
Extrapolation_List$a_el = cbind( "All"=Extrapolation_List$a_el[,1], 
                                 "EBS"=c(EBS_extrap$a_el[,1],rep(0,nrow(NBS_extrap$a_el)),rep(0,nrow(WBS_extrap$a_el))), 
                                 "NBS"=c(rep(0,nrow(EBS_extrap$a_el)),NBS_extrap$a_el[,1],rep(0,nrow(WBS_extrap$a_el))), 
                                 "WBS" =c(rep(0,nrow(EBS_extrap$a_el)),rep(0,nrow(NBS_extrap$a_el)),WBS_extrap$a_el[,1]) )
# Other_extrap$a_el$All_areas <- rep(4, nrow(Other_extrap$a_el))
# 
# Reduce to one column
colSums( Extrapolation_List$a_el ) #check area sums
saveRDS(Extrapolation_List, file = paste0("./data/ak/Extrapolation_List_AK.rds"))


