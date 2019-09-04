## custom wrapper with make_extrap_grids
## takes input data, combines and stratifies as desired
## output is a saved RDS of format extrapolation_list saved to /extrap


build_Extrap <- function(Data_Geostat, strata.limits.input){
  
  ## Strata limits can be the R1:5 as in the GAM paper; arguments familiar to VAST; or a data.frame
  
  stname <-  ifelse(class(strata.limits.input) == "data.frame", 'Custom',paste0(strata.limits.input,collapse = "" ))
  
  if(class(strata.limits.input) == "data.frame"){
    strata.limits <- strata.limits.input ## go with user input
    
  } else if(grepl( paste0("R",1:5,collapse = "|"),strata.limits.input)[1]){
    ## build full strata limits object and subset as needed
    strata.limits.full <- data.frame(
      'STRATA' = c("R5","R4", "R3","R2","R1"),
      'west_border' = c(-180, -145, -135,-135,-135),
      'east_border' = c(-145, -135, -110,-110,-110),
      'north_border' = c(65, 65, 65,50,50),
      'south_border' = c(50, 50, 50, 36,26 ))
    strata.limits <- subset(strata.limits.full, STRATA %in% as.factor(strata.limits.input))
    
    ## VAST-type input
  } else if(class(strata.limits.input) == "string" & !grepl( paste0("R",1:5,collapse = "|"),strata.limits)[1]){
    stop("not yet ready to deal with a regional de & signation for the strata limits; use basic VAST instead")
  }
  
  tempExtrap <-
    make_extrapolation_info(
      Region = 'User',
      strata.limits = strata.limits,
      input_grid = data.frame(
        'Lon' = Data_Geostat$Lon,
        'Lat' = Data_Geostat$Lat,
        'Area_km2' = rep(4, nrow(Data_Geostat))
      )
    )
  
  #check area sums
  if(any(colSums( tempExtrap$a_el ) == 0)){
    stop("Colsums equal zero (you're missing data for) ",paste0(strata.limits.input[which(colSums( tempExtrap$a_el ) == 0)], sep = " "))
  } 
  
  save(tempExtrap, file = paste0('./data/extraps/',stname,"_extrap.rds"))
  message(paste0("Saved extrapolation file to ./data/extraps/",stname,"_extrap.rds"))
  return(tempExtrap)
}