## Load and sort raw data
require(dplyr); require(reshape2)
require(VAST)

iph0 <- read.csv("./data/manual_compile_2019-05-13.csv", na.strings = "#N/A")



with(iph0,make_data(b_i = Sablefish_n3,
                    a_i = 2,
                    c_iz = )
# load("./data/iph_2019-05-13.Rdata") 
# Your observation data (often called Data_Geostat in Thorson’s examples) is a 
## data frame that needs observations (e.g. CPUE), year, vessel
# (if you want a catchability covariate),
## area swept (e.g. 0.01, input from data), 
## Latitude, Longitude, Stratum name, Survey name, 
## and others (bottom depth if modelling 3D, time of day, 
## other descriptors specific to your study)

Data_Geostat <- iph0 %>% 
  select(YEAR,  Latitude, Longitude, Stratum = Region, Catch_No = Sablefish_n3 ) %>%
  mutate(Survey = 'IPHC',AreaSwept_km2 = 0.01)

TmbData = VAST::make_data("Version" = Version,
                          "FieldConfig" = FieldConfig,
                          "OverdispersionConfig" = OverdispersionConfig,
                          "RhoConfig" = RhoConfig,
                          "ObsModel" = ObsModel,
                          "c_iz" = rep(0,nrow(Data_Geostat)),
                          "b_i" = Data_Geostat$Catch_No,
                          "a_i" = Data_Geostat$AreaSwept_km2,
                          "s_i" = Data_Geostat$knot_x - 1,
                          "t_iz" = as.numeric(Data_Geostat$Year_Num),
                          "a_xl" = Spatial_List$a_xl,
                          "MeshList" = Spatial_List$MeshList, 
                          "GridList" = Spatial_List$GridList,
                          "Method" = Spatial_List$Method,
                          "Options" = Options,
                          "X_xtp" = X_xtp.x,
                          “Aniso” = Aniso ) 

iph0 %>% 
  select(YEAR = YEAR,
         Length..cm = NA,
         Sex = NA,
         START_LATITUDE = latitude,
         START_LONGITUDE = longitude,
         GEAR_DEPTH == Depth..F.,)

save(iph, file = "./data/iph_2019-05-13.Rdata")

wcsurv <- WareHouse.All.Ages.Env %>%
  filter(common_name == 'sablefish' &
           !is.na(age_years) & !is.na(length_cm) & sex != 'U') %>%
  select(
    YEAR = year,
    LENGTH..cm. = length_cm,
    AGE = age_years,
    Sex = sex,
    START_LATITUDE = latitude_dd,
    START_LONGITUDE = longitude_dd,
    REG = Salinity_at_Gear_psu,
    SPECIES_CODE = pass,
    GEAR_DEPTH = depth_ftm,
    Temp = Temperature_at_Surface_c,
    GEAR_TEMPERATURE = Temperature_at_Gear_c
  )

# wcsurv <- bio %>% 
#   filter(Common_name == 'sablefish' &
#            !is.na(Age) &
#            !is.na(Length_cm) &
#            Sex != 'U') %>%
#   select(
#     YEAR = Year,
#     LENGTH..cm. = Length_cm,
#     AGE = Age,
#     Sex = Sex,
#     START_LATITUDE = Latitude_dd,
#     START_LONGITUDE = Longitude_dd,
#     REG = Tow,
#     SPECIES_CODE = Tow,
#     GEAR_DEPTH = Depth_m,
#     Temp = Tow,
#     GEAR_TEMPERATURE = Tow
#   )
wcsurv$REG <- 'WC'
wcsurv$SPECIES_CODE <- 20510
## British Columbia ---
## "If you wanted to start by just looking at a relatively consistent 
## and standardized sub-set of the data then I would subset for 
## SET_TYPE = OFFSHORE STANDARDIZED & StRS which are the two 
## coastwide trap based surveys (see CDN slide deck from workshop 
## for more details)." -- B Connors

bcsurv <- read.csv("C://users/mkapur/dropbox/uw/sab-growth/data/raw/BC/LWMSO.w_lat_long.csv") %>%
  filter(
    !is.na(SPECIMEN_AGE) & !is.na(Fork_Length) &
      SPECIMEN_SEX_CODE %in% c("1", "2") &
      NS_AREA != "" & SABLE_AREA_GROUP != "" & slat != 0 & 
      SABLE_SET_TYPE %in% c('StRS','OFFSHORE STANDARDIZED')
  ) %>%
  select(SPECIMEN_AGE, Fork_Length, SPECIMEN_SEX_CODE, YEAR, slat, slon) %>%
  mutate(Sex = ifelse(SPECIMEN_SEX_CODE == "2", 'F', "M"), 
         Fork_Length = Fork_Length / 10) %>%
  plyr::rename(
    c(
      "slat" = "Latitude_dd",
      "slon" = "Longitude_dd",
      "SPECIMEN_AGE" = "Age",
      "Fork_Length" = "Length_cm",
      "YEAR" = "Year"
    )
  ) %>%
  select(
    YEAR = Year,
    LENGTH..cm. = Length_cm,
    AGE = Age,
    Sex,
    START_LATITUDE = Latitude_dd,
    START_LONGITUDE =  Longitude_dd
  ) %>%
  ## 20135 is mean of other two, otherwise ~40K for BC
  sample_n(.,20135) %>%
  mutate(
    REG = "BC",
    SPECIES_CODE = 20510,
    GEAR_DEPTH = NA,
    Temp = NA,
    GEAR_TEMPERATURE = NA
  )    
# ## ALASKA ----
aksurv <- read.csv("c://users/mkapur/dropbox/uw/sab-growth/data/raw/ak/AK_age_view_2018.csv") %>%
  ## drop period before 1995 and filter for top 6 as in Echave
  filter(., grepl(paste0(c("Southeast",'Kodiak',"Chirikof","Shumagin","Bering","Aleutian"), collapse="|"), GEOGRAPHIC_AREA_NAME)) %>%
  filter(SEX != 3 & !is.na(AGE) & !is.na(LENGTH) ) %>%
  mutate(SEX = ifelse(SEX == 2, 'F', "M")) %>%
  select(
    YEAR,
    LENGTH..cm. = LENGTH,
    AGE,
    Sex = SEX,
    START_LATITUDE =  STARTLAT,
    START_LONGITUDE = STARTLONG
  ) %>%
  mutate(
    REG = "AK",
    SPECIES_CODE = 20510,
    GEAR_DEPTH = NA,
    Temp = NA,
    GEAR_TEMPERATURE = NA
  )  


vast_data <- rbind(wcsurv,bcsurv,aksurv)%>%  filter(START_LONGITUDE < 0 ) %>% mutate(
  
)
save(vast_data, file = paste0(getwd(),"/data/vast_data.rda"))





# with(vast_data, plot(START_LATITUDE ~ START_LONGITUDE, pch = 19))

sab_corrected <- build_corrected_df(
  vast_data,
  species_code = 20510,
  sex = 'F',
  age = "4",
 renames =  c('Year',
    'Lat','Lon',
    'length', 'depth', 'temp')
)
min(sab_corrected$Lat) ## should be 32

vast_data$REG <- as.factor(vast_data$REG)
levels(vast_data$REG) <- c("Alaska","Canada","California Current")
nct <- vast_data %>% group_by(REG) %>% summarise(n = n())

  ggplot(vast_data, aes(x = LENGTH..cm., fill = Sex)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.05,0.9),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=14))+
  scale_fill_manual(values = c("#d8b365","#5ab4ac"))+
  scale_alpha(guide = 'none') +
    labs(x = 'Length (cm)', y = "") +
geom_histogram() +
 annotate("text", label = paste0("n = ",nct$n), x = 100, y = 2500)+
    facet_wrap(~ REG)
  ggsave(file =  "C:/Users/mkapur/Dropbox/UW/sab-growth/plots/vast_rawHist.png",
         plot = last_plot(), height = 8, width = 12, unit = 'in', dpi = 520)


save(sab_corrected, file = "./data/sab_corrected.rda")
