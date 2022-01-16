## 2022 update of input RDS data to run in VAST.
## headers should be Catch_KG, Year, Vessel, AreaSwept, Lat, Lon, Pass

require(dplyr)
require(here)
require(nwfscSurvey)

## Load WC Data ----
## what was downloaded for 2021 update (just NWFSC combo, 2003-2019)
load("C:/Users/mkapur/Dropbox/UW/assessments/sab-2021/dataprep/raw_data/survey/Catch__NWFSC.Combo_2021-02-08.rda") ## 'Out'
wc.combo <- Out %>%
  mutate(Survey = 'NWFSC_Combo')%>%  
  mutate(AreaSwept = Area_Swept_ha*0.01) %>%
  select(Survey,  
         # Catch_KG = cpue_kg_km2,
         Catch_KG = total_catch_wt_kg, ## this is what is used in VASTWestCoast
         Year, Vessel, AreaSwept,
         Lat = Latitude_dd, Lon = Longitude_dd, Pass)

## download the Triennial data
# nwfscSurvey::PullCatch.fn(Name = "sablefish", SurveyName = "Triennial",SaveFile = TRUE, Dir = here('data'))
load("C:/Users/mkapur/Dropbox/UW/sab-idx/data/Catch__Triennial_2022-01-13.rda")
wc.tri <- Out %>%
  mutate(AreaSwept = Area_Swept_ha*0.01) %>%
  mutate(Survey = ifelse(Year > 1995,"Triennial_late", "Triennial_early")) %>%
  select(Survey,   
         # Catch_KG = cpue_kg_km2,
         Catch_KG = total_catch_wt_kg,
         Year, Vessel, AreaSwept,
         Lat = Latitude_dd, Lon = Longitude_dd, Pass= Tow)

wc_input <- rbind(wc.combo, wc.tri) %>%
  select(Survey, Catch_KG, Year, Vessel, AreaSwept,  Lat, Lon , Pass)


## Load AK Data ----
## I had to go into AKFIN and based on what I obtained before, downloaded the "catch summary view with nulls" (this is LL surv data)
## and "race cpue by haul" (this is GOA trawl survey data).
## for the LL data, this is at the haul level thus there are dupes across stations; previously Kari had sent me
## a custom spreadsheet, but I will have to work with this as-is.

# ak.ll0 <- bind_rows(read.csv(here('data','catch_summary_view_with_nulls1979-1999.csv'), header=TRUE,skip = 6),
#                             read.csv(here('data','catch_summary_view_with_nulls2000-2020.csv'), header=TRUE,skip = 6))
# ak.ll <- ak.ll0 %>% 
#   filter(Year > 1989) %>%
#   mutate(Catch2 = ifelse(is.na(Catch),0,Catch),
#          Start.Longitude..DD. =    ifelse(  Start.Longitude..DD. > 0,   Start.Longitude..DD.*-1, Start.Longitude..DD.),
#          Vessel = ifelse(Year < 1994, "Ocean Prowler", 
#                          ifelse(Year %% 2 == 0, 
#                                 "Alaskan Leader",  "Ocean Prowler"  ) )) %>% 
#   group_by(Year, Haul,Vessel, Station.Number) %>%
#   summarise( Catch_KG=mean(Catch2),
#              Lon =mean(Start.Longitude..DD.),
#              Lat = mean(Start.Latitude..DD.)) %>%
#   mutate(Survey = 'AK_DOM_LL', AreaSwept = 0.01,
#         ) %>%
#   select(Survey, Catch_KG, Year, Vessel, AreaSwept,
#          Lat , Lon , Pass = Haul)


## DH indicated to use <700m and drop 1984, 1987 and split at 1993
ak.goa <- read.csv( here('data',"race_cpue_by_haul-011322.csv"), header=TRUE,skip = 7) %>% 
  filter( Gear.Depth <= 500 & !(Year %in% c(1984,1987)) ) %>%
  mutate(Vessel = as.factor(Vessel.Number),
         Catch_KG = ifelse(is.na(Weight.CPUE..kg.km2.),0,Weight.CPUE..kg.km2.),
         Lon =    ifelse(  Starting.Longitude..dd. > 0,   Starting.Longitude..dd.*-1, Starting.Longitude..dd.),
         AreaSwept=Effort..km2.,
         Survey = ifelse(Survey == 'GOA' & Year > 1993, 'GOA_LATE', 
                         ifelse(Survey == 'GOA' & Year <= 1993, 'GOA_EARLY',
                                ifelse(Survey == 'AI' & Year > 1993, 'AI_EARLY',
                                       ifelse(Survey == 'AI' & Year <= 1993, 'AI_EARLY', Survey))))) %>%
 filter(Survey == 'GOA_LATE'  | Survey == 'GOA_EARLY') %>%
  select(Survey, Catch_KG, Year, Vessel, AreaSwept,
         Lat =  Starting.Latitude..dd., Lon , Pass = Haul.Join.ID)

ak_input <- rbind(ak.goa)

inputVAST <- bind_rows(wc_input, ak_input)
inputVAST$AreaSwept[is.na(inputVAST$AreaSwept)] <- 0.02 ## single row from AI_EARLY
save(inputVAST, file = here('data',paste0(Sys.Date(),'inputVast.csv')))
saveRDS(inputVAST, file = here('data',paste0(Sys.Date(),'inputVast.rds')))


## sanity check: if really in KGs, 
## AK should be on the order of 200 000 000 (e8)
## and WC on the order of e7

inputVAST %>% group_by(Year,Survey) %>% summarise(sum(Catch_KG)) %>% View()

## plots of input data

ggplot(inputVAST, aes(x = Lon, 
                      y = Lat, 
                      col =Survey)) +
  geom_point()+
  # theme(legend.position = 'none')
  facet_wrap(~Year)


