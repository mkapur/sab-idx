require(dplyr)
require(ggplot2)
require(reshape2)
require(mapdata)
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "navy", "#F0E442" )
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Data_Geostat.Rdata")
## reboot of jim code showing survey regions and sample sizes [he used base] ----
usa <- map_data("world")
mgmtLims <- data.frame(ymax = c(65,65, 49),
                       ymin = c(49,49, 30), 
                       xmax = c(-180,-134, -115), 
                       xmin = c(-134,-115, -132))

plist<-list()
plist[[1]] <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = 'grey55') +
  coord_quickmap(clip = 'off') +
  scale_x_continuous(expand = c(0,0), limits = c(-180,-110), breaks = seq(-180,-120,10), labels = paste(seq(-180,-120,10), "°W")) +
  scale_y_continuous(expand = c(0,0), limits = c(30,75), breaks = seq(30,75,10), labels =  paste(seq(30,75,10), "°N"))  +
  # theme_minimal() +
  kaputils::theme_black() +
  theme(panel.grid = element_blank(),
        legend.position = 'left',
        legend.text = element_text(size = 10)) +
  geom_rect(data = mgmtLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
            fill =NA, size = 1, colour = 'red') + 
  geom_label(aes( x = c(rep(-120,2),-145),
                  y = c(40,55,55),
                  label = c("Cal Curr.","BC","AK")),
             size = 3) +
  
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  geom_point(data = Data_Geostat, alpha = 0.01,
             aes(x = Lon,  y = Lat, color = factor(Survey))) +
  geom_label(aes( x = c(rep(-120,2),-145),
                  y = c(40,55,55),
                  label = c("Cal Curr.","BC","AK")),
             size = 3) +
  # scale_color_manual(values = c('orchid','gold',cbbPalette)) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Survey')
plist[[2]] <- Data_Geostat %>%
  group_by(Survey, Year) %>%
  summarise(n = n()) %>%
  ggplot(., aes(x = Year, y = n, fill = Survey)) +
  # theme_minimal() +
  kaputils::theme_black() +
  theme(legend.position = 'none') +
  # geom_point(size = 4) +
  geom_bar(stat = 'identity', position = 'stack')+
  # scale_y_continuous(limits = c(0,1100), breaks = seq(0,1000,1000)) +
  # scale_fill_manual(values = c('orchid','grey22',cbbPalette)) +
  labs(x = 'Year', y = 'Sample Size', color = 'Survey')


ggsave(plot = Rmisc::multiplot(plotlist = plist, cols = 1) ,
       file = "./figures/datamap_size-BLACK.png",
       width = 5, height = 7, units = 'in', dpi = 440)

## EDA on effort metrics
# Data_Geostat %>% group_by(State) %>%
#   dplyr::summarise(mean(AreaSwept_km2))




## Comparison of VAST outputs with assessment values by region ----

sab2019_2 <- read.csv(paste0("C:/Users/",comp.name,"/Dropbox/UW/sab-idx/data/sab2019update_vast.csv"))%>% 
  # sab2019$Fleet[sab2019$Fleet == 'All_areas'] <- "2019 WCBTS Used in Assessment"
  mutate(Unit = NA, SD_mt = NA, Fleet = Fleet_name ) %>%
  select(Yr, Unit, Fleet, Obs, SE, SD_mt)
# "SAB_2019_Assessment CPUE [vast]"
names(sab2019_2)[c(1,4,5)] <- c('Year','Estimate_metric_tons', "SD_log")
# babysteps version ----
## load CPUE used in 2019 SAB assessment [from KFJ on Gdrive]
sab2019 <- read.csv(paste0("C:/Users/",comp.name,"/Dropbox/UW/sab-idx/runs/sabWCVAST/WCGBTS/Table_for_SS3.csv"))



# bind_rows(read.csv(paste0(DateFile,"/Table_for_SS3.csv")), sab2019, sab2019_2) %>%
#   filter(Fleet %in% c("AKSHLF",
#                       "All_areas",
#                       "California_current")) %>%
#   ggplot(.,
#          aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
#   theme_minimal()+
#   theme(panel.grid = element_blank(),
#         legend.position = 'bottom') +
#   scale_y_continuous(limits = c(0,300000)) +
#   scale_color_brewer(palette = 'Accent') +
#   labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices vs Assessment values')) +
#   scale_color_manual(values = c('grey40','grey44','blue'))+
#   geom_line(pch = 1, cex = 3) +
#   geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)



# old version ----
## how about we instead use the raw data_geostat?

assc <- read.csv("./data/assessment_CPUE.csv") %>%
  mutate(Source = 'assessment', Estimate_metric_tons = Value ,
         Fleet2 = substr(Index,1,2)) %>%
  filter(Type == 'Abundance' |Type == 'Biomass')  %>%
  filter(!grepl('PUE', Index),!grepl('NUM', Index)) %>%
  bind_rows(.,
            read.csv(paste0(DataFile,"/BC/BC_sable_survey_data.Aug262019.csv"))  %>% 
              filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & !is.na(TOTAL_SABLE_WEIGHT) & 
                       SABLE_SET_TYPE == 'StRS'  ) %>% group_by(SET_YEAR) %>% 
              dplyr::summarise(meanCPUE = mean(cpue)) %>%
              mutate(Year = SET_YEAR, Value = meanCPUE, CV = NA, 
                     Index = 'Filter_StRS', Type = 'Biomass', Source = 'Assessment',
                     Estimate_metric_tons =meanCPUE, Fleet2 = 
                       'BC') %>%
              select(-meanCPUE, -SET_YEAR)) %>%
  bind_rows(.,
            read.csv(paste0(DataFile,"/BC/BC_sable_survey_data.Aug262019.csv"))  %>% 
              filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & !is.na(TOTAL_SABLE_WEIGHT) & 
                       SABLE_SET_TYPE == 'OFFSHORE STANDARDIZED'  ) %>% group_by(SET_YEAR) %>% 
              dplyr::summarise(meanCPUE = mean(cpue)) %>%
              mutate(Year = SET_YEAR, Value = meanCPUE, CV = NA, 
                     Index = 'BC_OFFSHORE_STD', Type = 'Biomass', Source = 'Assessment',
                     Estimate_metric_tons =meanCPUE, Fleet2 = 
                       'BC') %>%
              select(-meanCPUE, -SET_YEAR)) %>%
  bind_rows(.,
            read.csv(paste0(DataFile,"/BC/BC_trawl_survey_sable_data.Oct312019.csv"))  %>% 
              filter(LONGITUDE <= 0 & !is.na(CATCH_WEIGHT)) %>%
              group_by(YEAR) %>% 
              dplyr::summarise(meanCPUE = mean(CATCH_WEIGHT)) %>%
              mutate(Year = YEAR, Value = meanCPUE, CV = NA, 
                     Index = 'Filter_BCTrawl', Type = 'Biomass', Source = 'Assessment',
                     Estimate_metric_tons =meanCPUE, Fleet2 = 
                       'BC') %>%
              select(-meanCPUE, -YEAR)) %>%
  bind_rows(., sab2019_2 %>% 
              mutate(Fleet2 = 'WC', Index = Fleet, Value = Estimate_metric_tons, Type = 'Abundance',
                     Source = 'assessment') %>%
              select(Year, Estimate_metric_tons,  Index, Type, Source, Fleet2))



# assc$Fleet[-grep(paste0(c('NUM','PUE',collapse= "|")),assc$Fleet)]

## Correct scales
assc$Estimate_metric_tons[assc$Fleet2 == 'AK'] <- assc$Value[assc$Fleet2 == 'AK']*  1000
# assc$Estimate_metric_tons[assc$Fleet2 == 'WC'] <- assc$Value * 1000
assc$Estimate_metric_tons[assc$Fleet2 == 'BC'] <- assc$Value[assc$Fleet2 == 'BC'] * 1000


names(assc) <- c('Year','Value','SD_log',"Fleet","TYPE", 'Source',"Estimate_metric_tons","Fleet2")

assc <- assc %>%
  select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, Fleet2 ) %>%
  mutate(uci=NA, lci = NA)

## see line 81 here for conv https://github.com/James-Thorson-NOAA/FishStatsUtils/blob/master/R/plot_index.R
# vastc <- 
vastc <- read.csv("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Table_for_SS3.csv") %>%
# read.csv(paste0(DateFile,"Table_for_SS3.csv")) %>%
                    mutate(TYPE = 'Abundance', Source = 'VAST',
                           lci = Estimate_metric_tons-SD_mt,
                           uci = Estimate_metric_tons+SD_mt) %>%
  select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, uci, lci ) #%>%
  # filter(Fleet != 'Gulf_of_Alaska')
vastc$Fleet2 <- NA
for(i in 1:nrow(vastc)){
  vastc$Fleet2[i] <- ifelse(vastc$Fleet[i] == "California_current",
                             "WC", 
                             ifelse(vastc$Fleet[i] == "British_Columbia", "BC",
                                    "AK"))
  if(vastc$Fleet[i] == 'AllAreas')   vastc$Fleet2[i] <- "ALL"
}

## needed to do this for WCGBTS/GOALATE AS REF
# vastc$Estimate_metric_tons[vastc$Fleet2 == 'BC'] <-
#   vastc$Estimate_metric_tons[vastc$Fleet2 == 'BC'] * 1E-2
# vastc$uci[vastc$Fleet2 == 'BC'] <- vastc$uci[vastc$Fleet2 == 'BC'] * 1E-2
# vastc$lci[vastc$Fleet2 == 'BC'] <- vastc$lci[vastc$Fleet2 == 'BC'] * 1E-2
# vastc$lci[vastc$lci < 0 ] <- 0
# vastc$Estimate_metric_tons[vastc$Fleet2 == 'WC'] <-
#   vastc$Estimate_metric_tons[vastc$Fleet2 == 'WC'] * 1E-2
# vastc$uci[vastc$Fleet2 == 'WC'] <- vastc$uci[vastc$Fleet2 == 'WC'] * 1E-2
# vastc$lci[vastc$Fleet2 == 'WC'] <- vastc$lci[vastc$Fleet2 == 'WC'] * 1E-2
# vastc$Estimate_metric_tons[vastc$Fleet2 == 'AK' ] <-
#   vastc$Estimate_metric_tons[vastc$Fleet2 == 'AK'] * 1E-4
# vastc$uci[vastc$Fleet2 == 'AK'] <-
#   vastc$uci[vastc$Fleet2 == 'AK'] * 1E-4
# vastc$lci[vastc$Fleet2 == 'AK'] <- vastc$lci[vastc$Fleet2 == 'AK'] * 1E-4
# # 
# vastc$Estimate_metric_tons[vastc$Fleet == 'Gulf_of_Alaska'] <-
#   vastc$Estimate_metric_tons[vastc$Fleet == 'Gulf_of_Alaska'] * 1e3/1e4
# vastc$uci[vastc$Fleet == 'Gulf_of_Alaska'] <-
#   vastc$uci[vastc$Fleet == 'Gulf_of_Alaska'] * 1e3/1e4
# vastc$lci[vastc$Fleet == 'Gulf_of_Alaska'] <-
#   vastc$lci[vastc$Fleet == 'Gulf_of_Alaska'] * 1e3/1e4





## truncate CI so plot is visible
# vastc$uci[vastc$Fleet2 == 'AK'] <- ifelse(vastc$uci[vastc$Fleet2 == 'AK'] > 7e5,
#                                           7e5,
#                                           vastc$uci[vastc$Fleet2 == 'AK'])
# 
# vastc$lci[vastc$Fleet2 == 'AK'] <- ifelse(vastc$lci[vastc$Fleet2 == 'AK'] > 2e5,
#                                           2e5,
#                                           vastc$lci[vastc$Fleet2 == 'AK'])
# 


fleetSel <- c(1:4,6,7,10,8,11,12)

custnames <- c(paste0('VAST ',c('California Current','British Columbia',
                                'Aleutian Islands', 'Gulf of Alaska','Eastern Bering Sea') ),
               'AK Domestic Longline', "AK Gulf Trawl", 'BC Offshore Standardized','BC Synoptic Trawl','BC Trap Stratified',
               'Triennial','WCGBTS')[fleetSel]

genPal <- c('brown','dodgerblue','goldenrod','grey22')

## compare_all plot ----
rbind(vastc,assc) %>%
  filter(Fleet2 %in% c('WC','BC', 'AK')[1:3]) %>%
  filter(Fleet %in% c("California_current","British_Columbia",
                      'Aleutian_Islands',"Gulf_of_Alaska",
                      "Eastern_Bering_Sea",
                      'AK_DOM_LL',"AK_GOA_TRW",'BC_OFFSHORE_STD',
                      "Filter_BCTrawl","Filter_StRS", "AKSHLF",  "NWCBO")[fleetSel]) %>%
  filter(Year < 2019) %>%
  # filter(Fleet %in% c("British_Columbia","Filter_BCTrawl")) %>%
  ggplot(., aes(x = Year, y = Estimate_metric_tons, 
                col = Fleet, linetype = Fleet)) +
  theme_bw()+
  # kaputils::theme_black()+
  theme(panel.grid = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 12),
  strip.text.x = element_text(
    size = 16
  )) +  
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Fleet), 
              alpha = 0.15, col = 'grey',
              show.legend = FALSE) +
  scale_x_continuous(limits = c(1980,2018),
                     breaks = seq(1980,2020,10)) +
  scale_fill_manual(values = c(rep('blue',3), 'dodgerblue3',
                               cbbPalette),
                    labels = c(custnames)) +
  scale_color_manual( values = c(rep('blue',3), 'dodgerblue3',
                                 cbbPalette),
                      labels = c(custnames)) +
  scale_linetype_manual(values = c(rep('solid',4),
                                   rep('dashed',40)),
                        labels = c(custnames)) +
  labs(x = 'Year', y = 'Estimate (mt)', color = "", linetype = "",
       title = '',
       subtitle = 'BC&AK assessment vals have been multiplied by 1000; input dat was div by 1000') +

  facet_wrap(~Fleet2, scales = 'free_y', ncol = 3)


  ggsave(plot = last_plot(),
       # file = paste0("./figures/",Sys.Date(),"_idx_comparison.png"),
       file = paste0(DateFile,"/compare_all-update.png"),
       height = 8, width = 12, unit='in',dpi = 320)
  
  
  
  

vastc %>% group_by(Fleet) %>% summarise(mean(exp(SD_log)))                          
# ## Prettier index plot ----
# source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")
vastc %>%
  filter(Fleet != 'Eastern_Bering_Sea') %>%
ggplot(.,
         aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
    theme_bw()+
  # kaputils::theme_black() +

    scale_color_manual(values = cbbPalette) +
    labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices')) +
    geom_line(lwd = 0.9)+
    # geom_point(pch = 1, cex = 3) +
    geom_ribbon(aes(ymin = lci,
                    ymax = uci,  fill = Fleet),
                alpha = 0.2,
                show.legend = FALSE)
#   
ggsave(plot = last_plot(), file = paste0(DateFile,"Index-Biomass2-BLACK.png"),
       height = 6, width = 8, unit='in',dpi = 360)


# source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")

ggplot(read.csv(paste0(DateFile,"/Table_for_SS3.csv")) %>%
         filter(Fleet != 'Eastern_Bering_Sea') ,
         # mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),
       aes(x = Year, y = log(Estimate_metric_tons), col = Fleet)) +
  theme_bw()+
  # theme(panel.grid.minor = element_blank()) +
  # scale_y_continuous(limits = c(0,25)) +
  scale_color_manual(values = cbbPalette) +
  labs(x = 'Year', y = 'Estimate log(mt)', title = paste0('VAST-Standardized Indices (log space)')) +
  geom_line(lwd = 0.9)+
  geom_point(pch = 1, cex = 3) +
  labs(y = 'log(mt)')
# geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)
# 
# ggsave(plot = last_plot(), file = paste0(DateFile,"logIndex-Biomass2.png"), height = 6, width = 8, unit='in',dpi = 520)
# 
# ggplot(read.csv(paste0(DateFile,"/Table_for_SS3_original.csv")) %>% 
#          # filter(Fleet != 'All') %>%
#          mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),
#        aes(x = Year, y =Estimate_metric_tons, col = Fleet)) +
#   theme_mk()+
#   # theme(panel.grid.minor = element_blank()) +
#   # scale_y_continuous(limits = c(0,25)) +
#   scale_color_brewer(palette = 'Spectral') +
#   labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices')) +
#   geom_line(lwd = 0.9)+
#   geom_point(pch = 1, cex = 3) 
# ggsave(plot = last_plot(), file = paste0(DateFile,"Index-Biomass2.png"), height = 6, width = 8, unit='in',dpi = 520)

# Plot center of gravity
