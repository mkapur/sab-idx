require(dplyr)
require(ggplot2)
require(reshape2)
require(mapdata)
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "navy", "#F0E442" )
## reboot of jim code showing survey regions and sample sizes [he used base] ----
usa <- map_data("world")

plist<-list()
plist[[1]] <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = 'grey55') +
  coord_quickmap(clip = 'off') +
  scale_x_continuous(expand = c(0,0), limits = c(-180,-110), breaks = seq(-180,-120,10), labels = paste(seq(-180,-120,10), "°W")) +
  scale_y_continuous(expand = c(0,0), limits = c(30,75), breaks = seq(30,75,10), labels =  paste(seq(30,75,10), "°N"))  +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'left',
        legend.text = element_text(size = 10)) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  geom_point(data = Data_Geostat, alpha = 0.3,
             aes(x = Lon,
                                      y = Lat, 
                                      color = factor(Survey))) +
  scale_color_manual(values = cbbPalette) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Survey')
  

plist[[2]] <- Data_Geostat %>%
  group_by(Survey, Year) %>%
  summarise(n = n()) %>%
  ggplot(., aes(x = Year, y = n, fill = Survey)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  # geom_point(size = 4) +
  geom_bar(stat = 'identity', position = 'stack')+
  # scale_y_continuous(limits = c(0,1100), breaks = seq(0,1000,1000)) +
  scale_fill_manual(values = cbbPalette) +
  labs(x = 'Year', y = 'Sample Size', color = 'Survey')


ggsave(plot = Rmisc::multiplot(plotlist = plist, cols = 1) , 
       file = "./figures/datamap_size.png",
       width = 5, height = 7, units = 'in', dpi = 440)

## EDA on effort metrics
Data_Geostat %>% group_by(State) %>%
  dplyr::summarise(mean(AreaSwept_km2))

## Comparison of VAST outputs with assessment values by region ----
## how about we instead use the raw data_geostat?

assc <- read.csv("./data/assessment_CPUE.csv") %>%
  mutate(Source = 'assessment', Estimate_metric_tons = Value ,
         Fleet2 = substr(Index,1,2)) %>%
  filter(Type == 'Abundance' |Type == 'Biomass')  %>%
  filter(!grepl('PUE', Index),!grepl('NUM', Index)) %>%
  bind_rows(.,
            read.csv(paste0(DataFile,"/BC/BC_sable_survey_data.Aug262019.csv"))  %>% 
              filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & !is.na(TOTAL_SABLE_WEIGHT) & 
                       SABLE_SET_TYPE == 'StRS') %>% group_by(SET_YEAR) %>% 
              dplyr::summarise(meanCPUE = mean(cpue)) %>%
              mutate(Year = SET_YEAR, Value = meanCPUE, CV = NA, 
                     Index = 'Filter_StRS', Type = 'Biomass', Source = 'Assessment',
                     Estimate_metric_tons =meanCPUE, Fleet2 = 
                       'BC') %>%
              select(-meanCPUE, -SET_YEAR))


  
  
# assc$Fleet[-grep(paste0(c('NUM','PUE',collapse= "|")),assc$Fleet)]

## Correct scales
assc$Estimate_metric_tons[assc$Fleet2 == 'AK'] <- assc$Value[assc$Fleet2 == 'AK']*  1000
# assc$Estimate_metric_tons[assc$Fleet2 == 'WC'] <- assc$Value * 1000
assc$Estimate_metric_tons[assc$Fleet2 == 'BC'] <- assc$Value[assc$Fleet2 == 'BC'] * 1000


names(assc) <- c('Year','Value','SD_log',"Fleet","TYPE", 'Source',"Estimate_metric_tons","Fleet2")

assc <- assc %>%select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, Fleet2 ) %>%
  mutate(uci=NA, lci = NA)

## see line 81 here for conv https://github.com/James-Thorson-NOAA/FishStatsUtils/blob/master/R/plot_index.R
vastc <- read.csv(paste0(DateFile,"/Table_for_SS3_original.csv")) %>%
                    mutate(TYPE = 'Abundance', Source = 'VAST',
                           lci = Estimate_metric_tons-SD_mt,
                           uci = Estimate_metric_tons+SD_mt) %>%
  select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, uci, lci ) %>%
  filter(Fleet != 'Eastern_Bering_Sea')
vastc$Fleet2 <- NA
for(i in 1:nrow(vastc)){
  vastc$Fleet2[i] <- ifelse(vastc$Fleet[i] == "California_current",
                             "WC", 
                             ifelse(vastc$Fleet[i] == "British_Columbia", "BC",
                                    "AK"))
  if(vastc$Fleet[i] == 'All')   vastc$Fleet2[i] <- NA
  
}

rbind(vastc,assc) %>%
  filter(Fleet2 %in% c('WC','BC','AK')) %>%
  filter(Fleet %in% c("California_current","British_Columbia","Gulf_of_Alaska","Eastern_Bering_Sea",
                      'AK_DOM_LL','AK_GOA_TRW','Filter_StRS','BC_TRAP_SRV_STRAT','WC_TRI_TRW','WC_SH_SLP_TRW')) %>%
  ggplot(., aes(x = Year, y =Estimate_metric_tons, col = Fleet, linetype = Fleet)) +
  theme_mk()+
  # theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(1975,2020)) +
  # scale_color_brewer(palette = 'Spectral') +
  scale_color_manual(values = c(rep('blue',3),'gold2','gold','seagreen2','seagreen3','seagreen4','grey22','grey44'))+
  scale_linetype_manual(values = c(rep('solid',3), rep('dashed',40))) +
  labs(x = 'Year', y = 'Estimate (mt)', 
       title = 'Indices from VAST and Assessment, by region') +
  geom_line(lwd = 0.9)+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, col = 'grey') +
  # geom_point(pch = 1, cex = 3) +
  facet_wrap(~Fleet2, scales = 'free_y', ncol = 3)
ggsave(plot = last_plot(), file = paste0("./figures/",Sys.Date(),"_idx_comparison.png"), height = 6, width = 8, unit='in',dpi = 520)


                           
♥## Prettier index plot ----
source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")

ggplot(read.csv(paste0(DateFile,"/Table_for_SS3.csv")) %>% 
         mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),
       aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
  theme_mk()+
  theme(panel.grid = element_blank()) +
  # scale_y_continuous(limits = c(0,95000)) +
  scale_color_brewer(palette = 'Accent') +
  labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices')) +
  # geom_line(lwd = 0s.9)+
  geom_point(pch = 1, cex = 3) +
  geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)

# ggsave(plot = last_plot(), file = paste0(Run2Dir,"Index-Biomass2.png"), height = 6, width = 8, unit='in',dpi = 520)


# source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")

# ggplot(read.csv(paste0(DateFile,"/Table_for_SS3_original.csv")) %>% 
#          filter(Fleet != 'All') %>%
#          # mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),
#        aes(x = Year, y = log(Estimate_metric_tons), col = Fleet)) +
#   theme_mk()+
#   # theme(panel.grid.minor = element_blank()) +
#   # scale_y_continuous(limits = c(0,25)) +
#   scale_color_brewer(palette = 'Spectral') +
#   labs(x = 'Year', y = 'Estimate log(mt)', title = paste0('VAST-Standardized Indices (log space)')) +
#   geom_line(lwd = 0.9)+
#   geom_point(pch = 1, cex = 3) +
#   labs(y = 'log(mt)')
# # geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)
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
