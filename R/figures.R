

## EDA on effort metrics
Data_Geostat %>% group_by(State) %>%
  dplyr::summarise(mean(AreaSwept_km2))


## Prettier index plot ----
source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")

ggplot(read.csv(paste0(Run2Dir,"/Table_for_SS3.csv")) %>% 
         mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
  theme_mk()+
  theme(panel.grid = element_blank()) +
  scale_y_continuous(limits = c(0,95000)) +
  scale_color_brewer(palette = 'Accent') +
  labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices')) +
  geom_line(lwd = 0.9)+
  geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)

ggsave(plot = last_plot(), file = paste0(Run2Dir,"Index-Biomass2.png"), height = 6, width = 8, unit='in',dpi = 520)
