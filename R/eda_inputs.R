require(ggplot2)
require(ggsidekick)

## plot the raw mean catch by survey x year
## looks like BC does do some cpue stdziation (see email from brendan with figure)
Data_Geostat %>%
  group_by(Year, Survey, Region) %>%
  summarise(meanC = mean(Catch_KG), sdc = sd(Catch_KG)) %>%
  ggplot(., aes(x = Year, y = meanC, color = Survey, fill = Survey)) +
  geom_point(alpha = 1) +
  # geom_ribbon(aes(ymin = meanC-1.96*sdc, ymax = meanC+1.96*sdc))+
  scale_color_brewer(palette = 'Spectral') +
  theme_sleek()+
  facet_wrap(~Region, scales = 'free_y') +
  labs(title = 'mean catch_kg in data_geostat')

ggsave(last_plot(),
       file = here("figures","meanCatch_Survey.png"))
## compare BC_EARLY in assessment (from brendan) with BC_offstd here

omsurv <- 
  read_csv("C:/Users/Maia Kapur/Dropbox/UW/sab-mse/input/input_data/OM_indices_BaseQ=WCGBTS.csv") %>% 
  mutate(Year = 1960:2019) %>%
  select(Year, BC_EARLY)

## evidently these are not the same, further enforcing that some stdization
## occurs in BC pre-hoc.
## this doesn't detract from double dipping concerns.
Data_Geostat %>%
  group_by(Year, Survey, Region) %>%
  summarise(meanC = mean(Catch_KG), sdc = sd(Catch_KG)) %>%
  filter(Survey == 'BC_OffStd') %>%
  merge(., omsurv, by = 'Year') %>%
  select(Year, meanC, BC_EARLY) %>%
  reshape2::melt(., id = 'Year') %>%
  ggplot(., aes(x = Year, y = log(value*10), color = variable)) +
  theme_sleek() +
  geom_point()
ggsave(last_plot(),
       file = here("figures","BC_EARLYvsBCOffStd.png"))
