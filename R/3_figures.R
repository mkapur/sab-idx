require(dplyr)
require(here)
require(ggplot2)
require(reshape2)
require(mapdata)
require(ggsidekick)
require(here);require(TMB)
sf::sf_use_s2(FALSE)

source("C:/Users/mkapur/Dropbox/UW/sab-mse/R/plotting/colorPals.R", echo=FALSE)

ak_run <- "2023-03-27-AK_500-145-v13-1-DOMLL-OBS2-4_Region=GOA"
wc_run <- "2022-01-15-WC_500"

# survfltPal <-matrix( c("#015b58" ,"#2c6184", "#1f455e", "#1f455e" ,"#984e73" ,"#a8bbcc"), 
#                      ncol = 6) 
# mgmtPal <- matrix(paste0("#",c("66827a","e7a923","9e2a2b")),nrow = 1)## 3 Demographic regions AK -> WC


## table with parameter estimates, formatted ----
load(here('runs',ak_run,'parameter_estimates.Rdata'))
TMB::summary.sdreport( parameter_estimates$SD,"fixed") %>%
  data.frame() %>%
  mutate(desc = NA, survey = 'AK GOA Trawl Survey') %>%
  select(desc, Estimate  ,Std..Error, survey ) %>%
  write.csv(here('tables',paste0(Sys.Date(),"-Table2-AK_LL.csv")))

load(here('runs',wc_run,'parameter_estimates.Rdata'))
TMB::summary.sdreport( parameter_estimates$SD,"fixed") %>%
  data.frame() %>%
  mutate(desc = NA, survey = 'AK GOA Trawl Survey') %>%
  select(desc, Estimate  ,Std..Error, survey ) %>%
  write.csv(here('tables',paste0(Sys.Date(),"-Table2-AK.csv")))
load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2022-01-15-WC_500/parameter_estimates.RData")
TMB::summary.sdreport( parameter_estimates$SD,"fixed") %>%
  data.frame() %>%
  mutate(desc = NA, survey = 'California Current Surveys') %>%
  select(desc, Estimate  ,Std..Error, survey ) %>%
  write.csv(here('tables',paste0(Sys.Date(),"-Table2-CC.csv")))
## Estimated Values vs assessment ----

## custom prediction maps ----
dyn.load(dynlib(here("VAST_v13_1_0"))) ## needs to be loaded for plot_maps to work
#* AK
akfit <- load(here('runs',ak_run,'fit.rds'))
# akfit <- readRDS("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2022-01-15-AK_500-145-v13_1-500m-obs20/2022-01-15-AK_500fit.RDS")
projargs_plot = "+proj=utm +datum=WGS84 +units=km +zone=3"
#projargs_plot = "+proj=moll +lon_0=-150 +datum=WGS84 +units=km"
#projargs_plot = "+proj=natearth +lon_0=-180 +datum=WGS84 +units=km"
mapl <- make_map_info(  Region =  "gulf_of_alaska",
                spatial_list = akfit$spatial_list,
                Extrapolation_List = akfit$extrapolation_list)
plot_maps(
  plot_set = 3,
  akfit,
  year_labels = akfit$year_labels,
  PlotDF = mapl$PlotDF,
  projargs = "+proj=utm +datum=WGS84 +units=km +zone=3",
  working_dir = here("runs",ak_run),
  quiet = FALSE,
  years_to_plot = akfit$years_to_plot,
  country = c("united states of america", "canada", "mexico", "russia", 'japan')
)

#* WC
wcfit <- readRDS("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2022-02-14-WC_500-nonEncounter-redo/2022-02-14-WC_500-nonEncounter-redofit.RDS")
projargs_plot = "+proj=utm +datum=WGS84 +units=km +zone=3"
#projargs_plot = "+proj=moll +lon_0=-150 +datum=WGS84 +units=km"
#projargs_plot = "+proj=natearth +lon_0=-180 +datum=WGS84 +units=km"
mapl <- make_map_info(  Region =  "California_Current",
                        spatial_list = wcfit$spatial_list,
                        Extrapolation_List = wcfit$extrapolation_list)
plot_maps(
  plot_set = 3,
  akfit,
  year_labels = akfit$year_labels,
  PlotDF = mapl$PlotDF,
  projargs = "+proj=utm +datum=WGS84 +units=km +zone=3",
  working_dir = here("runs","2022-02-14-WC_500-nonEncounter-redo/"),
  quiet = FALSE,
  years_to_plot = akfit$years_to_plot,
  country = c("united states of america", "canada", "mexico", "russia", 'japan')
)

#* load quants_m ----
load("C:/Users/mkapur/Dropbox/UW/sab-mse/input/input_data/quants_m.rdata") 
assidx <- quantsM %>% 
  filter(var == 'Index' )%>%
  filter(!(fleet == 'Triennial' & Yr == 2004)) %>% ## drop so doesn't sum
  mutate(Estimate_metric_tons = Value) %>% ## in kt
  select(Year = Yr,Estimate_metric_tons, se_log = CV, REG) %>%
  mutate(SRC = 'Regional_Assessment') 

assidx$Year <- as.numeric(assidx$Year)
assidx$Estimate_metric_tons[assidx$REG == 'AK'] <- assidx$Estimate_metric_tons[assidx$REG == 'AK']*1000 ##  multiply by 1000 to get mt
assidx$Estimate_metric_tons[assidx$REG == 'BC'] <- assidx$Estimate_metric_tons[assidx$REG == 'BC']*1000 ##  multiply by 1000 to get mt
assidx$REG[assidx$REG == 'WC'] <- 'CC'

## vast output says "kg" so div by 1000 to get mt
wcsurv <- read.csv(here('runs',wc_run,'index.csv')) %>%
  mutate(Estimate_metric_tons =  Estimate/1000) %>%
  select(Year = Time,
         Fleet = Stratum,
         Estimate_metric_tons ,
         se_log = Std..Error.for.ln.Estimate.) %>%
  filter(Estimate_metric_tons != 0)
wcsurv$Fleet <- ifelse(wcsurv$Fleet == 'Stratum_1','CC_VAST_C1','CC_VAST_C2')
wcsurv$REG <- 'CC'

fit_prelim2021_rec18 <- readRDS("C:/Users/mkapur/Dropbox/UW/sab-mse/input/downloads/fit_prelim2021_rec18/fit_prelim2021_rec18.rds")
bcreport <- fit_prelim2021_rec18$repOpt 
bcpost <- fit_prelim2021_rec18$posts
bcsurv0 <-  bcreport$I_pft
bcsurv1<-data.frame(t(bcsurv0[1,c(1,4,5),])) %>%
  mutate(Year = 1965:(1964+57)) 
names(bcsurv1)[1:3] <- c('Trap','BC_OFFStd','BC_StRs') 
bcsurv <- bcsurv1 %>%
  melt(id = 'Year') %>%
  mutate(se_log = NA,Estimate_metric_tons = value*1000) %>%
  select(Year, Fleet = variable, Estimate_metric_tons, se_log) %>%
  filter(Fleet != 'Trap')
bcsurv$se_log <- ifelse(bcsurv$Fleet == 'BC_OFFStd',0.29,0.21)
bcsurv$REG <- 'BC'

## vast output says "kg" so div by 1000 to get mt
aksurv <- read.csv(here('runs',ak_run,'index.csv')) %>%
  mutate(Estimate_metric_tons =  Estimate/1000) %>%
  select(Year = Time,
         Fleet = Stratum,
         Estimate_metric_tons ,
         se_log = Std..Error.for.ln.Estimate.) %>%
  filter(Estimate_metric_tons != 0)
aksurv$REG <- 'AK'
aksurv$Fleet <- ifelse(aksurv$Fleet == 'Stratum_1','AK_VAST_A4','AK_VAST_A3')

survey_master_flt <- rbind( wcsurv,aksurv,  bcsurv) %>% 
  mutate(SRC = 'INTO_OM') %>%
  bind_rows(., assidx)  %>%
  mutate(Fleet = ifelse(is.na(Fleet),'Regional_Assessment',Fleet)) %>%
  filter(Estimate_metric_tons > 0) %>%
  mutate(estimate_kt = Estimate_metric_tons/1000,
         lci = estimate_kt - estimate_kt*se_log,
         uci = estimate_kt + estimate_kt*se_log)

survey_master_mgmt <- survey_master_flt %>%
  group_by(Year, REG, SRC) %>%
  summarise(estimate_kt=sum(estimate_kt),
            lci = sum(lci), uci = sum(uci)) %>%
  mutate(plotgroup = ifelse(SRC != 'Regional_Assessment',
                            paste(REG, SRC), SRC))

#* raw indices, by fleet x mgmt ----

ggplot(subset(survey_master_flt, SRC != 'Regional_Assessment'), aes(x = Year, y = estimate_kt, color = Fleet )) +
  theme(legend.position = 'top')+
  scale_color_manual(values = survfltPal[c(2,1,3,4,6,5)],
                     labels = labs_survflts)+
  geom_point() +
  geom_linerange(aes(ymin = lci, ymax = uci)) +
  labs(x = 'Year', y = 'Estimate (kt)', color = '')+
  facet_wrap(~REG,scales = 'free_y', labeller = labeller(REG = labs_mgmt))

ggsave(last_plot(),
       file = here('figures',paste0(Sys.Date(),'-input_indices.png')),
       height = 7, width = 10, unit= 'in', dpi = 520)                   

## index comparison by fleet x mgmt ----

ggplot(subset(survey_master_flt, 
              !(SRC == 'Regional_Assessment' & REG == 'BC')), 
       aes(x = Year, y = estimate_kt, color = Fleet )) +
  theme(legend.position = 'top')+
  scale_color_manual(values = c(survfltPal[c(2,1,3,4,6,5)],'grey77'),
                     labels = c(labs_survflts,'Regional Assessment'))+
  geom_point() +
  geom_linerange(aes(ymin = lci, ymax = uci)) +
  labs(x = 'Year', y = 'Estimate (kt)', color = '')+
  facet_wrap(~REG,scales = 'free_y', labeller = labeller(REG = labs_mgmt))

ggsave(last_plot(),
       file = here('figures',paste0(Sys.Date(),'-input_indices_compare.png')),
       height = 7, width = 10, unit= 'in', dpi = 520)   

## index comparison by mgmt (sum) ----
ggplot(subset(survey_master_mgmt, 
              !(SRC == 'Regional_Assessment' & REG == 'BC')), 
       aes(x = Year, y = estimate_kt, color = plotgroup )) +
  ggsidekick::theme_sleek(base_size = 14)+
  theme(legend.position = 'top')+
  scale_color_manual(values = c(mgmtPal, 'grey77'),
                     labels = c(paste0('Total input survey(s) ',labs_mgmt[1:3]),
                                'Regional Assessment'))+
  geom_point() +
  geom_linerange(aes(ymin = lci, ymax = uci)) +
  labs(x = 'Year', y = 'Estimate (kt)', color = '')+
  facet_wrap(~REG,scales = 'free_y', labeller = labeller(REG = labs_mgmt))

ggsave(last_plot(),
       file = here('figures',paste0(Sys.Date(),'-input_indices_compare_m.png')),
       height = 7, width = 10, unit= 'in', dpi = 520)   


#* index comparison with strata summation ----
# 
# mgmtPalUse <- rep(mgmtPal[c(1,3,2)],2)
# mgmtPalUse[c(2,4,6)] <- 'grey55'
#   
# 
#   ggplot(., aes(x = Year, 
#                 y = emt, 
#                 alpha = SRC,
#                 color = interaction(SRC ,REG ), 
#                 fill  = interaction(SRC,REG  ))) +
#   theme_sleek(base_size = 12) + 
#   theme(legend.position = 'none') +
#   # scale_x_continuous(limits = c(1980,2020),
#   #                    labels = seq(1980,2020,10))+
#   scale_alpha_manual(values = c(0.4,0.2))+
#   scale_color_manual(values = mgmtPalUse)+
#   scale_fill_manual(values = mgmtPalUse)+
#   
#   geom_ribbon(aes(ymin = emt-emt*esel,
#                   ymax =  emt+emt*esel,
#                   width = 0), col = 'white')+
#   # geom_errorbar(aes(ymin = emt-emt*esel,
#   #                   ymax =  emt+emt*esel,
#   #                   width = 0))+
#   geom_point(size = 1.5) +
#   geom_line(lwd = 0.9)+
#   labs(x = 'Year', 
#        # subtitle='BC uses regional inputs as-is.
#        # AK & WC values have been summed across strata for plotting purposes.',
#        y = 'Relative Survey Abundance (mt)') +
#   facet_wrap(~REG, scales = 'free_y')
# 
# ggsave(plot =  last_plot(),
#        file = here('figures',paste0(Sys.Date(),'-idx_comparison1.png')),
#        width = 10, height = 6, units = 'in', dpi = 440)
# 
# #* index comparison with strata summation v2 ----
# ## no BC and labeled fleets
# mgmtPalUse <- c("#66827a",'grey55', "#9e2a2b",'grey55')
# 
# rbind( wcsurv,aksurv) %>% 
#   mutate(SRC = 'INTO_OM') %>%
#   bind_rows(., assidx %>% filter(REG != 'BC') ) %>%
#   filter(Estimate_metric_tons > 0) %>%
#   group_by(Year, REG, SRC )%>%
#   summarise(emt = sum(Estimate_metric_tons), esel = mean(se_log)) %>%
#   ggplot(., aes(x = Year, 
#                 y = emt, 
#                 alpha = SRC,
#                 color = interaction(SRC ,REG ), 
#                 fill  = interaction(SRC,REG  ))) +
#   theme_sleek(base_size = 12) + 
#   theme(legend.position = 'none') +
#   scale_alpha_manual(values = c(0.65,0.35), guide = 'none')+ 
#   scale_color_manual(values = mgmtPalUse)+
#   scale_fill_manual(values = mgmtPalUse)+
# 
#   geom_ribbon(aes(ymin = emt-emt*esel,
#                   ymax =  emt+emt*esel,
#                   width = 0), col = NA)+
#   geom_point(size = 1.5) +
#   geom_line(lwd = 0.9)+
#   labs(x = 'Year', 
#        y = 'Relative Survey Abundance (mt)') +
#   facet_wrap(~REG,
#              # scales = 'free_y'
#              )
# 
# ggsave(plot =  last_plot(),
#        file = here('figures',paste0(Sys.Date(),'-idx_comparison1_v2.png')),
#        width = 10, height = 6, units = 'in', dpi = 440)
# 
# #* index comparison without strata summation ----
# mgmtPalUse <- c(   mgmtPal[2],
#                    "#936A10",
#                    mgmtPal[3],
#                    "#D45E60", 
#                    mgmtPal[1],
#                    "#5A726A",
#                    'grey44')
# rbind( wcsurv,aksurv,  bcsurv) %>% 
#   mutate(SRC = 'INTO_OM') %>%
#   bind_rows(., assidx) %>%
#   filter(Estimate_metric_tons > 0) %>%
#   filter(!(is.na(Fleet) & REG == 'BC') & Year < 2020) %>%
#   mutate(emt =Estimate_metric_tons, esel = se_log ) %>%
#   ggplot(., aes(x = Year, 
#                 y = emt, 
#                 group = Fleet,
#                 alpha = SRC,
#                 color = Fleet, 
#                 fill  = Fleet)) +
#   theme_sleek(base_size = 15) + 
#   theme(legend.position = 'none') +
#   # scale_alpha_manual(values = c(0.3,0.2))+
#   # scale_color_manual(values = rev(mgmtPalUse))+
#   # scale_fill_manual(values = mgmtPalUse)+
#   
#   geom_ribbon(aes(ymin = emt-emt*esel,
#                   ymax =  emt+emt*esel,
#                   width = 0), col = NA)+
#   geom_point(size = 1.5) +
#   geom_line(lwd = 0.9)+
#   labs(x = 'Year', 
#        y = 'Relative Survey Abundance (mt)') +
#   facet_wrap(~REG, scales = 'free_y')
# 
# ggsave(plot =  last_plot(),
#        file = here('figures',paste0(Sys.Date(),'-idx_comparison2.png')),
#        width = 10, height = 6, units = 'in', dpi = 440)
# #* index comparison without strata summation V2 ----
# 
# mgmtPalUse <- c( "#015b58", "#2c6184", "#984e73" ,"#a8bbcc" , 'white' )
# 
# fltlabs = c('Subarea A4 VAST',
#             'Subarea A3 VAST',
#             'Subarea C2 VAST',
#             'Subarea C1 VAST',
#             'Regional Assessment')
# tt <- rbind( wcsurv,bcsurv,aksurv) %>% 
#   mutate(SRC = 'INTO_OM') %>%
#   bind_rows(., assidx) %>%
#   filter(Estimate_metric_tons > 0) %>%
#   filter(!(is.na(Fleet) & REG == 'BC') & Year < 2020) %>%
#   mutate(emt =Estimate_metric_tons, esel = se_log ) 
# tt$Fleet <- factor(tt$Fleet, levels = c('AK_VAST_A4','AK_VAST_A3','CC_VAST_C2','CC_VAST_C1'))  
# tt$Fleet[is.na(tt$Fleet)] <- 'Regional Assessment'
# dose.labs <-c("Alaska", "US West Coast")
# names(dose.labs) <-  c("AK", "CC") 
# 
# ggplot(tt, aes(x = Year, 
#                 y = emt/1000, 
#                 group = Fleet,
#                 color = Fleet, 
#                 fill  = Fleet)) +
#   theme_sleek(base_size = 15) + 
#   theme(legend.position = 'top') +
#   scale_color_manual(values = mgmtPalUse, labels = fltlabs)+
#   scale_fill_manual(values = mgmtPalUse, labels = fltlabs)+
#   geom_linerange(aes(ymin = (emt-emt*esel)/1000,
#                   ymax =  (emt+emt*esel)/1000,
#                   width = 0))+
#   geom_point(size = 1.5) +
#   labs(x = 'Year', 
#        color = '',fill = '',
#        y = 'Relative Survey Abundance (kt)') +
#   facet_wrap(~REG, 
#              scales = 'free_y',
#              labeller = labeller(REG = dose.labs))
# 
# ggsave(plot =  last_plot(),
#        file = here('figures',paste0(Sys.Date(),'-idx_comparison2_v2.png')),
#        width = 9, height = 5, units = 'in', dpi = 520)
# 
# rbind(tt, bcsurv) %>%
#   group_by(Year, REG, SRC) %>%
#   summarise(esel = sum(esel),
#             emt= sum(emt)) %>%
# 
# ggplot(., aes(x = Year, 
#                y = emt/1000,
#               color = interaction(REG,SRC))) +
#   theme_sleek(base_size = 15) + 
#   theme(legend.position = 'top') +
#   scale_color_manual(values = mgmtPalUse, labels = fltlabs)+
#   scale_fill_manual(values = mgmtPalUse, labels = fltlabs)+
#   geom_linerange(aes(ymin = (emt-emt*esel)/1000,
#                      ymax =  (emt+emt*esel)/1000,
#                      width = 0))+
#   geom_point(size = 1.5) +
#   labs(x = 'Year', 
#        color = '',fill = '',
#        y = 'Relative Survey Abundance (kt)') +
#   facet_wrap(~REG, 
#              scales = 'free_y',
#              labeller = labeller(REG = dose.labs))
# 
# ggsave(plot =  last_plot(),
#        file = here('figures',paste0(Sys.Date(),'-idx_comparison2_v2.png')),
#        width = 9, height = 5, units = 'in', dpi = 520)
# 
# 
# survfltPal0 <-  matrix(PNWColors::pnw_palette(name = 'Bay',n=5), ncol = 5) ## for VAST outputs
# survfltPal <- paste0("#",c("00496f","086788","0f85a0","7eae73","edd746",
#                            "edb123","ed8b00","e56612","E1541B","dd4124"))
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# # load("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Data_Geostat.Rdata")
# 
# 
# ## reboot of jim code showing survey regions and sample sizes [he used base] ----
# usa <- map_data("world")
# ## Updated F1 map of strata using Luke's approach ----
# load( "C:/Users/mkapur/Dropbox/UW/sab-mse/input/downloads/eez_nepac_regions.rda") ## regions
# load("C:/Users/mkapur/Dropbox/UW/sab-mse/input/downloads/sub_area_clips_50N.Rdata") ## clips (list)
# regions <- eez_nepac_regions
# st_crs(regions) <- st_crs(regions)
# regions$Region_Name <- c('Alaska (AK)',   'British Columbia (BC)',  "California Current (CC)")
# Data_Geostat <- readRDS(file =  here('data','2022-02-01inputVast.rds'))
# Data_Geostat$Lon <-  with(Data_Geostat, Lon + 360)
# 
# mgmtLims <- data.frame(ymax = c(65,65, 49),
#                        ymin = c(49,49, 30), 
#                        xmax = c(-180,-134, -115), 
#                        xmin = c(-134,-115, -132))
# 
# plist<-list()
# ggplot(data = regions) + 
#   theme_classic(base_size = 14) +
#   coord_sf(xlim = c(165, 245), ylim = c(26, 65)) +
#   labs(x ="",y="") + 
#   # theme(legend.position = 'none')+
#   
#   ## mgmt fill
#   geom_sf(data = clips[[1]], fill = mgmtPal[1], alpha = 0.75, color = NA)+
#   geom_sf(data = clips[[2]], fill = mgmtPal[1], alpha = 0.75, color = NA)+
#   geom_sf(data = clips[[5]], fill = mgmtPal[3], alpha = 0.75, color = NA)+
#   geom_sf(data = clips[[6]], fill = mgmtPal[3], alpha = 0.75, color = NA) +
#   
# 
#   ## add data points
#   geom_point(data = Data_Geostat, alpha = 0.01,
#              aes(x = Lon,  y = Lat, color = factor(Survey))) +
#   geom_sf_label(data = clips[[1]],aes(label = 'Alaska (AK)'),size = 4, fill = 'white', color = mgmtPal[1])+
#   geom_sf_label(data = clips[[5]],aes(label = 'California Current (CC)'),size = 4, fill = 'white', color = mgmtPal[3])
#   scale_color_manual(values = survfltPal)
# 
# 
# plist[[1]] <- ggplot() +
#   geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = 'grey55') +
#   coord_quickmap(clip = 'off') +
#   scale_x_continuous(expand = c(0,0), limits = c(-180,-110), breaks = seq(-180,-120,10), labels = paste(seq(-180,-120,10), "°W")) +
#   scale_y_continuous(expand = c(0,0), limits = c(30,75), breaks = seq(30,75,10), labels =  paste(seq(30,75,10), "°N"))  +
#   ggsidekick::theme_sleek()+
#   # kaputils::theme_black() +
#   theme(panel.grid = element_blank(),
#         legend.position = 'left',
#         legend.text = element_text(size = 10)) +
#   geom_rect(data = mgmtLims, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
#             fill =NA, size = 1, colour = 'red') + 
#   geom_label(aes( x = c(rep(-120,2),-145),
#                   y = c(40,55,55),
#                   label = c("Cal Curr.","BC","AK")),
#              size = 3) +
#   
#   guides(colour = guide_legend(override.aes = list(alpha=1))) +
#   geom_point(data = Data_Geostat, alpha = 0.01,
#              aes(x = Lon,  y = Lat, color = factor(Survey))) +
#   geom_label(aes( x = c(rep(-120,2),-145),
#                   y = c(40,55,55),
#                   label = c("Cal Curr.","BC","AK")),
#              size = 3) +
#   scale_color_manual(values = survfltPal) +
#   labs(x = 'Longitude', y = 'Latitude', color = 'Survey')
# plist[[2]] <- Data_Geostat %>%
#   group_by(Survey, Year) %>%
#   summarise(n = n()) %>%
#   mutate(n2 = (n-min(n))/(max(n)-min(n))) %>%
#   ggplot(., aes(x = Year, y = Survey, fill = Survey)) +
#   ggsidekick::theme_sleek(base_size = 10) +
#   # kaputils::theme_black() +
#   theme(legend.position = 'bottom') +
#   scale_alpha(guide = 'none') +
#   # geom_point(size = 4) +
#   # geom_bar(stat = 'identity', position = 'stack')+
#   geom_tile(aes(alpha =  n2))+
#   coord_equal()+
#   scale_fill_manual(values =survfltPal)+
#   labs(x = 'Year', y = '', color = 'Survey', 
#        title = 'transparency indicates relative # samples used in standardization')
# 
# Rmisc::multiplot(plotlist = plist, cols = 1)
# 
# ggsave(plot =  Rmisc::multiplot(plotlist = plist, cols = 1),
#        file = "./figures/data_map_update.png",
#        width = 8, height = 10, units = 'in', dpi = 440)
# 
# ## EDA on effort metrics
# # Data_Geostat %>% group_by(State) %>%
# #   dplyr::summarise(mean(AreaSwept_km2))
# 
# 
# 
# 
# ## Comparison of VAST outputs with assessment values by region ----
# 
# sab2019_2 <- read.csv(here('data',"sab2019update_vast.csv"))%>% 
#   # sab2019$Fleet[sab2019$Fleet == 'All_areas'] <- "2019 WCBTS Used in Assessment"
#   mutate(Unit = NA, SD_mt = NA, Fleet = Fleet_name ) %>%
#   select(Yr, Unit, Fleet, Obs, SE, SD_mt)
# # "SAB_2019_Assessment CPUE [vast]"
# names(sab2019_2)[c(1,4,5)] <- c('Year','Estimate_metric_tons', "SD_log")
# ## load CPUE used in 2019 SAB assessment [from KFJ on Gdrive]
# # sab2019 <- read.csv(here('runs',"sabWCVAST/WCGBTS/Table_for_SS3.csv"))
# 
# 
# 
# # bind_rows(read.csv(paste0(DateFile,"/Table_for_SS3.csv")), sab2019, sab2019_2) %>%
# #   filter(Fleet %in% c("AKSHLF",
# #                       "All_areas",
# #                       "California_current")) %>%
# #   ggplot(.,
# #          aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
# #   theme_minimal()+
# #   theme(panel.grid = element_blank(),
# #         legend.position = 'bottom') +
# #   scale_y_continuous(limits = c(0,300000)) +
# #   scale_color_brewer(palette = 'Accent') +
# #   labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices vs Assessment values')) +
# #   scale_color_manual(values = c('grey40','grey44','blue'))+
# #   geom_line(pch = 1, cex = 3) +
# #   geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)
# 
# 
# 
# # data for compare  ----
# ## how about we instead use the raw data_geostat?
# # 
# # assc <- read.csv(here("data","assessment_CPUE.csv")) %>%
# #   mutate(Source = 'assessment', Estimate_metric_tons = Value ,
# #          Fleet2 = substr(Index,1,2)) %>%
# #   filter(Type == 'Abundance' |Type == 'Biomass')  %>%
# #   filter(!grepl('PUE', Index),!grepl('NUM', Index)) %>%
# #   bind_rows(.,
# #             read.csv(here("data","BC","BC_sable_survey_data.23Dec2019.csv"))  %>% 
# #             # read.csv(here("data","BC","BC_sable_survey_data.Aug262019.csv"))  %>% 
# #               filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & !is.na(TOTAL_SABLE_WEIGHT) & 
# #                        SABLE_SET_TYPE == 'StRS'  ) %>% group_by(SET_YEAR) %>% 
# #               dplyr::summarise(meanCPUE = mean(cpue)) %>%
# #               mutate(Year = SET_YEAR, Value = meanCPUE, CV = NA, 
# #                      Index = 'Filter_StRS', Type = 'Biomass', Source = 'Assessment',
# #                      Estimate_metric_tons =meanCPUE, Fleet2 = 
# #                        'BC') %>%
# #               select(-meanCPUE, -SET_YEAR)) %>%
# #   bind_rows(.,
# #             read.csv(here("data","BC","BC_sable_survey_data.23Dec2019.csv"))  %>% 
# #               # read.csv(here("data","BC","BC_sable_survey_data.Aug262019.csv"))  %>% 
# #               filter(START_LONGITUDE <= 0 & !is.na(CPUE_TRAPS) & !is.na(TOTAL_SABLE_WEIGHT) & 
# #                        SABLE_SET_TYPE == 'OFFSHORE STANDARDIZED'  ) %>% group_by(SET_YEAR) %>% 
# #               dplyr::summarise(meanCPUE = mean(cpue)) %>%
# #               mutate(Year = SET_YEAR, Value = meanCPUE, CV = NA, 
# #                      Index = 'BC_OFFSHORE_STD', Type = 'Biomass', Source = 'Assessment',
# #                      Estimate_metric_tons =meanCPUE, Fleet2 = 
# #                        'BC') %>%
# #               select(-meanCPUE, -SET_YEAR)) %>%
# #   bind_rows(.,
# #             read.csv(paste0(DataFile,"/BC/BC_trawl_survey_sable_data.Oct312019.csv"))  %>% 
# #               filter(LONGITUDE <= 0 & !is.na(CATCH_WEIGHT)) %>%
# #               group_by(YEAR) %>% 
# #               dplyr::summarise(meanCPUE = mean(CATCH_WEIGHT)) %>%
# #               mutate(Year = YEAR, Value = meanCPUE, CV = NA, 
# #                      Index = 'Filter_BCTrawl', Type = 'Biomass', Source = 'Assessment',
# #                      Estimate_metric_tons =meanCPUE, Fleet2 = 
# #                        'BC') %>%
# #               select(-meanCPUE, -YEAR)) %>%
# #   bind_rows(., sab2019_2 %>% 
# #               mutate(Fleet2 = 'WC', Index = Fleet, Value = Estimate_metric_tons, Type = 'Abundance',
# #                      Source = 'assessment') %>%
# #               select(Year, Estimate_metric_tons,  Index, Type, Source, Fleet2))
# # 
# # 
# # 
# # # assc$Fleet[-grep(paste0(c('NUM','PUE',collapse= "|")),assc$Fleet)]
# # 
# # ## Correct scales
# # assc$Estimate_metric_tons[assc$Fleet2 == 'AK'] <- assc$Value[assc$Fleet2 == 'AK']*1000
# # # assc$Estimate_metric_tons[assc$Fleet2 == 'WC'] <- assc$Value * 1000
# # assc$Estimate_metric_tons[assc$Fleet2 == 'BC'] <- assc$Value[assc$Fleet2 == 'BC']*1000
# # 
# # 
# # names(assc) <- c('Year','Value','SD_log',"Fleet","TYPE", 'Source',"Estimate_metric_tons","Fleet2")
# # 
# # assc <- assc %>%
# #   select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, Fleet2 ) %>%
# #   mutate(uci=NA, lci = NA)
# 
# # write.csv(assc, file = here("data","assc.csv"), row.names = FALSE)
# assc <- read.csv(here("data","assc.csv"))
# # DateFile <-"C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-09-22_nx=250_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2019/"
# ## see line 81 here for conv https://github.com/James-Thorson-NOAA/FishStatsUtils/blob/master/R/plot_index.R
# vastc <-
# # vastc <- read.csv("C:/Users/mkapur/Dropbox/UW/sab-idx/runs/2020-01-23_nx=500_Triennial_WCGBTS_BCs_BCo_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2018/Table_for_SS3.csv") %>%
# read.csv(paste0(DateFile,"Table_for_SS3.csv")) %>%
#                     mutate(TYPE = 'Abundance', Source = 'VAST',
#                            lci = Estimate_metric_tons-SD_mt,
#                            uci = Estimate_metric_tons+SD_mt) %>%
#   select(Year, Fleet, Estimate_metric_tons, SD_log, TYPE, Source, uci, lci ) #%>%
#   # filter(Fleet != 'Gulf_of_Alaska')
# vastc$Fleet2 <- NA
# for(i in 1:nrow(vastc)){
#   vastc$Fleet2[i] <- ifelse(vastc$Fleet[i] == "California_current",
#                              "WC", 
#                              ifelse(vastc$Fleet[i] == "British_Columbia", "BC",
#                                     "AK"))
#   if(vastc$Fleet[i] == 'AllAreas')   vastc$Fleet2[i] <- "ALL"
# }
# # 
# # ## needed to do this for WCGBTS/GOALATE AS REF
# vastc$Estimate_metric_tons <- vastc$Estimate_metric_tons*1000
# vastc$uci <- vastc$uci*1000
# vastc$lci <- vastc$lci*1000
# 
# # vastc$Estimate_metric_tons[vastc$Fleet2 == 'BC'] <-
# #   vastc$Estimate_metric_tons[vastc$Fleet2 == 'BC'] * 1E-2
# # vastc$uci[vastc$Fleet2 == 'BC'] <- vastc$uci[vastc$Fleet2 == 'BC'] * 1E-2
# # vastc$lci[vastc$Fleet2 == 'BC'] <- vastc$lci[vastc$Fleet2 == 'BC'] * 1E-2
# # vastc$lci[vastc$lci < 0 ] <- 0
# # vastc$Estimate_metric_tons[vastc$Fleet2 == 'WC'] <-
# #   vastc$Estimate_metric_tons[vastc$Fleet2 == 'WC'] * 1E-2
# # vastc$uci[vastc$Fleet2 == 'WC'] <- vastc$uci[vastc$Fleet2 == 'WC'] * 1E-2
# # vastc$lci[vastc$Fleet2 == 'WC'] <- vastc$lci[vastc$Fleet2 == 'WC'] * 1E-2
# # vastc$Estimate_metric_tons[vastc$Fleet2 == 'AK' ] <-
# #   vastc$Estimate_metric_tons[vastc$Fleet2 == 'AK'] * 1E-4
# # vastc$uci[vastc$Fleet2 == 'AK'] <-
# #   vastc$uci[vastc$Fleet2 == 'AK'] * 1E-4
# # vastc$lci[vastc$Fleet2 == 'AK'] <- vastc$lci[vastc$Fleet2 == 'AK'] * 1E-4
# # #
# # vastc$Estimate_metric_tons[vastc$Fleet == 'Gulf_of_Alaska'] <-
# #   vastc$Estimate_metric_tons[vastc$Fleet == 'Gulf_of_Alaska'] * 1e3/1e4
# # vastc$uci[vastc$Fleet == 'Gulf_of_Alaska'] <-
# #   vastc$uci[vastc$Fleet == 'Gulf_of_Alaska'] * 1e3/1e4
# # vastc$lci[vastc$Fleet == 'Gulf_of_Alaska'] <-
# #   vastc$lci[vastc$Fleet == 'Gulf_of_Alaska'] * 1e3/1e4
# 
# 
# 
# 
# 
# ## truncate CI so plot is visible
# # vastc$uci[vastc$Fleet2 == 'AK'] <- ifelse(vastc$uci[vastc$Fleet2 == 'AK'] > 7e5,
# #                                           7e5,
# #                                           vastc$uci[vastc$Fleet2 == 'AK'])
# # 
# # vastc$lci[vastc$Fleet2 == 'AK'] <- ifelse(vastc$lci[vastc$Fleet2 == 'AK'] > 2e5,
# #                                           2e5,
# #                                           vastc$lci[vastc$Fleet2 == 'AK'])
# # 
# 
# 
# fleetSel <- c(1:4,6,7,10,8,11,12)
# 
# custnames <- c(paste0('VAST ',c('California Current','British Columbia',
#                                 'Aleutian Islands', 'Gulf of Alaska','Eastern Bering Sea') ),
#                'AK Domestic Longline', "AK Gulf Trawl", 'BC Offshore Standardized','BC Synoptic Trawl','BC Trap Stratified',
#                'Triennial','WCGBTS')[fleetSel]
# 
# 
# 
# 
# ## compare_all plot ----
# rbind(vastc,assc) %>%
#   filter(Fleet2 %in% c('WC','BC', 'AK')[1:3]) %>%
#   filter(Fleet %in% c("California_current","British_Columbia",
#                       'Aleutian_Islands',"Gulf_of_Alaska",
#                       "Eastern_Bering_Sea",
#                       'AK_DOM_LL',"AK_GOA_TRW",'BC_OFFSHORE_STD',
#                       "Filter_BCTrawl","Filter_StRS", "AKSHLF",  "NWCBO")[fleetSel]) %>%
#   # filter(Fleet %in% c("British_Columbia","Filter_BCTrawl")) %>%
#   ggplot(., aes(x = Year, y = Estimate_metric_tons, 
#                 col = Fleet, linetype = Fleet)) +
#   theme_bw()+
#   # kaputils::theme_black()+
#   theme(panel.grid = element_blank(),
#         legend.position = 'right',
#         legend.text = element_text(size = 12),
#   strip.text.x = element_text(
#     size = 16
#   )) +  
#   geom_line(lwd = 1) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = Fleet), 
#               alpha = 0.15, col = 'grey',
#               show.legend = FALSE) +
#   scale_x_continuous(limits = c(1980,2020),
#                      breaks = seq(1980,2020,10)) +
#   scale_fill_manual(values = c(rep('blue',3), 'dodgerblue3',
#                                cbbPalette),
#                     labels = c(custnames)) +
#   scale_color_manual( values = c(rep('blue',3), 'dodgerblue3',
#                                  cbbPalette),
#                       labels = c(custnames)) +
#   scale_linetype_manual(values = c(rep('solid',4),
#                                    rep('dashed',40)),
#                         labels = c(custnames)) +
#   labs(x = 'Year', y = 'Estimate (mt)', color = "", linetype = "",
#        title = '',
#        subtitle = 'All VAST Estimates have been multiplied by 1000;
#        (input dat was div by 1000)') +
# 
#   facet_wrap(~Fleet2, scales = 'free_y', ncol = 3)
# 
# 
#   ggsave(plot = last_plot(),
#        # file = paste0("./figures/",Sys.Date(),"_idx_comparison.png"),
#        file = paste0(DateFile,"/compare_all-update.png"),
#        height = 8, width = 12, unit='in',dpi = 320)
#   
#   
# 
#   
# 
# vastc %>% group_by(Fleet) %>% summarise(mean(exp(SD_log)))                          
# # ## Prettier index plot ----
# # source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")
# vastc %>%
#   filter(Fleet != 'Eastern_Bering_Sea' & Fleet != 'AllAreas') %>%
#   ggplot(.,
#          aes(x = Year, y = Estimate_metric_tons, col = Fleet)) +
#   theme_bw()+
#   # kaputils::theme_black() +
#   ggsidekick::theme_sleek() +
#   theme(legend.position = c(0.8,0.9)) +
#   scale_color_manual(values = survfltPal[c(1,4,7,9)]) +
#   scale_fill_manual(values = survfltPal[c(1,4,7,9)]) +
#   labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices')) +
#   geom_line(lwd = 0.9)+
#   # scale_y_continuous(limits = c(0,2.5e5)) +
#   # geom_point(pch = 1, cex = 3) +
#   geom_ribbon(aes(ymin = lci,
#                   ymax = uci,  fill = Fleet),
#               alpha = 0.2,
#               show.legend = FALSE)
# #   
# ggsave(plot = last_plot(), file = paste0(DateFile,"Index-Biomass2.png"),
#        height = 6, width = 8, unit='in',dpi = 360)
# 
# 
# # source("C:/Users/maia kapur/Dropbox/kaputils/R/theme_mk.R")
# 
# ggplot(read.csv(paste0(DateFile,"/Table_for_SS3.csv")) %>%
#          filter(Fleet != 'Eastern_Bering_Sea' & Fleet != 'AllAreas') ,
#          # mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),
#        aes(x = Year, y = log(Estimate_metric_tons), col = Fleet)) +
#   theme_minimal()+
#   scale_y_continuous(limits = c(5,15)) +
#   scale_color_manual(values = survfltPal) +
#   labs(x = 'Year', y = 'Estimate log(mt)', 
#        title = paste0('VAST-Standardized Indices (log space)')) +
#   geom_line(lwd = 0.9)+
#   geom_point(pch = 1, cex = 3) +
#   labs(y = 'log(mt)')
# # geom_errorbar(aes(ymin = Estimate_metric_tons-SD_mt, ymax = Estimate_metric_tons+SD_mt), lwd = 0.9)
# # 
# # ggsave(plot = last_plot(), file = paste0(DateFile,"logIndex-Biomass2.png"), height = 6, width = 8, unit='in',dpi = 520)
# # 
# # ggplot(read.csv(paste0(DateFile,"/Table_for_SS3_original.csv")) %>% 
# #          # filter(Fleet != 'All') %>%
# #          mutate(Year = seq(min(Data_Geostat$Year), max(Data_Geostat$Year))),
# #        aes(x = Year, y =Estimate_metric_tons, col = Fleet)) +
# #   theme_mk()+
# #   # theme(panel.grid.minor = element_blank()) +
# #   # scale_y_continuous(limits = c(0,25)) +
# #   scale_color_brewer(palette = 'Spectral') +
# #   labs(x = 'Year', y = 'Estimate (mt)', title = paste0('VAST-Standardized Indices')) +
# #   geom_line(lwd = 0.9)+
# #   geom_point(pch = 1, cex = 3) 
# # ggsave(plot = last_plot(), file = paste0(DateFile,"Index-Biomass2.png"), height = 6, width = 8, unit='in',dpi = 520)
# 
# # Plot center of gravity
