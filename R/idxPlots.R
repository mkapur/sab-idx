## sourceable script to autogen applicable plots
require(dplyr)
require(ggplot2)
require(reshape2)
require(mapdata)
require(ggsidekick)
require(here)
survfltPal <- paste0("#",c("00496f","086788","0f85a0","7eae73","edd746",
                           "edb123","ed8b00","e56612","E1541B","dd4124"))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Surveys_to_include <- c("Triennial", "WCGBTS", "BCs", "BCo",
                        "BCt", "AK_DOM_LL", "GOA", "EBS")[c(1:4,6:7)] #This will only work for years after 2003


Region <- NULL 
## This is Thorson's -- Kelli had a way of pre-subsetting to have N/S embedded
# if( TRUE ){
if(any(c("WCGBTS","Triennial") %in% Surveys_to_include)) Region = c( Region, "California_current")
if("BCs" %in% Surveys_to_include | "BCt" %in% Surveys_to_include) Region = c( Region, "British_Columbia" )
if("GOA" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska" )
if("GOA" %in% Surveys_to_include) Region = c( Region, "Aleutian_Islands" )

if("EBS"  %in% Surveys_to_include) Region = c( Region, "Eastern_Bering_Sea" )
if("AK_DOM_LL" %in% Surveys_to_include) Region = c( Region, "Gulf_of_Alaska", "Eastern_Bering_Sea" )



Year_Set <- min(Data_Geostat[,'Year']):max(Data_Geostat[,'Year'])
# Plot index
Index <- plot_biomass_index( DirName=paste0(outfile,"/"), 
                             TmbData=TmbData, 
                             use_biascorr = FALSE,
                             Sdreport=Opt$SD, 
                             Year_Set=Year_Set, 
                             strata_names=c('AllAreas',Region), 
                             plot_log=TRUE, 
                             width=6, height=6 ) # , total_area_km2=sum(a_xl[,1])

cat('ran plot_biomass_index \n')

assc <- read.csv(here("data","assc.csv")) %>% filter(Fleet2 != "BC") ## custom bc
bcidx <- read.csv(here('data','BC','bcom_indexseries.csv')) %>% 
  melt(id = "YEAR") %>% filter(variable != "nominal.Trap.CPUE") %>%
  mutate(value = value*1000) %>%
  mutate(SD_log = NA, TYPE = NA, Source = 'assessment', Fleet2 = 'BC', uci = NA, lci = NA)
names(bcidx)[1:3] <-c('Year','Fleet','Estimate_metric_tons')
assc <- rbind(assc, bcidx )

## see line 81 here for conv https://github.com/James-Thorson-NOAA/FishStatsUtils/blob/master/R/plot_index.R
vastc <-
  read.csv(paste0(outfile,"/Table_for_SS3.csv")) %>%
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
if(BaseQ != 'AK_DOM_LL'){
  vastc$Estimate_metric_tons <- vastc$Estimate_metric_tons*1000
  vastc$uci <- vastc$uci*1000
  vastc$lci <- vastc$lci*1000
  cat('multiplied est and lci by 1000 \n')
}

## custnames must match the order they appear on the plot

# fleetSel <- c(1:4,6,7,10,8,11,12)
fleetSel <- c(6,7,10,4,2,1,3,11,8,9)

custnames <-
  c(
    paste0(
      'VAST ',
      c(
        'California Current',
        'British Columbia',
        'Gulf of Alaska',
        'Aleutian Islands',
        'Eastern Bering Sea'
      )
    ),
    'AK Domestic Longline',
    "AK Gulf Trawl",
    'BC Offshore Standardized',
    'BC Trap Stratified',
    'Triennial',
    'WCGBTS'
  )[fleetSel]


## compare_all plot ----
rbind(vastc,assc) %>%
  filter(Estimate_metric_tons >0 )%>%
  filter(Fleet %in% c("California_current",
                      "British_Columbia",
                      'Aleutian_Islands',
                      "Gulf_of_Alaska",
                      'AK_DOM_LL',
                      "AK_GOA_TRW",
                      'std..survey',
                      "StRS.survey",
                      "AKSHLF", 
                      "NWCBO")) %>%
  # filter(Fleet %in% c("British_Columbia","Filter_BCTrawl")) %>%
  ggplot(., aes(x = Year, y = Estimate_metric_tons, 
                col = Fleet, 
                shape = tolower(Source),
                linetype = tolower(Source))) +
  theme_sleek()+
  # kaputils::theme_black()+
  theme(panel.grid = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 12),
        strip.text.x = element_text(
          size = 16
        )) +  
  geom_line(lwd = 1) +
  # geom_point(show.legend = FALSE) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = Fleet), 
              alpha = 0.15,
              col = 'grey',
              show.legend = FALSE) +
  scale_x_continuous(limits = c(1980,2020),
                     breaks = seq(1980,2020,10)) +
  scale_fill_manual(values = c(survfltPal[c(1,2,9)],
                              'blue','blue','blue','dodgerblue3',
                              survfltPal[c(8,4,5)]),
                    labels = c(custnames)) +
  scale_color_manual( values = c(survfltPal[c(1,2,9)],
                                 'blue','blue','blue','dodgerblue3',
                                 survfltPal[c(8,4,5)]),
                                 labels = c(custnames)) +
  scale_linetype_manual(values = c('dotted','solid')) +
  # scale_shape_manual(values = c(19,NA)) +
  labs(x = 'Year', y = 'Estimate (mt)', color = "", linetype = "",
       title = '',
       subtitle = ifelse(BaseQ != 'AK_DOM_LL','All VAST Estimates have been multiplied by 1000;
       (input dat was div by 1000)',"")) +
  
  facet_wrap(~Fleet2, scales = 'free_y', ncol = 3)


ggsave(plot = last_plot(),
       # file = paste0("./figures/",Sys.Date(),"_idx_comparison.png"),
       file = paste0(outfile,"/compare_idx.png"),
       height = 8, width = 12, unit='in',dpi = 320)
cat('saved compare_idx.png \n')

plot_data( Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, 
           Data_Geostat=Data_Geostat, PlotDir=outfile, 
           Plot1_name="Data_and_knots.png", Plot2_name="Data_by_year.png", col="red")
cat('ran plot_data \n')


plot_range_index( Sdreport=Opt$SD, Report=Report, Year_Set=Year_Set, TmbData=TmbData, 
                  Znames=colnames(TmbData$Z_xm), PlotDir=outfile )
cat('ran plot_range_index \n')

# Plot Anisotropy
plot_anisotropy( FileName=paste0(outfile,"Aniso.png"), Report=Report )
cat('ran plot_anisotropy \n')

# Plot encounter rate diagnostics
# plot_quantile_diagnostic( Report=Report, TmbData=TmbData, outfile=outfile)

# Positive catch rate diagnostics
# Q <- SpatialDeltaGLMM::plot_quantile_diagnostic( TmbData=TmbData, Report=Report ) # 
# save(Q, file = paste0(outfile,'Q.rdata'))
# cat('ran & saved Q \n')
# 
# # Pearson residuals diagnostics
# plot_residuals( Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], 
#                 extrapolation_list = Extrapolation_List,
#                 TmbData=TmbData, Report=Report, Q=Q, savedir=outfile, spatial_list=Spatial_List )
# cat('ran plot_residuals \n')
projargs_plot = "+proj=utm +datum=WGS84 +units=km +zone=3" ## gives spTransform error w latest fishstat
plot_maps(
  plot_set = 3,
  Report = Save$Report,
  PlotDF = MapDetails_List[["PlotDF"]],
  working_dir = paste0(outfile,"/"),
  Year_Set = Year_Set,
  Years2Include = (1:length(Year_Set))[Year_Set >1994 & Year_Set %%5==0],
  country = c("united states of america", "canada", "mexico", "russia", 'japan'),
  projargs = projargs_plot
)

cat('ran plot_maps \n')

## plot easting-northing shifts
#To plot effective area occupied, please re-run with Options['Calculate_effective_area']=1
plot_range_index( Sdreport=Save$Opt$SD, Report=Save$Report, Year_Set=Year_Set, 
                  TmbData=TmbData, Znames=colnames(TmbData$Z_xm),
                  PlotDir=paste0(outfile,"/") )
cat('ran plot_range_index \n')

# source("https://raw.githubusercontent.com/nwfsc-assess/VASTWestCoast/2473eb0ca2c25aa780e39ff1a94e7252d0d335bc/R/summary_nwfsc.R")
source(here("R","summary_nwfscMK.R"))
TableC <- summary_nwfscMK(obj = Save$Obj, 
                          sdreport = Save$Opt$SD, 
                          savedir = paste0(outfile,"/"))[[3]]

TableC %>% data.frame() %>% 
  # exp() %>% 
  round(.,2) %>% 
  mutate('PAR'=row.names(TableC)) %>%
  write.csv(.,file = paste0(outfile,'/tableC_mod.csv'))
