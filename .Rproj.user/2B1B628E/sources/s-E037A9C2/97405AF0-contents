## scrape data from IPHC website
require(dplyr)
reqiure(ggplot2)
# library(openxlsx)

## https://www.iphc.int/data/iphc-secretariat-survey-data
# for(yr in 1998:2018){
#   if(yr == 2018){
#     newFile <- paste0("https://www.iphc.int/uploads/data/survey/2018/2018-publicdata-by-regarea-metric-set.xlsx")
#   } else if(yr == 2016){
#     newFile <-   "https://www.iphc.int/uploads/data/survey/2016/2016fissall.xlsx"
#   } else if(yr == 2017){
#     newFile <-  "https://www.iphc.int/uploads/data/survey/2017/2017fisscoastwide-static.xlsx"
#   } else if(yr == 1998  | yr == 1999){
#     yr2 <- substr(yr,3,4)
#     newFile <-  paste0("https://www.iphc.int/uploads/data/survey/",yr,"/",yr2,"ssaall",ifelse(yr == 1998,".xls","update.xls"))
#   }else if(yr >=2001 & yr < 2010){
#     yr2 <- paste0("2k",substr(yr,4,4))
#     newFile <- paste0("https://www.iphc.int/uploads/data/survey/",yr,"/",yr2,"ssaall.xls")
#   }else if(yr == 2000){
#     newFile <- "https://www.iphc.int/uploads/data/survey/2000/2kssaall.xls"
#   }else if(yr == 2011 |yr==2012){
#     newFile <- paste0("https://www.iphc.int/uploads/data/survey/",yr,"/",yr,"ssaall_revised.xls")
#   }else if(yr >=2013 & yr <2016){
#     newFile <-  paste0("https://www.iphc.int/uploads/data/survey/",yr,"/",yr,"ssaall.xlsx")
#     
#     # yr2 <- substr(yr,4,4)
#   }
#   download.file(newFile,
#                 paste0("./data/",yr,"-SCRAPE.xlsx"),
#                 method="wininet", #use "curl" for OS X / Linux, "wininet" for Windows
#                 mode="wb") # "wb" means "write binary"
# }

iph <- read.csv("./data/manual_compile_2019-05-13.csv", na.strings = "#N/A")

## some summaries
iph %>% group_by(YEAR) %>% summarise(n=n()) %>% ggplot(.,aes(x=YEAR, y = n)) +geom_bar(stat='identity')+theme_classic()+scale_fill_grey()
iph %>% group_by(YEAR) %>% summarise(nSab = sum(Sablefish_n3)) %>% ggplot(.,aes(x=YEAR, y = nSab)) +geom_bar(stat='identity')+theme_classic()+scale_fill_grey()

require(mapdata)
usa <- map_data("world")
iph$long2 <- as.numeric(gsub("'", "",iph$Longitude, fixed = TRUE) %>% gsub(".", "", ., fixed = TRUE) %>% gsub(" ", ".", ., fixed = TRUE))  
iph$lat2 <- as.numeric(gsub("'", "",iph$Latitude, fixed = TRUE) %>% gsub(".", "", ., fixed = TRUE) %>% gsub(" ", ".", ., fixed = TRUE))  
iph$long2 <- ifelse(iph$long2>10000, iph$long2/10000,iph$long2)
iph$lat2 <- ifelse(iph$lat2>10000, iph$lat2/10000,iph$lat2)
iph$skates_set2 <- as.numeric(as.character(iph$skates_set))
iph$sabnum <- as.numeric(iph$Sablefish_n3)/iph$skates_set2 ## proportion of mean

ymins = c(1995,2006)
ymaxs = c(2005,2017)
for(y in 1:2){
ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) +
  coord_quickmap() +
  scale_x_continuous(expand = c(0,0), limits = c(-180,-110), breaks = seq(-180,-120,20), labels = paste(seq(-180,-120,20), "°W")) +
  scale_y_continuous(expand = c(0,0), limits = c(30,75), breaks = seq(30,75,10), labels =  paste(seq(30,75,10), "°N"))  +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.title =element_blank(),
        legend.position = 'right',
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size = 12)) +
  geom_point(data = subset(iph, YEAR >= ymins[y] & YEAR <= ymaxs[y] & sabnum >0 & sabnum < 10), 
             aes(x = long2, y = lat2, fill = sabnum), 
             shape = 21, 
             alpha = 0.5,
             stroke = 0) +
  scale_fill_viridis_c()+
  labs(fill = "Nonzero Sablefish NPUE",
       title = paste0('IPHC FISS Sablefish Bycatch ',ymins[y],"-",ymaxs[y])) +
  facet_wrap(~YEAR)
}


with(iph, plot(lat2 ~ long2, type = 'p', color = factor(sabnum)))
