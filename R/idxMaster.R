## Jan 2021 update
## trying to minimize repeated process
## kapurm@uw.edu

## I downloaded R v4.0.03 on desktop 01 Feb 2021, and updated Rtools
## Updating all VAST, FishStatsUtils here to start fresh.

## load pre-made datasets
require(here)
require(VAST)
require(dplyr)

## I ended up running separate VASTs for each of AK, WC 
## with DOM_LL and WCGBTS as BaseQ, respectively.
## This was done to overcome versioning issues in VAST,
## memory errors when trying to run everything with > 500 knots,
## and humungous SDs when BC was included,
## not to mention the inability to reproduce general trends from each reason.
## Therefore I am "stitching" together indices from 3 regions which were each created differently.
## this code draws them out of the relevant file and saves the CSV both here and in sab-mse.
## plotting for OM is in sab-mse/dataprep.

## cols are "Year"                 "Unit"       
# "Fleet"                "Estimate_metric_tons" "SD_log"              "SD_mt"
ak <- read.csv(here('runs','2021-02-12_nx=500_AK_DOM_LL_GOA_baseQ=AK_DOM_LL1980_2019',
                    'Table_for_SS3.csv'))  %>%
  distinct(Fleet, Year, Estimate_metric_tons, .keep_all = TRUE) %>% ## remove any dupes
  filter(Fleet %in% c('Gulf_of_Alaska', 'Aleutian_Islands'))
wc <- read.csv(here('runs','2021-02-12_nx=500_Triennial_WCGBTS_baseQ=WCGBTS1980_2019',
                    'Table_for_SS3.csv'))

with(ak, plot(Year ~ Estimate_metric_tons, col =  factor(Fleet)))
legend('topright', legend = unique(ak$Fleet), col = factor( unique(ak$Fleet)), pch = 1)


VAST <- rbind(ak, wc)
VAST$Fleet <- ifelse(VAST$Fleet == "California_current","CC",
                     ifelse(VAST$Fleet == "Gulf_of_Alaska","AK",
                     "AI"))
write.csv(VAST, file = here('runs',paste0(Sys.Date(),"ak_wc_vast.csv")), row.names = FALSE)
write.csv(VAST, 
          file ="C:/Users/maia kapur/Dropbox/UW/sab-mse/input/raw_data/survey/Indices_SS3_2021-02-15AK_WC_VAST.csv", row.names = FALSE)
