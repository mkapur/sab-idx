require(r4ss)
require(dplyr)
# sab2019 <- SS_output("C:/Users/mkapur/Dropbox/UW/assessments/sablefish-2019-update/200.00_base_files_29May/100.00_base_files/afsc_selex/100.24/")


sab2019 <- SS_output("C:/Users/mkapur/Dropbox/UW/assessments/sablefish-2019-update/STAR_runs/201.00_base_files_1July_ToSTAR_FixedGear")

sab2019$cpue %>%
  filter(Fleet_name != 'ENV') %>%
  select(Fleet_name, Yr, Obs, SE) %>%
  write.csv(.,"C:/Users/mkapur/Dropbox/UW/sab-idx/data/sab2019update_vast.csv", row.names = F)
