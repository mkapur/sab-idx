# remotes::install_github("nwfsc-assess/VASTWestCoast")
library(VASTWestCoast)
library(here)
VAST_spp(dir = getwd(), species = "Triennial_Anoplopoma_fimbria")


# settings <- get_settings(species = "Triennial_Anoplopoma_fimbria")
# surveyspp <- get_spp(settings$Species)
# Database <- get_data(survey = surveyspp["survey"], species = surveyspp["species"])
# check <- VAST_do(Database = Database, settings = settings,
#                  conditiondir = getwd(), compiledir = getwd())
 
Sim_Settings <- list(
  "Species" = "Triennial_Anoplopoma_fimbria",
  # lognormal
  "ObsModelcondition" = c(1, 0),
  # gamma
  # "ObsModelcondition" = c(2, 0),
  # Poisson link
  # "ObsModelcondition" = c(2, 1),
  "nknots" = 250,
  overdispersion = NULL,
  "strata" = data.frame("STRATA" = c("coast", "north", "south"),
                        'north_border' = c(49, 49, 36),
                        'south_border' =   c(32, 36, 32),
                        'deep_border' = c(1280, 1280, 1280),
                        'shallow_border'=c(55, 55, 55)),
  "depth" = c("no", "linear", "squared")[1],
  "Passcondition" = FALSE)
downloaddir <- here('runs','tri_vast/')

Sim_Settings$survey = "Triennial"

test <- VAST_condition(
  conditiondir = downloaddir,
  settings = Sim_Settings, 
  spp = Sim_Settings$Species,
  sensitivity =T  ## so it runs later years as well.
  # datadir = downloaddir,
)



settings <- get_settings(Sim_Settings)

surveyspp <- get_spp("Triennial_Anoplopoma_fimbria")
survey <- surveyspp["survey"]
Database <- get_data(survey = survey, species = surveyspp["species"])
Sim_Settings$Species <- 'Anoplopoma_fimbria'

check <- VAST_do(
  Database,
  # Database = Database[Database[, "Year"] >= 1994, ],
  conditiondir = paste(downloaddir, "late", sep = "_"),
  settings = Sim_Settings,
  compiledir = compiledir)

Database <- get_data(survey = 'Tri', species ="Anoplopoma_fimbria")
VAST_do(
  Database = Database[Database[, "Year"] >= 1994, ],
  conditiondir = paste(conditiondir, "late", sep = "_"),
  settings = settings,
  compiledir = compiledir)
# mapply(VAST_diagnostics, 
#        c(downloaddir, list.dirs(downloaddir)))