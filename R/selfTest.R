## Self Test per https://github.com/James-Thorson-NOAA/VAST/wiki/Simulator
## Needs to be >= V4.4
## Using run from 23 Jan 2020



## needs to be a fresh run; got singularity issues on updated versions
source("./R/draft3_v3.R")


Sim <- Obj$simulate( complete=TRUE )

