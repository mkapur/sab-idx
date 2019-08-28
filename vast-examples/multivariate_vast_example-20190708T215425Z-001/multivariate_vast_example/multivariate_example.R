library(TMB)
library(VAST)
library(ggplot2)
library(magrittr)
library(dplyr)

### load data set and subset down
example <- load_example( data_set="five_species_ordination" )
dat <- data.frame(example$sampling_data) %>% filter(Year %in% 2013:2015)
example$sampling_data <- dat
Lat_i <- dat$Lat
Lon_i <- dat$Lon
t_i <- dat$Year
c_i <- as.numeric(dat$species_number)-1
b_i <- dat$Catch_KG
a_i <- dat$AreaSwept_km2
ggplot(dat, aes(Lon, Lat, color=log(Catch_KG))) + geom_point() +
  facet_grid(species_number~Year) +scale_color_distiller(palette='Spectral') + theme_bw()


### Make defaults settings
settings <- make_settings( n_x=30, Region=example$Region, purpose="ordination",
  strata.limits=example$strata.limits, n_categories=2 )
## Change default settings. Turn off the spatiotemporal and beta factor
## analysis components (so it runs faster)
settings$FieldConfig[1,] <- 2 # spatial (omega)
settings$FieldConfig[2,] <- 0 # spatiotemporal (epsilon)
settings$FieldConfig[3,] <- 'IID' # intercepts (betas)
## Turn off temporal smoothers
settings$RhoConfig[c('Beta1','Beta2')]  <- c(0,3)
settings$RhoConfig[c('Epsilon1','Epsilon2')]  <-  0

### Run model w/ two factors
fit2 <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i)
## Plot results in separate folder
savedir <- 'sfa2/'; dir.create(savedir)
results <- plot_results(settings=settings, fit=fit2, working_dir=savedir )

### Try with 3 and 4
settings$FieldConfig[1,] <- 3 # full rank L for Omegas
fit3 <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i)
savedir <- 'sfa3/'; dir.create(savedir)
results <- plot_results(settings=settings, fit=fit3, working_dir=savedir )
settings$FieldConfig[1,] <- 4 # full rank L for Omegas
fit4 <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i)
savedir <- 'sfa4/'; dir.create(savedir)
results <- plot_results(settings=settings, fit=fit4, working_dir=savedir )

### and can mix factors for the two LP, note 5 is full rank
settings$FieldConfig[1,] <- c(5,3)
fit5 <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i)
savedir <- 'sfa5/'; dir.create(savedir)
results <- plot_results(settings=settings, fit=fit5, working_dir=savedir )

### Can also set them independent
settings$FieldConfig[1,] <- 'IID'
fitIID <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i)
savedir <- 'sfaIID/'; dir.create(savedir)
results <- plot_results(settings=settings, fit=fitIID, working_dir=savedir )

### Can also set them independent by reworking the Map and Initial values,
### setting off diagonals of the L matrix to zero and not estimating them.
settings$FieldConfig[1,] <- 5
fit0 <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i, run_model=FALSE)
## Turn off all but diagonal elements and set to 0
Pars <- fit0$tmb_list$Parameters
Pars$L_omega1_z[-c(1,3,6,10,15)] <- 0
Pars$L_omega2_z[-c(1,3,6,10,15)] <- 0
Map <- fit0$tmb_list$Map
L_off <- rep(NA, len=15)
L_off[c(1,3,6,10,15)] <- 1:5
Map$L_omega1_z <- Map$L_omega2_z <- factor(L_off)
fit0 <- fit_model(settings=settings, Lat_i=Lat_i, Lon_i=Lon_i, t_i=t_i,
                  c_i=c_i, b_i=b_i, a_i=a_i,
                  model_args=list(Map=Map, Parameters=Pars))
savedir <- 'sfa0/'; dir.create(savedir)
results <- plot_results(settings=settings, fit=fitIID, working_dir=savedir )

fit0$ParHat$L_omega1
fitIID$ParHat$L_omega1_z

### Check AIC
aics <- c(fit0$parameter_estimates$AIC, fitIID$parameter_estimates$AIC,
          fit2$parameter_estimates$AIC, fit3$parameter_estimates$AIC,
          fit4$parameter_estimates$AIC, fit5$parameter_estimates$AIC)
aics-min(aics)



### Look at the L matrices and corresponding correlations matrices
## Take L vector and create correlation matrix, modified from
## https://github.com/James-Thorson-NOAA/VAST/blob/master/inst/executables/VAST_v8_0_0.cpp#L69
L_to_cor <- function(Lvec, ncol, nrow=5){
  L <- matrix(0, nrow, ncol)
  counter <- 1
  for(r in 1:nrow){
    for(c in 1:ncol){
      if(r>=c){
        L[r,c] <- Lvec[counter]
        counter <- counter+1
      }
    }
  }
 cov2cor(L %*% t(L))
}

fit2$Report$lowercov_uppercor_omega1
L_to_cor(fit2$ParHat$L_omega1, ncol=2)

fit4$Report$lowercov_uppercor_omega1
L_to_cor(fit4$ParHat$L_omega1, ncol=4)

fit5$Report$lowercov_uppercor_omega1
L_to_cor(fit5$ParHat$L_omega1, ncol=5)
