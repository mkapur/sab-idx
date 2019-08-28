## Load libraries
library(RandomFields)
library(tidyr)
library(magrittr)
library(ggplot2)
install.packages("FishStatsUtils")
library(FishStatsUtils)

## Demonstrate SFA with a simple simulation
set.seed(254)
n_s <- 1000 # n sites
n_f <- 3 # n factors
n_c <- 6 # n categories
Loc <- data.frame(x=runif(n_s), y=runif(n_s))

## Randomly generate the L matrix
L <- t(matrix(rnorm(n_f * n_c), nrow=n_f, ncol=n_c))
## Zero out the upper triangle
for (i in 1:n_f) L[seq(from=1, to=i-1, length=i-1),i] <- 0
L

## Simulate independent random fields (factors)
model_O <- RMgauss(var=1, scale=.25)
Omegainput <- matrix(NA, ncol=n_f, nrow=n_s)
for (i in 1:n_f)
  Omegainput[, i] <- RFsimulate(model=model_O, x=Loc$x, y=Loc$y)@data[,1]
## Quick plot of spatial fields
data.frame(Loc, Omegainput) %>% gather( factor, value, -x, -y) %>%
  ggplot(aes(x, y, size=abs(value), color=value>0)) + geom_point(pch=1) + facet_wrap('factor')

## Now multiply by L to get the categories
mu <-  t(L %*% t(Omegainput))
data.frame(Loc, mu) %>% gather( factor, value, -x, -y) %>%
  ggplot(aes(x, y, size=abs(value), color=value>0)) + geom_point(pch=1) + facet_wrap('factor')

## Recover correlation matrix from L
corr  <-  round(cov2cor(L %*% t(L)),2)
corr
pairs(mu)

## VAST rotates the L matrix and Omegas to be more interpretable, using PCA
## or Varimax.
par(mfrow=c(1,3))
for(i in 1:3) plot_loadings(L, i)
Omega_sft <- array(NA, dim=c(nrow(Omegainput), ncol(Omegainput), 1))
Omega_sft[,,1] <- Omegainput
rotated <- FishStatsUtils::rotate_factors(L_pj=L, Psi=Omega_sft)
## This is the rotate factors
data.frame(Loc, rotated$Psi_rot[,,1]) %>% gather( factor, value, -x, -y) %>%
  ggplot(aes(x, y, size=abs(value), color=value>0)) + geom_point(pch=1) + facet_wrap('factor')

