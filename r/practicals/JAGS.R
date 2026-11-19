install.packages('R2jags', dependencies = TRUE) # installs coda too
install.packages ("rjags")
install.packages('coda', dependencies = TRUE)
install.packages('lattice',dependencies = TRUE)
install.packages('MCMCvis', dependencies = TRUE)

library(R2jags); library(MCMCvis); library(coda); library(lattice)

set.seed(45621)

# model definition
jags.mod.coin <- function(){
  Y ~ dbin(0.5,10) # our data model
  P8 <- ifelse(Y>7,1,0) # the probability of interest
}

jags.mod.fit.coin <- jags(data = list(), model.file = jags.mod.coin,
                          parameters.to.save = c('Y','P8'),n.chains=1,
                          DIC=FALSE, n.burnin=0,n.iter = 100)

print(jags.mod.fit.coin)
traceplot(jags.mod.fit.coin)



library(R2jags)
library(coda)
library(lattice)
library(MCMCvis)
library(tidyverse)


