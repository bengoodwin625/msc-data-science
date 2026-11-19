load("~/Downloads/datasets.RData")

library(R2jags); library(MCMCvis); library(coda); library(lattice)

jags.mod <- function(){
  # likelihood
  for(i in 1:N){
    r[i] ~ dbin(p[i],n[i])
    p[i] <- 1/(1+exp(-(alpha + beta*(x[i]-mean(x)))))
  }
  # priors
  alpha ~ dnorm(0,1/10000)
  beta ~ dnorm(0,1/10000)
}

# data
jags.data <- list(x,r,n,N)
print(jags.data)

# parameters to monitor
jags.param <- c("alpha","beta")

# initial values for 2 chains
inits1 <- list(alpha=-5, beta=10)
inits2 <- list(alpha=5, beta=-10)
jags.inits <- list(inits1,inits2)

# fit model
jags.mod.fit <- jags(data = jags.data, inits = jags.inits,
                     parameters.to.save = jags.param,
                     n.chain=2, n.burnin=0, n.iter = 10000,
                     model.file = jags.mod)

