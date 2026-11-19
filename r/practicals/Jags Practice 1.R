library(R2jags); library(MCMCvis); library(coda); library(lattice)


# QUESTION 1


jags.mod.comp <- function(){
  for (i in 1:10) {
    RT[i] ~ dgamma(4, 0.4)}
  CS[1] <- RT[1]
  for (i in 2:10){
    CS[i] <- CS[i-1] + RT[i]
  }
  for (i in 1:10){
    DONE[i] <- ifelse(CS[i] <= 100, 1, 0)}
  FINISHED <- sum(DONE[])
}

jags.mod.fit.comp <- jags(data = list(), n.iter = 10000, DIC=FALSE,
                          parameters.to.save = c('FINISHED','SUCCESS', 'DONE'),
                          model.file = jags.mod.comp,n.chains=1)


print(jags.mod.fit.comp)

jagsfit.mcmc.comp <- as.mcmc(jags.mod.fit.comp)

MCMCtrace(jagsfit.mcmc.comp,
          params = 'DONE', # parameter of interest
          type = 'trace', # density plot
          ind = TRUE, # separate density lines for each chain 
          pdf = FALSE)

# QUESTION 3

library(readr)
load("~/Downloads/datasets.RData")
data <- read_csv("~/Downloads/datasets.RData")
View(data)


data(sunspot.year)
SunS <- data.frame(Year=1750:1849,X=sunspot.year[51:150])

#Precision is 0.001, the 0.001 = 1/SD^2, 0.001SD^2 = 1, SD^2 = 100
sigma <- 100
t <- 1:100
data_list <- list(
  X = SunS$X,
  n = nrow(SunS) # Total number of observations
)

#Priors - just noting them here
#c ~ dnorm(0,0.001)
#theta ~ dnorm(0,0.001)
#sigma ~ dunif(0,100)
#Xt ~ dnorm(Mt, sigma)
#Mt = c + θXt−1
#m1 ∼ dnorm(0,0.001)

###---------------------------------------------------------------------------------
#Creating Model
jags.mod.Suns <- function(){
  
  c ~ dnorm(0,0.001)
  theta ~ dnorm(0,0.001)
  sigma ~ dunif(0,100)
  p <- 1 / (sigma * sigma)
  #Likelihood for X_t
  for (i in 2:n){
    X[i] ~ dnorm(m[i], p)
    m[i] <- c + theta * X[i - 1]  # Mean based on AR(1) model
  }
  m[1] ~ dnorm(0,0.001)
}


init_values <- list(
  list(c = 0, theta = 0, sigma = 10),
  list(c = 0.1, theta = -0.1, sigma = 15),
  list(c = -0.1, theta = 0.1, sigma = 5)
)

#Monitor parameters
params <- c("c", "theta", "sigma", "m")

jags.mod.fit.Suns<-jags(data = data_list, inits = init_values, n.iter=10000, n.burnin=1000 ,DIC=FALSE,
                        parameters.to.save=params,
                        model.file=jags.mod.Suns,n.chains=3)
print(jags.mod.fit.Suns)
summary(jags.mod.fit.Suns)

jagsfit.mcmc.Suns <- as.mcmc(jags.mod.fit.Suns)
summary(jagsfit.mcmc.Suns)
traceplot(jagsfit.mcmc.Suns)
#--------------------------------------------------------------------------------------------------------------------------
#Used Chatgpt here
#Posterior Mean of m_t and 95% credibal intervals
#Extract posterior samples for m[1] to m[n]
m_samples <- jagsfit.mcmc.Suns[[1]][, grepl("^m\\[", colnames(jagsfit.mcmc.Suns[[1]]))]

#Check the structure of m_samples
head(m_samples)

#Calculate posterior means for each time point (m_t)
posterior_means <- apply(m_samples, 2, mean)

#Calculate 95% credible intervals for each time point (m_t)
credible_intervals <- apply(m_samples, 2, function(x) quantile(x, c(0.025, 0.975)))


#Create a data frame for the posterior means and credible intervals
results <- data.frame(
  Time = 1:length(posterior_means),  # Time index (1, 2, ..., n)
  PosteriorMean = posterior_means,
  CI_Lower = credible_intervals[1, ],
  CI_Upper = credible_intervals[2, ]
)

#Check the results data frame
head(results)

#-------------------------------------------------------------------------------------------------------------------

#Plot the results
library(ggplot2)

ggplot(results, aes(x = Time)) +
  geom_line(aes(y = PosteriorMean), color = "red", size = 1) +  # Posterior mean
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "gray", alpha = 0.5) +  # 95% CI
  geom_point(aes(x = 1:length(SunS$X), y = SunS$X), color = "blue", size = 2) +  # Original data points
  labs(title = "Sunspot Data and Posterior Estimates for m_t from Model",
       x = "Time", y = "Sunspot Number") +
  theme_minimal()

