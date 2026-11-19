## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment=NA, collapse=T, autodep=T, 
                      fig.align='center',
                      dev.args=list(pointsize=9), size ='tiny',
                      fig.width=4, fig.height=2.5,
                      message=F, warning=F, autodep=T,
                      dev="cairo_pdf")
knitr::knit_hooks$set(purl = knitr::hook_purl)

## -----------------------------------------------------------------------------
# Loading datasets required

load("~/Downloads/datasets.RData")



# SESSION 1:



## ---- fig.width=4, fig.height=3-----------------------------------------------
# x values 
x <- seq(-5, 5, by = 0.1)

# Getting probability density function
y <- dnorm(x, mean = 0, sd = 1)

# Plotting normal distrbution 
plot(x, y, type = 'l', xlab = 'x', ylab = 'Density', lwd = 2)

## ---- fig.width=6, fig.height=3-----------------------------------------------
# 1x2 plot
par(mfrow=c(1,2))

# x values 
x <- seq(-10, 10, by = 0.1)

# Getting probability density function
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 4, sd = 1)
y3 <- dnorm(x, mean = -4, sd = 1)

# Plotting Normal distrbution 
plot(x, y1, type = 'l', xlab = 'x', ylab = 'Density', lwd = 2)
lines(x, y2, col = 'blue', lwd = 2)
lines(x, y3, col = 'red', lwd = 2)

# Getting probability density function
y1 <- dnorm(x, mean = -2, sd = 1)
y2 <- dnorm(x, mean = 0, sd = 2)
y3 <- dnorm(x, mean = 2, sd = 3)

# Plotting normal distrbution 
plot(x, y1, type = 'l', xlab = 'x', ylab = 'Density', lwd = 2)
lines(x, y2, col = 'blue', lwd = 2)
lines(x, y3, col = 'red', lwd = 2)


## ---- echo=FALSE, fig.height=3.5, fig.width=4.5-------------------------------
set.seed(10)
par(mfrow = c(2, 3), mgp = c(2, 1, 0), mar = rep(2, 4))
# Gamma pdf
x <- seq(0.01,20,by=0.01)
y <- dgamma(x,2.5,0.8)
plot(x, y, type="l",col = "steelblue",xlim=range(x),main = "Gamma",xlab = "Y",lwd=2)
# Exonential pdf
y <- dexp(x, 3)
plot(x, y, type="l",col = "steelblue",xlim=range(x),main = "Exponential",xlab = "Y",lwd=2)
# Poisson pmf
x <- 0:50
y <- dpois(x, lambda = 10)
plot(x, y, type="h",col = "steelblue",xlim=range(x),main = "Poisson",xlab = "Y",lwd=2)
# Negative Binomial pmf
y <- dnbinom(x, mu=10, size=2)
plot(x, y, type="h",col = "steelblue",xlim=range(x),main = "Negative Binomial",xlab = "Y",lwd=2)
# Bernoulli pmf
x <- 0:1
y <- dbinom(x,size=1,prob=0.3)
plot(x, y, type="h",col = "steelblue",xlim=range(x),main = "Bernoulli",xlab = "Y",lwd=2,ylim=c(0,1))
# Binomial pmf
x <- 0:10
y <- dbinom(x,size=10,prob=0.3)
plot(x, y, type="h",col = "steelblue",xlim=range(x),main = "Binomial",xlab = "Y",lwd=2)

## -----------------------------------------------------------------------------
# Number of rows
nrow(fish)

# Summary
summary(fish)

# Top 12 rows
head(fish, 12)

## -----------------------------------------------------------------------------
# Top 12 rows
head(fish, 12)

## -----------------------------------------------------------------------------
# Getting proportions
fish$prop <- fish$escape/fish$total

# Plotting proportion as a function of length
plot(fish$length, fish$prop, pch = 20, xlab = 'Length (in cm)', ylab = 'Proportion escaping')

## -----------------------------------------------------------------------------
# Model the proportions
model <- lm(prop ~ length, data = fish)

# Plot proportion as a function of length
plot(fish$length, fish$prop, pch = 20, xlab = 'Length (in cm)', 
     ylab = 'Proportion escaping',ylim=c(-0.5,1))

# Plot the fitted line
lines(fish$length, model$fitted.values, lwd = 2)

## -----------------------------------------------------------------------------
# Generate 1000 values between 0 and 1 uniformly
z <- seq(0,1,length=500) 
z[1:10]

# Define the logit as an R function
logit <- function(z){ log(z/(1-z)) } 

# Plot the logit
plot(logit(z),z,type="l",lwd=3) 

## -----------------------------------------------------------------------------
# Transform the proportions using logit (with adjustment for 0's and 1's)
fish$logitZ <- logit((fish$escape+0.5)/(fish$total+1))

# Fit simple linear model and plot the mean
fish.lm <- lm(logitZ ~ length, data = fish)

# Plot the data 
plot(fish$length,fish$logitZ, pch=20,
     ylab = "Adjusted propotion",
     xlab = "Length (in cm)")

# Add the fitted model
xx <- seq(20, 90, len=100) 
lines(xx, predict(fish.lm, newdata = data.frame(length = xx)),lwd = 3)

## -----------------------------------------------------------------------------
# Fit linear model with quadratic relationship
fish$length2 <- fish$length^2
fish.lm2 <- lm(logitZ~length+length2,data=fish)

# Plot the data 
plot(fish$length,fish$logitZ, pch=20,
     ylab = "Adjusted propotion",
     xlab = "Length (in cm)")

# Add the fitted mean
xx <- seq(20, 90, len=100) 
lines(xx, predict(fish.lm, newdata = data.frame(length = xx)),lwd = 3)
lines(xx,predict(fish.lm2, newdata = data.frame(length = xx, length2 = xx^2)), 
      lwd = 3, col = "red")

## -----------------------------------------------------------------------------
# Fit linear model with cubic relationship
fish$length3 <- fish$length^3
fish.lm3 <- lm(logitZ~length+length2+length3,data=fish)

# Fit linear model with quartic relationship
fish$length4 <- fish$length^4
fish.lm4 <- lm(logitZ~length+length2+length3+length4,data=fish)

# Plot the data 
plot(fish$length,fish$logitZ, pch=20,
     ylab = "Adjusted propotion",
     xlab = "Length (in cm)")

# Add the fitted means
xx <- seq(20, 90, len=100) 
lines(xx, predict(fish.lm, newdata = data.frame(length = xx)),lwd = 3)
lines(xx,predict(fish.lm2, newdata = data.frame(length = xx, length2 = xx^2)), 
      lwd = 3, col = "red")
lines(xx, predict(fish.lm3, newdata = data.frame(length = xx, length2 = xx^2, 
                                                 length3 = xx^3)), lwd = 3, col = "blue")
lines(xx, predict(fish.lm4, newdata = data.frame(length = xx,length2 = xx^2, 
                                                 length3 = xx^3, length4 = xx^4)), lwd = 3, col = "green")

## -----------------------------------------------------------------------------
N <- 10
pi <- 0.7
plot(0:10,dbinom(0:10,N,pi),type="h",lwd=3,xlab="y",ylab="p(y;N,pi)")





# SESSION 2:







## ----eval = FALSE-------------------------------------------------------------
#  # Install required packages
#  install.packages("plot3D")
#  
#  # Load required packages into the library
#  library(plot3D)

## ----echo = FALSE-------------------------------------------------------------
# Load required packages into the library
library(plot3D)

## -----------------------------------------------------------------------------
# First 12 rows 
head(fish, 12)

## -----------------------------------------------------------------------------
# Getting proportions
fish$prop <- fish$escape/fish$total

# Plotting proportion as a function of length
plot(fish$length, fish$prop, pch = 20, xlab = 'Length (in cm)', ylab = 'Proportion escaping')

## ----fig.height=3-------------------------------------------------------------
# Inverse logit function with covariates
inv_logit <- function(x,b0,b1){ exp(b0+b1*x)/(1+exp(b0+b1*x)) }

# Sequences of 
x <- seq(20,90,len=200)

# Plotting output of inverse logit function for different betas
plot(x,inv_logit(x, b0 = 20, b1 = -0.6), type = "l",lwd = 2, 
     ylim = c(0, 1), ylab = "inv_logit(b0+b1x)")
lines(x,inv_logit(x, b0 = 1, b1 = 0), lwd = 2,col = "red")
lines(x,inv_logit(x, b0 = -20, b1 = 0.35), lwd = 2, col = "blue")

## ----fig.width=4, fig.height=4------------------------------------------------
fish.likelihood <- function (like_look = c(15, 25, -1, -0.4)){
  ## Create a function to evaluate minus the log-likelihood
  likelihood <- function(beta){
    result <- sum(lchoose(fish$total, fish$escape)) + sum(fish$escape * 
                                                            (beta[1] + beta[2] * fish$length)) - sum(fish$total * 
                                                                                                       log(1 + exp(beta[1] + beta[2] * fish$length)))
    return(result)
  }
  ## Plot the likelihood surface
  # Create sequences of length 25 for beta0 and beta1 values
  beta0grid <- seq(like_look[1], like_look[2], len = 25)
  beta1grid <- seq(like_look[3], like_look[4], len = 25)
  # Create a grid from those sequences (grid size is 25x25=625)
  grid <- expand.grid(beta0grid, beta1grid)
  # Create empty vector to store the value of the log-likelihood 
  # at each grid point 
  like <- rep(0, 625)
  ## For loop to calculate the likelihood at each gridpoint
  for (i in 1:625){
    like[i] = likelihood(as.numeric(grid[i, ]))
  }
  ## Put the likelihood values into a matrix to use with the persp3D command
  like <- matrix(like, 25, 25)
  ## Plot the 3D surface
  persp3D(beta0grid, beta1grid, like, theta = 240, phi = 20,
          ticktype = "detailed", xlab = "beta0", ylab = "beta1", 
          zlab = "log likelihood", expand = 2/3, shade = 0.5)	
}
# run the function
fish.likelihood()

## -----------------------------------------------------------------------------
# define the logit as an R function
logit <- function(z){ log(z/(1-z)) } 

# Create new transformed proportion variable as per lecture slides
fish$logitZ <- logit((fish$escape+0.5)/(fish$total+1))

# Plot the transformed proportion variable against fish length
plot(fish$length,fish$logitZ,xlab="fish length",
     ylab="logit(prop escaping)",pch=20)

# Fit a linear model
fishmodel_lm <- lm(logitZ~length+I(length^2)+I(length^3),data=fish)
summary(fishmodel_lm)

# Add the estimated line to the existing scatterplot
xx <- seq(20,90,len=200)
preds <- predict(fishmodel_lm,newdata=data.frame(length=xx),interval="confidence")
lines(xx,preds[,"fit"],col="blue",lwd=2)
lines(xx,preds[,"upr"],col="blue",lwd=2,lty=2)
lines(xx,preds[,"lwr"],col="blue",lwd=2,lty=2)

## -----------------------------------------------------------------------------
# Create prop. vs length plot again
plot(fish$length,fish$escape/fish$total,xlab="fish length", 
     ylab="prop escaping",pch=20)

# Add the fitted proportions from the linear model, 
# with 95% CIs calculated by transforming the end points 
# of the CI at the logit scale.
inv_logit2 <- function(x){
  exp(x)/(1+exp(x))
}
lines(xx,inv_logit2(preds[,"fit"]),lwd=2,col="red")
lines(xx,inv_logit2(preds[,"lwr"]),lwd=2,col="red",lty=2)
lines(xx,inv_logit2(preds[,"upr"]),lwd=2,col="red",lty=2)

## -----------------------------------------------------------------------------
# negative log-likelihood function
# beta is a vector of coefficients
likelihood <- function(beta){
  # Log-likelihood
  result <- sum(lchoose(fish$total, fish$escape)) + 
    sum(fish$escape * (beta[1] + beta[2] * fish$length)) - 
    sum(fish$total * log(1 + exp(beta[1] + beta[2] * fish$length)))
  # Return negative log-likelihood
  return(-result)
}

## -----------------------------------------------------------------------------
## Involves some trial and error of course
beta_init <-  c(15, -0.5) # I started up using c(0, 1) 

## Calculate probabilities (proportions) using those starting values
yfit <- (exp(beta_init[1] + beta_init[2] * fish$length))/
  (1 + exp(beta_init[1] + beta_init[2] * fish$length))

# Plot the probability curve 
plot(fish$length,fish$escape/fish$total,xlab="fish length", 
     ylab="prop escaping",pch=20)
lines(sort(fish$length), yfit[order(fish$length)],lwd=2,lty=2,col="grey50")

## ----warning=FALSE------------------------------------------------------------
## Minimize minus the log-likelihood
out <- nlm(likelihood, # Our likelihood function
           p = beta_init, # Our initial values
           hessian = T, # We want to compute the hessian
           gradtol = 1e-10, # Tolerance for converging to the maximum
           iterlim = 1000) # Will only consider 1000 iterations in our algorithm

## -----------------------------------------------------------------------------
## Print out estimates of beta0 and beta1
betas <- out$estimate
round(betas,3) # beta0 and beta1 estimates

## Print out the log-likelihood at maximum
round(-out$minimum,3) # log likelihood at maximum

## -----------------------------------------------------------------------------
# Plot the data
plot(fish$length,fish$escape/fish$total,xlab="fish length", 
     ylab="prop escaping",pch=20)

## Add estimated probabilities to the plot
yfit <- inv_logit(fish$length,betas[1],betas[2])
lines(sort(fish$length), yfit[order(fish$length)],lwd=2,col="blue")

