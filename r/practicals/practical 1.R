




## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment=NA, collapse=T, autodep=T, 
                      fig.align='center',
                      dev.args=list(pointsize=9), size ='tiny',
                      fig.width=4, fig.height=2.5,
                      message=F, warning=F, autodep=T,
                      dev="cairo_pdf")
knitr::knit_hooks$set(purl = knitr::hook_purl)
Sys.setenv(NVBLAS_CONFIG_FILE="/etc/nvblas.conf")

## -----------------------------------------------------------------------------
# Loading datasets required
# Loading datasets required
library(readr)
datasets <- read_csv("~/Downloads/datasets.RData")
View(datasets)

## -----------------------------------------------------------------------------
### Create a function to evaluate minus the log-likelihood 
likelihood <- function(beta){ 
  # Log-likelihood
  result <- sum(lchoose(fish$total, fish$escape)) + 
    sum(fish$escape * (beta[1] + beta[2] * fish$length)) - 
    sum(fish$total * log(1 + exp(beta[1] + beta[2] * fish$length)))
  # Returning negative log-likelihood
  return(-result)
}

## -----------------------------------------------------------------------------
# Function to evaluate the negative log-likelihood
# of the quadratic model
likelihood2 <- function(beta){
  # Log-likelihood
  result <- sum(lchoose(fish$total, fish$escape)) + 
    sum(fish$escape * (beta[1] + beta[2] * fish$length + beta[3] * fish$length^2)) - 
    sum(fish$total * log(1 + exp(beta[1] + beta[2] * fish$length + beta[3] * fish$length^2)))
  # Returning negative log-likelihood
  return(-result)
}

## -----------------------------------------------------------------------------
# Estimating the MLE
out2 <- nlm(likelihood2,
            p = c(1, 0, 0),
            hessian = T,
            iterlim = 10000,
            steptol = 1e-10)

# Reporting estimates
out2$estimate

## -----------------------------------------------------------------------------
# Estimating the MLE
out <- nlm(likelihood,
           p = c(15, -0.5),
           hessian = T,
           iterlim = 10000,
           steptol = 1e-10)

# Reporting estimates
out$estimate

## -----------------------------------------------------------------------------
# Extracting betas
betas_orig <- out$estimate
betas_mdfd <- out2$estimate

# Plot the data
plot(fish$length,fish$escape/fish$total,xlab="fish length", ylab="prop escaping",pch=19)

# Add estimated probabilities from original model to the plot
yfit <- (exp(betas_orig[1] + betas_orig[2] * fish$length))/(1 + exp(betas_orig[1] + betas_orig[2] * fish$length))
lines(sort(fish$length), yfit[order(fish$length)],lwd=2,col="blue")

# Add estimated probabilities from original model to the plot
yfit <- (exp(betas_mdfd[1] + betas_mdfd[2] * fish$length + betas_mdfd[3] * fish$length^2))/
  (1 + exp(betas_mdfd[1] + betas_mdfd[2] * fish$length + betas_mdfd[3] * fish$length^2))
lines(sort(fish$length), yfit[order(fish$length)],lwd=2,col="red")

## -----------------------------------------------------------------------------
likelihood <- function(theta){
  n <- nrow(sexr)
  result <- -n*lgamma(theta[3]) + sum( (theta[3]-1)*log(sexr$ratio) - 
                                         (sexr$ratio*theta[3])/(exp(theta[1]+theta[2]*sexr$tinmort)) - 
                                         theta[3]*(theta[1]+theta[2]*sexr$tinmort) + theta[3]*log(theta[3])  )
  -result
}

## -----------------------------------------------------------------------------
# Initial values
inits <- c(0.05,0,10)

# Plotting data
plot(sexr$tinmort,sexr$ratio,pch=20, xlab = 'Infant mortality', ylab = 'Ratio of births')

# PLotting initial starting guess
xx <- seq(0,0.3,len=200)
lines(xx,exp(inits[1]+inits[2]*xx),col="red", lwd = 2)

# Fitting MLE
out <- nlm(likelihood,
           p = inits,
           hessian = T,
           iterlim = 10000,
           steptol = 1e-10)

# Reporting estimates
round(out$estimate, 2) 

## -----------------------------------------------------------------------------
# Initial values
inits <- c(0.05,0,10)

# Plotting data
plot(sexr$tinmort,sexr$ratio,pch=20, xlab = 'Infant mortality', ylab = 'Ratio of births')

# Plotting initial starting guess
xx <- seq(0,0.3,len=200)
lines(xx, exp(out$estimate[1]+out$estimate[2]*xx),col="blue", lwd = 2)



  




