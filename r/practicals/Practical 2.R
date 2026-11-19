# Loading datasets required
library(readr)
load("~/Downloads/datasets.RData")
datasets <- read_csv("~/Downloads/datasets.RData")
View(datasets)

library(MASS)

#---------------a----------------
# Estimating the beta parameters standard error


# Original Model
likelihood <- function(beta){ 
  # Log-likelihood
  result <- sum(lchoose(fish$total, fish$escape)) + 
    sum(fish$escape * (beta[1] + beta[2] * fish$length)) - 
    sum(fish$total * log(1 + exp(beta[1] + beta[2] * fish$length)))
  # Returning negative log-likelihood
  return(-result)
}



## Initial values
beta_init <- c(15,-0.5) # I started up using c(0, 1)
## Minimize minus the log-likelihood
out <- nlm(likelihood, # Our likeihood function
           p = beta_init, # Our initial values
           hessian = T, # We want to compute the hessian
           gradtol = 1e-10, # Tolerance for converging to the maximum
           iterlim = 1000) # Will only consider 1000 iterations in our algorithm
## Print out estimates of beta0 and beta1
betas <- out$estimate
# Plot the data
plot(fish$length,fish$escape/fish$total,xlab="fish length", ylab="prop escaping",pch=19)
# Inverse logit function with covariates
inv_logit <- function(x,b0,b1){ exp(b0+b1*x)/(1+exp(b0+b1*x)) }
## Add estimated probabilities to the plot
# grid of length values
range(fish$length)
lengthGrid <- 21:87
yfit <- inv_logit(lengthGrid,betas[1],betas[2])
lines(lengthGrid, yfit,lwd=2,col="blue")



# Modified Model
likelihood2 <- function(beta){
  # Log-likelihood
  result <- sum(lchoose(fish$total, fish$escape)) + 
    sum(fish$escape * (beta[1] + beta[2] * fish$length + beta[3] * fish$length^2)) - 
    sum(fish$total * log(1 + exp(beta[1] + beta[2] * fish$length + beta[3] * fish$length^2)))
  # Returning negative log-likelihood
  return(-result)
}





## Initial values
beta_init <- c(15,-0.5,0) # I started up using c(0, 1)
## Minimize minus the log-likelihood
out2 <- nlm(likelihood2, # Our likeihood function
           p = beta_init, # Our initial values
           hessian = T, # We want to compute the hessian
           gradtol = 1e-10, # Tolerance for converging to the maximum
           iterlim = 1000) # Will only consider 1000 iterations in our algorithm
## Print out estimates of beta0 and beta1
betas2 <- out2$estimate
# Plot the data
plot(fish$length, fish$escape / fish$total, 
     xlab = "fish length", 
     ylab = "prop escaping", 
     pch = 19)

# Inverse logit function with covariates (modified for the quadratic term)
inv_logit <- function(x, b0, b1, b2) {
  exp(b0 + b1 * x + b2 * x^2) / (1 + exp(b0 + b1 * x + b2 * x^2))
}

# Add estimated probabilities to the plot
lengthGrid <- seq(min(fish$length), max(fish$length), length.out = 100)
yfit <- inv_logit(lengthGrid, betas[1], betas[2], betas[3])
lines(lengthGrid, yfit, lwd = 2, col = "blue")




out$hessian

OIM <- solve(out$hessian) ## observed information matrix
OIM

VarianceBeta <- diag(OIM)
VarianceBeta

stand_errors <- sqrt(VarianceBeta)
stand_errors

hessian_matrix <- out2$hessian

if(det(hessian_matrix) > 0) {
  OIM2 <- solve(hessian_matrix)
}else{
  OIM2 <- ginv(hessian_matrix)
}

VarianceBeta2 <- diag(OIM2)
stand_errors2 <- sqrt(VarianceBeta2)

print(stand_errors2)

#------------b-------------
# Estimating a 95% confidence interval
betas <- out$estimate
betas2 <- out2$estimate

# Put in a vector for b0
c(betas[1]- 1.96 * stand_errors[1],
  betas[1] + 1.96 * stand_errors[1])

c(betas2[1]- 1.96 * stand_errors2[1],
  betas2[1] + 1.96 * stand_errors2[1])

# Put in a vector for b1
c(betas[2]- 1.96 * stand_errors[2],
  betas[2] + 1.96 * stand_errors[2])

c(betas2[2]- 1.96 * stand_errors2[2],
  betas2[2] + 1.96 * stand_errors2[2])

# Vector for b2
c(betas2[3]- 1.96 * stand_errors2[3],
  betas2[3] + 1.96 * stand_errors2[3])


#-----------c----------
# Testing the hypothesis that b2=0 and the 5% level

modified_model <- glm(cbind(escape, total - escape) ~ length + I(length^2), family = binomial, data = fish)
beta2 <- summary(modified_model)$coefficients["I(length^2)", "Estimate"]
se_beta2 <- summary(modified_model)$coefficients["I(length^2)", "Std. Error"]

W <- (beta2 / se_beta2)^2
p_value <- pchisq(W, df = 1, lower.tail = FALSE)
p_value

# Since the p value is less than 0.05 the null hypothesis is rejected

#--------------d----------------
# Computing the AIC to find out which model is preferred
# The lower AIC is the preferred model 

original_model <- glm(cbind(escape, total - escape) ~ length, family = binomial, data = fish)
modified_model <- glm(cbind(escape, total - escape) ~ length + I(length^2), family = binomial, data = fish)

aic_original <- AIC(original_model)
aic_modified <- AIC(modified_model)

# Print the AIC values
print(aic_original)
print(aic_modified)

# Therefore the original model is preferred





