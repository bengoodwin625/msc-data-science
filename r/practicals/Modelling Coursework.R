# Loading datasets required
library(readr)
load("~/Downloads/datasets.RData")
datasets <- read_csv("~/Downloads/datasets.RData")
View(datasets)

#------Question 1------------
# Part a)
# -The model can't be fit as a linear regression model because a linear regression model assumes a linear relationship
#  between the independant and dependant variables, which shows a slight inverse log distribution

#Park b)

print(nlmodel)

plot(nlmodel$x, nlmodel$y, col = "black", 
     xlab = "x", ylab = "y", main = "nl model")

# Defining the log likelyhood

likelihood <- function(theta1, theta2, sigma2, y, x) {
  mu <- (theta1 * x) / (theta2 + x)  
  -sum(dnorm(y, mean = mu, sd = sqrt(sigma2), log = TRUE))  # Negative log-likelihood
}


# Part c)

# Define the mylike() function
mylike <- function(params, y, x) {
  # Extract the parameters
  theta1 <- params[1]
  theta2 <- params[2]
  sigma2 <- params[3]
  
  # Compute the mean
  mu <- (theta1 * x) / (theta2 + x)
  
  # Compute the negative log-likelihood
  nll <- -sum(dnorm(y, mean = mu, sd = sqrt(sigma2), log = TRUE))
  return(nll)
}
# Example data
x <- c(0.1, 0.2, 0.3, 0.4, 0.5)  # Example x values
y <- c(5.2, 7.8, 10.1, 9.5, 8.7) # Example y values

# Initial parameter guesses
initial_params <- c(theta1 = 10, theta2 = 0.5, sigma2 = 2)

# Compute the negative log-likelihood
nll_value <- mylike(initial_params, y, x)
nll_value




# Part (d): Numerical Minimization of the Log-Likelihood
# Load necessary library
library(ggplot2)

# Example dataset (replace with actual data)
set.seed(42)
x <- runif(100, 0, 1)          # Example x values
y <- 150 * x / (0.5 + x) + rnorm(100, sd = 10)  # Example y values

# Define the mylike() function
mylike <- function(params, y, x) {
  theta1 <- params[1]
  theta2 <- params[2]
  sigma2 <- params[3]
  mu <- (theta1 * x) / (theta2 + x)
  -sum(dnorm(y, mean = mu, sd = sqrt(sigma2), log = TRUE))
}

# Starting parameter guesses
start_params <- c(theta1 = 200, theta2 = 0.1, sigma2 = 50)

# Minimize the negative log-likelihood
result <- nlm(
  f = mylike,
  p = start_params,
  y = y,
  x = x
)

# Extract estimated parameters
theta1_hat <- result$estimate[1]
theta2_hat <- result$estimate[2]
sigma2_hat <- result$estimate[3]

# Print results
cat("Estimated Parameters:\n")
cat("Theta1:", theta1_hat, "\n")
cat("Theta2:", theta2_hat, "\n")
cat("Sigma^2:", sigma2_hat, "\n")

# Compute fitted values
predicted <- (theta1_hat * x) / (theta2_hat + x)

# Create a scatter plot with fitted curve
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_point(color = "black") +
  geom_line(aes(y = predicted), color = "blue", size = 1) +
  labs(
    title = "Scatter Plot with Fitted Mean Relationship",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

Sys.setenv(DISPLAY=":0")


print(nlmodel)













mylike <- function(params) {
  # Extract parameters
  theta1 <- params[1]
  theta2 <- params[2]
  sigma2 <- params[3]
  
  # Ensure sigma2 is positive to avoid log errors
  if (sigma2 <= 0) return(Inf)
  
  # Compute the predicted mean function
  mu <- (theta1 * nlmodel$x) / (theta2 + nlmodel$x)
  
  # Compute the negative log-likelihood
  result <- (-50 * log(2 * pi)) - 
    (50 * log(sigma2)) -
    (1 / (2 * sigma2)) * sum((nlmodel$y - mu)^2)
  
  return(-result)  # Ensure it returns a scalar
}
sigma_init <- sd(nlmodel$y)

print(sigma_init)

# Initial parameter values (as a vector)
param_init <- c(226, 0.09, 39.45189)

# Optimize using nlm()
out <- nlm(
  f = mylike,      # Function to minimize (no need for x, y here)
  p = param_init,  # Initial parameter values
  hessian = TRUE,
  gradtol = 1e-10,
  iterlim = 1000
)

# Display estimated parameters
out$estimate


# Invert the negated Hessian to obtain the Observed Information Matrix
OIM <- solve(out$hessian)

# The diagonal entries are the variances of theta1 and theta2 respectively so # obta
VarianceTheta <- diag(OIM)
VarianceTheta

# and then square root them to obtain standard errors
stand_error <- sqrt(VarianceTheta)
# reporting standard errors
stand_error


# Estimating CIs
CIs <- cbind(betas- 1.96 * stand_error, betas + 1.96 * stand_error)




print(aids)
