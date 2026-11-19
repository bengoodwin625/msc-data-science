# Loading datasets required
library(readr)
load("~/Downloads/datasets.RData")
datasets <- read_csv("~/Downloads/datasets.RData")
View(datasets)

#-------2-------
# A)
#Fit LM
# Fit the model
model1 <- glm(cbind(escape, total - escape) ~ length, family = binomial(link = "logit"), data = fish)
summary(model1)

# B)
# Estimating 95% CIs
confint(model1, level = 0.95)

# C)
# Extracting deviance
model1$deviance
deviance_original <- model1$deviance

df <- model1$df.residual

p_value <- 1 - pchisq(deviance_original, df)
print(p_value)


# D)

model2 <- glm(cbind(escape, total - escape) ~ length + I(length^2), family = binomial, data = fish)
summary(model2)

LRT_statistic <- model1$deviance - model2$deviance
p_value_quad <- 1 - pchisq(LRT_statistic, df = 1)  # df = 1 for one additional parameter
print(p_value_quad)


#------------3-----------
# A)
print(aidsbelgium)

plot(aidsbelgium$t, aidsbelgium$y, type = "b", col = "blue", 
     xlab = "Year", ylab = "Number of AIDS Cases", main = "AIDS Cases in Belgium (1981-1993)")

# Fit a Poisson regression model
aids_model <- glm(y ~ t, family = poisso(link="log"), data = aidsbelgium)
summary(aids_model)




