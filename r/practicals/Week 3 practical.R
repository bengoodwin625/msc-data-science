require(Amelia) 
require(tidyverse) 
require(broom)
library(Amelia)

set.seed(1234)
x <- c(sample(1:10, 7, TRUE), rep(NA,3))
x

mean(x)

# na.rm removes the missing values from the sample
mean(x, na.rm = TRUE)

# Estimate for the sample error is the standard deviation diveded by the root of the sample size
sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))

# Impute, takes the elements of the x that are missing
# And adds 3 randomly chosen x values to replace the missing ones

imp <- replicate(5, c(x[!is.na(x)], sample(x[!is.na(x)], 3, TRUE)), simplify = FALSE)
imp

# This calculates q bar, by using sapply to take a mean of each list element of 'imp'
# and then taking the mean of those means

qbar <- mean(sapply(imp,mean))

# This calculates the within variance
within.var <- (1/5)*sum((sapply(imp,sd)/sqrt(10))^2)

# This calculates the between variance
between.var <- (1/4)*sum((sapply(imp,mean)-qbar)^2)

# This calculates T
bigt <- within.var + (1+(1/5))*between.var

# This calculates the standard error of T
sebigt <- sqrt(bigt)

qbar
sebigt

# An example of using lappy is to find the square of each element in a vector
lapply(1:3, function(x) x^2)

n = c(2,3,5)
s = c("aa","bb", "cc", "dd", "ee")
b = c(TRUE, FALSE, TRUE, FALSE, FALSE)
x = list(n, s, b, 3)

# We retrieve a list slice with the single square bracket [] operator 
x[2]

x[c(2, 4)]

# Creating a matrix of means
coefs.imp <- imp %>%
  sapply(mean) %>%
  matrix()

# Creating a matrix of standard errors
ses.imp <- imp %>%
  sapply(sd) %>%
  '/'(sqrt(10)) %>%
  matrix()
mi.meld(coefs.imp, ses.imp)

data(africa)
summary(africa)

# This fits a linear model were GDP is predicted using civil liverties and trade
regr_model <- lm(gdp_pc ~ civlib + trade, data = africa)
summary(regr_model)

# We will now use the amelia function to create 5 datasets in which the missing values are imputed

imp_amelia <- amelia(x = africa,
                     m = 5,
                     idvars = "country",
                     logs = "gdp_pc",
                     p2s = 1)

# We can now produce a figure showing the missing values
missmap(imp_amelia)

# Extrating the individual datasets:
head(imp_amelia$imputations$imp1)
tail(imp_amelia$imputations$imp1)

head(imp_amelia$imputations$imp2)
tail(imp_amelia$imputations$imp2)

# We could then run a regression model on each imputed dataset
regr_model <- lm(gdp_pc ~ civlib + trade, data=imp_amelia$imputations$imp1)
summary(regr_model)

# The lapply function applies a function over a list or vector rather than individually
lm.amelia_out <- lapply(imp_amelia$imputations, function(i) lm(gdp_pc ~ civlib + trade,
                                                               data = i))
lm.amelia_out

coefs.amelia <- lm.amelia_out %>%
  sapply(coef) %>%
  t()

# This creates a function to extract a standard error from a model
my_SE_extractor <- function(model) {summary(model)$coef[,"Std. Error"]}

ses.amelia <- lm.amelia_out %>%
  sapply(my_SE_extractor) %>%
  t()

# Now we are ready to use mi.meld
mi.meld(coefs.amelia, ses.amelia)

# The colMeans function takes the mean of columsn and returns a vector the same length as the 
# number of columns
coefmeans <- colMeans(coefs.amelia)

# Building the components for Rubin's rules

V <- colMeans(ses.amelia^2)
B <- ((1/(5-1))* colSums((sweep(coefs.amelia,2,coefmeans,"-"))^2))

# Calculating the total variance
T <- V + (1 + 1/5)*B

SEt <- sqrt(T)
SEt

# Comparing with mi.meld
mi.meld(coefs.amelia, ses.amelia)

# Tidyverse version
all_imputations <- bind_rows(unclass(imp_amelia$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

all_imputations

models_imputations <- all_imputations %>%
  mutate(model = data %>% map(~ lm(gdp_pc ~ trade + civlib, data=.)),
         tidied = model %>% map(~tidy(., conf.int = TRUE)),
         glance = model %>% map(~ glance(.)))
models_imputations

models_imputations %>%
  filter(m == "imp1") %>%
  unnest(tidied)

params <- models_imputations %>%
  unnest(tidied) %>%
  select(m, term, estimate, std.error) %>%
  gather(key, value, estimate, std.error) %>%
  spread(term, value)

params

just_coefs <- params %>%
  filter(key=="estimate") %>%
  ungroup %>%
  select(-m,-key)

just_coefs

# Now only extract the standard errors
just_ses <- params %>%
  filter(key == "std.error") %>%
  ungroup %>%
  select(-m, -key)

just_ses
coefs_melded <- mi.meld(just_coefs, just_ses)

model_degree_freedom <- models_imputations %>% unnest(glance) %>%
  filter(m == "imp1") %>%
  pull(df.residual)
melded_summary <- as.data.frame(cbind(t(coefs_melded$q.mi), t(coefs_melded$se.mi))) %>%
  magrittr::set_colnames(c("estimate", "std.error")) %>% mutate(term = rownames(.)) %>%
  select(term, everything()) %>%
  mutate(statistic = estimate / std.error,
         conf.low = estimate + std.error * qt(0.025, model_degree_freedom), conf.high = estimate + std.error * qt(0.975, model_degree_freedom),
         p.value = 2 * pt(abs(statistic), model_degree_freedom, lower.tail = FALSE))
melded_summary

coefs_melded
