library(readr)
tb <- load("~/Downloads/datasets_project.RData")

TBdata



# Loading packages
library(fields)
library(maps)
library(sp)

# PLotting map of cases
plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB counts for 2014")


library(mgcv)
model_poisson <- gam(TB ~ Indigenous + Illiteracy + Urbanisation + Density + 
                       Poverty + Poor_Sanitation + Unemployment + Timeliness +
                       s(Year, k = 3) + s(lon, lat, k = 50),
                     family = poisson(link = "log"),
                     data = TBdata)

summary(model_poisson)

library(MASS)
model_nb <- gam(TB ~ Indigenous + Illiteracy + Urbanisation + Density + 
                  Poverty + Poor_Sanitation + Unemployment + Timeliness +
                  s(Year, k = 3) + s(lon, lat, k = 50), 
                family = nb(), 
                data = TBdata)

summary(model_nb)

AIC(model_poisson, model_nb)

gam.check(model_poisson)
gam.check(model_nb)


TBdata$tb_rate <- TBdata$TB / TBdata$Population
TBdata

predicted_rate <- predict(model, type = "response")
plot.map(predicted_rate, n.levels = 7, main = "Predicted TB Risk Across Brazil")

