install.packages("tidyr")
library(tidyr)


install.packages("ggplot2")
install.packages("geometry")
library(ggplot2)
library(geometry)
p1 <- ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=5, colour='black', fill="lightgreen")

p1 <- p1 + geom_vline(xintercept=mean(mtcars$mpg),col='blue',linewidth=2)

p1


mtcars
mtcars$mpg
mean(mtcars$mpg)
