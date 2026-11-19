install.packages("foreign")
library("foreign")

# Calculating 3+4
3+4

# Printing 'Hello!'
print("Hello!")

# Sum 1,2 and 3
sum(1,2,3)

# Absolute value of 2 and -2
abs(2); abs(-2)

# Assign the value of 42 to x
x <- 42

# Print out the value of the object x
x

# Add 2 to x
x+2

# Add 5 to x
x+5

# Add 5 to x and reassigning to x
y <- x+5

# Print out the value of the object x
y



# Storing 5 values in a vector and assigning to 'a'
a <- c(1,2,3,4,5)
# Storing 5 values in a vector and assigning to 'b'
b <-c(-2,-4,6,7,8)

# Extracting the first element of a
a[1]
# Extracting the first and third element of a
a[c(1,3)]

# Lets add a and b
a +b 

# Lets times a by 2 and add 5
2*a+5

# Storing 5 values in 'a' 
a <- c(1,2,3,4,5)
# Storing 5 values in 'b'
b <- c(-2,-4,6,7,6)

# Creating a datafram fr
mydata <- data.frame(a,b)

# printing mydata
mydata

# List all the datasets stored in R
data()

#Activity
 c <- c(2,3,1,6,-1)
 c
 activitydata <- data.frame(a,b,c)

 
# Viewing the structure of the drugs dataset
nrow(mydata)

# Viewing the first 5 rows of the drugs dataset
ncol(mydata)

# Display the variable names
names(mydata)

# Viewing the structure of the drugs dataset
str(mydata)

# Viewing the first 5 rows of the drugs dataset
head(mydata)

# Extracting the variable a from mydata
mydata[,'a']

# Extracting the row (or observation) from mydata
mydata[1,]

# Extracting the 3rd row for variable b from mydata
mydata[3,'b']

# Extracting the variable a from mydata
mydata$a

# Creating variable aplusb by adding a and b together
mydata$aplusb <- mydata$a + mydata$b
# Printing the variable aplusb
mydata$aplusb

# Checking 1==2
1==2
# Checking 1 ==1
1==1

# Subsetting mydata where 'a' is less than 4
subset(mydata, a<4)

mydata <- mtcars

# Lets view the first few riws
head(mydata)

# Drug costs in a vector
drugcost <- c(2.0,4.0,6.2,8.0,8.0,8.0,12.0,15.0,15.0,18.0,18.2,20.0,20.0,20.0,21.0,22.0, 24.2,27.0,27.0,27.0,28.0,29.7,29.7,29.7,29.7,29.7,30.0,30.0,31.0,41.4,42.3, 45.4,45.4,45.4,45.4,45.4,45.4,45.4,45.4,45.4,49.4,50.8,51.4,53.4,56.0,57.0, 57.4,59.0,59.4,60.0,61.5,65.4,65.4,65.4,65.4,67.0,90.8,92.0,94.0,94.0,94.0, 94.0,94.0,94.0,94.7,105.0,125.0,126.0,130.0,130.0,130.0,151.2,160.0,177.0, 187.0,187.0,194.4,194.4,194.4,212.4,213.0,233.0,267.0,320.4)

# Sample size
length(drugcost)

# Calculate the mean drug cost
mean(drugcost)

# Calculate the mean drug cost
sum(drugcost)/length(drugcost)

median(drugcost)
var(drugcost)

# Calculate the standard deviation of drug costs
sqrt(var(drugcost))
sd(drugcost)

# Minimum and maximum drug cost
min(drugcost)
max(drugcost)
# Range
max(drugcost) - min(drugcost)

# Lower and upper quartile of the drug costs
quantile(drugcost, probs = 0.25)
quantile(drugcost, probs = 0.75)
IQR(drugcost)

# Summarising the drug costs
summary(drugcost)

# Drug class in a vector
drugclass <- c("A","C","C","C","A","A","A","B","A","B","B","B","B","A","A","A","B", "A","A","A","C","A","B","B","C","A","C","A","C","A","B","C","C","B", "B","A","C","B","A","B","B","A","A","C","B","B","A","B","C","C","B", "A","C","B","C","A","B","C","C","C","A","A","A","C","B","A","A","B", "C","A","B","A","A","B","C","C","A","A","C","C","A","B","C","C")

# Calculating the number of each drug type
table(drugclass)

# Number of each drug type
prop.table(table(drugclass))
# Number of each drug type as a percentage
100*prop.table(table(drugclass))

# Catagorising the drugcost: (i) Low (1-50), (ii) Medium (50-100),
# (iii) High (100-200) and (iii) Very high (+201)
drugcostcat <- cut(drugcost,
                   breaks = c(0,50,100,200,max(drugcost)),
                   labels = c('Low','medium', 'High', 'Very High'))

# Calculating the number in each group
table(drugcostcat)
# Proportion of each drug type
prop.table(table(drugcostcat))
# Proportion of each type as a percentage
100*prop.table(table(drugcostcat))

# Call the mtcars dataset into R
data(mtcars)

# Viewing the structure of the dataset
str(mtcars)
# Viewing the first 6 rows of the dataset
head(mtcars)

# Extracting the number of cylinders
mtcars$cyl

# Calculating the mean of miles per gallon
mean(mtcars$mpg)
median(mtcars$mpg)
sd(mtcars$mpg)

# Proportion of automatics and manual cars as percentage
100*prop.table(table(mtcars$am))

# Calculating the mean miles per gallon
# for automatic and manual cars separatly
aggregate(mpg ~ am,
          data=mtcars,
          FUN = mean)
aggregate(mpg ~ am,
          data=mtcars,
          FUN = median)
if(mtcars$mpg[1]>20) "True"
ifelse(mtcars$mpg[1]>30, "True", "False")



# PRACTICAL 3
install.packages("tidyverse")
library(tidyverse)
?iris

filter(iris, Species == "versicolor")

# Sort the data by species (alphabetically) and then by Sepal Length (numerically)
arrange(iris, Species, Sepal.Length)

# Select just the Species, Sepal.Length and Sepal.Width columns from the data set
select(iris, Species, Sepal.Length, Sepal.Width)

# There is even a set of functions to help extract columns based on pattern matching
select(iris, Species, starts_with("Sepal"))

# Can remove columns by using a - operator
select(iris, -starts_with("petal"))
#or
select(iris, -Petal.Length, -Petal.Width)

# Add a new column
mutate(iris, Sepal.Length2 = Sepal.Length^2)

#Pipe operator, %>% allowed us to enable the chain functions together
iris %>% filter(Species == "versicolor") %>% head()

# Pipes cab also be chained together multiple times
iris %>%
  filter(Species == "versicolor") %>%
  select(Species, starts_with("Sepal")) %>%
  mutate(Sepal.Length2 = Sepal.Length^2) %>%
  arrange(Sepal.Length) %>%
  head()

#Grouping and Summarising
iris %>%
  group_by(Species) %>%
  summarise(mn = mean(Sepal.Length))

# Produce a table of estimates for the mean and variance of both Sepal lengths and widths, within each species
iris %>%
  group_by(Species) %>%
  summarise(MeanSepalL = mean(Sepal.Length), VarSepalL = var(Sepal.Length),
             MeanSepalW = mean(Sepal.Width), VarSepalW = var(Sepal.Width))


gp_income <- (indicator_gapminder_gdp_per_capita_ppp)
gp_income

# Rename the first column using the rename() function
gp_income <- gp_income %>%
  rename(country = "GDP per capita")
head(gp_income)

# Collapse all the columns except country
gp_income  <- gp_income %>%
  gather(key = year, value = gdp, -country)
gp_income

gp_income <- gp_income %>%
  mutate(year = as.numeric(year))
gp_income

sum(is.na(gp_income$country))

# Can examine these rows by filtering
gp_income %>% filter(is.na(country)) %>% summary()

# Removing
gp_income <- gp_income %>% filter(!is.na(country))

# Can also remove the rows that have no GDP information, denoted by NA
gp_income <- gp_income %>% filter(!is.na(gdp))

# Restricting ourselves to looking at the data from 1990 onwards
gp_income <- gp_income %>% filter(year > 1990)
head(gp_income)

summary(gp_income)

# To do all of the previous data cleaning operation using pipes, we can write
gp_income <- (indicator_gapminder_gdp_per_capita_ppp) %>%
  rename(country = "GDP per capita") %>%
  gather(year, gdp, -country) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(gdp)) %>%
  filter(year >1990)

# To produce a mean GDP for each country, grouping the data by country and then averaging the GDP values within each group
gp_income %>%
  group_by(country) %>%
  summarise(mn = mean(gdp))

# Produce the mean GDP for each year, averaged across countries
gp_income %>%
  group_by(year) %>%
  summarise(mn = mean(gdp))



# Creating beautiful tables in R
install.packages("gt", repos = "hyyp://cran.us.r-project.org")
library(gt)
library(dplyr)

# Relabel the 'am' variable to more meaningful labels
mtcars_relabelled <- mtcars %>%
  mutate(am = recode(am, '0' = "Automatic", '1' = "Manual"))
head(mtcars_relabelled)

# Creating summary statistics (mean, median and SD) by group (based on 'am' variable)
summary_stats <- mtcars_relabelled %>%
  group_by(am) %>%
  summarise(
    Mean_MPG = mean(mpg),
    Median_MPG = median(mpg),
    SD_MPG = sd(mpg),
    Mean_HP = mean(hp),
    Median_HP = median(hp),
    SD_HP = sd(hp),
    Mean_Disp = mean(disp),
    Median_Disp = median(disp),
    SD_Disp = sd(disp)
  )%>%
  ungroup()

summary_stats

# Creating the table using the gt library 
table <- summary_stats %>%
  gt() %>%
  # Rename the 'am' column for clarity
  cols_label(am="Transmission") %>%
  # Add title and subtitle
  tab_header(
    title = "Summary Statistics by Transmission Type",
    subtitle = "Comparision of MPG, Horsepower, and Displacement"
  ) %>%
  # Add sorce note
  tab_source_note(
    source_note = "Data: mtcars dataset from base R"
  ) %>%
  # Format columns to display statistics with two decimal points
  fmt_number(
    columns = 2:10,
    decimals = 1
  ) %>%
  # Modify the column labels for better presentation
  cols_label(
    Mean_MPG = "Mean MPG",
    Median_MPG = "Median MPG",
    SD_MPG = "SD MPG",
    Mean_HP = "Mean HP",
    Median_HP = "Median HP",
    SD_HP = "SD HP",
    Mean_Disp = "Mean Disp",
    Median_Disp = "Median Disp",
    SD_Disp = "SD Disp"
  ) %>%
  # Set custom column widths to reduce table width
  cols_width(
    am ~ px(100),
    everything() ~ px(60)
    
  )

table

# This highlights the Mean MPG column to draw the readers eye
table <- table %>%
  tab_style(
    style = cell_fill(color="lightblue"),
    locations = cells_body(
      columns = Mean_MPG
    )
  )

table

# This adds in a footnote
table <- table %>%
  tab_footnote(
    footnote = "MPG: Miles per gallon",
    locations = cells_column_labels(columns = Mean_MPG)
  )

table