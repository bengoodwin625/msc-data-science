require(ggplot2) 
require(raster)
install.packages("dplyr")
library(dplyr)


## Analysis Task

women_bmi_data <- (data9b_women)

men_bmi_data <- (data9b_men)

head(men_bmi_data)

head(women_bmi_data)

combined_bmi_data <- rbind(women_bmi_data, men_bmi_data)

combined_bmi_data

women_bmi_data$gender <- "Female" 
men_bmi_data$gender <- "Male" 

combined_bmi_data <- rbind(women_bmi_data, men_bmi_data)

head(combined_bmi_data)

install.packages("dplyr")
library(dplyr)

step_count_summary <- combined_bmi_data %>%
  group_by(gender) %>%
  summarise(
    mean_steps = mean(steps, na.rm = TRUE),
    median_steps = median(steps, na.rm = TRUE),
    sd_steps = sd(steps, na.rm = TRUE),
    count = n()
  )

print(step_count_summary)

str(combined_bmi_data)

library(ggplot2)

ggplot(combined_bmi_data, aes(x = gender, y = steps, fill = gender)) +
  geom_boxplot() +
  labs(title = "Step Counts by Gender", x = "Gender", y = "Step Count") +
  theme_minimal()+
  scale_fill_manual(values = c("Female" = "lightblue", "Male" = "lightgreen"))

ggplot(combined_bmi_data, aes(x = steps, y = bmi)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Steps vs. BMI", 
       x = "Number of Steps", 
       y = "BMI") +
  theme_minimal()



# Bar chart of the cars by number of cylinders using ggplot

ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill='lightgreen',color='black') +
  labs(x='Number of cylinders',y="frequency")

# Pie chart of cars by number of cylinders using ggplot

ggplot(mtcars, aes(x=factor(1),fill=factor(cyl))) +
  geom_bar(width=1) +
  coord_polar(theta = "y") +
  labs(x="",y="",fill="Number of cylinders")

# Histogram of the fuel consumption using ggplot

ggplot(mtcars,aes(x=mpg)) +
  geom_histogram(binwidth=5, fill='lightgreen',color='black') +
  labs(x="Fuel consumoption (in mpg)",y="Frequency")

# Box plots of fule consumption using ggplot

ggplot(mtcars, aes(x=factor(cyl),y=mpg)) +
  geom_boxplot(fill='lightgreen',color='black') +
  labs(x='Number of cylinders',y='Fuel consumption (in mpg)')

# Violin plot of fuel consumption using ggplot

ggplot(mtcars, aes(x=factor(cyl) ,y=mpg)) +
  geom_violin(fill='lightgreen',color='black') +
  labs(x='Number of cylinders',y='Fuel consumption (in mpg)')
  
# Scatter plot of cars weight by fuel consumption using ggplot

ggplot(data = mtcars, aes(x=wt,y=mpg)) +
  geom_point(color='red') +
  labs(x='Weight',y='Fuel consumption (in mpg)')

# Customising the plot

ggplot(data = mtcars, aes(x=wt,y=mpg,color=factor(cyl))) +
  geom_point() +
  labs(x='Weight',y='Fuel consumption (in mpg)',color="Number of cylinders")


ggplot(data = mtcars, aes(x=wt,y=mpg,shape=factor(cyl))) +
  geom_point() +
  labs(x='Weight',y='Fuel consumption (in mpg)',shape="Number of cylinders")

# Altering point size and transparency

ggplot(data = mtcars, aes(x=wt,y=mpg,shape=factor(cyl))) +
  geom_point(aes(x=wt,y=mpg),cex=10,alpha=0.3) +
  labs(x='Weight',y='Fuel consumption (in mpg)',shape="Number of cylinders")

# Adding trend lines

ggplot(data = mtcars, aes(x=wt,y=mpg,color=factor(cyl))) +
  geom_point(aes(x=wt,y=mpg)) +
  geom_smooth(method="lm") +
  labs(x='Weight',y='Fuel consumption (in mpg)',shape="Number of cylinders")

# Facetting

ggplot(data=mtcars, aes(x=wt,y=mpt)) +
  geom_point(aes(x=wt,y=mpg)) +
  labs(x="Weight", y="Fuel consumption (in mpg)") +
  facet_grid(. ~ cyl)

# Line plots

data(economics)

# line plot of unemployment using ggplot2

ggplot(data=economics, aes(x=date,y=uempmed)) +
  geom_line(color='red',) +
  labs(x='Date',y='Number of uneomplyed (in 1000s)')