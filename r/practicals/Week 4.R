# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Assuming BENJAMIN_2_DRAFT_2 is already loaded
players_stats <- BENJAMIN_2_DRAFT_2

# Deleting all the rows without a recorded entry engagement
ps_clean <- players_stats[!is.na(players_stats$`Entry Engagement`), ]

# Changing the entry engagement column so that every kill=1 and death=0
ps_clean2 <- ps_clean %>%
  mutate(`Entry Engagement` = recode(`Entry Engagement`, "Kill" = 1, "Death" = 0)) %>%
  mutate(`Win/Loss` = recode(`Win/Loss`, "Win" = 1, "Loss" = 0)) %>%
  mutate(`ATK/DEF` = recode(`ATK/DEF`, "ATK" = "Attack", "DEF" = "Defense"))

# Summarizing the data to calculate counts of wins and losses
summary_ps <- ps_clean2 %>%
  group_by(`Entry Engagement`, `Win/Loss`, `Game Type`, `ATK/DEF`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(`Entry Engagement`, `Game Type`, `ATK/DEF`) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create the plot showing round win/loss percentages based on entry engagement, game type, and ATK/DEF
ggplot(summary_ps, aes(x = factor(`Entry Engagement`), y = percentage, fill = interaction(`Win/Loss`, `ATK/DEF`))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Game Type`) +  # Create separate plots for each Game Type
  labs(x = "Outcome of the First Engagement",
       y = "Percentage of Round Outcome",
       fill = "Outcome & Type") +  # Update legend title
  scale_x_discrete(labels = c("0" = "Death", "1" = "Kill")) +
  scale_fill_manual(values = c(
    "0.Attack" = "red", "1.Attack" = "darkred",
    "0.Defense" = "green", "1.Defense" = "darkgreen",
    "1.Attack" = "blue", "0.Defense" = "purple",
    "1.Defense" = "lavender"
  ),
  labels = c(
    "0.Attack" = "Attack Loss", 
    "1.Attack" = "Attack Win", 
    "0.Defense" = "Defense Loss", 
    "1.Defense" = "Defense Win"
  )) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(),  # Ensure ticks on x-axis
        axis.text.x = element_text(size = 12))  # Adjust text size for readability



ggplot(summary_ps, aes(x = `Game Type`, y = percentage, fill = interaction(`Win/Loss`, `ATK/DEF`))) +
  geom_area(position = "stack") +
  labs(x = "Game Type",
       y = "Percentage of Round Outcome",
       fill = "Outcome & Type") +
  theme_minimal()

# Load required packages
library(ggplot2)
library(dplyr)

# Sample data creation
data <- data.frame(
  Round = rep(1:3, each = 10), # 3 rounds example
  Fight = rep(1:10, times = 3), # 10 fights per round
  Kills = c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5,  # Round 1
            0, 0, 1, 1, 2, 2, 3, 3, 4, 5,  # Round 2
            0, 1, 2, 2, 3, 3, 3, 4, 4, 5)  # Round 3
)

# Create the slope graph
ggplot(data, aes(x = Fight, y = Kills, group = Round, color = as.factor(Round))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 0:5) +
  labs(
    title = "Kill Progression in Rainbow Six Siege Rounds",
    x = "Fight Number",
    y = "Cumulative Kills",
    color = "Round"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Storing here

# Reshape and plot the subset
data_long_subset <- data_subset %>%
  pivot_longer(cols = starts_with("Cumulative_Fight"),
               names_to = "Fight",
               values_to = "Cumulative_Fights")

ggplot(data_long_subset, aes(x = Fight, y = Cumulative_Fights, group = Row)) +
  geom_line(aes(color = factor(Row)), size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Slope Graph of Cumulative Fights (Subset)", 
       x = "Fight", 
       y = "Cumulative Number of Kills") +
  scale_x_discrete(labels = 1:10) +  # Change x-axis labels to 1-10
  theme_minimal() 







# Display the round kills as a function of time

# First step is to assign each fight a value so can track the increase in kills over the course of the round
# by using the "Fight 'n'" columns

mir_kill <- c("BK", "EK", "KK", "WK", "GK","Bk", "Ek", "Kk", "Wk", "Gk") # Since some of the data is upper and lower case
opp_kill <- c("BD", "ED", "KD", "WD", "GD","Bd", "Ed", "Kd", "Wd", "Gd")

fights <- data.frame(
  ps_clean2$`Fight 1`,
  ps_clean2$`Fight 2`,
  ps_clean2$`Fight 3`,
  ps_clean2$`Fight 4`,
  ps_clean2$`Fight 5`,
  ps_clean2$`Fight 6`,
  ps_clean2$`Fight 7`,
  ps_clean2$`Fight 8`,
  ps_clean2$`Fight 9`,
  ps_clean2$`Fight 10`
)

# There is some missing data in the fights so need to delete those files in the fights dataset
fights[is.na(fights)] <- 0

# Now I need to convert all the times MIR got a kill to 1 and anytime MIR died to a 0
fights_bin <- data.frame(
  Fight_1_Match = ifelse(fights$ps_clean2..Fight.1. %in% mir_kill, 1, 0),
  Fight_2_Match = ifelse(fights$ps_clean2..Fight.2. %in% mir_kill, 1, 0),
  Fight_3_Match = ifelse(fights$ps_clean2..Fight.3. %in% mir_kill, 1, 0),
  Fight_4_Match = ifelse(fights$ps_clean2..Fight.4. %in% mir_kill, 1, 0),
  Fight_5_Match = ifelse(fights$ps_clean2..Fight.5. %in% mir_kill, 1, 0),
  Fight_6_Match = ifelse(fights$ps_clean2..Fight.6. %in% mir_kill, 1, 0),
  Fight_7_Match = ifelse(fights$ps_clean2..Fight.7. %in% mir_kill, 1, 0),
  Fight_8_Match = ifelse(fights$ps_clean2..Fight.8. %in% mir_kill, 1, 0),
  Fight_9_Match = ifelse(fights$ps_clean2..Fight.9. %in% mir_kill, 1, 0),
  Fight_10_Match = ifelse(fights$ps_clean2..Fight.10. %in% mir_kill, 1, 0)
)
fights_bin

# Now the fight outcome is either 1 for win or 0 for loss I can calculate the sum of the fight outcomes
# in order to track the fights over the course of the 10 rounds

cumulative_sums <- t(apply(fights_bin, 1, cumsum))

# Store the data in a dataframe
cumulative_fights <- as.data.frame(cumulative_sums)

# Renaming the columns
colnames(cumulative_fights) <- paste0("Cumulative_", colnames(fights_bin))
print(head(cumulative_fights))

# Deleting columns if the number of kills > 5 since its not possible

# Assuming 'your_data' is your dataset, and we don't know the column name of the last column
# Remove rows where the value in the last column is greater than 5
correct_cumulative_fights <- cumulative_fights %>%
  filter(Cumulative_Fight_10_Match <= 5)

# View the filtered data
correct_cumulative_fights


# Now I'm trying to plot the fight outcome through time using a slope graph

data <- correct_cumulative_fights

# Add a row identifier column to keep track of each observation
data <- data %>%
  mutate(Row = row_number())

# Make sure the Fight column is treated as an ordered factor
data_long$Fight <- factor(data_long$Fight, 
                          levels = c("Cumulative_Fight_1_Match", 
                                     "Cumulative_Fight_2_Match", 
                                     "Cumulative_Fight_3_Match", 
                                     "Cumulative_Fight_4_Match", 
                                     "Cumulative_Fight_5_Match", 
                                     "Cumulative_Fight_6_Match", 
                                     "Cumulative_Fight_7_Match", 
                                     "Cumulative_Fight_8_Match", 
                                     "Cumulative_Fight_9_Match", 
                                     "Cumulative_Fight_10_Match"))

data_subset <- data_long[1:50, ] # First 20 rows
library(randomcoloR)
distinct_colors <- distinctColorPalette(length(unique(data_subset$Row)))

# Plotting the slope graph 
ggplot(data_subset, aes(x = Fight, y = Cumulative_Fights, group = Row)) +
  geom_line(aes(color = factor(Row)), size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Slope Graph of Cumulative Fights", 
       x = "Fight", 
       y = "Cumulative Number of Fights") +
  scale_x_discrete(labels = 1:10) +  # Use 1-10 on the x-axis
  scale_color_manual(values = distinct_colors) + 
  theme_minimal() +
  theme(legend.position = "none") # Remove legend if not needed 









