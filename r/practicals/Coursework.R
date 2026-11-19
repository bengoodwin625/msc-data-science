library(tidyverse)
library(ggplot2)


(BENJAMIN_2_DRAFT_2)

players_stats <- BENJAMIN_2_DRAFT_2


# Deleting all the rows without a recorded entry engagement
ps_clean <- players_stats[!is.na(players_stats$"Entry Engagement"), ]
ps_clean

# Changing the entry engagement column so that every kill=1 and death=0
library(dplyr)


ps_clean2 <- ps_clean %>%
  mutate(`Entry Engagement` = recode(`Entry Engagement`, 
                                     "Kill" = 1, 
                                     "Death" = 0))
ps_clean2

# Attempting to plot the data of entry engagement against round win conversion
summary_ps <- ps_clean2 %>%
  group_by(`Entry Engagement`, `Win/Loss`) %>%
  summarise(count = n(), .groups = 'drop')

# Create the plot showing round conversion based off of entry success 
ggplot(summary_ps, aes(x = factor(`Entry Engagement`), y = count, fill = `Win/Loss`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Outcome of the first engagement", 
       y = "Count", 
       fill = "Win/Loss") +
  scale_x_discrete(labels = c("0" = "Death", "1" = "Kill")) +
  theme_minimal()