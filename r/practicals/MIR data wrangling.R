# What specific gameplay conditions, such as entry engagement outcomes, 
# and map type, are most strongly correlated with increased round win rates 
# for professional Rainbow Six Siege teams?

# Load necessary libraries
install.packages("tidyr")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis) 
library(lme4)
library(randomcoloR)
library(plotly)

players_stats <- BENJAMIN_2_DRAFT_2

# Cleaning up the data to get rid of variables that won't be used
players_stats <- players_stats %>% select(-ID, -Strat) # These are not useful as the ID is just for tracking which game it is
                                                       # and the Strat means nothing for this as it is an internal code

# Deleting all the rows without a recorded entry engagement
ps_clean <- players_stats[!is.na(players_stats$`Entry Engagement`), ]

# Changing the entry engagement column so that every kill=1 and death=0, and the bomb being planted = 1
ps_clean2 <- ps_clean %>%
  mutate(`Entry Engagement` = recode(`Entry Engagement`, "Kill" = 1, "Death" = 0)) %>%
  mutate(`Win/Loss` = recode(`Win/Loss`, "Win" = 1, "Loss" = 0)) %>%
  mutate(`ATK/DEF` = recode(`ATK/DEF`, "ATK" = "Attack", "DEF" = "Defense")) %>%
  mutate(`Plant` = recode(`Plant`, "PLANTED" = 1, "NO PLANT" = 0))

# Summarizing the data to calculate counts of wins and losses
summary_ps <- ps_clean2 %>%
  group_by(`Entry Engagement`, `Win/Loss`, `Game Type`, `ATK/DEF`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(`Entry Engagement`, `Game Type`, `ATK/DEF`) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create the plot showing round win/loss percentages based on entry engagement, game type, and ATK/DEF
# Create the plot showing round win/loss percentages based on entry engagement, game type, and ATK/DEF
# Create the plot showing round win/loss percentages based on entry engagement, game type, and ATK/DEF
# Create the plot showing round win/loss percentages based on entry engagement, game type, and ATK/DEF
# Create the plot showing round win/loss percentages based on entry engagement, game type, and ATK/DEF
# Modify the plot
# Assuming the extracted hex code for the teal color is #2D8A8F (example)

ggplot(summary_ps, aes(x = factor(`Entry Engagement`), y = percentage, fill = interaction(`Win/Loss`, `ATK/DEF`))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Game Type`, scales = "free_x", strip.position = "top") +
  labs(x = "Outcome of the First Engagement",
       y = "Percentage of Round Outcome",
       fill = "Outcome & Type") +  
  scale_x_discrete(labels = c("0" = "Death", "1" = "Kill")) +
  scale_fill_manual(values = c(
    "0.Attack" = "#4B306A",   # Dark purple for Attack Loss
    "1.Attack" = "#58B368",   # Light green for Attack Win
    "0.Defense" = "#6E3C8F",  # More vibrant purple for Defense Loss
    "1.Defense" = "#2D8A8F"   # New teal color for Defense Win
  ),
  labels = c(
    "0.Attack" = "Attack Loss", 
    "1.Attack" = "Attack Win", 
    "0.Defense" = "Defense Loss", 
    "1.Defense" = "Defense Win"
  )) +
  theme_minimal() +
  theme(
    axis.ticks.x = element_line(),
    axis.text.x = element_text(size = 12),
    strip.text = element_text(size = 10),  # Reduced Game Type label font size
    strip.placement = "outside"
  )




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
data_long <- data %>%
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




#------------------------------------Hierarchical Model------------------------------------------------#



# Heirachal model
data <- BENJAMIN_2_DRAFT_2
data <- data %>%
  mutate(MAP = str_replace(MAP, "oregon", "Oregon")) %>%
  mutate(TEAM = str_replace(TEAM, "Blossom", "BlossoM")) %>%
  mutate(TEAM = str_replace(TEAM, "FURY", "Fury")) %>%
  mutate(TEAM = str_replace(TEAM, "FearX", "Fear X")) %>%
  mutate(TEAM = str_replace(TEAM, "DIRE WOLVES", "Dire Wolves")) %>%
  mutate(TEAM = str_replace(TEAM, "VITE", "Vite"))
  

# Cleaning the data
cleaned_data <- data %>%
  select(-contains("Unnamed")) %>%
  drop_na(TEAM, MAP, `Win/Loss`, `ATK/DEF`)

table(cleaned_data$`Win/Loss`)

# Convert necessary columns to factors
cleaned_data$TEAM <- as.factor(cleaned_data$TEAM)
cleaned_data$MAP <- as.factor(cleaned_data$MAP)
cleaned_data$`ATK/DEF` <- as.factor(cleaned_data$`ATK/DEF`)

# Convert Win/Loss so that win = 1 and Loss = 0
cleaned_data$`Win/Loss` <- ifelse(cleaned_data$`Win/Loss` == "Win", 1, 0)

# Fit the hierarchical model
if (length(unique(cleaned_data$`Win/Loss`)) > 1) {
  winloss_side <- glmer(`Win/Loss` ~ `ATK/DEF` + (1 | TEAM) + (1 | MAP), 
                 data = cleaned_data, 
                 family = binomial)
  
  # Summary of the model
  summary(winloss_side)
} else {
  cat("The response variable 'Win/Loss' is constant (no variation).")
}

# Plotting it as a box plot to see the relation

# Fit the hierarchical generalized linear mixed model
#winloss_side <- glmer(`Win/Loss` ~ `ATK/DEF` + (1 | TEAM) + (1 | MAP), 
               #data = cleaned_data, 
               #family = binomial)

#fixed_effects <- as.data.frame(summary(winloss_side)$coefficients)

#ggplot(fixed_effects, aes(x = rownames(fixed_effects), y = Estimate)) +
  #geom_point(size = 4) +
  #geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, 
                    #ymax = Estimate + 1.96 * `Std. Error`), width = 0.2) +
 # labs(x = "Fixed Effects", y = "Estimate", 
       #title = "Fixed Effects of the GLMM Model") +
  #theme_minimal()

#ggplot(cleaned_data, aes(x = `ATK/DEF`, y = predicted)) +
  #geom_boxplot() +
  #labs(x = "ATK vs DEF", y = "Predicted Probability of Win", 
       #title = "Probability of Winning Based on Side") +
  #theme_minimal()

# Attempting to plot as an actual heirachcal model 
#cleaned_data$predicted <- predict(winloss_side, type = "response")

#ggplot(cleaned_data, aes(x = predicted, y = `Win/Loss`)) +
  #geom_jitter(width = 0.02, height = 0.05, color = "black", alpha = 0.6) +  # Jittered scatter points
  #geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +  # Logistic regression line
  #labs(x = "Predicted Probability of Win", 
       #y = "Win/Loss (1 = Win, 0 = Loss)", 
       #title = "Scatter Plot of Win/Loss with Logistic Regression Line") +
  #theme_minimal()



# Attempt 2


# % of entry kills per site 
# Using TEAM as a level
# Cleaning the data and calculate percentage of entry kills per unique site
cleaned_data <- data %>%
  filter(!is.na(`Entry Engagement`) & !is.na(`Win/Loss`) & !is.na(MAP) & !is.na(`Site`)) %>%
  group_by(Site) %>%
  mutate(
    total_engagements = n(),
    kills = sum(`Entry Engagement` == "Kill"),
    kill_percentage = (kills / total_engagements) * 100
  ) %>%
  ungroup()

cleaned_data$TEAM <- as.factor(cleaned_data$TEAM)
cleaned_data$`Win/Loss` <- ifelse(cleaned_data$`Win/Loss` == "Win", 1, 0)

# Fit a generalized linear mixed-effects model (GLMM)
winloss_entry_team <- glmer(`Win/Loss` ~ `Entry Engagement` * `ATK/DEF` + (1 | `TEAM`), 
                       data = cleaned_data, 
                       family = binomial)
summary(winloss_entry_team)

# Create predicted probabilities from the model
cleaned_data$predicted <- predict(winloss_entry_team, type = "response")

# Step 1: Aggregate data by Site to calculate the average kill_percentage and predicted win probability
aggregated_data <- cleaned_data %>%
  group_by(Site) %>%
  summarize(
    avg_kill_percentage = mean(kill_percentage, na.rm = TRUE),
    avg_predicted = mean(predicted, na.rm = TRUE),
    TEAM = first(TEAM)  # Keep the first team if you want a team associated with each site
  )

# Step 2: Create the scatter plot with the aggregated data (each site represented once)
library(ggplot2)

# Updated ggplot code
p1 <- ggplot(aggregated_data, aes(x = avg_kill_percentage, y = avg_predicted * 100, color = Site)) + 
  geom_point(size = 3, alpha = 0.7) +  # Larger points with transparency
  geom_smooth(method = "lm", color = "darkgray", se = FALSE, linewidth = 0.8, linetype = "dashed") +  # Dashed regression line
  labs(
    x = "Average Entry Kill Success Rate per Site (%)", 
    y = "Average Probability of Round Win (%)", 
    title = "Adjusted for Opposition"
  ) + 
  scale_color_viridis_d(option = "D", end = 0.9) +  # Automatically assigns colors with a large palette
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),  # Centered and bold title
    axis.title = element_text(size = 10, face = "bold"),               # Bold axis titles
    axis.text = element_text(size = 9),                                # Adjust axis text size
    legend.position = "right",                                         # Legend on the right
    legend.text = element_text(size = 8),                              # Smaller legend text
    legend.title = element_blank(),                                    # Remove legend title for simplicity
    panel.grid.major = element_line(color = "gray90"),                 # Light major grid lines
    panel.grid.minor = element_blank(),                                # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),       # White background
    panel.border = element_rect(color = "gray80", fill = NA)           # Light gray border around plot
  )

p1




# # % of entry kills per site 
# Using MAP as a level
# Cleaning the data and calculate percentage of entry kills per unique site

#winloss_entry <- glmer(`Win/Loss` ~ kill_percentage + (1 | MAP), 
                       #data = cleaned_data, 
                       #family = binomial)
#summary(winloss_entry)

# Create predicted probabilities from the model
#cleaned_data$predicted <- predict(winloss_entry, type = "response")

# Scatter plot of kill percentage vs. predicted win probability with jitter
# Step 1: Aggregate data by Site to calculate the average kill_percentage and predicted win probability
#aggregated_data <- cleaned_data %>%
  #group_by(Site, MAP) %>%  # Group by Site and MAP
  #summarize(
    #avg_kill_percentage = mean(kill_percentage, na.rm = TRUE),
    #avg_predicted = mean(predicted, na.rm = TRUE)
  )

# Step 2: Create the scatter plot with the aggregated data (each site represented once)
#ggplot(aggregated_data, aes(x = avg_kill_percentage, y = avg_predicted, color = Site)) + 
  #geom_point() +  # Scatter plot of aggregated data, colored by MAP
  #geom_jitter(width = 0.2, height = 0.02) +  
  #geom_smooth(method = "lm", color = "black") +  # Linear regression line (global)
  #labs(x = "Average Entry Kill Percentage per Site", 
       #y = "Average Probability of Win", 
       #title = "Level MAP - Aggregated by Site") + 
  #theme_minimal() +
  #theme(legend.position = "right")




# Level at TEAM again but attempting to reduce the lineararity - playing with site


cleaned_data <- data %>%
  filter(!is.na(`Entry Engagement`) & !is.na(`Win/Loss`) & !is.na(MAP) & !is.na(`Site`)) %>%
  group_by(Site) %>%
  mutate(
    total_engagements = n(),
    kills = sum(`Entry Engagement` == "Kill"),
    kill_percentage = (kills / total_engagements) * 100
  ) %>%
  ungroup()

cleaned_data$TEAM <- as.factor(cleaned_data$TEAM)
cleaned_data$`Win/Loss` <- ifelse(cleaned_data$`Win/Loss` == "Win", 1, 0)

# Fit a generalized linear mixed-effects model (GLMM)
winloss_entry_map <- glmer(`Win/Loss` ~ `Entry Engagement` * `ATK/DEF`+ (1 | MAP), 
                       data = cleaned_data, 
                       family = binomial)
summary(winloss_entry_map)

# Create predicted probabilities from the model
cleaned_data$predicted <- predict(winloss_entry_map, type = "response")

# Aggregate data by Site, computing the mean kill_percentage and predicted probabilities per site
aggregated_data <- cleaned_data %>%
  group_by(Site, MAP) %>%
  summarize(
    avg_kill_percentage = mean(kill_percentage, na.rm = TRUE),
    avg_predicted = mean(predicted, na.rm = TRUE)
  )


# Create the static ggplot with aggregated data
library(ggplot2)

p2 <- ggplot(aggregated_data, aes(x = avg_kill_percentage, y = avg_predicted*100, color = Site)) + 
  geom_point(size = 3, alpha = 0.7) +  # Larger points with transparency
  geom_smooth(method = "lm", color = "darkgray", se = FALSE, linewidth = 0.8, linetype = "dashed") +  # Dashed regression line
  labs(
    x = "Average Entry Kill Success Rate per Site (%)", 
    y = "Average Probability of Round Win (%)", 
    title = "Adjusted for Map"
  ) + 
  scale_color_viridis_d(option = "D", end = 0.9) +  # Automatically assigns colors with a large palette
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),  # Centered and bold title
    axis.title = element_text(size = 10, face = "bold"),               # Bold axis titles
    axis.text = element_text(size = 9),                                # Adjust axis text size
    legend.position = "right",                                         # Legend on the right
    legend.text = element_text(size = 8),                              # Smaller legend text
    legend.title = element_blank(),                                    # Remove legend title for simplicity
    panel.grid.major = element_line(color = "gray90"),                 # Light major grid lines
    panel.grid.minor = element_blank(),                                # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),       # White background
    panel.border = element_rect(color = "gray80", fill = NA)           # Light gray border around plot
  )

p2

# Set a seed for reproducibility
set.seed(42)

# Extract random effects for TEAM and add confidence intervals
team_effects <- ranef(winloss_entry_team, condVar = TRUE)$TEAM %>%
  as.data.frame() %>%
  rename(Intercept = `(Intercept)`) %>%
  mutate(Group = "TEAM", Level = rownames(ranef(winloss_entry_team)$TEAM))

team_effects <- team_effects %>%
  mutate(lower = Intercept - 1.96 * sqrt(attr(ranef(winloss_entry_team, condVar = TRUE)$TEAM, "postVar")[1, , ]),
         upper = Intercept + 1.96 * sqrt(attr(ranef(winloss_entry_team, condVar = TRUE)$TEAM, "postVar")[1, , ]))

# Extract random effects for MAP and add confidence intervals
map_effects <- ranef(winloss_entry_map, condVar = TRUE)$MAP %>%
  as.data.frame() %>%
  rename(Intercept = `(Intercept)`) %>%
  mutate(Group = "MAP", Level = rownames(ranef(winloss_entry_map)$MAP))

map_effects <- map_effects %>%
  mutate(lower = Intercept - 1.96 * sqrt(attr(ranef(winloss_entry_map, condVar = TRUE)$MAP, "postVar")[1, , ]),
         upper = Intercept + 1.96 * sqrt(attr(ranef(winloss_entry_map, condVar = TRUE)$MAP, "postVar")[1, , ]))

# Combine both datasets for plotting
random_effects <- bind_rows(team_effects, map_effects)

# Randomize the order of levels on the y-axis
random_effects <- random_effects %>%
  mutate(Level = factor(Level, levels = sample(unique(Level))))

# Plot random effects with confidence intervals, with Level on the y-axis
ggplot(random_effects, aes(x = Intercept, y = Level, color = Group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("TEAM" = "#58B368", "MAP" = "#6E3C8F")) +
  labs(title = "Random Effects for Team and Map",
       x = "Random Intercept",
       y = "Group Levels") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) 

# Creating fixed and random effect tables

# Extract the fixed effects from the model
fixed_effects <- tidy(winloss_entry_team, effects = "fixed")

# Format the Parameter names, display p-values in scientific notation, and create the table
fixed_effects %>%
  select(term, estimate, std.error, statistic, p.value) %>%  # Select relevant columns
  rename(
    Parameter = term,
    Estimate = estimate,
    `Standard Error` = std.error,
    `z value` = statistic,
    `p-value` = p.value
  ) %>%
  # Update the Parameter names to be more descriptive
  mutate(
    Parameter = case_when(
      Parameter == "(Intercept)" ~ "Intercept",
      Parameter == "`Entry Engagement`Kill" ~ "Entry Engagement (Kill)",
      Parameter == "`ATK/DEF`DEF" ~ "ATK/DEF (Defense)",
      Parameter == "`Entry Engagement`Kill:`ATK/DEF`DEF" ~ "Entry Engagement (Kill) : ATK/DEF (Defense)",
      TRUE ~ Parameter  # Retain original name if not matched
    ),
    `p-value` = format(`p-value`, scientific = TRUE, digits = 3)  # Format p-values in scientific notation
  ) %>%
  kbl(
    caption = "Fixed Effects Summary for Generalized Linear Mixed-Effects Model (GLMM)",
    digits = 4,  # Show enough significant figures
    align = "c"  # Center-align all columns for a cleaner look
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",  # Center the table on the page
    bootstrap_options = c("striped", "hover", "condensed", "responsive")  # Clean and compact styling options
  ) %>%
  column_spec(1, bold = TRUE) %>%  # Emphasize the parameter names
  row_spec(0, bold = TRUE, background = "#D3D3D3") %>%  # Style the header row
  add_header_above(c(" " = 1, "Fixed Effects" = 4), bold = TRUE, background = "#A9A9A9") %>%  # Header spanning columns
  footnote(
    general = "Note: Estimates and standard errors are displayed to four significant figures. P-values are shown in scientific notation where applicable.",
    footnote_as_chunk = TRUE, general_title = ""
  )  # Add a clean footnote for additional context

summary(winloss_entry_team)





winloss_entry_map <- glmer(`Win/Loss` ~ `Entry Engagement` * `ATK/DEF`+ (1 | TEAM) + (1 | MAP), 
                           data = cleaned_data, 
                           family = binomial)
summary(winloss_entry_map)

# Create predicted probabilities from the model
cleaned_data$predicted <- predict(winloss_entry_map, type = "response")

# Aggregate data by Site, computing the mean kill_percentage and predicted probabilities per site
aggregated_data <- cleaned_data %>%
  group_by(Site, MAP) %>%
  summarize(
    avg_kill_percentage = mean(kill_percentage, na.rm = TRUE),
    avg_predicted = mean(predicted, na.rm = TRUE)
  )


# Create the static ggplot with aggregated data
library(ggplot2)

p3 <- ggplot(aggregated_data, aes(x = avg_kill_percentage, y = avg_predicted*100, color = Site)) + 
  geom_point(size = 3, alpha = 0.7) +  # Larger points with transparency
  geom_smooth(method = "lm", color = "darkgray", se = FALSE, linewidth = 0.8, linetype = "dashed") +  # Dashed regression line
  labs(
    x = "Average Entry Kill Success Rate per Site (%)", 
    y = "Average Probability of Round Win (%)", 
    title = "Combined for Map and Team"
  ) + 
  scale_color_viridis_d(option = "D", end = 0.9) +  # Automatically assigns colors with a large palette
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),  # Centered and bold title
    axis.title = element_text(size = 10, face = "bold"),               # Bold axis titles
    axis.text = element_text(size = 9),                                # Adjust axis text size
    legend.position = "right",                                         # Legend on the right
    legend.text = element_text(size = 8),                              # Smaller legend text
    legend.title = element_blank(),                                    # Remove legend title for simplicity
    panel.grid.major = element_line(color = "gray90"),                 # Light major grid lines
    panel.grid.minor = element_blank(),                                # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),       # White background
    panel.border = element_rect(color = "gray80", fill = NA)           # Light gray border around plot
  )

p3










# Extract random effects for TEAM and MAP models with variance and standard deviation
team_random_effects <- tidy(winloss_entry_team, effects = "ran_pars", conf.int = FALSE) %>%
  mutate(Model = "TEAM") %>%
  select(Model, term, estimate)  # Keep only necessary columns

map_random_effects <- tidy(winloss_entry_map, effects = "ran_pars", conf.int = FALSE) %>%
  mutate(Model = "MAP") %>%
  select(Model, term, estimate)  # Keep only necessary columns

# Add standard deviation calculation to each model's data
team_random_effects <- team_random_effects %>%
  mutate(`Variance` = estimate, `Std. Dev.` = sqrt(estimate)) %>%
  select(Model, term, `Variance`, `Std. Dev.`)

map_random_effects <- map_random_effects %>%
  mutate(`Variance` = estimate, `Std. Dev.` = sqrt(estimate)) %>%
  select(Model, term, `Variance`, `Std. Dev.`)

# Combine the TEAM and MAP random effects data into a single table
random_effects <- bind_rows(team_random_effects, map_random_effects) %>%
  rename(`Random Effect` = term)  # Rename term column for clarity

# Extract the model summary
model_summary <- summary(winloss_entry_team)

# Extract variance and standard deviation of the TEAM random effect
team_variance <- as.numeric(model_summary$varcor$TEAM[1, 1])  # Extracts variance
team_sd <- sqrt(team_variance)  # Calculates standard deviation from variance

# Print values for verification
cat("TEAM Random Effect Variance:", team_variance, "\n")
cat("TEAM Random Effect Standard Deviation:", team_sd, "\n")

# Create a table with the extracted values
random_effects_table <- data.frame(
  Model = "TEAM",
  `Random Effect` = "Intercept",
  Variance = team_variance,
  `Standard Deviation` = team_sd
)

# Format the table for inclusion in a report
random_effects_table %>%
  kbl(
    caption = "Random Effect Summary for TEAM Model",
    digits = 4,
    align = "c"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Random Effects" = 3), bold = TRUE, background = "#A9A9A9") %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3") %>%
  footnote(
    general = "Note: Variance and standard deviation of the TEAM random effect, extracted directly from the model summary.",
    footnote_as_chunk = TRUE, general_title = ""
  )





# Convert to interactive plot
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot






# -------------------------------Table Time--------------------------------------------#

# Table packages
install.packages("kableExtra")
library(kableExtra)

cleaned_data <- data %>%
  filter(!is.na(`Win/Loss`), !is.na(TEAM), !is.na(MAP), !is.na(`Entry Engagement`))

# Convert columns with 2 outcomes to 1 and 0
cleaned_data$TEAM <- as.factor(cleaned_data$TEAM)
cleaned_data$MAP <- as.factor(cleaned_data$MAP)
cleaned_data$`Win/Loss` <- ifelse(cleaned_data$`Win/Loss` == "Win", 1, 0)
cleaned_data$`Entry Engagement` <- ifelse(cleaned_data$`Entry Engagement` == "Kill", 1, 0)

# Table of Win Rates by Team
win_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = round(mean(`Win/Loss`) * 100, 1)
  ) %>%
  arrange(desc(win_rate))
print("Win Rates by Team:")
print(win_rate_by_team)

# Table of Win Rates by Map
win_rate_by_map <- cleaned_data %>%
  group_by(MAP) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = round(mean(`Win/Loss`) * 100, 1)
  ) %>%
  arrange(desc(win_rate))
print("Win Rates by Map:")
print(win_rate_by_map)

entry_vs_winloss <- table(cleaned_data$`Entry Engagement`, cleaned_data$`Win/Loss`)
print("Cross Tabulation: Entry Engagement vs Win/Loss:")
print(entry_vs_winloss)

# Calculate Entry Kill Success Rate by Map
entry_kill_rate_by_map <- cleaned_data %>%
  group_by(MAP) %>%
  summarize(
    total_engagements = n(),
    kills = sum(`Entry Engagement`),
    kill_rate = round(mean(`Entry Engagement`) * 100, 1)
  ) %>%
  arrange(desc(kill_rate))

# Add a row for total engagements and kills success rate
summary_row_kill <- entry_kill_rate_by_map %>%
  summarize(
    MAP = "Total",
    total_engagements = sum(total_engagements),
    kills = sum(kills),
    kill_rate = round(mean(kill_rate), 1)
  )
entry_kill_rate_by_map <- bind_rows(entry_kill_rate_by_map, summary_row_kill)

# Display Entry Kill Success Rate by Map using kableExtra
entry_kill_rate_by_map %>%
  kable(
    col.names = c("Map", "Total Engagements", "Kills", "Kill Rate (%)"),
    caption = "Kill Success Rate by Map",
    format = "html"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed")
  ) %>%
  column_spec(4, bold = TRUE, color = "white", background = spec_color(entry_kill_rate_by_map$kill_rate, end = 0.7, direction = 1)) %>%
  add_header_above(c(" " = 1, "Engagement Stats" = 3)) %>%  # Header row
  row_spec(nrow(entry_kill_rate_by_map), bold = TRUE, color = "black") %>%  
  footnote(
    general = "Kill rates are calculated as the percentage of kills out of total engagements.",
    general_title = "Note:"
  )



# Table of Win Rates by Map

# Calculate Win Rates by Map
win_rate_by_map <- cleaned_data %>%
  group_by(MAP) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = round(mean(`Win/Loss`) * 100 , 1)
  ) %>%
  arrange(desc(win_rate))

# Add a summary row for total games and wins
summary_row <- win_rate_by_map %>%
  summarize(
    MAP = "Total",
    total_games = sum(total_games),
    wins = sum(wins),
    win_rate = round(mean(win_rate), 1)
  )

# Combine the main table with the summary row
win_rate_by_map <- bind_rows(win_rate_by_map, summary_row)

# Display Win Rates by Map using kableExtra
win_rate_by_map %>%
  kable(
    col.names = c("Map", "Total Games", "Total Wins", "Win Rate (%)"),
    caption = "Win Rates by Map",
    format = "html"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "bordered")
  ) %>%
  column_spec(4, bold = TRUE, color = "white", background = spec_color(win_rate_by_map$win_rate, end = 0.7, direction = 1)) %>%
  add_header_above(c(" " = 1, "Map Stats" = 3)) %>%  # Header Row
  row_spec(nrow(win_rate_by_map), bold = TRUE, color = "black") %>%  
  footnote(
    general = "Win rates are calculated as the percentage of wins out of total games.",
    general_title = "Note:"
  )


# Table of win rate by team

# Calculate Win Rates by Team
win_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = mean(`Win/Loss`) * 100
  ) %>%
  arrange(desc(win_rate))


# Add a summary row for total games and wins
summary_row <- win_rate_by_team %>%
  summarize(
    TEAM = "Total",
    total_games = sum(total_games),
    wins = sum(wins),
    win_rate = mean(win_rate)
  )

# Combine the main table with the summary row
win_rate_by_team <- bind_rows(win_rate_by_team, summary_row)

# Round win_rate to 1 decimal place
win_rate_by_team <- win_rate_by_team %>%
  mutate(win_rate = round(win_rate, 1))

# Display Win Rates by Team using kableExtra with bordered style
win_rate_by_team %>%
  kable(
    col.names = c("Team", "Total Rounds", "Total Wins", "Win Rate (%)"),
    caption = "Round Win Rates by Team",
    format = "html"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "bordered")
  ) %>%
  column_spec(4, bold = TRUE, color = "white", background = spec_color(win_rate_by_team$win_rate, end = 0.8, direction = 1)) %>%
  add_header_above(c(" " = 1, "Team Win Rate Stats" = 3)) %>%
  row_spec(nrow(win_rate_by_team), bold = TRUE, color = "black") %>%
  footnote(
    general = "Win rates are calculated as the percentage of wins out of total rounds.",
    general_title = "Note:"
  )






#----------------------------------------Tables of entry kill % --------------
                  

# Calculate Win Rates by Team
win_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = mean(`Win/Loss`) * 100
  ) %>%
  arrange(desc(win_rate))

# Calculate Entry Kill Rates by Team
entry_kill_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    entry_rate = mean(`Entry Engagement`) * 100  # Assuming 1 = successful entry kill, 0 = no kill
  )

# Combine Win Rate and Entry Kill Rate by merging the tables
combined_table <- win_rate_by_team %>%
  left_join(entry_kill_rate_by_team, by = "TEAM") %>%
  arrange(desc(win_rate))

# Add a summary row for total games, wins, and entry rate
summary_row <- combined_table %>%
  summarize(
    TEAM = "Total",
    total_games = sum(total_games),
    wins = sum(wins),
    win_rate = mean(win_rate),
    entry_rate = mean(entry_rate, na.rm = TRUE)
  )

# Append the summary row to the main table
combined_table <- bind_rows(combined_table, summary_row)

# Round win_rate and entry_rate to 1 decimal place
combined_table <- combined_table %>%
  mutate(
    win_rate = round(win_rate, 1),
    entry_rate = round(entry_rate, 1)
  )

# Display Combined Table using kableExtra with bordered style
combined_table %>%
  kable(
    col.names = c("Team", "Total Rounds", "Total Wins", "Win Rate (%)", "Entry Kill Rate (%)"),
    caption = "Round Win Rates and Entry Kill Rates by Team",
    format = "html"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "bordered")
  ) %>%
  column_spec(4, bold = TRUE, color = "white", background = spec_color(combined_table$win_rate, end = 0.8, direction = 1)) %>%
  column_spec(5, bold = TRUE, color = "white", background = spec_color(combined_table$entry_rate, end = 0.8, direction = 1)) %>%
  add_header_above(c(" " = 1, "Team Performance Stats" = 4)) %>%
  row_spec(nrow(combined_table), bold = TRUE, color = "black") %>%
  footnote(
    general = "Win rates and entry kill rates are calculated as percentages.",
    general_title = "Note:"
  )






# Convert Plant column to numeric: 1 for "Planted", 0 for any other value
cleaned_data <- cleaned_data %>%
  mutate(Plant = ifelse(Plant == "PLANTED", 1, 0))

# Calculate Win Rates by Map
win_rate_by_map <- cleaned_data %>%
  group_by(MAP) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = mean(`Win/Loss`) * 100
  ) %>%
  arrange(desc(win_rate))

# Calculate Entry Kill Rates by Map
entry_kill_rate_by_map <- cleaned_data %>%
  group_by(MAP) %>%
  summarize(
    entry_rate = mean(`Entry Engagement`) * 100  # Assuming 1 = successful entry kill, 0 = no kill
  )

# Calculate Plant Rates by Map
plant_rate_by_map <- cleaned_data %>%
  group_by(MAP) %>%
  summarize(
    plant_rate = mean(Plant) * 100  # Calculating the percentage of rounds with a plant
  )

# Combine Win Rate, Entry Kill Rate, and Plant Rate by merging the tables
combined_map_table <- win_rate_by_map %>%
  left_join(entry_kill_rate_by_map, by = "MAP") %>%
  left_join(plant_rate_by_map, by = "MAP") %>%
  arrange(desc(win_rate))

# Add a summary row for total games, wins, entry rate, and plant rate
summary_row <- combined_map_table %>%
  summarize(
    MAP = "Total",
    total_games = sum(total_games),
    wins = sum(wins),
    win_rate = mean(win_rate),
    entry_rate = mean(entry_rate, na.rm = TRUE),
    plant_rate = mean(plant_rate, na.rm = TRUE)
  )

# Append the summary row to the main table
combined_map_table <- bind_rows(combined_map_table, summary_row)

# Round win_rate, entry_rate, and plant_rate to 1 decimal place
combined_map_table <- combined_map_table %>%
  mutate(
    win_rate = round(win_rate, 1),
    entry_rate = round(entry_rate, 1),
    plant_rate = round(plant_rate, 1)
  )

# Display Combined Table using kableExtra with bordered style
combined_map_table %>%
  kable(
    col.names = c("Map", "Total Rounds", "Total Wins", "Win Rate (%)", "Entry Kill Rate (%)", "Plant Rate (%)"),
    caption = "Round Stats per Map",
    format = "html"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "bordered")
  ) %>%
  column_spec(4, bold = TRUE, color = "white", background = spec_color(combined_map_table$win_rate, end = 0.8, direction = 1)) %>%
  column_spec(5, bold = TRUE, color = "white", background = spec_color(combined_map_table$entry_rate, end = 0.8, direction = 1)) %>%
  column_spec(6, bold = TRUE, color = "white", background = spec_color(combined_map_table$plant_rate, end = 0.8, direction = 1)) %>%
  add_header_above(c(" " = 1, "Map Performance Stats" = 5)) %>%
  row_spec(nrow(combined_map_table), bold = TRUE, color = "black") %>%
  footnote(
    general = "Win rates, entry kill rates, and plant rates are calculated as percentages.",
    general_title = "Note:"
  )





# Convert Plant column to numeric: 1 for "PLANTED", 0 for any other value
cleaned_data <- cleaned_data %>%
  mutate(Plant = ifelse(Plant == "PLANTED", 1, 0))

# Calculate Win Rates by Team
win_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    total_games = n(),
    wins = sum(`Win/Loss`),
    win_rate = mean(`Win/Loss`) * 100
  ) %>%
  arrange(desc(win_rate))

# Calculate Entry Kill Rates by Team
entry_kill_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    entry_rate = mean(`Entry Engagement`) * 100  # Assuming 1 = successful entry kill, 0 = no kill
  )

# Calculate Plant Rates by Team
plant_rate_by_team <- cleaned_data %>%
  group_by(TEAM) %>%
  summarize(
    plant_rate = mean(Plant) * 100  # Calculating the percentage of rounds with a plant
  )

# Combine Win Rate, Entry Kill Rate, and Plant Rate by merging the tables
combined_team_table <- win_rate_by_team %>%
  left_join(entry_kill_rate_by_team, by = "TEAM") %>%
  left_join(plant_rate_by_team, by = "TEAM") %>%
  arrange(desc(win_rate))

# Add a summary row for total games, wins, entry rate, and plant rate
summary_row <- combined_team_table %>%
  summarize(
    TEAM = "Total",
    total_games = sum(total_games),
    wins = sum(wins),
    win_rate = mean(win_rate),
    entry_rate = mean(entry_rate, na.rm = TRUE),
    plant_rate = mean(plant_rate, na.rm = TRUE)
  )

# Append the summary row to the main table
combined_team_table <- bind_rows(combined_team_table, summary_row)

# Round win_rate, entry_rate, and plant_rate to 1 decimal place
combined_team_table <- combined_team_table %>%
  mutate(
    win_rate = round(win_rate, 1),
    entry_rate = round(entry_rate, 1),
    plant_rate = round(plant_rate, 1)
  )

# Display Combined Table using kableExtra with bordered style
combined_team_table %>%
  kable(
    col.names = c("Team", "Total Games", "Total Wins", "Win Rate (%)", "Entry Kill Rate (%)", "Plant Rate (%)"),
    caption = "Round Win Rates, Entry Kill Rates, and Plant Rates by Team",
    format = "html"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "bordered")
  ) %>%
  column_spec(4, bold = TRUE, color = "white", background = spec_color(combined_team_table$win_rate, end = 0.8, direction = 1)) %>%
  column_spec(5, bold = TRUE, color = "white", background = spec_color(combined_team_table$entry_rate, end = 0.8, direction = 1)) %>%
  column_spec(6, bold = TRUE, color = "white", background = spec_color(combined_team_table$plant_rate, end = 0.8, direction = 1)) %>%
  add_header_above(c(" " = 1, "Team Performance Stats" = 5)) %>%
  row_spec(nrow(combined_team_table), bold = TRUE, color = "black") %>%
  footnote(
    general = "Win rates, entry kill rates, and plant rates are calculated as percentages.",
    general_title = "Note:"
  )

