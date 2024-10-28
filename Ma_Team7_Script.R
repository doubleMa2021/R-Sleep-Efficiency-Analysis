library(tidyverse)  
library(dplyr)      
library(ggplot2)   
library(psych)

# 1.1 Step 1: Load the dataset
sleep_data <- read.csv("Sleep_Efficiency.csv")

# 1.2 Step 2: Clean the data
# Handle Missing Values
# 1.2.1 Check for missing values
missing_data <- colSums(is.na(sleep_data))
print(missing_data)

# Remove rows with missing values
sleep_data_clean <- sleep_data %>% drop_na()

# 1.2.2 Handle Outliers
# Calculate Q1 (25th percentile) and Q3 (75th percentile) 
Q1_sleep_duration <- quantile(sleep_data_clean$Sleep.Duration, 0.25)
Q3_sleep_duration <- quantile(sleep_data_clean$Sleep.Duration, 0.75)
Q1_caffeine_consumption <- quantile(sleep_data_clean$Caffeine.Consumption, 0.25)
Q3_caffeine_consumption <- quantile(sleep_data_clean$Caffeine.Consumption, 0.75)

# Calculate IQR
IQR_sleep_duration <- Q3_sleep_duration - Q1_sleep_duration
IQR_caffeine_consumption <- Q3_caffeine_consumption - Q1_caffeine_consumption

# Define the bounds for detecting outliers (1.5 * IQR rule)
lower_bound_sleep_duration <- Q1_sleep_duration - 1.5 * IQR_sleep_duration
upper_bound_sleep_duration <- Q3_sleep_duration + 1.5 * IQR_sleep_duration
lower_bound_caffeine_consumption <- Q1_caffeine_consumption - 1.5 * IQR_caffeine_consumption
upper_bound_caffeine_consumption <- Q3_caffeine_consumption + 1.5 * IQR_caffeine_consumption

# Remove rows where column is outside of these bounds
cleaned_data <- sleep_data_clean %>%
  filter(Sleep.Duration >= lower_bound_sleep_duration & 
          Sleep.Duration <= upper_bound_sleep_duration & 
          Caffeine.Consumption >= lower_bound_caffeine_consumption & 
          Caffeine.Consumption <= upper_bound_caffeine_consumption)

#library(outliers)
# Apply IQR-based outlier removal on a numeric column
# fill = FALSE drops the outliers instead of replacing them
#data$A <- rm.outlier(data$A, fill = FALSE)  
#data[names(data)] <- lapply(data, function(col)
  #if(is.numeric(col)) rm.outlier(col, fill = FALSE) else col)

# 1.2.3 Handle Incorrect Data Types
# No more incorrect Data Types

# Step 3: Descriptive Statistics
describe(cleaned_data)  # Descriptive statistics for all numeric variables
summary(cleaned_data) # same as last one
str(cleaned_data) # Output each variable's name, type, and a preview of the values
glimpse(cleaned_data) # same as last one

# 1.3 Visualize the data
# Create Age Group variable
cleaned_data$Age.Group <- cut(cleaned_data$Age, breaks = c(0, 25, 60, 100), labels = c("Young", "Middle-aged", "Old"))
# Create a new column 'Sleep.Efficiency.Percentage' by multiplying 'Sleep.Efficiency' by 100
cleaned_data$Sleep.Efficiency.Percentage <- cleaned_data$Sleep.Efficiency * 100

# 1.3.1 Age and Gender distribution visualization
ggplot(cleaned_data, aes(x = Age.Group, fill = Gender)) +
  geom_bar(position = "dodge") +  # Bars side by side for comparison
  ggtitle("Age and Gender Distribution") +
  xlab("Age Group") +
  ylab("Count of Participants") +
  scale_fill_manual(values = c("pink", "blue")) +  # Custom colors for gender
  theme_minimal()

ggplot(cleaned_data, aes(x = Age, fill = Gender)) +
  geom_bar(position = "dodge") +  # Bars side by side for comparison
  ggtitle("Age and Gender Distribution") +
  xlab("Age") +
  ylab("Count of Participants") +
  scale_fill_manual(values = c("pink", "blue")) +  # Custom colors for gender
  theme_minimal()

# 1.3.2 Histogram for the distribution of Awakenings
ggplot(cleaned_data, aes(x = Awakenings)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Awakenings During Sleep") +
  xlab("Number of Awakenings") +
  ylab("Frequency")

# 1.3.3 Scatter Plot for Awakenings vs Sleep Efficiency
ggplot(cleaned_data, aes(x = Awakenings, y = Sleep.Efficiency.Percentage)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a trend line
  ggtitle("Awakenings vs Sleep Efficiency") +
  xlab("Number of Awakenings") +
  ylab("Sleep Efficiency (%)")

# 1.3.4 Bar Plot for Average Awakenings by Age Group
ggplot(cleaned_data, aes(x = Age.Group, y = Awakenings)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightgreen") +
  ggtitle("Average Awakenings by Age Group") +
  xlab("Age Group") +
  ylab("Average Number of Awakenings")

# 1.3.5 Histogram for REM Sleep Percentage
ggplot(cleaned_data, aes(x = REM.Sleep.Percentage)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  ggtitle("Distribution of REM Sleep Percentage") +
  xlab("REM Sleep Percentage (%)") +
  ylab("Frequency") +
  theme_minimal()

# 1.3.6 Scatter Plot for REM Sleep Percentage vs Sleep Efficiency
ggplot(cleaned_data, aes(x = REM.Sleep.Percentage, y = Sleep.Efficiency.Percentage)) +
  geom_point(color = "blue") +
  ggtitle("REM Sleep Percentage vs Sleep Efficiency") +
  xlab("REM Sleep Percentage (%)") +
  ylab("Sleep Efficiency (%)") +
  theme_minimal()

# 1.3.7 Scatter Plot for Caffeine Consumption vs Sleep Efficiency
ggplot(cleaned_data, aes(x = Caffeine.Consumption, y = Sleep.Efficiency.Percentage)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red") +  
  ggtitle("Caffeine Consumption vs Sleep Efficiency") +
  xlab("Caffeine Consumption (mg)") +
  ylab("Sleep Efficiency (%)") +
  theme_minimal()

# 1.3.8 Create a caffeine group variable
cleaned_data$Caffeine.Group <- cut(cleaned_data$Caffeine.Consumption,
                                   breaks = c( -Inf, 25, 50, 75, Inf),
                                   labels = c("Low", "Moderate", "High", "Very High"))

# Box Line diagram for Sleep Efficiency of different caffeine groups
ggplot(cleaned_data, aes(x = Caffeine.Group, y = Sleep.Efficiency.Percentage)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Sleep Efficiency by Caffeine Consumption Group") +
  xlab("Caffeine Consumption Group") +
  ylab("Sleep Efficiency (%)") +
  theme_minimal()

# 1.3.9 Box Plot for Sleep Efficiency by Alcohol Consumption
ggplot(cleaned_data, aes(x = factor(Alcohol.Consumption), y = Sleep.Efficiency.Percentage)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Sleep Efficiency vs Alcohol Consumption") +
  xlab("Alcohol Consumption (0 = No, 1 = Yes)") +
  ylab("Sleep Efficiency (%)")

# 1.3.10 Scatter Plot for Sleep Duration vs Alcohol Consumption
ggplot(cleaned_data, aes(x = factor(Alcohol.Consumption), y = Sleep.Duration)) +
  geom_jitter(width = 0.1, color = "darkgreen") +
  ggtitle("Sleep Duration vs Alcohol Consumption") +
  xlab("Alcohol Consumption (0 = No, 1 = Yes)") +
  ylab("Sleep Duration (hours)")

# 1.3.11 Bar Plot for Average Sleep Efficiency by Alcohol Consumption
ggplot(cleaned_data, aes(x = factor(Alcohol.Consumption), y = Sleep.Efficiency.Percentage)) +
  stat_summary(fun = "mean", geom = "bar", fill = "orange") +
  ggtitle("Average Sleep Efficiency by Alcohol Consumption") +
  xlab("Alcohol Consumption (0 = No, 1 = Yes)") +
  ylab("Average Sleep Efficiency (%)")

# 1.3.12 Bar Chart for Average Sleep Efficiency by Smoking Status
ggplot(cleaned_data, aes(x = factor(Smoking.Status), y = Sleep.Efficiency.Percentage)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("red", "blue")) +
  scale_x_discrete(labels = c("Non-Smoker", "Smoker")) +
  ggtitle("Average Sleep Efficiency by Smoking Status") +
  xlab("Smoking Status") +
  ylab("Average Sleep Efficiency (%)")

# 1.3.13 Grouping data by Exercise Frequency and calculating average Sleep Efficiency
exercise_efficiency <- cleaned_data %>%
  group_by(Exercise.Frequency) %>%
  summarise(Average.Sleep.Efficiency = mean(Sleep.Efficiency.Percentage, na.rm = TRUE))

# Bar Plot for Average Sleep Efficiency by Exercise Frequency
ggplot(exercise_efficiency, aes(x = Exercise.Frequency, y = Average.Sleep.Efficiency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Average Sleep Efficiency by Exercise Frequency") +
  xlab("Exercise Frequency") +
  ylab("Average Sleep Efficiency (%)")

