


install.packages(c("tidyverse","lubridate","GGally","janitor","scales"))

library(tidyverse)
library(lubridate)
library(GGally)
library(janitor)
library(scales)

# 2. LOAD DATA
df <- read.csv("C:/Users/MS994/Downloads/pds/crime_weather_cleaned2.csv")

# Clean column names
df <- clean_names(df)

# 3. DATA TYPE CONVERSION
df$date_rptd <- as.Date(df$date_rptd, format="%d-%m-%Y")
df$date_occ  <- as.Date(df$date_occ, format="%d-%m-%Y")

df$temp_max <- as.numeric(df$temp_max)
df$temp_min <- as.numeric(df$temp_min)
df$rain     <- as.numeric(df$rain)
df$vict_age <- as.numeric(df$vict_age)
df$time_occ <- as.numeric(df$time_occ)

# Convert categorical variables
df <- df %>%
  mutate(
    area_name   = as.factor(area_name),
    season      = as.factor(season),
    temp_level  = as.factor(temp_level),
    rain_flag   = as.factor(rain_flag),
    time_period = as.factor(time_period),
    weekday     = as.factor(weekday),
    vict_sex    = as.factor(vict_sex),
    vict_descent= as.factor(vict_descent)
  )

# Remove invalid ages
df <- df %>% filter(vict_age > 0)

# 4. BASIC EXPLORATION
str(df)

summary(df %>%
          select(temp_max, temp_min, rain, vict_age, time_occ))

table(df$season)
table(df$rain_flag)
table(df$time_period)
table(df$area_name)

# Missing values
colSums(is.na(df))

# 5. CORRELATION ANALYSIS
num_df <- df %>%
  select(temp_max, temp_min, rain, vict_age, time_occ)

cor_matrix <- cor(num_df, use = "complete.obs")
print(cor_matrix)

# Correlation heatmap
GGally::ggcorr(num_df, label = TRUE, label_round = 2)

# 6. OUTLIER & DISTRIBUTION ANALYSIS

# Histograms
ggplot(df, aes(temp_max)) +
  geom_histogram(bins=30, fill="steelblue") +
  theme_minimal() +
  labs(title="Temperature Distribution")

ggplot(df, aes(vict_age)) +
  geom_histogram(bins=30, fill="orange") +
  theme_minimal() +
  labs(title="Age Distribution")

# Boxplots
ggplot(df, aes(y=temp_max)) +
  geom_boxplot(fill="red") +
  theme_minimal() +
  labs(title="Temperature Outliers")

ggplot(df, aes(y=vict_age)) +
  geom_boxplot(fill="green") +
  theme_minimal() +
  labs(title="Age Outliers")

# 7. CRIME PATTERN ANALYSIS

# Crime by Season
df %>%
  count(season) %>%
  ggplot(aes(x = season, y = n, fill = season)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title="Crime Count by Season")

# Crime by Temperature Level
df %>%
  count(temp_level) %>%
  ggplot(aes(x = temp_level, y = n, fill = temp_level)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title="Crime vs Temperature Level")

# Rain vs Crime
df %>%
  count(rain_flag) %>%
  ggplot(aes(x = rain_flag, y = n, fill = rain_flag)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title="Crime: Rain vs No Rain")

# Time Period
df %>%
  count(time_period) %>%
  ggplot(aes(x = time_period, y = n, fill = time_period)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title="Crime by Time Period")

# Weekday
df %>%
  count(weekday) %>%
  ggplot(aes(x = reorder(weekday, n), y = n)) +
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title="Crime by Weekday")

# Area-wise Crime
df %>%
  count(area_name) %>%
  ggplot(aes(x = reorder(area_name, n), y = n)) +
  geom_bar(stat = "identity", fill="brown") +
  coord_flip() +
  theme_minimal() +
  labs(title="Crime by Area")

# 8. DEMOGRAPHIC ANALYSIS

# Age Groups
df %>%
  count(age_group) %>%
  ggplot(aes(x = age_group, y = n, fill = age_group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title="Crime by Age Group")

# Gender
df %>%
  count(vict_sex) %>%
  ggplot(aes(x = vict_sex, y = n, fill = vict_sex)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title="Crime by Gender")

# 9. SCATTER & RELATIONSHIPS

# Temperature vs Rain
ggplot(df, aes(temp_max, rain)) +
  geom_point(alpha=0.4) +
  theme_minimal() +
  labs(title="Temperature vs Rain")

# Age vs Time
ggplot(df, aes(time_period, vict_age)) +
  geom_boxplot(fill="cyan") +
  theme_minimal() +
  labs(title="Age vs Time of Crime")

# Monthly Trend
ggplot(df, aes(month)) +
  geom_bar(fill="gold") +
  theme_minimal() +
  labs(title="Monthly Crime Trend")