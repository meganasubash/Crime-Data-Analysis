---------------
# Set Up
---------------
# install.packages(c("tidyverse","lubridate","ggplot2","scales"))

# Load
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Load data
df <- read.csv("C:/Users/MS994/Downloads/pds/crime_weather_cleaned2.csv")

# Convert to proper format
df$DATE_OCC <- as.Date(df$DATE_OCC)
df$TIME_OCC <- as.numeric(df$TIME_OCC)



-----------------------
# Crime by Time of Day
-----------------------

# Crime by Hour
df %>%
  mutate(hour = as.numeric(substr(sprintf("%04d", TIME_OCC),1,2))) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line() +
  geom_point() +
  labs(title = "Crime Distribution by Hour",
       x = "Hour of Day",
       y = "Number of Crimes") +
  theme_minimal()

# Crime by Time Period
df %>%
  count(TIME_PERIOD) %>%
  ggplot(aes(x = TIME_PERIOD, y = n, fill = TIME_PERIOD)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Time Period") +
  theme_minimal()

# Crime by Weekday
df %>%
  count(WEEKDAY) %>%
  ggplot(aes(x = reorder(WEEKDAY, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime by Weekday",
       x = "Day",
       y = "Count") +
  theme_minimal()



-------------------
# Crime by Area
-------------------
df %>%
  count(AREA_NAME, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(AREA_NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Crime Areas",
       x = "Area",
       y = "Crime Count") +
  theme_minimal()

# Map(Heat Scatter)
ggplot(df, aes(x = LON, y = LAT)) +
  geom_point(alpha = 0.2, color = "red") +
  labs(title = "Crime Location Scatter Plot") +
  theme_minimal()



-----------------------
# Victim Demographics
-----------------------

# Age Distribution
ggplot(df, aes(x = VICT_AGE)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Victim Age Distribution") +
  theme_minimal()

# Age Groups
df %>%
  count(AGE_GROUP) %>%
  ggplot(aes(x = AGE_GROUP, y = n, fill = AGE_GROUP)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Age Group") +
  theme_minimal()

# Gender Distribution
df %>%
  count(VICT_SEX) %>%
  ggplot(aes(x = VICT_SEX, y = n, fill = VICT_SEX)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Gender") +
  theme_minimal()



--------------------
# Weapon Analysis
--------------------
df %>%
  count(WEAPON_DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(WEAPON_DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top Weapons Used") +
  theme_minimal()



--------------------
# Premises Analysis
--------------------
df %>%
  count(PREMIS_DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(PREMIS_DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top Crime Locations (Premises)") +
  theme_minimal()



----------------------
# Weather Impact
----------------------

# Rain vs. Crime
df %>%
  count(RAIN_FLAG) %>%
  ggplot(aes(x = RAIN_FLAG, y = n, fill = RAIN_FLAG)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime in Rain vs No Rain") +
  theme_minimal()

# Temperature vs. Crime
df %>%
  count(TEMP_LEVEL) %>%
  ggplot(aes(x = TEMP_LEVEL, y = n, fill = TEMP_LEVEL)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Temperature Level") +
  theme_minimal()

# Scatter (Temperature vs. Crime pattern)
ggplot(df, aes(x = TEMP_MAX, y = VICT_AGE)) +
  geom_point(alpha = 0.3) +
  labs(title = "Temperature vs Victim Age") +
  theme_minimal()



----------------------
# Crime Type Analysis
----------------------
df %>%
  count(CRM_CD_DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(CRM_CD_DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top Crime Types") +
  theme_minimal()



---------------------
# Correlation Heatmap
---------------------
install.packages("corrplot")
library(corrplot)
numeric_df <- df %>%
  select(TEMP_MAX, TEMP_MIN, RAIN, VICT_AGE) %>%
  na.omit()
cor_matrix <- cor(numeric_df)
corrplot(cor_matrix, method = "color")
