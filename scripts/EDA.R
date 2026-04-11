install.packages(c("tidyverse","lubridate","GGally","janitor"))
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(GGally)
library(janitor)
df <- read.csv("C:/Users/MS994/Downloads/pds/crime_weather_cleaned2.csv")

df <- clean_names(df)
colnames(df)
df$date_rptd <- as.Date(df$date_rptd, format="%d-%m-%Y")
df$date_occ  <- as.Date(df$date_occ, format="%d-%m-%Y")
df$temp_max <- as.numeric(df$temp_max)
df$temp_min <- as.numeric(df$temp_min)
df$rain     <- as.numeric(df$rain)
df$vict_age <- as.numeric(df$vict_age)
df$time_occ <- as.numeric(df$time_occ)
df$area_name   <- as.factor(df$area_name)
df$season      <- as.factor(df$season)
df$temp_level  <- as.factor(df$temp_level)
df$rain_flag   <- as.factor(df$rain_flag)
df$time_period <- as.factor(df$time_period)
df$weekday     <- as.factor(df$weekday)
df$vict_sex    <- as.factor(df$vict_sex)
df$vict_descent<- as.factor(df$vict_descent)
str(df)


summary
df %>%
  select(temp_max, temp_min, rain, vict_age, time_occ) %>%
  summary()
table(df$season)
table(df$rain_flag)
table(df$time_period)
table(df$area_name)
colSums(is.na(df))


#corrolation
num_df <- df %>%
  select(temp_max, temp_min, rain, vict_age, time_occ)
str(num_df)

cor_matrix <- cor(num_df, use = "complete.obs", method = "pearson")
cor_matrix

#outlier detection

GGally::ggcorr(num_df, label = TRUE, label_round = 2)
ggplot(df, aes(temp_max)) +
  geom_histogram(bins=30, fill="steelblue") +
  theme_minimal()

df <- df %>% filter(vict_age > 0)


ggplot(df, aes(vict_age)) +
  geom_histogram(bins=30, fill="orange") +
  theme_minimal()


#boxplots

ggplot(df, aes(y=temp_max)) +
  geom_boxplot(fill="red") +
  theme_minimal()

ggplot(df, aes(y=vict_age)) +
  geom_boxplot(fill="green") +
  theme_minimal()




#visualizations

ggplot(df, aes(season)) +
  geom_bar(fill="purple") +
  theme_minimal() +
  labs(title="Crime Count by Season")

#
ggplot(df, aes(temp_level)) +
  geom_bar(fill="tomato") +
  theme_minimal() +
  labs(title="Crime vs Temperature Level")



ggplot(df, aes(rain_flag)) +
  geom_bar(fill="skyblue") +
  theme_minimal() +
  labs(title="Crime: Rain vs No Rain")


ggplot(df, aes(time_period)) +
  geom_bar(fill="darkgreen") +
  theme_minimal() +
  labs(title="Crime by Time of Day")

ggplot(df, aes(area_name)) +
  geom_bar(fill="brown") +
  coord_flip() +
  theme_minimal() +
  labs(title="Crime by Area")

#scatter plots

ggplot(df, aes(temp_max, rain)) +
  geom_point(alpha=0.4) +
  theme_minimal() +
  labs(title="Temperature vs Rain")

ggplot(df, aes(time_period, vict_age)) +
  geom_boxplot(fill="cyan") +
  theme_minimal()

ggplot(df, aes(month)) +
  geom_bar(fill="gold") +
  theme_minimal() +
  labs(title="Monthly Crime Trend")


