# Exploratory Data Analysis Script
# libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(corrplot)

# Set directories
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "/data/outputs")
# Create output directories if they don't exist
PLOTS_DIR  <- file.path(OUTPUT_DIR, "plots")
# ensure plots directory exists
dir.create(PLOTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Load cleaned data
INPUT_CSV <- file.path(OUTPUT_DIR, "crime_weather_cleaned2.csv")
cat("Loading data from:", INPUT_CSV, "\n")
# Read CSV with appropriate types
df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)

# Convert date and time columns
df$DATE_OCC  <- as.Date(df$DATE_OCC)
df$TIME.OCC  <- as.numeric(df$TIME.OCC)

# Helper to save plots with consistent settings and print confirmation 
save_plot <- function(p, filename, w = 10, h = 6) {
  ggsave(file.path(PLOTS_DIR, filename), plot = p, width = w, height = h)
  cat("  Saved:", filename, "\n")
}

# 1. Crime by Hour
p1 <- df %>%
  mutate(hour = as.numeric(substr(sprintf("%04d", TIME.OCC), 1, 2))) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line() + geom_point() +
  labs(title = "Crime Distribution by Hour", x = "Hour of Day", y = "Number of Crimes") +
  theme_minimal()
save_plot(p1, "01_crime_by_hour.png")

#  2. Crime by Time Period 
p2 <- df %>%
  count(TIME_PERIOD) %>%
  ggplot(aes(x = TIME_PERIOD, y = n, fill = TIME_PERIOD)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Time Period") +
  theme_minimal()
save_plot(p2, "02_crime_by_time_period.png")

#  3. Crime by Weekday 
p3 <- df %>%
  count(WEEKDAY) %>%
  ggplot(aes(x = reorder(WEEKDAY, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime by Weekday", x = "Day", y = "Count") +
  theme_minimal()
save_plot(p3, "03_crime_by_weekday.png")

#  4. Top 10 Crime Areas 
p4 <- df %>%
  count(AREA.NAME, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(AREA.NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Crime Areas", x = "Area", y = "Crime Count") +
  theme_minimal()
save_plot(p4, "04_top10_areas.png")

#  5. Crime Location Scatter 
p5 <- ggplot(df, aes(x = LON, y = LAT)) +
  geom_point(alpha = 0.2, color = "red") +
  labs(title = "Crime Location Scatter Plot") +
  theme_minimal()
save_plot(p5, "05_crime_scatter_map.png")

#  6. Victim Age Distribution 
plot_df <- df %>% filter(VICT_AGE > 0)   # exclude -1 sentinel
p6 <- ggplot(plot_df, aes(x = VICT_AGE)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Victim Age Distribution") +
  theme_minimal()
save_plot(p6, "06_victim_age_dist.png")

#  7. Crime by Age Group 
p7 <- df %>%
  count(AGE_GROUP) %>%
  ggplot(aes(x = AGE_GROUP, y = n, fill = AGE_GROUP)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Age Group") +
  theme_minimal()
save_plot(p7, "07_crime_by_age_group.png")

#  8. Crime by Gender 
p8 <- df %>%
  count(VICT_SEX) %>%
  ggplot(aes(x = VICT_SEX, y = n, fill = VICT_SEX)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Gender") +
  theme_minimal()
save_plot(p8, "08_crime_by_gender.png")

#  9. Top Weapons 
p9 <- df %>%
  count(WEAPON.DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(WEAPON.DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Weapons Used") +
  theme_minimal()
save_plot(p9, "09_top_weapons.png")

#  10. Top Premises 
p10 <- df %>%
  count(PREMIS.DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(PREMIS.DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top Crime Premises") +
  theme_minimal()
save_plot(p10, "10_top_premises.png")

#  11. Rain vs Crime 
p11 <- df %>%
  count(RAIN_FLAG) %>%
  ggplot(aes(x = RAIN_FLAG, y = n, fill = RAIN_FLAG)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime: Rain vs No Rain") +
  theme_minimal()
save_plot(p11, "11_rain_vs_crime.png")

#  12. Temperature Level vs Crime 
p12 <- df %>%
  count(TEMP_LEVEL) %>%
  ggplot(aes(x = TEMP_LEVEL, y = n, fill = TEMP_LEVEL)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Temperature Level") +
  theme_minimal()
save_plot(p12, "12_crime_by_temp_level.png")

#  13. Top Crime Types 
p13 <- df %>%
  count(CRM.CD.DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(CRM.CD.DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Crime Types") +
  theme_minimal()
save_plot(p13, "13_top_crime_types.png")

#  14. Correlation Heatmap 
numeric_df <- df %>%
  select(TEMP_MAX, TEMP_MIN, RAIN, VICT_AGE) %>%
  filter(VICT_AGE > 0) %>%
  na.omit()

# Compute correlation matrix and plot heatmap
cor_matrix <- cor(numeric_df)
png(file.path(PLOTS_DIR, "14_correlation_heatmap.png"), width = 800, height = 700)
corrplot(cor_matrix, method = "color", addCoef.col = "black")
dev.off()
cat("  Saved: 14_correlation_heatmap.png\n")

cat("EDA complete. All plots saved to:", PLOTS_DIR, "\n")