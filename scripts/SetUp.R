# Install (only once)
install.packages(c("tidyverse","lubridate","ggplot2","scales"))

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
