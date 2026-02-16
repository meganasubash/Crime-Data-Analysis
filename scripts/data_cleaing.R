############################################################
# CRIME + WEATHER DATA CLEANING SCRIPT
############################################################

install.packages('stringr')

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)


############################################################
# LOAD DATA
############################################################
df <- fread("C:/Users/MS994/Downloads/pds/crime_weather_update.csv")

############################################################
# REMOVE DUPLICATES
############################################################
df <- unique(df)

############################################################
# FIX DATE COLUMNS (REMOVE TIME)
############################################################
df$`Date Rptd` <- as.Date(
  as.POSIXct(df$`Date Rptd`, format="%m/%d/%Y %I:%M:%S %p")
)

df$DATE_OCC <- as.Date(df$DATE_OCC)

############################################################
# CREATE DATE FEATURES
############################################################
df$year  <- year(df$DATE_OCC)
df$month <- month(df$DATE_OCC)
df$day   <- day(df$DATE_OCC)
df$weekday <- weekdays(df$DATE_OCC)

############################################################
# FIX TIME COLUMN PROPERLY (CRITICAL)
############################################################

############################################################
# REBUILD TIME FROM ORIGINAL COLUMN "TIME OCC"
############################################################

df$`TIME OCC` <- as.character(df$`TIME OCC`)
df$`TIME OCC` <- stringr::str_pad(df$`TIME OCC`, width=4, side="left", pad="0")

df$hour <- as.numeric(substr(df$`TIME OCC`,1,2))*100 +
  as.numeric(substr(df$`TIME OCC`,3,4))

df$time_period <- cut(
  df$hour,
  breaks=c(-1,600,1200,1800,2400),
  labels=c("Night","Morning","Afternoon","Evening")
)



############################################################
# FIX VICTIM AGE COLUMN (CORRECT NAME)
############################################################
df$`Vict Age` <- as.numeric(df$`Vict Age`)
df$`Vict Age`[df$`Vict Age` <= 0 | df$`Vict Age` > 100] <- NA

df$age_group <- cut(
  df$`Vict Age`,
  breaks=c(0,18,30,50,70,Inf),
  labels=c("Child","Young","Adult","Senior","Elder")
)

############################################################
# FIX WEATHER NUMERIC
############################################################
df$temp_max <- as.numeric(df$temp_max)
df$temp_min <- as.numeric(df$temp_min)
df$rain     <- as.numeric(df$rain)

############################################################
# HANDLE WEAPON MISSING
############################################################
df$`Weapon Desc`[is.na(df$`Weapon Desc`)] <- "None"
df$`Weapon Used Cd`[is.na(df$`Weapon Used Cd`)] <- 0

############################################################
# HANDLE CRIME CODE MISSING
############################################################
df$`Crm Cd 2`[is.na(df$`Crm Cd 2`)] <- 0
df$`Crm Cd 3`[is.na(df$`Crm Cd 3`)] <- 0
df$`Crm Cd 4`[is.na(df$`Crm Cd 4`)] <- 0

############################################################
# HANDLE PREMISE CODE MISSING
############################################################
df$`Premis Cd`[is.na(df$`Premis Cd`)] <- 0

############################################################
# SEASON FEATURE
############################################################
df$season <- case_when(
  df$month %in% c(12,1,2) ~ "Winter",
  df$month %in% c(3,4,5) ~ "Spring",
  df$month %in% c(6,7,8) ~ "Summer",
  TRUE ~ "Fall"
)

############################################################
# WEATHER FEATURES
############################################################
df$rain_flag <- ifelse(df$rain > 0, "Rain", "No Rain")

df$temp_level <- cut(
  df$temp_max,
  breaks=c(-Inf,15,25,35,Inf),
  labels=c("Cold","Mild","Hot","Extreme")
)

############################################################
# FINAL NA CHECK
############################################################
colSums(is.na(df))

############################################################
# SAVE CLEAN DATA
############################################################
fwrite(
  df,
  "C:/Users/MS994/Downloads/crime_weather_cleaned_final.csv"
)
