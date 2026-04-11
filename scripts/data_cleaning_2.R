
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
#df <- fread("C:/Users/MS994/Downloads/pds/crime_weather_cleaned_final.csv")

OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "/data/outputs")

INPUT_CSV  <- file.path(OUTPUT_DIR, "crime_weather_cleaned_final.csv")
OUTPUT_CSV <- file.path(OUTPUT_DIR, "crime_weather_cleaned2.csv")



# LOAD DATA

df <- fread(INPUT_CSV)


# STANDARDIZE COLUMN NAMES

names(df) <- toupper(names(df))


# CLEAN LOCATION (SINGLE PASS)
##########
df <- df %>%
  mutate(
    LOCATION = str_squish(LOCATION),
    HOUSE_NO = str_extract(LOCATION, "^[0-9]+"),
    STREET_TYPE = word(LOCATION, -1),
    STREET_NAME = LOCATION %>%
      str_remove("^[0-9]+\\s*") %>%
      str_remove(paste0("\\s", STREET_TYPE, "$"))
  )


# CLEAN VICTIM DATA

df <- df %>%
  mutate(
    # Age cleaning
    VICT_AGE = as.numeric(`VICT AGE`),
    VICT_AGE = case_when(
      is.na(VICT_AGE) ~ -1,
      VICT_AGE <= 0 ~ -1,
      VICT_AGE > 120 ~ -1,
      TRUE ~ VICT_AGE
    ),
    
    # Sex standardization
    VICT_SEX = case_when(
      `VICT SEX` %in% c("M", "m") ~ "M",
      `VICT SEX` %in% c("F", "f") ~ "F",
      TRUE ~ "Unknown"
    ),
    
    # Descent mapping
    VICT_DESCENT = case_when(
      `VICT DESCENT` == "H" ~ "Hispanic",
      `VICT DESCENT` == "W" ~ "White",
      `VICT DESCENT` == "B" ~ "Black",
      `VICT DESCENT` == "A" ~ "Asian",
      `VICT DESCENT` == "O" ~ "Other",
      TRUE ~ "Unknown"
    ),
    
    # Age group fix
    AGE_GROUP = ifelse(is.na(AGE_GROUP) | AGE_GROUP == "", "Unknown", AGE_GROUP)
  ) %>%
  select(-`VICT AGE`, -`VICT SEX`, -`VICT DESCENT`)

# CLEAN MOCODES + FEATURE ENGINEERING

df <- df %>%
  mutate(
    MOCODES = str_squish(MOCODES),
    MOCODES = ifelse(is.na(MOCODES) | MOCODES == "", "None", MOCODES),
    MOCODE_COUNT = ifelse(
      MOCODES == "None",
      0,
      str_count(MOCODES, "\\d{4}")
    )
  )

# FINAL COLUMN SELECTION (STRUCTURED)

df <- df %>%
  select(
    # ---- CASE ----
    DR_NO, `DATE RPTD`,
    
    # ---- GEO ----
    AREA, `AREA NAME`, `RPT DIST NO`,
    LAT, LON,
    
    # ---- LOCATION ----
    LOCATION, HOUSE_NO, STREET_NAME, STREET_TYPE,
    `CROSS STREET`, `PREMIS CD`, `PREMIS DESC`,
    
    # ---- TIME ----
    DATE_OCC, YEAR, MONTH, DAY, WEEKDAY,
    `TIME OCC`, TIME_PERIOD, SEASON,
    
    # ---- CRIME ----
    `PART 1-2`,
    `CRM CD`, `CRM CD DESC`,
    `CRM CD 1`, `CRM CD 2`, `CRM CD 3`, `CRM CD 4`,
    MOCODES, MOCODE_COUNT,
    
    # ---- WEAPON ----
    `WEAPON USED CD`, `WEAPON DESC`,
    
    # ---- STATUS ----
    STATUS, `STATUS DESC`,
    
    # ---- VICTIM ----
    VICT_AGE, AGE_GROUP, VICT_SEX, VICT_DESCENT,
    
    # ---- WEATHER ----
    TEMP_MAX, TEMP_MIN, TEMP_LEVEL,
    RAIN, RAIN_FLAG
  )

fwrite(df, OUTPUT_CSV)