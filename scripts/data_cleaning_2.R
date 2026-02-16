library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

df <- fread("C:/Users/MS994/Downloads/pds/crime_weather_cleaned_final.csv")

# Drop any date-occurrence columns
df <- df %>%
  select(-`DATE OCC`)

# Check remaining columns
names(df)
dim(df)

#rearranging

df <- df %>%
  select(
    # ---- CASE INFO ----
    DR_NO,
    `Date Rptd`,
    AREA, `AREA NAME`, `Rpt Dist No`,
    
    # ---- TIME ----
    DATE_OCC,year, month, day, weekday,
    `TIME OCC`, hour, time_period,
    
    # ---- LOCATION ----
    LOCATION, `Cross Street`,
    LAT, LON,
    
    # ---- CRIME DETAILS ----
    `Part 1-2`,
    `Crm Cd`, `Crm Cd Desc`,
    Mocodes,
    `Premis Cd`, `Premis Desc`,
    `Weapon Used Cd`, `Weapon Desc`,
    Status, `Status Desc`,
    `Crm Cd 1`, `Crm Cd 2`, `Crm Cd 3`, `Crm Cd 4`,
    
    # ---- VICTIM ----
    `Vict Age`, age_group,
    `Vict Sex`, `Vict Descent`,
    
    # ---- WEATHER ----
    temp_max, temp_min, rain,
    rain_flag, temp_level,
    
    # ---- SEASON ----
    season
  )
names(df)
head(df)

#column names upper

names(df) <- toupper(names(df))
head(df$LOCATION, 20)
library(stringr)

df$LOCATION <- str_squish(df$LOCATION)

# house number = first number
df$HOUSE_NO <- str_extract(df$LOCATION, "^[0-9]+")

# street type = last word
df$STREET_TYPE <- word(df$LOCATION, -1)

# remove number and type
temp <- str_remove(df$LOCATION, "^[0-9]+ ")
temp <- str_remove(temp, paste0(" ", df$STREET_TYPE, "$"))

df$STREET_NAME <- temp
library(stringr)

df$LOCATION <- str_squish(df$LOCATION)

# house number = first number
df$HOUSE_NO <- str_extract(df$LOCATION, "^[0-9]+")

# street type = last word
df$STREET_TYPE <- word(df$LOCATION, -1)

# remove number and type
temp <- str_remove(df$LOCATION, "^[0-9]+ ")
temp <- str_remove(temp, paste0(" ", df$STREET_TYPE, "$"))

df$STREET_NAME <- temp

head(df[,c("LOCATION","HOUSE_NO","STREET_NAME","STREET_TYPE")],20)

# ===== VICTIM COLUMNS CLEANING =====

# 1. VICT AGE: Handle invalid/missing values
df <- df %>%
  mutate(
    VICT_AGE = as.numeric(`VICT AGE`),  # Convert to numeric (X becomes NA)
    VICT_AGE = case_when(
      is.na(VICT_AGE) ~ NA_real_,        # Keep NA as is
      VICT_AGE < 0 ~ NA_real_,            # Remove negative ages
      VICT_AGE > 120 ~ NA_real_,          # Remove unrealistic ages (>120)
      VICT_AGE == 0 ~ NA_real_,           # Remove zero ages
      TRUE ~ VICT_AGE
    )
  )

# 2. VICT SEX: Standardize and handle invalid values
df <- df %>%
  mutate(
    VICT_SEX = case_when(
      `VICT SEX` %in% c("M", "m") ~ "M",
      `VICT SEX` %in% c("F", "f") ~ "F",
      `VICT SEX` %in% c("X", "x", "-", "") ~ "Unknown",
      TRUE ~ "Unknown"
    )
  )

# 3. VICT DESCENT: Expand codes to full descriptions
# Option 1: Simplify to just "Hispanic"
df <- df %>%
  mutate(
    VICT_DESCENT = case_when(
      `VICT DESCENT` == "H" ~ "Hispanic",
      `VICT DESCENT` == "W" ~ "White",
      `VICT DESCENT` == "B" ~ "Black",
      `VICT DESCENT` == "A" ~ "Asian",
      `VICT DESCENT` == "O" ~ "Other",
      `VICT DESCENT` == "X" ~ "Unknown",
      is.na(`VICT DESCENT`) | `VICT DESCENT` == "" ~ "Unknown",
      TRUE ~ "Other"
    )
  )

View(head(df,50))
df %>% 
  select(`VICT AGE`, VICT_AGE, `VICT SEX`, VICT_SEX, 
         `VICT DESCENT`, VICT_DESCENT) %>% 
  head(20)

# Remove duplicate/original victim columns - keep only cleaned versions
df <- df %>%
  select(-`VICT AGE`, -`VICT SEX`, -`VICT DESCENT`)

# Verify the columns are removed
names(df)

# Check the cleaned victim columns
head(df %>% select(VICT_AGE, VICT_SEX, VICT_DESCENT), 20)
head(df)



# Handle missing values in AGE_GROUP
df <- df %>%
  mutate(
    AGE_GROUP = case_when(
      is.na(AGE_GROUP) | AGE_GROUP == "" ~ "Unknown",
      TRUE ~ AGE_GROUP
    )
  )

# Rearrange columns meaningfully


# Verify the rearrangement
names(df)

# Check for any missing AGE_GROUP values
table(df$AGE_GROUP, useNA = "always")

# View first few rows to confirm structure
head(df, 10)


# Remove HOUR column
df <- df %>%
  select(-HOUR)

# Recommended: Use -1 for missing ages
df <- df %>%
  mutate(
    VICT_AGE = ifelse(is.na(VICT_AGE), -1, VICT_AGE)
  
  )

# Clean MOCODES and add count feature
df <- df %>%
  mutate(
    MOCODES = str_squish(MOCODES),
    MOCODES = case_when(
      is.na(MOCODES) | MOCODES == "" ~ "None",
      TRUE ~ MOCODES
    ),
    MOCODE_COUNT = case_when(
      MOCODES == "None" ~ 0,
      TRUE ~ str_count(MOCODES, "\\d{4}")
    )
  )


df <- df %>%
  select(
    # ---- CASE IDENTIFICATION ----
    DR_NO,
    `DATE RPTD`,
    
    # ---- GEOGRAPHIC INFO ----
    AREA, `AREA NAME`, `RPT DIST NO`,
    LAT, LON,
    
    # ---- LOCATION DETAILS ----
    LOCATION, 
    HOUSE_NO, STREET_NAME, STREET_TYPE,
    `CROSS STREET`,
    `PREMIS CD`, `PREMIS DESC`,
    
    # ---- DATE & TIME ----
    DATE_OCC, YEAR, MONTH, DAY, WEEKDAY,
    `TIME OCC`, TIME_PERIOD,
    SEASON,
    
    # ---- CRIME CLASSIFICATION ----
    `PART 1-2`,
    `CRM CD`, `CRM CD DESC`,
    `CRM CD 1`, `CRM CD 2`, `CRM CD 3`, `CRM CD 4`,
    MOCODES,MOCODE_COUNT,
    
    # ---- WEAPON INFO ----
    `WEAPON USED CD`, `WEAPON DESC`,
    
    # ---- CASE STATUS ----
    STATUS, `STATUS DESC`,
    
    # ---- VICTIM DEMOGRAPHICS ----
    VICT_AGE, AGE_GROUP,
    VICT_SEX,
    VICT_DESCENT,
    
    # ---- WEATHER CONDITIONS ----
    TEMP_MAX, TEMP_MIN, TEMP_LEVEL,
    RAIN, RAIN_FLAG
  )

# Verify
summary(df$MOCODE_COUNT)
table(df$MOCODE_COUNT)
# Verify
table(df$VICT_AGE == -1)
sum(df$VICT_AGE == -1)
View(head(df,50))

# Export
fwrite(df, "C:/Users/MS994/Downloads/pds/crime_weather_cleaned2.csv")



