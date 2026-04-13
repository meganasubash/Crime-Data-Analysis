#  UNIFIED DATA CLEANING & FEATURE ENGINEERING
#  Input  : crime_weather_update.csv   (from api_weather_merge.R)
#  Output : crime_cleaned.csv          (ready for EDA + modelling)

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "/data/outputs")
INPUT_CSV  <- file.path(OUTPUT_DIR, "crime_weather_update.csv")
OUTPUT_CSV <- file.path(OUTPUT_DIR, "crime_cleaned.csv")

cat("=== UNIFIED DATA CLEANING ===\n")
cat("Reading:", INPUT_CSV, "\n")


# 1. LOAD
 
df <- fread(INPUT_CSV)
cat("Raw dimensions:", nrow(df), "x", ncol(df), "\n")

 
# 2. DROP COLUMNS BEFORE ANYTHING ELSE
#    Reason documented for every drop.
 

# Date Rptd  – when the crime was *reported*, not when it happened.
#              DATE_OCC (occurrence) is what matters analytically.
# Rpt Dist No – fine-grained sub-district code (1135 values).
#               AREA (21 divisions) is the right geographic grain.
# Crm Cd 1   – duplicate of Crm Cd in 99.7 % of rows.
# Crm Cd 2   – 91.7 % null; secondary charge rarely present.
# Crm Cd 3   – 99.7 % null. Useless.
# Crm Cd 4   – 100 % null in practice. Useless.
# Weapon Used Cd – numeric code; Weapon Desc carries same info as text.
# Premis Cd  – numeric code; Premis Desc carries same info as text.
# Cross Street   – 83.9 % null; not reliable enough to use.
# DATE OCC   – raw string column; we already have the parsed DATE_OCC.
# LOCATION   – free-text address; LAT/LON give the same info cleanly.

drop_cols <- c(
  "Date Rptd",
  "Rpt Dist No",
  "Crm Cd 1", "Crm Cd 2", "Crm Cd 3", "Crm Cd 4",
  "Weapon Used Cd",
  "Premis Cd",
  "Cross Street",
  "DATE OCC",
  "LOCATION"
)

# Only drop what actually exists (safe for re-runs)
drop_cols <- intersect(drop_cols, names(df))
df <- df[, (drop_cols) := NULL]

cat("After column drop:", nrow(df), "x", ncol(df), "\n")

 
# 3. REMOVE EXACT DUPLICATES
 
before <- nrow(df)
df <- unique(df)
cat("Removed", before - nrow(df), "exact duplicate rows\n")

 
# 4. DATE & TIME
 

# DATE_OCC: coerce to proper Date
df$DATE_OCC <- as.Date(df$DATE_OCC)

# Drop any rows where occurrence date couldn't be parsed
df <- df[!is.na(DATE_OCC)]

# TIME OCC is stored as HHMM integer (e.g. 845 = 08:45, 2130 = 21:30).
# Extract real hour (0-23) and minute.
df$TIME_OCC <- as.integer(df$`TIME OCC`)
df[, `TIME OCC` := NULL]                       # remove original spacing variant

df$HOUR   <- df$TIME_OCC %/% 100L              # 0-23
df$MINUTE <- df$TIME_OCC %% 100L               # 0-59

# Clamp any bad values
df$HOUR[df$HOUR > 23]     <- NA
df$MINUTE[df$MINUTE > 59] <- NA

# Derived date parts
df$YEAR    <- year(df$DATE_OCC)
df$MONTH   <- month(df$DATE_OCC)
df$DAY     <- mday(df$DATE_OCC)
df$WEEKDAY <- weekdays(df$DATE_OCC)

# Time of day bucket (based on real hour)
df$TIME_PERIOD <- cut(
  df$HOUR,
  breaks = c(-1, 5, 11, 17, 23),
  labels = c("Night", "Morning", "Afternoon", "Evening"),
  right  = TRUE
)

# Season
df$SEASON <- case_when(
  df$MONTH %in% c(12, 1, 2) ~ "Winter",
  df$MONTH %in% c(3, 4, 5)  ~ "Spring",
  df$MONTH %in% c(6, 7, 8)  ~ "Summer",
  TRUE                       ~ "Fall"
)

 
# 5. GPS COORDINATES
#    LAT = 0 / LON = 0 means "location unknown" in LAPD data.
 
df$LAT[df$LAT == 0] <- NA
df$LON[df$LON == 0] <- NA

 
# 6. VICTIM AGE
#    LAPD records age = 0 for crimes where victim age is
#    not applicable (e.g. vehicle theft, property crime).
#    Negative ages are data entry errors.
 
df$VICT_AGE <- as.integer(df$`Vict Age`)
df[, `Vict Age` := NULL]

df$VICT_AGE[df$VICT_AGE <= 0 | df$VICT_AGE > 120] <- NA   # treat as unknown

df$AGE_GROUP <- cut(
  df$VICT_AGE,
  breaks = c(0, 17, 29, 49, 69, Inf),
  labels = c("Child", "Young Adult", "Adult", "Senior", "Elder"),
  right  = TRUE
)
df$AGE_GROUP <- as.character(df$AGE_GROUP)
df$AGE_GROUP[is.na(df$AGE_GROUP)] <- "Unknown"

 
# 7. VICTIM SEX
 
df$VICT_SEX <- case_when(
  df$`Vict Sex` %in% c("M", "m")             ~ "Male",
  df$`Vict Sex` %in% c("F", "f")             ~ "Female",
  is.na(df$`Vict Sex`) |
    df$`Vict Sex` %in% c("X", "H", "-", "") ~ "Unknown",
  TRUE                                        ~ "Unknown"
)
df[, `Vict Sex` := NULL]

 
# 8. VICTIM DESCENT  (all 19 official LAPD codes)
 
df$VICT_DESCENT <- case_when(
  df$`Vict Descent` == "H" ~ "Hispanic",
  df$`Vict Descent` == "W" ~ "White",
  df$`Vict Descent` == "B" ~ "Black",
  df$`Vict Descent` == "A" ~ "Other Asian",
  df$`Vict Descent` == "C" ~ "Chinese",
  df$`Vict Descent` == "D" ~ "Cambodian",
  df$`Vict Descent` == "F" ~ "Filipino",
  df$`Vict Descent` == "G" ~ "Guamanian",
  df$`Vict Descent` == "I" ~ "American Indian",
  df$`Vict Descent` == "J" ~ "Japanese",
  df$`Vict Descent` == "K" ~ "Korean",
  df$`Vict Descent` == "L" ~ "Laotian",
  df$`Vict Descent` == "O" ~ "Other",
  df$`Vict Descent` == "P" ~ "Pacific Islander",
  df$`Vict Descent` == "S" ~ "Samoan",
  df$`Vict Descent` == "U" ~ "Hawaiian",
  df$`Vict Descent` == "V" ~ "Vietnamese",
  df$`Vict Descent` == "Z" ~ "Asian Indian",
  TRUE                      ~ "Unknown"      # covers X, NA, blank
)
df[, `Vict Descent` := NULL]

 
# 9. WEAPON
#    65 % of records have no weapon → fill with "None".
 
df$WEAPON_DESC <- as.character(df$`Weapon Desc`)
df$WEAPON_DESC[is.na(df$WEAPON_DESC) | df$WEAPON_DESC == ""] <- "None"
df[, `Weapon Desc` := NULL]

# Binary flag – simpler for models
df$WEAPON_USED <- ifelse(df$WEAPON_DESC == "None", 0L, 1L)

 
# 10. PREMISE
 
df$PREMIS_DESC <- as.character(df$`Premis Desc`)
df$PREMIS_DESC[is.na(df$PREMIS_DESC) | df$PREMIS_DESC == ""] <- "Unknown"
df[, `Premis Desc` := NULL]

 
# 11. MOCODES (method-of-operation codes)
#     Keep only the count – the raw string is not directly
#     usable by standard models.
 
df$MOCODES <- str_squish(as.character(df$Mocodes))
df$MOCODES[is.na(df$MOCODES) | df$MOCODES == ""] <- "None"
df$MOCODE_COUNT <- ifelse(
  df$MOCODES == "None",
  0L,
  str_count(df$MOCODES, "\\d{4}")
)
df[, Mocodes := NULL]   # raw string no longer needed

 
# 12. CRIME CLASSIFICATION
 

# Primary text description – keep as-is
df$CRM_CD_DESC <- as.character(df$`Crm Cd Desc`)
df[, `Crm Cd Desc` := NULL]

# Numeric crime code – keep (useful as a numeric feature / join key)
setnames(df, "Crm Cd", "CRM_CD")

# Binary severity target derived from Part 1-2
#   Part 1: serious / violent crimes (FBI UCR Part I)
#   Part 2: minor / non-violent offences
df$CRIME_SEVERITY <- ifelse(df$`Part 1-2` == 1L, "Serious", "Minor")
df[, `Part 1-2` := NULL]          # encoded into CRIME_SEVERITY

 
# 13. CASE STATUS
 
df$STATUS      <- as.character(df$Status)
df$STATUS_DESC <- as.character(df$`Status Desc`)
df[, Status      := NULL]
df[, `Status Desc` := NULL]

# Simpler "arrested" flag for modelling
df$ARRESTED <- ifelse(df$STATUS %in% c("AA", "JA"), 1L, 0L)

 
# 14. WEATHER  (joined from Open-Meteo in api_weather_merge.R)
 
df$TEMP_MAX <- as.numeric(df$temp_max)
df$TEMP_MIN <- as.numeric(df$temp_min)
df$RAIN     <- as.numeric(df$rain)
df[, c("temp_max", "temp_min", "rain") := NULL]

# Categorical weather features
df$RAIN_FLAG <- ifelse(is.na(df$RAIN) | df$RAIN == 0, "No Rain", "Rain")

df$TEMP_CATEGORY <- cut(
  df$TEMP_MAX,
  breaks = c(-Inf, 10, 20, 30, Inf),
  labels = c("Cold", "Mild", "Warm", "Hot"),
  right  = TRUE
)

 
# 15. AREA / GEOGRAPHY
 
# AREA is the numeric division code; AREA NAME is its label.
# Keep both but rename for clarity.
setnames(df, "AREA",      "AREA_CD")
setnames(df, "AREA NAME", "AREA_NAME")

 
# 16. RENAME REMAINING RAW COLUMNS FOR CONSISTENCY
 
setnames(df, "DR_NO",    "DR_NO")     # unique case ID – keep
setnames(df, "MOCODES",  "MOCODES")   # already set

 
# 17. FINAL COLUMN ORDER
 
final_cols <- c(
  # ── Case identifier ──────────────────────────────────────
  "DR_NO",

  # ── Date & Time ───────────────────────────────────────────
  "DATE_OCC", "YEAR", "MONTH", "DAY", "WEEKDAY", "SEASON",
  "TIME_OCC", "HOUR", "MINUTE", "TIME_PERIOD",

  # ── Geography ─────────────────────────────────────────────
  "AREA_CD", "AREA_NAME",
  "LAT", "LON",

  # ── Crime classification ──────────────────────────────────
  "CRM_CD", "CRM_CD_DESC",
  "CRIME_SEVERITY",           # Serious / Minor  (target variable)
  "MOCODES", "MOCODE_COUNT",

  # ── Premise & Weapon ─────────────────────────────────────
  "PREMIS_DESC",
  "WEAPON_DESC", "WEAPON_USED",

  # ── Case status ───────────────────────────────────────────
  "STATUS", "STATUS_DESC", "ARRESTED",

  # ── Victim demographics ───────────────────────────────────
  "VICT_AGE", "AGE_GROUP",
  "VICT_SEX", "VICT_DESCENT",

  # ── Weather ───────────────────────────────────────────────
  "TEMP_MAX", "TEMP_MIN", "TEMP_CATEGORY",
  "RAIN", "RAIN_FLAG"
)

# Keep only columns that actually exist (guards against re-runs)
final_cols <- intersect(final_cols, names(df))
df <- df[, ..final_cols]

 
# 18. FINAL ROW-LEVEL QUALITY FILTER
#    Remove records that are too incomplete to be useful:
#    missing occurrence date (already filtered above) or
#    GPS coordinate exactly 0 with no area info.
 
before <- nrow(df)
df <- df[!is.na(DATE_OCC)]       # already clean but guard
cat("Rows dropped in final QC:", before - nrow(df), "\n")

# 19. SUMMARY REPORT
 
cat("\n=== FINAL DATASET SUMMARY ===\n")
cat("Rows   :", nrow(df), "\n")
cat("Columns:", ncol(df), "\n\n")

cat("── Null counts ──\n")
null_counts <- sapply(df, function(x) sum(is.na(x)))
print(null_counts[null_counts > 0])

cat("\n── CRIME_SEVERITY distribution ──\n")
print(table(df$CRIME_SEVERITY))

cat("\n── TIME_PERIOD distribution ──\n")
print(table(df$TIME_PERIOD, useNA = "always"))

cat("\n── AGE_GROUP distribution ──\n")
print(table(df$AGE_GROUP))

cat("\n── VICT_SEX distribution ──\n")
print(table(df$VICT_SEX))

cat("\n── TEMP_CATEGORY distribution ──\n")
print(table(df$TEMP_CATEGORY, useNA = "always"))

cat("\n── RAIN_FLAG distribution ──\n")
print(table(df$RAIN_FLAG))

cat("\n── WEAPON_USED distribution ──\n")
print(table(df$WEAPON_USED))

 
# 20. SAVE
 
fwrite(df, OUTPUT_CSV)
cat("\nCleaned data saved to:", OUTPUT_CSV, "\n")
cat("=== CLEANING COMPLETE ===\n")