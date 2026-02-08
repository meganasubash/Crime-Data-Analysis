library(data.table)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)

crime_data <- fread("C:/Users/MS994/Downloads/Crime_Data_from_2020_to_Present.csv")

crime_data$DATE_OCC <- as.POSIXct(
  crime_data$`DATE OCC`,
  format="%m/%d/%Y %I:%M:%S %p"
)

crime_data$DATE_OCC <- as.Date(crime_data$DATE_OCC)

all_dates <- sort(unique(crime_data$DATE_OCC))

get_weather <- function(date){
  
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=34.05&longitude=-118.24",
    "&start_date=", date,
    "&end_date=", date,
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum",
    "&timezone=America/Los_Angeles"
  )
  
  res <- GET(url)
  txt <- content(res, "text", encoding="UTF-8")
  data <- fromJSON(txt)
  
  if(is.null(data$daily)) return(NULL)
  
  data.frame(
    DATE_OCC = as.Date(date),
    temp_max = data$daily$temperature_2m_max[1],
    temp_min = data$daily$temperature_2m_min[1],
    rain = data$daily$precipitation_sum[1]
  )
}

weather_list <- list()


weather_list <- list()

for(i in seq_along(all_dates)){
  
  cat("API call:", all_dates[i], "\n")
  
  try({
    
    res <- GET(
      paste0(
        "https://archive-api.open-meteo.com/v1/archive?",
        "latitude=34.05&longitude=-118.24",
        "&start_date=", all_dates[i],
        "&end_date=", all_dates[i],
        "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum",
        "&timezone=America/Los_Angeles"
      ),
      add_headers(
        "User-Agent" = "R-Student-Project",
        "Accept" = "application/json"
      )
    )
    
    txt <- content(res, "text", encoding="UTF-8")
    data <- fromJSON(txt)
    
    if(!is.null(data$daily)){
      df <- data.frame(
        DATE_OCC = as.Date(all_dates[i]),
        temp_max = data$daily$temperature_2m_max[1],
        temp_min = data$daily$temperature_2m_min[1],
        rain = data$daily$precipitation_sum[1]
      )
      
      weather_list[[length(weather_list)+1]] <- df
    }
    
  }, silent = TRUE)
  
  Sys.sleep(0.2)
}


weather_df <- bind_rows(weather_list)
dim(weather_df)


crime_merged <- left_join(crime_data, weather_df, by="DATE_OCC")

fwrite(
  crime_merged,
  "C:/Users/MS994/Downloads/crime_weather_update.csv"
)
dim(weather_df)
head(weather_df)
