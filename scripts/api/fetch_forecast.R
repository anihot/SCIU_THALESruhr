library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Config
# Center of Bochum (covering most sensors)
LAT <- 51.48
LON <- 7.21
OUTPUT_FILE <- "data/processed/weather_forecast.csv"

cat("🌤 Fetching weather forecast from Open-Meteo...\n")

# Build URL
# We want hourly precipitation for the next 7 days (default)
url <- paste0(
    "https://api.open-meteo.com/v1/forecast?",
    "latitude=", LAT,
    "&longitude=", LON,
    "&hourly=precipitation,precipitation_probability,temperature_2m",
    "&timezone=Europe%2FBerlin"
)

# Request
response <- GET(url)

if (status_code(response) != 200) {
    stop("Failed to fetch forecast from Open-Meteo. Status: ", status_code(response))
}

# Parse
data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Extract hourly data
hourly <- data$hourly
forecast_df <- data.frame(
    timestamp = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "Europe/Berlin"),
    precipitation_mm = hourly$precipitation,
    probability_percent = hourly$precipitation_probability,
    temp_c = hourly$temperature_2m
)

# Filter for next 72 hours for cleaner overview in README/Map if needed,
# but keep full 7 days in CSV for analysis
# forecast_df <- forecast_df %>% filter(timestamp >= now())

# Save
if (!dir.exists(dirname(OUTPUT_FILE))) {
    dir.create(dirname(OUTPUT_FILE), recursive = TRUE)
}

write_csv(forecast_df, OUTPUT_FILE)

cat("SUCCESS: Forecast saved to", OUTPUT_FILE, "\n")
cat(
    "Next major rain expected at:",
    forecast_df %>%
        filter(precipitation_mm > 0) %>%
        slice(1) %>%
        pull(timestamp) %>%
        as.character() %||% "No rain forecast",
    "\n"
)
