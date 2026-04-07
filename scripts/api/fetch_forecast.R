library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
OUTPUT_FILE   <- "data/processed/weather_forecast.csv"

cat("🌤 Fetching per-sensor weather forecasts from Open-Meteo...\n")

# 1. Sensor-Koordinaten laden
if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
sensors <- read_csv(metadata_file, show_col_types = FALSE)

cat("  Sensors:", nrow(sensors), "\n")

# 2. Pro Sensor abfragen
all_forecasts <- list()

for (i in seq_len(nrow(sensors))) {
    station_name <- sensors$station[i]
    lat <- sensors$lat[i]
    lon <- sensors$lon[i]

    cat("  Fetching forecast for", station_name, "(", lat, ",", lon, ")...\n")

    url <- paste0(
        "https://api.open-meteo.com/v1/forecast?",
        "latitude=", lat,
        "&longitude=", lon,
        "&hourly=precipitation,precipitation_probability,temperature_2m",
        "&timezone=Europe%2FBerlin"
    )

    # Request with retry logic for transient errors
    max_retries <- 3
    response <- NULL
    for (attempt in seq_len(max_retries)) {
        tryCatch({
            response <- GET(url, timeout(30))
            if (status_code(response) >= 500) {
                cat("    Attempt", attempt, "failed: HTTP", status_code(response), "\n")
                response <- NULL
                if (attempt < max_retries) Sys.sleep(5 * attempt)
            } else {
                break
            }
        }, error = function(e) {
            cat("    Attempt", attempt, "failed:", conditionMessage(e), "\n")
            if (attempt < max_retries) Sys.sleep(5 * attempt)
        })
    }

    if (is.null(response) || status_code(response) != 200) {
        cat("    WARNING: Skipping", station_name, "after", max_retries, "attempts\n")
        next
    }

    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    hourly <- data$hourly

    forecast_df <- data.frame(
        station = station_name,
        timestamp = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "Europe/Berlin"),
        precipitation_mm = hourly$precipitation,
        probability_percent = hourly$precipitation_probability,
        temp_c = hourly$temperature_2m
    )

    all_forecasts[[i]] <- forecast_df
    cat("    OK:", nrow(forecast_df), "hourly values\n")

    # Kurze Pause zwischen Requests (API fair use)
    if (i < nrow(sensors)) Sys.sleep(0.5)
}

# 3. Zusammenführen und speichern
combined <- bind_rows(all_forecasts)

if (!dir.exists(dirname(OUTPUT_FILE))) {
    dir.create(dirname(OUTPUT_FILE), recursive = TRUE)
}

if (nrow(combined) == 0) {
    if (file.exists(OUTPUT_FILE)) {
        cat("\nWARNING: All sensors failed. Keeping existing forecast file.\n")
    } else {
        cat("\nWARNING: All sensors failed and no existing forecast file available.\n")
    }
    quit(status = 0)
}

write_csv(combined, OUTPUT_FILE)

n_ok <- length(unique(combined$station))
cat("\nSUCCESS: Per-sensor forecast saved to", OUTPUT_FILE, "\n")
cat("  Stations OK:", n_ok, "/", nrow(sensors), "\n")
cat("  Total rows:", nrow(combined), "\n")

# Summary: nächster Regen pro Station
for (s in unique(combined$station)) {
    next_rain <- combined %>%
        filter(station == s, precipitation_mm > 0) %>%
        slice(1)
    if (nrow(next_rain) > 0) {
        cat("  ", s, "- nächster Regen:", as.character(next_rain$timestamp), "\n")
    } else {
        cat("  ", s, "- kein Regen in der Vorhersage\n")
    }
}
