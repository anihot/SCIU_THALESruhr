library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# Config
events_file <- "data/detected_events.csv"
precip_file <- "data/precipitation_at_sensors.csv"
output_file <- "data/event_correlation_summary.csv"
LEAD_IN_HOURS <- 3 # Look at rain 3 hours before event start

cat("🔗 Starting Event Correlation with Precipitation Data...\n")

if (!file.exists(events_file) || !file.exists(precip_file)) {
    stop("Missing required input files: events or precipitation data.")
}

# 1. Load Data
events <- read_csv(events_file, show_col_types = FALSE)
precip <- read_csv(precip_file, show_col_types = FALSE)

# Ensure timestamps are POSIXct
events <- events %>%
    mutate(
        start_time = as.POSIXct(start_time),
        end_time = as.POSIXct(end_time)
    )

precip <- precip %>%
    mutate(timestamp = as.POSIXct(timestamp))

# 2. Correlate
# For each event, find the total rain in its window + lead-in
correlation <- events %>%
    rowwise() %>%
    mutate(
        window_start = start_time - hours(LEAD_IN_HOURS),
        window_end = end_time,

        # Filter precip for this station and timeframe
        event_precip = list(
            precip %>%
                filter(
                    station == .data$station,
                    timestamp >= .data$window_start,
                    timestamp <= .data$window_end
                )
        ),
        total_precip_mm = sum(event_precip$precipitation_mm, na.rm = TRUE),
        max_intensity_mm_h = ifelse(nrow(event_precip) > 0, max(event_precip$precipitation_mm, na.rm = TRUE), 0),
        rain_detected = ifelse(total_precip_mm > 0, TRUE, FALSE)
    ) %>%
    select(-event_precip, -window_start, -window_end) %>%
    ungroup()

# 3. Export
write_excel_csv(correlation, output_file)

cat("SUCCESS: Correlation summary saved to", output_file, "\n")
cat("Events with detected rain:", sum(correlation$rain_detected), "/", nrow(correlation), "\n")
