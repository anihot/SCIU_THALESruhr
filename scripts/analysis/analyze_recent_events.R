# analyze_recent_events.R
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# Helper config
events_file <- "data/processed/detected_events.csv"
precip_file <- "data/processed/precipitation_at_sensors.csv"
LEAD_IN_HOURS <- 3

cat("\n--- Running Ad-hoc Analysis for Last 3 Days ---\n")

# Run event detection first to ensure we have the RAW events (unfiltered by rain)
source("scripts/analysis/event_detection.R")

events <- read_csv(events_file, show_col_types = FALSE)
precip <- read_csv(precip_file, show_col_types = FALSE)

events$start_time <- as.POSIXct(events$start_time, tz="UTC")
events$end_time <- as.POSIXct(events$end_time, tz="UTC")
precip$timestamp <- as.POSIXct(precip$timestamp, tz="UTC")

# Filter for last 3 days
threshold_time <- Sys.time() - days(3)
recent_events <- events %>% filter(start_time >= threshold_time)

if(nrow(recent_events) == 0) {
  cat("\nNo events detected in the last 3 days.\n")
} else {
  # Cross-reference with precipitation
  recent_events <- recent_events %>%
    rowwise() %>%
    mutate(
      window_start = start_time - hours(LEAD_IN_HOURS),
      window_end = end_time,
      cur_station = station,
      cur_start = window_start,
      cur_end = window_end,
      event_precip = list(
          precip %>%
              filter(
                  station == cur_station,
                  timestamp >= cur_start,
                  timestamp <= cur_end
              )
      ),
      total_precip_mm = sum(event_precip[[1]]$precipitation_mm, na.rm = TRUE),
      would_be_discarded = ifelse(total_precip_mm == 0 & station != "Wasserbaulabor_2", "YES", "NO")
    ) %>%
    select(station, start_time, duration_min, peak_level_cm, event_type, total_precip_mm, would_be_discarded) %>%
    ungroup()

  print(recent_events, n = 100)
  
  discarded_count <- sum(recent_events$would_be_discarded == "YES")
  cat(sprintf("\nFound %d events in the last 3 days.\n", nrow(recent_events)))
  cat(sprintf("%d of these events have NO recorded precipitation and would have been discarded by the standard pipeline!\n", discarded_count))
}
