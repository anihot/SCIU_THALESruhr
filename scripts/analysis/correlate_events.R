library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# Config
events_file <- "data/processed/detected_events.csv"
precip_file <- "data/processed/precipitation_at_sensors.csv"
output_file <- "data/processed/event_correlation_summary.csv"
historical_log <- "data/metadata/historical_verified_events.csv"
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
if (nrow(precip) == 0) {
    cat("Warning: Precipitation data is empty. No events will be verified by rain.\n")
    # Return an empty dataframe with the original columns to maintain consistency
    correlation <- events[0, ]
} else {
    # For each event, find the total rain in its window + lead-in
    correlation <- events %>%
        rowwise() %>%
        mutate(
            window_start = start_time - hours(LEAD_IN_HOURS),
            window_end = end_time,

            # Filter precip for this station and timeframe
            # Assign to temp vars to avoid scoping issues in nested filter
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
            max_intensity_mm_h = ifelse(nrow(event_precip[[1]]) > 0, max(event_precip[[1]]$precipitation_mm, na.rm = TRUE), 0),
            radolan_verified = ifelse(total_precip_mm > 0, TRUE, FALSE)
        ) %>%
        # All sensor-detected events are retained. radolan_verified is informational only:
        # RADOLAN may not capture light rain (< ~0.5 mm/h) and the extraction is point-based.
        # Event classification in event_detection.R serves as the primary quality filter.
        select(-event_precip, -window_start, -window_end, -cur_station, -cur_start, -cur_end) %>%
        ungroup()
}

# 3. Export & Overwrite
# Overwrite the original event log with only rain-verified events
write_excel_csv(correlation, events_file)

# Update Markdown table as well
library(knitr)
if (nrow(correlation) > 0) {
    md_table <- kable(correlation, format = "markdown")
    write_lines(c("# Detected Events Log", "", md_table), "data/processed/detected_events.md")
} else {
    write_lines(c("# Detected Events Log", "", "No rain-verified events found."), "data/processed/detected_events.md")
}

cat("SUCCESS: Overwrote", events_file, "with precipitation-correlated events.\n")

# 4. Update Historical Log (Persistent Memory)
if (nrow(correlation) > 0) {
    if (file_exists(historical_log)) {
        # Load existing, append only new ones (avoid duplicates by comparing timestamps)
        hist_orig <- read_csv(historical_log, show_col_types = FALSE) %>%
            mutate(start_time = as.POSIXct(start_time), end_time = as.POSIXct(end_time))

        # Simple joining/anti_join to ensure we don't duplicate based on exact station + start_time
        new_entries <- correlation %>% anti_join(hist_orig, by = c("station", "start_time"))

        if (nrow(new_entries) > 0) {
            hist_updated <- bind_rows(hist_orig, new_entries)
            write_excel_csv(hist_updated, historical_log)
            cat("Added", nrow(new_entries), "new entries to historical log.\n")
        }
    } else {
        # Create new log
        write_excel_csv(correlation, historical_log)
        cat("Created new historical log with", nrow(correlation), "entries.\n")
    }
}

cat("Retained", nrow(correlation), "out of", nrow(events), "original detections.\n")
