library(readr)
library(dplyr)
library(lubridate)

# Config
events_file <- "data/detected_events.csv"
rain_garden_file <- "data/rain_extra/schillerschule/schillerschule_garden.csv"
rain_yard_file <- "data/rain_extra/schillerschule/schillerschule_yard.csv"
LEAD_IN_HOURS <- 3

cat("🚀 Starting Schillerschule Rain Correlation Analysis...\n")

# 1. Load Sensor Events
events <- read_csv(events_file, show_col_types = FALSE) %>%
    filter(station %in% c("Königsallee_Springorum", "Wasserstraße_Springorum")) %>%
    mutate(
        start_time = as.POSIXct(start_time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"),
        end_time = as.POSIXct(end_time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
    )

if (nrow(events) == 0) {
    stop("No events found for Königsallee or Wasserstraße.")
}

cat("Found", nrow(events), "events for the target stations.\n")

# 2. Function to load and filter large rain data
# We only care about entries that could overlap with events (2025-01-01 onwards to be safe)
get_rain_data <- function(file_path) {
    cat("Reading rain data from:", basename(file_path), "...\n")
    # Using read_csv with col_select to save memory
    # Column 1 is TIMESTAMP, Column 12 is Rain_mm_Tot
    rain <- read_csv(file_path,
        col_select = c(TIMESTAMP, Rain_mm_Tot),
        col_types = cols(
            TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S%z"),
            Rain_mm_Tot = col_double()
        )
    ) %>%
        filter(TIMESTAMP >= as.POSIXct("2025-01-01", tz = "UTC"))
    return(rain)
}

# Load rain data
rain_yard <- get_rain_data(rain_yard_file)
cat("Loaded", nrow(rain_yard), "recent rain records from Yard.\n")
rain_garden <- get_rain_data(rain_garden_file)
cat("Loaded", nrow(rain_garden), "recent rain records from Garden.\n")

# 3. Perform Correlation
correlation_results <- events %>%
    rowwise() %>%
    mutate(
        window_start = start_time - hours(LEAD_IN_HOURS),
        window_end = end_time,

        # Calculate total rain in window from Yard
        rain_yard_mm = sum(rain_yard$Rain_mm_Tot[
            rain_yard$TIMESTAMP >= window_start &
                rain_yard$TIMESTAMP <= window_end
        ], na.rm = TRUE),

        # Calculate total rain in window from Garden
        rain_garden_mm = sum(rain_garden$Rain_mm_Tot[
            rain_garden$TIMESTAMP >= window_start &
                rain_garden$TIMESTAMP <= window_end
        ], na.rm = TRUE),
        total_rain_mm = max(rain_yard_mm, rain_garden_mm),
        rain_verified = total_rain_mm > 0
    ) %>%
    ungroup() %>%
    select(-window_start, -window_end)

# 4. Summary and Reporting
verified_count <- sum(correlation_results$rain_verified)
cat("\nResults Summary:\n")
cat("Total target events:", nrow(events), "\n")
cat("Verified by Schillerschule rain:", verified_count, "\n")
cat("Verification rate:", round(verified_count / nrow(events) * 100, 1), "%\n")

# Save detailed results
output_file <- "data/schillerschule_analysiert.csv"
write_csv(correlation_results, output_file)

# Print top verified events
cat("\nTop 10 Rain-Verified Events:\n")
correlation_results %>%
    filter(rain_verified) %>%
    arrange(desc(total_rain_mm)) %>%
    head(10) %>%
    select(station, start_time, total_rain_mm, peak_level_cm) %>%
    print()

cat("\nAnalysis complete. Results saved to", output_file, "\n")
