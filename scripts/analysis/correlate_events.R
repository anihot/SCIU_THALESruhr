library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
events_file    <- "data/processed/detected_events.csv"
historical_log <- "data/metadata/historical_verified_events.csv"
historical_md  <- "data/metadata/historical_verified_events.md"

cat("Starting Event Correlation Filter...\n")

if (!file.exists(events_file)) {
    stop("Missing required input file: detected_events.csv")
}

# 1. Load events (already include total_precip_mm, max_intensity_mm_h, radolan_verified
#    from event_detection.R)
events <- read_csv(events_file, show_col_types = FALSE) %>%
    mutate(
        start_time = as.POSIXct(start_time),
        end_time   = as.POSIXct(end_time)
    )

cat("Loaded", nrow(events), "events from detection.\n")

# 2. Filter: keep rain-verified events + events without precipitation data
#    - radolan_verified == TRUE  → RADOLAN confirmed rain
#    - is.na(radolan_verified)   → no precipitation data available, retain to avoid false negatives
#    - Wasserbaulabor_2          → indoor test sensor, always kept
correlation <- events %>%
    filter(
        radolan_verified == TRUE |
        is.na(radolan_verified) |
        station == "Wasserbaulabor_2"
    )

cat("Retained", nrow(correlation), "out of", nrow(events), "events after rain filter.\n")

# 3. Overwrite detected_events.csv with filtered set
write_excel_csv(correlation, events_file)

library(knitr)
if (nrow(correlation) > 0) {
    md_table <- kable(correlation, format = "markdown")
    write_lines(c("# Detected Events Log", "", md_table), "data/processed/detected_events.md")
} else {
    write_lines(
        c("# Detected Events Log", "", "No rain-verified events found."),
        "data/processed/detected_events.md"
    )
}

cat("SUCCESS: Overwrote", events_file, "with", nrow(correlation), "rain-verified events.\n")

# 4. Update Historical Log
if (nrow(correlation) > 0) {
    if (file_exists(historical_log)) {
        hist_orig <- read_csv(historical_log, show_col_types = FALSE) %>%
            mutate(
                start_time = as.POSIXct(start_time),
                end_time   = as.POSIXct(end_time),
                peak_time  = as.POSIXct(peak_time)
            )

        # Zeitstempel-Typen angleichen (event_detection liefert ggf. character)
        correlation <- correlation %>%
            mutate(
                start_time = as.POSIXct(start_time),
                end_time   = as.POSIXct(end_time),
                peak_time  = as.POSIXct(peak_time)
            )

        new_entries <- correlation %>% anti_join(hist_orig, by = c("station", "start_time"))

        if (nrow(new_entries) > 0) {
            hist_updated <- bind_rows(hist_orig, new_entries)
            write_excel_csv(hist_updated, historical_log)
            write_lines(c("# Historische Ereignisse (verifiziert)", "",
                          knitr::kable(hist_updated, format = "markdown")), historical_md)
            cat("Added", nrow(new_entries), "new entries to historical log.\n")
        } else {
            cat("No new entries for historical log.\n")
        }
    } else {
        write_excel_csv(correlation, historical_log)
        write_lines(c("# Historische Ereignisse (verifiziert)", "",
                      knitr::kable(correlation, format = "markdown")), historical_md)
        cat("Created new historical log with", nrow(correlation), "entries.\n")
    }
}
