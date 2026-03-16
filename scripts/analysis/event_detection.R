library(readr)
library(dplyr)
library(lubridate)
library(fs)
library(tidyr)

# Config
cleaned_dir <- "data/processed/cleaned_analysis"
events_file <- "data/processed/detected_events.csv"
events_md_file <- "data/processed/detected_events.md"
THRESHOLD <- 0.015 # 1.5 cm - Threshold for flooding detection
MIN_GAP_MINS <- 20 # Minimum gap between separate events

detect_events <- function(df, station_name) {
    if (nrow(df) == 0) {
        return(NULL)
    }

    # Ensure chronological order
    df <- df %>% arrange(Zeit_Datum)

    # Filter for potential event points
    activity <- df %>%
        filter(level > THRESHOLD)

    if (nrow(activity) == 0) {
        return(NULL)
    }

    # Group contiguous points (within MIN_GAP_MINS gap)
    # Logic: If distance to previous point > MIN_GAP_MINS, it's a new event
    activity <- activity %>%
        mutate(
            gap = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum, default = first(Zeit_Datum)), units = "mins")),
            new_event = ifelse(gap > MIN_GAP_MINS, 1, 0),
            event_id = cumsum(new_event)
        )

    # Summarize events
    events <- activity %>%
        group_by(event_id) %>%
        summarise(
            station = station_name,
            start_time = min(Zeit_Datum),
            end_time = max(Zeit_Datum),
            peak_level_m = max(level),
            peak_time = Zeit_Datum[which.max(level)],
            duration_min = as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "mins")),
            points_count = n(),
            .groups = "drop"
        ) %>%
        mutate(
            avg_gradient_cm_min = (peak_level_m * 100) / (as.numeric(difftime(peak_time, start_time, units = "mins")) + 0.1),
            avg_gradient_cm_min = round(avg_gradient_cm_min, 3),
            peak_level_cm = round(peak_level_m * 100, 2),
            event_type = "Unbekannt"
        ) %>%
        # Classify events based on signature
        mutate(
            event_type = case_when(
                # "Experiment / Anomalie" signature (e.g., Feb 18th event)
                # Extremely sharp gradient (> 0.5 cm/min) AND short relative to peak height
                # Or very short absolute duration (< 45 mins) with high peak (> 2cm)
                (avg_gradient_cm_min > 0.5) | (duration_min < 45 & peak_level_cm > 2.0) ~ "Experiment / Künstlich",
                
                # Default natural rain profile: slower build-up, longer duration
                TRUE ~ "Regenereignis / Natürlich"
            )
        ) %>%
        select(station, start_time, end_time, peak_level_cm, peak_time, duration_min, avg_gradient_cm_min, event_type, points_count) %>%
        mutate(
            station = gsub("_merged_export", "", station)
        )

    return(events)
}

# MAIN EXECUTION
cat("Starting Automated Event Detection...\n")

if (!dir_exists(cleaned_dir)) {
    stop("Cleaned data directory not found.")
}

cleaned_files <- dir_ls(cleaned_dir, glob = "*.csv")
all_events <- list()

for (file_path in cleaned_files) {
    file_name <- path_file(file_path)
    station <- gsub("_cleaned\\.csv$", "", file_name)

    cat("Scanning", station, "...\n")

    # Use tryCatch to handle potentially malformed CSVs
    tryCatch(
        {
            df <- read_csv(file_path, show_col_types = FALSE)
            ev <- detect_events(df, station)
            if (!is.null(ev)) {
                all_events[[station]] <- ev
                cat("  Found", nrow(ev), "events.\n")
            }
        },
        error = function(e) {
            cat("  Error processing", station, ":", e$message, "\n")
        }
    )
}

if (length(all_events) > 0) {
    events_df <- bind_rows(all_events) %>% arrange(desc(start_time))

    # Export as CSV with BOM for Excel/GitHub interactivity
    # write_excel_csv adds the UTF-8 BOM
    write_excel_csv(events_df, events_file)

    # Export as Markdown table for quick viewing on GitHub
    library(knitr)
    md_table <- kable(events_df, format = "markdown")
    # Ensure UTF-8 for write_lines
    write_lines(c("# Detected Events Log", "", md_table), events_md_file)

    cat("\nSUCCESS: Detected", nrow(events_df), "total events across all stations.\n")
    cat("CSV log saved to:", events_file, "\n")
    cat("Markdown log saved to:", events_md_file, "\n")
} else {
    cat("\nNo events detected in any station. Writing empty log file...\n")
    # Write empty dataframe with headers to ensure subsequent scripts can read it
    empty_df <- tibble(
        station = character(),
        start_time = POSIXct(),
        end_time = POSIXct(),
        peak_level_cm = numeric(),
        peak_time = POSIXct(),
        duration_min = numeric(),
        avg_gradient_cm_min = numeric(),
        points_count = integer()
    )
    write_excel_csv(empty_df, events_file)
    write_lines(c("# Detected Events Log", "", "Keine Ereignisse erkannt."), events_md_file)
}
