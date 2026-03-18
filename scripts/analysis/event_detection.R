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
MIN_DURATION_MINS <- 5  # Events kürzer als 5 min = Rauschen / Einzelspike
MAX_DURATION_MINS <- 60 # Events länger als 60 min werden nicht berücksichtigt
MIN_RISE_MINS     <- 3   # Mindestanstiegszeit bis zum Peak (filtert Blips/Fahrzeuge)
MIN_FALL_MINS     <- 3   # Mindestabfallzeit nach dem Peak (filtert Blips/Fahrzeuge)
MAX_LOCAL_PEAKS   <- 2   # Max. lokale Peaks im Ereignis (filtert Pulse Chains)
MAX_PLATEAU_FRAC  <- 0.5 # Max. Anteil Messwerte >= 85% des Peaks (filtert Box-Signale)

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
            rise_min         = as.numeric(difftime(Zeit_Datum[which.max(level)], min(Zeit_Datum), units = "mins")),
            fall_min         = as.numeric(difftime(max(Zeit_Datum), Zeit_Datum[which.max(level)], units = "mins")),
            points_count     = n(),
            plateau_fraction = {
                lvl <- level
                round(mean(lvl >= 0.85 * max(lvl)), 3)
            },
            n_local_peaks    = {
                lvl <- level[order(Zeit_Datum)]
                n   <- length(lvl)
                if (n < 3) 1L else {
                    interior <- sum(lvl[2:(n-1)] > lvl[1:(n-2)] & lvl[2:(n-1)] > lvl[3:n])
                    as.integer(interior +
                        ifelse(lvl[1] > lvl[2], 1L, 0L) +
                        ifelse(lvl[n] > lvl[n-1], 1L, 0L))
                }
            },
            .groups = "drop"
        ) %>%
        mutate(
            avg_gradient_cm_min = (peak_level_m * 100) / (as.numeric(difftime(peak_time, start_time, units = "mins")) + 0.1),
            avg_gradient_cm_min = round(avg_gradient_cm_min, 3),
            peak_level_cm = round(peak_level_m * 100, 2),
            event_type = "Unbekannt"
        ) %>%
        # Classify events based on sensor signature
        mutate(
            event_type = case_when(
                # "Sturzflut" (Flash Flood) signature:
                # Extremely sharp gradient (> 0.5 cm/min) OR short intense pulse (< 45 mins, peak > 2cm)
                (avg_gradient_cm_min > 0.5) | (duration_min < 45 & peak_level_cm > 2.0) ~ "Sturzflut-Ereignis",

                # Significant rain event: notable peak level (>= 2cm) with gradual rise
                # Likely corresponds to DWD Level 2+ (Starkregen >= 15 mm/h) intensity
                peak_level_cm >= 2.0 ~ "Regenereignis / Natürlich",

                # Light rain: detectable but low peak (< 2cm) – likely below DWD Starkregen thresholds
                TRUE ~ "Leichter Regen / Unterhalb DWD-Schwelle"
            )
        ) %>%
        filter(
            duration_min     >= MIN_DURATION_MINS,
            duration_min     <= MAX_DURATION_MINS,
            rise_min         >= MIN_RISE_MINS,
            fall_min         >= MIN_FALL_MINS,
            n_local_peaks    <= MAX_LOCAL_PEAKS,
            plateau_fraction <= MAX_PLATEAU_FRAC
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

    # Export gesamt (alle Stationen)
    write_excel_csv(events_df, events_file)

    library(knitr)
    md_table <- kable(events_df, format = "markdown")
    write_lines(c("# Detected Events Log", "", md_table), events_md_file)

    cat("\nSUCCESS: Detected", nrow(events_df), "total events across all stations.\n")
    cat("CSV log saved to:", events_file, "\n")
    cat("Markdown log saved to:", events_md_file, "\n")

    # Export je Station
    station_events_dir <- "data/processed/events_by_station"
    if (!dir_exists(station_events_dir)) dir_create(station_events_dir)

    for (st in unique(events_df$station)) {
        st_df      <- events_df %>% filter(station == st) %>% arrange(desc(start_time))
        safe_name  <- gsub("[^a-zA-Z0-9_]", "_", st)
        st_csv     <- file.path(station_events_dir, paste0(safe_name, "_events.csv"))
        st_md      <- file.path(station_events_dir, paste0(safe_name, "_events.md"))

        write_excel_csv(st_df, st_csv)
        write_lines(
            c(paste0("# Ereignisse: ", st), "", kable(st_df, format = "markdown")),
            st_md
        )
        cat("  Station", st, "->", nrow(st_df), "Ereignisse gespeichert.\n")
    }
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
