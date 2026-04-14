library(readr)
library(dplyr)
library(lubridate)
library(fs)
library(tidyr)

# Config
cleaned_dir    <- "data/processed/cleaned_analysis"
events_file    <- "data/processed/detected_events.csv"
events_md_file <- "data/processed/detected_events.md"
precip_file    <- "data/processed/precipitation_at_sensors.csv"

THRESHOLD          <- 0.004 # 0.4 cm - Peak-Schwelle (Seed-Punkte)
LOW_THRESHOLD      <- 0.002 # 0.2 cm - Schwelle für "erhöht" (active_fraction)
MIN_GAP_MINS       <- 60    # Gaps < 60 min → zusammenhängendes Ereignis
MIN_DURATION_MINS  <- 5     # Events kürzer als 5 min = Rauschen / Einzelspike
MAX_DURATION_MINS  <- 180   # Events länger als 3h werden nicht berücksichtigt
MIN_RISE_MINS      <- 3     # Mindestanstiegszeit bis zum Peak (filtert Blips/Fahrzeuge)
MIN_FALL_MINS      <- 3     # Mindestabfallzeit nach dem Peak (filtert Blips/Fahrzeuge)
MAX_LOCAL_PEAKS    <- 2     # Max. lokale Peaks im Ereignis (filtert Pulse Chains)
MAX_PLATEAU_FRAC   <- 0.5   # Max. Anteil Messwerte >= 85% des Peaks (filtert Box-Signale)
MIN_ACTIVE_FRAC    <- 0.5   # Min. Anteil Messpunkte im Fenster mit Pegel > LOW_THRESHOLD (filtert Spike-Chains)
LEAD_IN_HOURS      <- 3     # Niederschlag-Vorlauf-Fenster vor Event-Start

# Load precipitation data once (global, reused across all stations)
precip <- tibble(
    timestamp          = as.POSIXct(character()),
    station            = character(),
    precipitation_mm   = numeric()
)
precip_available <- FALSE

if (file.exists(precip_file)) {
    precip_raw <- read_csv(precip_file, show_col_types = FALSE)
    if (nrow(precip_raw) > 0) {
        precip           <- precip_raw %>% mutate(timestamp = as.POSIXct(timestamp))
        precip_available <- TRUE
        cat("Precipitation data loaded:", nrow(precip), "records.\n")
    } else {
        cat("Warning: Precipitation file is empty. Events classified without rain context.\n")
    }
}

detect_events <- function(df, station_name, precip) {
    if (nrow(df) == 0) {
        return(NULL)
    }

    # Ensure chronological order
    df <- df %>% arrange(Zeit_Datum)

    # Seeds: nur Punkte über THRESHOLD bilden Events (keine LOW_THRESHOLD-Aktivität,
    # um Spike-Chains nicht künstlich zusammenzufassen)
    activity <- df %>%
        filter(level > THRESHOLD)

    if (nrow(activity) == 0) {
        return(NULL)
    }

    # Group contiguous points (within MIN_GAP_MINS gap)
    activity <- activity %>%
        mutate(
            gap       = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum, default = first(Zeit_Datum)), units = "mins")),
            new_event = ifelse(gap > MIN_GAP_MINS, 1, 0),
            event_id  = cumsum(new_event)
        )

    # Summarize events
    events <- activity %>%
        group_by(event_id) %>%
        summarise(
            station          = station_name,
            start_time       = min(Zeit_Datum),
            end_time         = max(Zeit_Datum),
            peak_level_m     = max(level),
            peak_time        = Zeit_Datum[which.max(level)],
            duration_min     = as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "mins")),
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
            peak_level_cm       = round(peak_level_m * 100, 2)
        )

    # --- Anti-spike: active_fraction aus dem vollen Zeitfenster (nicht nur Activity) ---
    # Anteil der Originalmesspunkte im [start_time, end_time]-Fenster, deren Pegel > LOW_THRESHOLD.
    # Eine Spike-Chain hat zwischen den Peaks meist 0 → niedriger active_fraction.
    events <- events %>%
        rowwise() %>%
        mutate(
            active_fraction = {
                slice_lvls <- df$level[df$Zeit_Datum >= start_time & df$Zeit_Datum <= end_time]
                if (length(slice_lvls) == 0) 0 else round(mean(slice_lvls > LOW_THRESHOLD, na.rm = TRUE), 3)
            }
        ) %>%
        ungroup()

    # --- Precipitation context per event (3h lead-in window) ---
    station_precip <- precip %>% filter(station == station_name)

    if (nrow(station_precip) > 0) {
        events <- events %>%
            rowwise() %>%
            mutate(
                .t0                = start_time - hours(LEAD_IN_HOURS),
                .t1                = end_time,
                .ep                = list(station_precip %>% filter(timestamp >= .t0, timestamp <= .t1)),
                total_precip_mm    = round(sum(.ep[[1]]$precipitation_mm, na.rm = TRUE), 2),
                max_intensity_mm_h = ifelse(nrow(.ep[[1]]) > 0,
                                           round(max(.ep[[1]]$precipitation_mm, na.rm = TRUE) * 12, 2),  # mm/5min → mm/h
                                           0),
                radolan_verified   = total_precip_mm > 0
            ) %>%
            select(-.t0, -.t1, -.ep) %>%
            ungroup()
    } else {
        # No precipitation data for this station → columns NA, classification uses sensor-only logic
        events <- events %>%
            mutate(
                total_precip_mm    = NA_real_,
                max_intensity_mm_h = NA_real_,
                radolan_verified   = NA
            )
    }

    # --- Classify ---
    events <- events %>%
        mutate(
            event_type = case_when(
                # Precipitation data available but no rain detected → suspicious (infrastructure, artifact)
                !is.na(radolan_verified) & radolan_verified == FALSE ~ "Verdächtig / Kein Regen",

                # Flash flood: sharp gradient OR short intense pulse
                (avg_gradient_cm_min > 0.5) | (duration_min < 45 & peak_level_cm > 2.0) ~ "Sturzflut-Ereignis",

                # Significant rain event: notable peak with gradual rise
                peak_level_cm >= 2.0 ~ "Regenereignis / Natürlich",

                # Light rain: detectable but low peak
                TRUE ~ "Leichter Regen / Unterhalb DWD-Schwelle"
            )
        ) %>%
        filter(
            duration_min     >= MIN_DURATION_MINS,
            duration_min     <= MAX_DURATION_MINS,
            rise_min         >= MIN_RISE_MINS,
            fall_min         >= MIN_FALL_MINS,
            n_local_peaks    <= MAX_LOCAL_PEAKS,
            plateau_fraction <= MAX_PLATEAU_FRAC,
            active_fraction  >= MIN_ACTIVE_FRAC
        ) %>%
        select(
            station, start_time, end_time, peak_level_cm, peak_time,
            duration_min, avg_gradient_cm_min, event_type,
            total_precip_mm, max_intensity_mm_h, radolan_verified,
            points_count
        ) %>%
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
# Exclude "Wasserbaulabor_" (without "2") from event detection
cleaned_files <- cleaned_files[!grepl("Wasserbaulabor__", basename(cleaned_files))]
all_events <- list()

for (file_path in cleaned_files) {
    file_name <- path_file(file_path)
    station   <- gsub("_cleaned\\.csv$", "", file_name)

    cat("Scanning", station, "...\n")

    tryCatch(
        {
            df <- read_csv(file_path, show_col_types = FALSE)
            ev <- detect_events(df, station, precip)
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

    write_excel_csv(events_df, events_file)

    library(knitr)
    md_table <- kable(events_df, format = "markdown")
    write_lines(c("# Detected Events Log", "", md_table), events_md_file)

    cat("\nSUCCESS: Detected", nrow(events_df), "total events across all stations.\n")
    cat("CSV log saved to:", events_file, "\n")
    cat("Markdown log saved to:", events_md_file, "\n")

    # Export per station
    station_events_dir <- "data/processed/events_by_station"
    if (!dir_exists(station_events_dir)) dir_create(station_events_dir)

    for (st in unique(events_df$station)) {
        st_df     <- events_df %>% filter(station == st) %>% arrange(desc(start_time))
        safe_name <- gsub("[^a-zA-Z0-9_]", "_", st)
        st_csv    <- file.path(station_events_dir, paste0(safe_name, "_events.csv"))
        st_md     <- file.path(station_events_dir, paste0(safe_name, "_events.md"))

        write_excel_csv(st_df, st_csv)
        write_lines(
            c(paste0("# Ereignisse: ", st), "", kable(st_df, format = "markdown")),
            st_md
        )
        cat("  Station", st, "->", nrow(st_df), "Ereignisse gespeichert.\n")
    }
} else {
    cat("\nNo events detected in any station. Writing empty log file...\n")
    empty_df <- tibble(
        station            = character(),
        start_time         = as.POSIXct(character()),
        end_time           = as.POSIXct(character()),
        peak_level_cm      = numeric(),
        peak_time          = as.POSIXct(character()),
        duration_min       = numeric(),
        avg_gradient_cm_min = numeric(),
        event_type         = character(),
        total_precip_mm    = numeric(),
        max_intensity_mm_h = numeric(),
        radolan_verified   = logical(),
        points_count       = integer()
    )
    write_excel_csv(empty_df, events_file)
    write_lines(c("# Detected Events Log", "", "Keine Ereignisse erkannt."), events_md_file)
}
