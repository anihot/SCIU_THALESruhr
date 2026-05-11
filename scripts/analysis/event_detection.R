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
MAX_DURATION_MINS  <- 1440  # Events länger als 24h werden nicht berücksichtigt
MIN_RISE_MINS      <- 3     # Mindestanstiegszeit bis zum Peak (filtert Blips/Fahrzeuge)
MIN_FALL_MINS      <- 3     # Mindestabfallzeit nach dem Peak (filtert Blips/Fahrzeuge)
MAX_LOCAL_PEAKS    <- 10    # Max. lokale Peaks im Ereignis (lang anhaltende Events haben natürlich mehr Peaks)
MAX_PLATEAU_FRAC   <- 0.5   # Max. Anteil Messwerte >= 85% des Peaks (filtert Box-Signale)
MIN_ACTIVE_FRAC    <- 0.5   # Min. Anteil Messpunkte im Fenster mit Pegel > LOW_THRESHOLD (filtert Spike-Chains)
MIN_ROLLING_MEDIAN <- 0.002 # Min. Maximum einer rollierenden 3-Punkt-Median (filtert isolierte Spikes)
LEAD_IN_HOURS      <- 3     # Niederschlag-Vorlauf-Fenster vor Event-Start

# Per-Station-Schwellwerte (überschreiben globale Defaults)
# Königsallee: niedrigere Schwellen, weil die neu berechneten Level
# (rolling-distance-baseline statt API-Festwert) kleinere Absolutwerte
# haben und damit feiner aufgelöst sind.
STATION_THRESHOLDS <- list(
    "Königsallee_Springorum" = list(
        threshold      = 0.002,  # 0.2 cm über station_baseline (statt 0.4 cm)
        low_threshold  = 0.001,  # 0.1 cm über station_baseline (statt 0.2 cm)
        min_active_frac = 0.30   # 30 % aktive Punkte gefordert (statt 50 %)
    ),
    "An_der_Kost" = list(
        threshold      = 0.002,  # 0.2 cm — Sensor zeigt bei Regen nur 0.1–0.3 cm
        low_threshold  = 0.001,  # 0.1 cm
        min_active_frac = 0.30   # 30 %
    )
)

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

    # Per-Station Trockenpegel: Mode aller Level-Werte (gerundet auf 3 Stellen).
    # Dient als dynamische Nulllinie — Sensoren mit unterschiedlichen
    # Firmware-Referenzen werden so vergleichbar. Schwellwerte werden relativ
    # zur Nulllinie angewandt.
    valid_levels <- df$level[!is.na(df$level)]
    station_baseline <- if (length(valid_levels) > 0) {
        lv_round <- round(valid_levels, 3)
        as.numeric(names(sort(table(lv_round), decreasing = TRUE))[1])
    } else 0
    cat("    Station baseline:", round(station_baseline * 100, 2), "cm\n")

    # Station-spezifische Schwellwerte (falls definiert, sonst globale Defaults)
    station_key <- gsub("_merged_export$", "", station_name)
    st_ov <- STATION_THRESHOLDS[[station_key]]
    thresh_val    <- if (!is.null(st_ov)) st_ov$threshold       else THRESHOLD
    low_thresh    <- if (!is.null(st_ov)) st_ov$low_threshold   else LOW_THRESHOLD
    min_act_frac  <- if (!is.null(st_ov)) st_ov$min_active_frac else MIN_ACTIVE_FRAC
    if (!is.null(st_ov)) cat("    Using station-specific thresholds for:", station_key, "\n")

    low_abs  <- station_baseline + low_thresh
    high_abs <- station_baseline + thresh_val

    activity <- df %>% filter(!is.na(level) & level > low_abs)
    if (nrow(activity) == 0) return(NULL)

    activity <- activity %>%
        mutate(
            gap       = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum, default = first(Zeit_Datum)), units = "mins")),
            new_event = ifelse(gap > MIN_GAP_MINS, 1, 0),
            run_id    = cumsum(new_event)
        )

    run_bounds <- activity %>%
        group_by(run_id) %>%
        summarise(
            run_start = min(Zeit_Datum),
            run_end   = max(Zeit_Datum),
            run_peak  = max(level, na.rm = TRUE),
            .groups   = "drop"
        ) %>%
        filter(run_peak > high_abs)

    if (nrow(run_bounds) == 0) return(NULL)

    zeit <- df$Zeit_Datum
    lvl  <- df$level
    n    <- length(lvl)

    merged <- lapply(seq_len(nrow(run_bounds)), function(k) {
        lo <- which(zeit >= run_bounds$run_start[k])[1]
        hi <- tail(which(zeit <= run_bounds$run_end[k]), 1)
        c(as.integer(lo), as.integer(hi))
    })

    # --- Metriken pro (extended) Event aus df-Slice berechnen ---
    events <- lapply(seq_along(merged), function(k) {
        lo <- merged[[k]][1]; hi <- merged[[k]][2]
        slice_z <- zeit[lo:hi]
        slice_l <- lvl[lo:hi]
        # NAs aus dem Slice entfernen (rohe API-Level können NA haben)
        valid   <- !is.na(slice_l)
        slice_z_v <- slice_z[valid]
        slice_l_v <- slice_l[valid]
        if (length(slice_l_v) == 0) return(NULL)
        peak_i  <- which.max(slice_l_v)
        peak_l  <- slice_l_v[peak_i]
        peak_t  <- slice_z_v[peak_i]
        tibble(
            event_id         = k,
            station          = station_name,
            start_time       = slice_z[1],
            end_time         = slice_z[length(slice_z)],
            peak_level_m     = peak_l,
            peak_time        = peak_t,
            duration_min     = as.numeric(difftime(slice_z[length(slice_z)], slice_z[1], units = "mins")),
            rise_min         = as.numeric(difftime(peak_t, slice_z[1], units = "mins")),
            fall_min         = as.numeric(difftime(slice_z[length(slice_z)], peak_t, units = "mins")),
            points_count     = length(slice_l_v),
            plateau_fraction = round(mean(slice_l_v >= 0.85 * peak_l, na.rm = TRUE), 3),
            n_local_peaks    = {
                m <- length(slice_l_v)
                if (m < 3) 1L else {
                    interior <- sum(slice_l_v[2:(m-1)] > slice_l_v[1:(m-2)] &
                                    slice_l_v[2:(m-1)] > slice_l_v[3:m], na.rm = TRUE)
                    as.integer(interior +
                        ifelse(!is.na(slice_l_v[1]) & !is.na(slice_l_v[2]) & slice_l_v[1] > slice_l_v[2], 1L, 0L) +
                        ifelse(!is.na(slice_l_v[m]) & !is.na(slice_l_v[m-1]) & slice_l_v[m] > slice_l_v[m-1], 1L, 0L))
                }
            }
        )
    }) %>% bind_rows() %>%
        mutate(
            avg_gradient_cm_min = ((peak_level_m - station_baseline) * 100) / (as.numeric(difftime(peak_time, start_time, units = "mins")) + 0.1),
            avg_gradient_cm_min = round(avg_gradient_cm_min, 3),
            peak_level_cm       = round((peak_level_m - station_baseline) * 100, 2)
        )

    # --- Anti-spike: active_fraction aus dem vollen Zeitfenster (nicht nur Activity) ---
    # Anteil der Originalmesspunkte im [start_time, end_time]-Fenster, deren Pegel > LOW_THRESHOLD.
    # Eine Spike-Chain hat zwischen den Peaks meist 0 → niedriger active_fraction.
    events <- events %>%
        rowwise() %>%
        mutate(
            active_fraction = {
                slice_lvls <- df$level[df$Zeit_Datum >= start_time & df$Zeit_Datum <= end_time]
                if (length(slice_lvls) == 0) 0 else round(mean(slice_lvls > low_abs, na.rm = TRUE), 3)
            },
            max_rolling_median = {
                # Rollierende 3-Punkt-Median: echte Events haben >= 3 aufeinanderfolgende
                # erhöhte Werte, Spike-Chains brechen dazwischen auf 0 zusammen → Median ≈ 0
                slice_lvls <- df$level[df$Zeit_Datum >= start_time & df$Zeit_Datum <= end_time]
                n <- length(slice_lvls)
                if (n < 3) 0 else {
                    rm <- sapply(2:(n-1), function(i) median(slice_lvls[(i-1):(i+1)]))
                    round(max(rm, na.rm = TRUE), 5)
                }
            }
        ) %>%
        ungroup()

    # --- Precipitation context per event (3h lead-in window) ---
    # Fuzzy-Matching: precip-Station (z.B. "Königsallee") gegen station_name
    # (z.B. "Königsallee_Springorum_merged_export"). Suche nach Substring.
    precip_stations <- unique(precip$station)
    station_name_norm <- gsub("_", " ", gsub("_merged_export$", "", station_name))
    matched_precip <- precip_stations[
        sapply(precip_stations, function(ps) grepl(ps, station_name_norm, fixed = TRUE))
    ]
    precip_station_key <- if (length(matched_precip) > 0) matched_precip[1] else station_name
    station_precip <- precip %>% filter(station == precip_station_key)

    if (nrow(station_precip) > 0) {
        lead_window <- hours(LEAD_IN_HOURS)
        precip_stats <- lapply(seq_len(nrow(events)), function(i) {
            ep <- station_precip %>%
                filter(timestamp >= events$start_time[i] - lead_window,
                       timestamp <= events$end_time[i])
            list(
                total_precip_mm    = round(sum(ep$precipitation_mm, na.rm = TRUE), 2),
                max_intensity_mm_h = if (nrow(ep) > 0)
                    round(max(ep$precipitation_mm, na.rm = TRUE) * 12, 2) else 0  # mm/5min → mm/h
            )
        })
        events <- events %>%
            mutate(
                total_precip_mm    = vapply(precip_stats, `[[`, numeric(1), "total_precip_mm"),
                max_intensity_mm_h = vapply(precip_stats, `[[`, numeric(1), "max_intensity_mm_h"),
                radolan_verified   = total_precip_mm > 0
            )
    } else {
        # No precipitation data for this station → columns NA, classification uses sensor-only logic
        events <- events %>%
            mutate(
                total_precip_mm    = NA_real_,
                max_intensity_mm_h = NA_real_,
                radolan_verified   = NA
            )
    }

    # --- Require positive RADOLAN verification ---
    # Drop events where (a) precip data exists but no rain was measured
    # (infrastructure artifacts, passing vehicles, sensor noise) and
    # (b) precip coverage is missing entirely (we can't plot a meaningful
    # rain context, so these become confusing no-rain plots). Stations
    # without coverage should be added to data/metadata/sensor_metadata.csv.
    events <- events %>%
        filter(!is.na(radolan_verified) & radolan_verified)

    # --- Classify ---
    events <- events %>%
        mutate(
            event_type = case_when(
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
            n_local_peaks      <= MAX_LOCAL_PEAKS * pmax(1, ceiling(duration_min / 60)),
            plateau_fraction   <= MAX_PLATEAU_FRAC,
            active_fraction    >= min_act_frac,
            max_rolling_median >= MIN_ROLLING_MEDIAN
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
