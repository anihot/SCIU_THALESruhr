library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(fs)

# ── Config ────────────────────────────────────────────────────────────────────

schillerschule_dir  <- "data/raw/schillerschule"
cleaned_dir         <- "data/processed/cleaned_analysis"
output_csv          <- "data/processed/schillerschule_event_comparison.csv"
plots_dir           <- "data/output/plots/schillerschule"

# Stations to compare with Schillerschule rain gauge
TARGET_STATIONS <- c("Königsallee_Springorum", "Wasserstraße_Springorum")

THRESHOLD         <- 0.015  # 1.5 cm – event detection threshold
MIN_GAP_MINS      <- 20
MIN_DURATION_MINS <- 5
MAX_DURATION_MINS <- 60
LEAD_IN_HOURS     <- 3      # Window before event start for rain context
TRAIL_HOURS       <- 1      # Window after event end

cat("=== Schillerschule Rain Gauge – Sensor Event Comparison ===\n\n")

# ── 1. Load Schillerschule rain gauge data ────────────────────────────────────

load_schillerschule <- function(path) {
    df <- read_csv(path, show_col_types = FALSE) %>%
        select(TIMESTAMP, Rain_mm_Tot) %>%
        mutate(
            timestamp = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
            timestamp = with_tz(timestamp, "Europe/Berlin"),
            rain_mm   = as.numeric(Rain_mm_Tot)
        ) %>%
        filter(!is.na(timestamp), !is.na(rain_mm), rain_mm >= 0) %>%
        select(timestamp, rain_mm)
    return(df)
}

files_sc <- dir_ls(schillerschule_dir, glob = "*.csv")
if (length(files_sc) == 0) stop("No Schillerschule CSV files found in ", schillerschule_dir)

cat("Loading Schillerschule files:\n")
sc_list <- list()
for (f in files_sc) {
    cat(" -", path_file(f), "\n")
    sc_list[[path_file(f)]] <- load_schillerschule(f)
}

# Combine: per minute, take the max of yard/garden (conservative approach)
rain_sc <- bind_rows(sc_list) %>%
    group_by(timestamp) %>%
    summarise(rain_mm = max(rain_mm, na.rm = TRUE), .groups = "drop") %>%
    arrange(timestamp)

cat("  Loaded", nrow(rain_sc), "combined 1-minute rain records\n")
cat("  Range:", format(min(rain_sc$timestamp)), "to", format(max(rain_sc$timestamp)), "\n\n")

# ── 2. Detect events from cleaned sensor data ─────────────────────────────────

detect_events <- function(df, station_name) {
    if (nrow(df) == 0) return(NULL)
    df <- df %>% arrange(Zeit_Datum)

    activity <- df %>% filter(level > THRESHOLD)
    if (nrow(activity) == 0) return(NULL)

    activity <- activity %>%
        mutate(
            gap       = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum, default = first(Zeit_Datum)), units = "mins")),
            new_event = ifelse(gap > MIN_GAP_MINS, 1, 0),
            event_id  = cumsum(new_event)
        )

    events <- activity %>%
        group_by(event_id) %>%
        summarise(
            station      = station_name,
            start_time   = min(Zeit_Datum),
            end_time     = max(Zeit_Datum),
            peak_level_m = max(level),
            peak_time    = Zeit_Datum[which.max(level)],
            duration_min = as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "mins")),
            points_count = n(),
            .groups      = "drop"
        ) %>%
        mutate(
            peak_level_cm = round(peak_level_m * 100, 2)
        ) %>%
        filter(duration_min >= MIN_DURATION_MINS, duration_min <= MAX_DURATION_MINS) %>%
        select(station, start_time, end_time, peak_level_cm, peak_time, duration_min, points_count)

    return(events)
}

all_events <- list()
for (st in TARGET_STATIONS) {
    cleaned_file <- file.path(cleaned_dir, paste0(st, "_merged_export_cleaned.csv"))
    if (!file_exists(cleaned_file)) {
        cat("WARNING: Cleaned file not found for", st, "- skipping\n")
        next
    }
    df <- read_csv(cleaned_file, show_col_types = FALSE)
    ev <- detect_events(df, st)
    if (!is.null(ev) && nrow(ev) > 0) {
        all_events[[st]] <- ev
        cat("Station", st, "-> detected", nrow(ev), "events\n")
    } else {
        cat("Station", st, "-> no events found\n")
    }
}

if (length(all_events) == 0) stop("No events detected for target stations.")
events_df <- bind_rows(all_events) %>% arrange(start_time)
cat("\nTotal events for comparison:", nrow(events_df), "\n\n")

# ── 3. Compute Schillerschule-based lag per event ─────────────────────────────

MIN_RAIN_MM_MIN <- 0.01  # Minimum rain per minute to count as rain onset

compute_sc_lag <- function(event_start, event_end, event_peak) {
    window <- rain_sc %>%
        filter(
            timestamp >= (event_start - hours(LEAD_IN_HOURS)),
            timestamp <= (event_end   + hours(TRAIL_HOURS))
        )

    if (nrow(window) == 0 || sum(window$rain_mm, na.rm = TRUE) == 0) {
        return(list(
            sc_total_mm     = 0,
            sc_max_mm_min   = 0,
            sc_onset_time   = NA_character_,
            onset_lag_min   = NA_real_,
            peak_lag_min    = NA_real_,
            sc_peak_time    = NA_character_,
            sc_verified     = FALSE
        ))
    }

    total_mm   <- sum(window$rain_mm, na.rm = TRUE)
    max_mm_min <- max(window$rain_mm, na.rm = TRUE)

    # Rain onset: first minute with measurable rain in the lead-in window
    onset_row <- window %>%
        filter(timestamp <= event_start, rain_mm >= MIN_RAIN_MM_MIN) %>%
        arrange(timestamp) %>%
        slice_tail(n = 1)  # last rain before event start = most recent onset

    if (nrow(onset_row) == 0) {
        # Rain only starts after event start
        onset_row <- window %>%
            filter(rain_mm >= MIN_RAIN_MM_MIN) %>%
            arrange(timestamp) %>%
            slice_head(n = 1)
    }

    sc_peak_row <- window %>%
        filter(timestamp >= (event_start - hours(LEAD_IN_HOURS)),
               timestamp <= event_end) %>%
        arrange(desc(rain_mm)) %>%
        slice_head(n = 1)

    onset_lag_min <- if (nrow(onset_row) > 0)
        as.numeric(difftime(event_start, onset_row$timestamp[1], units = "mins"))
    else NA_real_

    peak_lag_min <- if (nrow(sc_peak_row) > 0)
        as.numeric(difftime(event_peak, sc_peak_row$timestamp[1], units = "mins"))
    else NA_real_

    list(
        sc_total_mm   = round(total_mm, 2),
        sc_max_mm_min = round(max_mm_min, 3),
        sc_onset_time = if (nrow(onset_row) > 0) format(onset_row$timestamp[1]) else NA_character_,
        onset_lag_min = round(onset_lag_min, 1),
        peak_lag_min  = round(peak_lag_min, 1),
        sc_peak_time  = if (nrow(sc_peak_row) > 0) format(sc_peak_row$timestamp[1]) else NA_character_,
        sc_verified   = total_mm > 0
    )
}

cat("Computing Schillerschule lag for each event...\n")
lag_results <- purrr::map_dfr(seq_len(nrow(events_df)), function(i) {
    ev <- events_df[i, ]
    res <- compute_sc_lag(ev$start_time, ev$end_time, ev$peak_time)
    as.data.frame(res)
})

comparison_df <- bind_cols(events_df, lag_results) %>%
    arrange(desc(start_time))

sc_verified_count <- sum(comparison_df$sc_verified, na.rm = TRUE)
cat("  Schillerschule-verified events:", sc_verified_count, "/", nrow(comparison_df), "\n\n")

# ── 4. Summary statistics ─────────────────────────────────────────────────────

verified <- comparison_df %>% filter(sc_verified == TRUE)
if (nrow(verified) > 0) {
    cat("=== Lag Summary (Schillerschule-verified events) ===\n")
    cat("Onset-Lag (rain -> sensor):\n")
    cat("  Median:", median(verified$onset_lag_min, na.rm = TRUE), "min\n")
    cat("  Mean:  ", round(mean(verified$onset_lag_min, na.rm = TRUE), 1), "min\n")
    cat("  Range: ", min(verified$onset_lag_min, na.rm = TRUE), "to",
        max(verified$onset_lag_min, na.rm = TRUE), "min\n")
    cat("Peak-Lag (rain peak -> sensor peak):\n")
    cat("  Median:", median(verified$peak_lag_min, na.rm = TRUE), "min\n")
    cat("  Mean:  ", round(mean(verified$peak_lag_min, na.rm = TRUE), 1), "min\n")
    cat("  Range: ", min(verified$peak_lag_min, na.rm = TRUE), "to",
        max(verified$peak_lag_min, na.rm = TRUE), "min\n\n")
    by_station <- verified %>%
        group_by(station) %>%
        summarise(
            n_events         = n(),
            median_onset_lag = round(median(onset_lag_min, na.rm = TRUE), 1),
            median_peak_lag  = round(median(peak_lag_min, na.rm = TRUE), 1),
            .groups = "drop"
        )
    cat("Per station:\n")
    print(as.data.frame(by_station))
    cat("\n")
}

# ── 5. Export CSV ─────────────────────────────────────────────────────────────

write_excel_csv(comparison_df, output_csv)
cat("Results saved to:", output_csv, "\n\n")

# ── 6. Generate plots ─────────────────────────────────────────────────────────

if (!dir_exists(plots_dir)) dir_create(plots_dir, recurse = TRUE)

for (st in unique(comparison_df$station)) {
    st_events <- comparison_df %>% filter(station == st, sc_verified == TRUE)
    if (nrow(st_events) == 0) {
        cat("No Schillerschule-verified events for", st, "- skipping plots\n")
        next
    }

    # Load cleaned sensor data for this station
    cleaned_file <- file.path(cleaned_dir, paste0(st, "_merged_export_cleaned.csv"))
    sensor_df <- read_csv(cleaned_file, show_col_types = FALSE) %>%
        mutate(level_cm = level * 100)

    cat("Generating plots for", st, "(", nrow(st_events), "events)...\n")

    for (i in seq_len(nrow(st_events))) {
        ev <- st_events[i, ]
        t_start <- ev$start_time - hours(LEAD_IN_HOURS)
        t_end   <- ev$end_time   + hours(TRAIL_HOURS)

        # Sensor window
        sensor_win <- sensor_df %>%
            filter(Zeit_Datum >= t_start, Zeit_Datum <= t_end)

        # Rain window
        rain_win <- rain_sc %>%
            filter(timestamp >= t_start, timestamp <= t_end)

        if (nrow(sensor_win) == 0) next

        max_level <- max(sensor_win$level_cm, na.rm = TRUE)
        max_rain  <- max(rain_win$rain_mm, na.rm = TRUE)
        if (!is.finite(max_level) || max_level < 1) max_level <- 1
        if (!is.finite(max_rain)  || max_rain  < 0.01) max_rain <- 0.1
        scale_factor <- max_level / max_rain

        p <- ggplot() +
            # Rain bars (secondary axis, scaled)
            geom_col(
                data = rain_win,
                aes(x = timestamp, y = rain_mm * scale_factor),
                fill = "steelblue", alpha = 0.5, width = 55
            ) +
            # Water level line
            geom_line(
                data = sensor_win,
                aes(x = Zeit_Datum, y = level_cm),
                color = "darkred", linewidth = 1
            ) +
            # Event window markers
            geom_vline(xintercept = as.numeric(ev$start_time), linetype = "dashed", color = "gray40") +
            geom_vline(xintercept = as.numeric(ev$end_time),   linetype = "dashed", color = "gray40") +
            scale_y_continuous(
                name     = "Wasserstand (cm)",
                limits   = c(0, max_level * 1.1),
                sec.axis = sec_axis(~ . / scale_factor, name = "Niederschlag Schillerschule (mm/min)")
            ) +
            scale_x_datetime(date_labels = "%H:%M\n%d.%m", date_breaks = "30 min") +
            labs(
                title    = paste0(gsub("_", " ", st), " – Ereignis ", format(ev$start_time, "%d.%m.%Y %H:%M")),
                subtitle = paste0(
                    "Onset-Lag: ", ifelse(is.na(ev$onset_lag_min), "n.v.", paste0(ev$onset_lag_min, " min")),
                    "  |  Peak-Lag: ", ifelse(is.na(ev$peak_lag_min), "n.v.", paste0(ev$peak_lag_min, " min")),
                    "  |  Schillerschule gesamt: ", ev$sc_total_mm, " mm"
                ),
                x        = NULL,
                caption  = "Regen: Schillerschule (1-min, max yard/garden)  |  Pegel: NIVUS-Sensor"
            ) +
            theme_minimal(base_size = 11) +
            theme(
                axis.text.x       = element_text(size = 8),
                axis.title.y.right = element_text(color = "steelblue"),
                axis.title.y.left  = element_text(color = "darkred"),
                plot.title         = element_text(face = "bold")
            )

        safe_st   <- gsub("[^a-zA-Z0-9_]", "_", st)
        safe_time <- format(ev$start_time, "%Y%m%d_%H%M")
        plot_file <- file.path(plots_dir, paste0(safe_st, "_", safe_time, ".png"))
        ggsave(plot_file, p, width = 12, height = 5, dpi = 150)
        cat("  Saved:", path_file(plot_file), "\n")
    }
}

cat("\nDone.\n")
