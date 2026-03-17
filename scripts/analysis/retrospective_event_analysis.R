library(readr)
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)
library(fs)
library(knitr)
library(tidyr)

# Config
cleaned_dir    <- "data/processed/cleaned_analysis"
events_file    <- "data/processed/detected_events.csv"
events_md_file <- "data/processed/detected_events.md"
historical_log <- "data/metadata/historical_verified_events.csv"

THRESHOLD      <- 0.015  # 1.5 cm
MIN_GAP_MINS   <- 20
LEAD_IN_HOURS  <- 3      # Look at rain 3h before event start

# Center of sensor area (Bochum/Hattingen)
LAT <- 51.48
LON <- 7.21

cat("=== Retrospective Event Analysis ===\n")
cat("Detects ALL historical events and correlates with Open-Meteo archive precipitation.\n\n")

# ── 1. Event Detection ────────────────────────────────────────────────────────

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
            avg_gradient_cm_min = (peak_level_m * 100) / (as.numeric(difftime(peak_time, start_time, units = "mins")) + 0.1),
            avg_gradient_cm_min = round(avg_gradient_cm_min, 3),
            peak_level_cm       = round(peak_level_m * 100, 2),
            event_type = case_when(
                (avg_gradient_cm_min > 0.5) | (duration_min < 45 & peak_level_cm > 2.0) ~ "Sturzflut-Ereignis",
                peak_level_cm >= 2.0 ~ "Regenereignis / Natürlich",
                TRUE ~ "Leichter Regen / Unterhalb DWD-Schwelle"
            )
        ) %>%
        select(station, start_time, end_time, peak_level_cm, peak_time,
               duration_min, avg_gradient_cm_min, event_type, points_count) %>%
        mutate(station = gsub("_merged_export", "", station))

    return(events)
}

if (!dir_exists(cleaned_dir)) stop("Cleaned data directory not found.")

cleaned_files <- dir_ls(cleaned_dir, glob = "*.csv")
all_events    <- list()

for (file_path in cleaned_files) {
    station <- gsub("_cleaned\\.csv$", "", path_file(file_path))
    cat("Scanning", station, "...\n")

    tryCatch({
        df <- read_csv(file_path, show_col_types = FALSE)
        ev <- detect_events(df, station)
        if (!is.null(ev)) {
            all_events[[station]] <- ev
            cat("  Found", nrow(ev), "events.\n")
        }
    }, error = function(e) {
        cat("  Error processing", station, ":", e$message, "\n")
    })
}

if (length(all_events) == 0) {
    cat("\nNo events detected across all stations. Exiting.\n")
    quit(save = "no", status = 0)
}

events_df <- bind_rows(all_events) %>% arrange(start_time)
cat("\nTotal events detected:", nrow(events_df), "\n")

# ── 2. Fetch Historical Precipitation (Open-Meteo Archive) ───────────────────

start_date <- format(min(as.Date(events_df$start_time)) - days(1), "%Y-%m-%d")
end_date   <- format(Sys.Date(), "%Y-%m-%d")

cat("\nFetching Open-Meteo archive precipitation from", start_date, "to", end_date, "...\n")

url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", LAT, "&longitude=", LON,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&hourly=precipitation",
    "&timezone=Europe%2FBerlin"
)

response <- try(GET(url), silent = TRUE)

if (inherits(response, "try-error") || status_code(response) != 200) {
    cat("WARNING: Could not fetch Open-Meteo archive data. Events will be kept without precipitation context.\n")
    precip_df <- NULL
} else {
    data      <- fromJSON(content(response, "text", encoding = "UTF-8"))
    precip_df <- data.frame(
        timestamp      = as.POSIXct(data$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "Europe/Berlin"),
        precipitation_mm = data$hourly$precipitation
    )
    cat("Loaded", nrow(precip_df), "hourly precipitation records.\n")
}

# ── 3. Correlate Events with Precipitation ────────────────────────────────────

if (!is.null(precip_df)) {
    events_df <- events_df %>%
        rowwise() %>%
        mutate(
            window_start     = start_time - hours(LEAD_IN_HOURS),
            window_end       = end_time,
            window_precip    = list(precip_df %>% filter(timestamp >= window_start, timestamp <= window_end)),
            total_precip_mm  = sum(window_precip[[1]]$precipitation_mm, na.rm = TRUE),
            max_intensity_mm_h = ifelse(
                nrow(window_precip[[1]]) > 0,
                max(window_precip[[1]]$precipitation_mm, na.rm = TRUE),
                0
            ),
            openmeteo_verified = total_precip_mm > 0
        ) %>%
        select(-window_precip, -window_start, -window_end) %>%
        ungroup()

    # DWD risk level based on hourly precipitation intensity
    events_df <- events_df %>%
        mutate(
            dwd_risk_level = case_when(
                max_intensity_mm_h >= 40 ~ "Level 4 – Extremes Unwetter (>= 40 mm/h)",
                max_intensity_mm_h >= 25 ~ "Level 3 – Unwetterwarnung (>= 25 mm/h)",
                max_intensity_mm_h >= 15 ~ "Level 2 – Starkregen (>= 15 mm/h)",
                max_intensity_mm_h >= 0.5 ~ "Level 1 – Leichter Regen (0.5–15 mm/h)",
                TRUE ~ "Level 0 – Kein nennenswerter Regen"
            )
        )

    verified_count   <- sum(events_df$openmeteo_verified, na.rm = TRUE)
    unverified_count <- nrow(events_df) - verified_count
    cat("\nPrecipitation correlation complete:\n")
    cat("  Rain-verified (Open-Meteo):", verified_count, "\n")
    cat("  No rain detected:          ", unverified_count, "(kept – sensor signature is primary evidence)\n")
} else {
    events_df <- events_df %>%
        mutate(
            total_precip_mm    = NA_real_,
            max_intensity_mm_h = NA_real_,
            openmeteo_verified = NA,
            dwd_risk_level     = NA_character_
        )
}

# ── 4. Export ─────────────────────────────────────────────────────────────────

events_out <- events_df %>% arrange(desc(start_time))

write_excel_csv(events_out, events_file)

md_table <- kable(events_out, format = "markdown")
write_lines(c("# Detected Events Log (Retrospective)", "", md_table), events_md_file)

cat("\nSUCCESS: Wrote", nrow(events_out), "events to", events_file, "\n")

# ── 5. Update Historical Log ──────────────────────────────────────────────────

if (nrow(events_out) > 0) {
    if (file_exists(historical_log)) {
        hist_orig    <- read_csv(historical_log, show_col_types = FALSE) %>%
            mutate(start_time = as.POSIXct(start_time), end_time = as.POSIXct(end_time))
        new_entries  <- events_out %>% anti_join(hist_orig, by = c("station", "start_time"))
        hist_updated <- bind_rows(hist_orig, new_entries) %>% arrange(start_time)
        write_excel_csv(hist_updated, historical_log)
        cat("Historical log updated:", nrow(new_entries), "new entries added",
            "(", nrow(hist_updated), "total).\n")
    } else {
        write_excel_csv(events_out, historical_log)
        cat("Historical log created with", nrow(events_out), "entries.\n")
    }
}
