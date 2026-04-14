library(readr)
library(dplyr)
library(lubridate)
library(fs)
library(knitr)
library(tidyr)
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

# Config
cleaned_dir    <- "data/processed/cleaned_analysis"
# Retrospektive Analyse schreibt in EIGENE Dateien (nicht detected_events.csv überschreiben!)
events_file    <- "data/processed/retrospective_events.csv"
events_md_file <- "data/processed/retrospective_events.md"
historical_log <- "data/metadata/historical_verified_events.csv"
historical_md  <- "data/metadata/historical_verified_events.md"
precip_file    <- "data/processed/precipitation_at_sensors.csv"

THRESHOLD          <- 0.004  # 0.4 cm - Seed-Schwelle
LOW_THRESHOLD      <- 0.002  # 0.2 cm - Schwelle für "erhöht" (active_fraction)
MIN_GAP_MINS       <- 60     # Gaps < 60 min → zusammenhängendes Ereignis
MIN_DURATION_MINS  <- 5   # Events kürzer als 5 min = Rauschen / Einzelspike
MAX_DURATION_MINS  <- 180 # Events länger als 3h werden nicht berücksichtigt
MIN_RISE_MINS      <- 3   # Mindestanstiegszeit bis zum Peak (filtert Blips/Fahrzeuge)
MIN_FALL_MINS      <- 3   # Mindestabfallzeit nach dem Peak (filtert Blips/Fahrzeuge)
MAX_LOCAL_PEAKS    <- 2   # Max. lokale Peaks im Ereignis (filtert Pulse Chains)
MAX_PLATEAU_FRAC   <- 0.5 # Max. Anteil Messwerte >= 85% des Peaks (filtert Box-Signale)
MIN_ACTIVE_FRAC    <- 0.5 # Min. Anteil Messpunkte mit Pegel > LOW_THRESHOLD (filtert Spike-Chains)
MIN_ROLLING_MEDIAN <- 0.002 # Min. Maximum einer rollierenden 3-Punkt-Median (filtert isolierte Spikes)
LEAD_IN_HOURS      <- 3   # Niederschlag-Vorlauf-Fenster vor Event-Start

cat("=== Retrospective Event Analysis ===\n")
cat("Detects ALL historical events and correlates with Open-Meteo archive precipitation.\n\n")

# ── 1. Event Detection ────────────────────────────────────────────────────────

detect_events <- function(df, station_name) {
    if (nrow(df) == 0) return(NULL)

    df <- df %>% arrange(Zeit_Datum)

    # Seeds: Punkte über THRESHOLD (keine LOW_THRESHOLD-Gruppierung → vermeidet Spike-Chain-Merges)
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
        rowwise() %>%
        mutate(
            active_fraction = {
                slice_lvls <- df$level[df$Zeit_Datum >= start_time & df$Zeit_Datum <= end_time]
                if (length(slice_lvls) == 0) 0 else round(mean(slice_lvls > LOW_THRESHOLD, na.rm = TRUE), 3)
            },
            max_rolling_median = {
                slice_lvls <- df$level[df$Zeit_Datum >= start_time & df$Zeit_Datum <= end_time]
                n <- length(slice_lvls)
                if (n < 3) 0 else {
                    rm <- sapply(2:(n-1), function(i) median(slice_lvls[(i-1):(i+1)]))
                    round(max(rm, na.rm = TRUE), 5)
                }
            }
        ) %>%
        ungroup() %>%
        filter(
            duration_min       >= MIN_DURATION_MINS,
            duration_min       <= MAX_DURATION_MINS,
            rise_min           >= MIN_RISE_MINS,
            fall_min           >= MIN_FALL_MINS,
            n_local_peaks      <= MAX_LOCAL_PEAKS,
            plateau_fraction   <= MAX_PLATEAU_FRAC,
            active_fraction    >= MIN_ACTIVE_FRAC,
            max_rolling_median >= MIN_ROLLING_MEDIAN
        ) %>%
        select(station, start_time, end_time, peak_level_cm, peak_time,
               duration_min, avg_gradient_cm_min, event_type, points_count) %>%
        mutate(station = gsub("_merged_export", "", station))

    return(events)
}

if (!dir_exists(cleaned_dir)) stop("Cleaned data directory not found.")

cleaned_files <- dir_ls(cleaned_dir, glob = "*.csv")
# Exclude "Wasserbaulabor_" (without "2") from retrospective analysis
cleaned_files <- cleaned_files[!grepl("Wasserbaulabor__", basename(cleaned_files))]
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

# ── 2. RADOLAN-Niederschlagsdaten laden (gemessen, pro Sensor) ────────────────

precip <- tibble(timestamp = as.POSIXct(character()), station = character(), precipitation_mm = numeric())
precip_available <- FALSE

if (file.exists(precip_file)) {
    precip_raw <- read_csv(precip_file, show_col_types = FALSE)
    if (nrow(precip_raw) > 0) {
        precip           <- precip_raw %>% mutate(timestamp = as.POSIXct(timestamp))
        precip_available <- TRUE
        cat("RADOLAN precipitation data loaded:", nrow(precip), "records.\n")
    } else {
        cat("WARNING: RADOLAN precipitation file is empty.\n")
    }
} else {
    cat("WARNING: RADOLAN precipitation file not found:", precip_file, "\n")
    cat("  Events will be kept without precipitation context.\n")
}

# ── 3. Correlate Events with RADOLAN Precipitation (pro Sensor) ──────────────

if (precip_available) {
    precip_stations_unique <- unique(precip$station)
    match_precip_station <- function(st) {
        st_norm <- gsub("_", " ", st)
        m <- precip_stations_unique[
            sapply(precip_stations_unique, function(ps) grepl(ps, st_norm, fixed = TRUE))
        ]
        if (length(m) > 0) m[1] else st
    }

    precip_stats <- purrr::pmap_dfr(
        list(events_df$station, events_df$start_time, events_df$end_time),
        function(st, t_start, t_end) {
            precip_key <- match_precip_station(st)
            # RADOLAN-Daten für diese Station im Zeitfenster (3h Vorlauf + Event)
            station_precip <- precip %>%
                dplyr::filter(station == precip_key,
                              timestamp >= (t_start - hours(LEAD_IN_HOURS)),
                              timestamp <= t_end)
            data.frame(
                total_precip_mm    = round(sum(station_precip$precipitation_mm, na.rm = TRUE), 2),
                max_intensity_mm_h = if (nrow(station_precip) > 0)
                    round(max(station_precip$precipitation_mm, na.rm = TRUE) * 12, 2)  # mm/5min → mm/h
                else 0
            )
        }
    )

    events_df <- bind_cols(events_df, precip_stats) %>%
        mutate(
            radolan_verified = total_precip_mm > 0,
            dwd_risk_level = case_when(
                max_intensity_mm_h >= 40  ~ "Level 4 – Extremes Unwetter (>= 40 mm/h)",
                max_intensity_mm_h >= 25  ~ "Level 3 – Unwetterwarnung (>= 25 mm/h)",
                max_intensity_mm_h >= 15  ~ "Level 2 – Starkregen (>= 15 mm/h)",
                max_intensity_mm_h >= 0.5 ~ "Level 1 – Leichter Regen (0.5–15 mm/h)",
                TRUE                      ~ "Level 0 – Kein nennenswerter Regen"
            )
        )

    # Nur Ereignisse behalten, bei denen RADOLAN Niederschlag bestätigt hat
    # ODER kein RADOLAN-Daten für den Zeitraum vorliegen (um False Negatives zu vermeiden).
    # Wasserbaulabor_2 ist ein Indoor-Testsensor und wird immer behalten.
    n_before         <- nrow(events_df)
    events_df        <- events_df %>%
        filter(radolan_verified == TRUE |
               is.na(radolan_verified) |
               grepl("Wasserbaulabor", station, ignore.case = TRUE))
    verified_count   <- nrow(events_df)
    unverified_count <- n_before - verified_count
    cat("\nPrecipitation correlation complete (RADOLAN):\n")
    cat("  Rain-verified:", verified_count, "\n")
    cat("  Herausgefiltert (kein Regen):", unverified_count, "\n")
} else {
    events_df <- events_df %>%
        mutate(
            total_precip_mm    = NA_real_,
            max_intensity_mm_h = NA_real_,
            radolan_verified   = NA,
            dwd_risk_level     = NA_character_
        )
}

# ── 4. Export ─────────────────────────────────────────────────────────────────

events_out <- events_df %>% arrange(desc(start_time))

write_excel_csv(events_out, events_file)

md_table <- kable(events_out, format = "markdown")
write_lines(c("# Detected Events Log (Retrospective)", "", md_table), events_md_file)

cat("\nSUCCESS: Wrote", nrow(events_out), "events to", events_file, "\n")

# ── 5. Export je Station ──────────────────────────────────────────────────────

station_events_dir <- "data/processed/events_by_station"
if (!dir_exists(station_events_dir)) dir_create(station_events_dir)

for (st in unique(events_out$station)) {
    st_df     <- events_out %>% filter(station == st) %>% arrange(desc(start_time))
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", st)
    st_csv    <- file.path(station_events_dir, paste0(safe_name, "_events.csv"))
    st_md     <- file.path(station_events_dir, paste0(safe_name, "_events.md"))

    write_excel_csv(st_df, st_csv)
    write_lines(
        c(paste0("# Ereignisse: ", st), "", knitr::kable(st_df, format = "markdown")),
        st_md
    )
    cat("  Station", st, "->", nrow(st_df), "Ereignisse gespeichert.\n")
}

# ── 7. Update Historical Log ──────────────────────────────────────────────────

if (nrow(events_out) > 0) {
    if (file_exists(historical_log)) {
        hist_orig    <- read_csv(historical_log, show_col_types = FALSE) %>%
            mutate(
                start_time = as.POSIXct(start_time),
                end_time   = as.POSIXct(end_time),
                peak_time  = as.POSIXct(peak_time)
            )
        new_entries  <- events_out %>% anti_join(hist_orig, by = c("station", "start_time"))
        hist_updated <- bind_rows(hist_orig, new_entries) %>% arrange(start_time)
        write_excel_csv(hist_updated, historical_log)
        write_lines(c("# Historische Ereignisse (verifiziert)", "",
                      kable(hist_updated, format = "markdown")), historical_md)
        cat("Historical log updated:", nrow(new_entries), "new entries added",
            "(", nrow(hist_updated), "total).\n")
    } else {
        write_excel_csv(events_out, historical_log)
        write_lines(c("# Historische Ereignisse (verifiziert)", "",
                      kable(events_out, format = "markdown")), historical_md)
        cat("Historical log created with", nrow(events_out), "entries.\n")
    }
}

# ── 8. Export: all_detected_events.xlsx ───────────────────────────────────────
#    Nutzt detected_events.csv (von event_detection.R + correlate_events.R),
#    NICHT die retrospektive Analyse — damit alle Events in Summary & Excel erscheinen.

xlsx_file     <- "data/output/all_detected_events.xlsx"
summary_file  <- "data/output/events_summary_table.md"

# Events aus der regulären Pipeline laden (nicht aus retrospektiver Analyse)
main_events_file <- "data/processed/detected_events.csv"
if (file_exists(main_events_file)) {
    events_for_export <- read_csv(main_events_file, show_col_types = FALSE)
    cat("Loaded", nrow(events_for_export), "events from detected_events.csv for export.\n")
} else {
    events_for_export <- events_out
}

if (nrow(events_for_export) > 0) {
    wb <- createWorkbook()

    # Sheet 1: alle Events
    addWorksheet(wb, "Alle Ereignisse")
    writeDataTable(wb, "Alle Ereignisse", events_for_export, tableStyle = "TableStyleMedium9")

    # Spaltenbreiten anpassen
    setColWidths(wb, "Alle Ereignisse", cols = 1:ncol(events_for_export), widths = "auto")

    # Sheet 2: Stationsübersicht
    station_summary <- events_for_export %>%
        group_by(station) %>%
        summarise(
            Ereignisse        = n(),
            Ø_Peak_cm         = round(mean(peak_level_cm), 1),
            Max_Peak_cm       = max(peak_level_cm),
            Sturzflut_Anzahl  = sum(event_type == "Sturzflut-Ereignis", na.rm = TRUE),
            Regen_Anzahl      = sum(event_type == "Regenereignis / Natürlich", na.rm = TRUE),
            Verdächtig_Anzahl = sum(event_type == "Verdächtig / Kein Regen", na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(desc(Ereignisse))

    addWorksheet(wb, "Stationsübersicht")
    writeDataTable(wb, "Stationsübersicht", station_summary, tableStyle = "TableStyleMedium2")
    setColWidths(wb, "Stationsübersicht", cols = 1:ncol(station_summary), widths = "auto")

    # Sheet 3: Top 20 intensivste Events
    top20 <- events_for_export %>%
        arrange(desc(peak_level_cm)) %>%
        head(20) %>%
        select(any_of(c("station", "start_time", "duration_min", "peak_level_cm",
               "avg_gradient_cm_min", "event_type",
               "total_precip_mm", "max_intensity_mm_h", "dwd_risk_level")))

    addWorksheet(wb, "Top 20 Events")
    writeDataTable(wb, "Top 20 Events", top20, tableStyle = "TableStyleMedium3")
    setColWidths(wb, "Top 20 Events", cols = 1:ncol(top20), widths = "auto")

    saveWorkbook(wb, xlsx_file, overwrite = TRUE)
    cat("Excel export saved to:", xlsx_file, "\n")

    # ── 9. Export: events_summary_table.md ────────────────────────────────────

    md_lines <- c(
        "# Sensor Event Analysis Summary",
        "",
        "## Station Overview",
        "",
        kable(station_summary, format = "markdown"),
        "",
        "## Top 20 Most Intense Events",
        "",
        kable(top20, format = "markdown"),
        "",
        paste0("*Full log available in data/processed/detected_events.csv — ",
               nrow(events_for_export), " events total.*")
    )

    write_lines(md_lines, summary_file)
    cat("Summary table saved to:", summary_file, "\n")
}
