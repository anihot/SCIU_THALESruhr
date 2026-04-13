library(rdwd)
library(terra)
library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
metadata_file  <- "data/metadata/sensor_metadata.csv"
output_file_ry <- "data/processed/precipitation_at_sensors.csv"       # RY 5-min (Event-Detektion)
output_file_rw <- "data/processed/precipitation_hourly_at_sensors.csv" # RW stündlich, stationsgeeicht (Referenz)
temp_dir       <- "data/processed/tmp_radolan"

# ═══════════════════════════════════════════════════════════════════════════════
# RADOLAN Dual-Produkt-Fetcher
#
#   Produkt A – RADOLAN YW: 5-Minuten-Komposit, 1 km, NICHT stationsgeeicht
#     → Hohe zeitliche Auflösung für Event-Detektion & Visualisierung
#     → Einheit: mm / 5 min
#     → Tägliche Archive: .../5_minutes/radolan/recent/YW-YYMMDD.tar.gz (ab Sep 2024)
#
#   Stündliche Aggregation: YW 5-min → Stundensummen
#     → DWD RW (stündlich, stationsgeeicht) ist ab 2025 nicht mehr verfügbar
#     → Stattdessen: Aggregation der YW 5-min-Daten zu mm/h pro Sensor
#     → Wird für Lag-Analyse genutzt (pro-Sensor, besser als Open-Meteo Einzelpunkt)
# ═══════════════════════════════════════════════════════════════════════════════

cat("Starting RADOLAN Dual Fetcher (YW 5-min + RW hourly)...\n")

# Prüfe ob dwdradar verfügbar ist (benötigt zum Lesen von RADOLAN-Binärdateien)
if (requireNamespace("dwdradar", quietly = TRUE)) {
    cat("dwdradar package: available\n")
} else {
    cat("WARNING: dwdradar package not installed. Falling back to terra::rast() for RADOLAN reading.\n")
}

# 1. Sensor-Koordinaten laden
if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
sensors <- read_csv(metadata_file, show_col_types = FALSE) %>%
    select(station, lat, lon)

if (!dir_exists(temp_dir)) dir_create(temp_dir)

# Sensor-Vektor einmalig erstellen
sensor_vect_wgs84 <- vect(
    data.frame(lon = sensors$lon, lat = sensors$lat),
    geom = c("lon", "lat"),
    crs  = "EPSG:4326"
)

# 2. Gemeinsame Helper-Funktion: RADOLAN-Binärdatei → Sensorwerte extrahieren
.radolan_debug_printed <- FALSE  # Einmal pro Lauf Debug-Info ausgeben
.radolan_error_printed <- FALSE  # Ersten Fehler ausgeben zum Debugging

process_radolan_file <- function(file_path, out_file, nodata_threshold = 30) {
    # Primär: dwdradar::readRadarFile() direkt aufrufen — funktioniert zuverlässig
    # für RADOLAN-Binärdateien (raa01-yw_...-bin), egal ob komprimiert oder nicht.
    grid <- NULL

    if (requireNamespace("dwdradar", quietly = TRUE)) {
        res <- tryCatch(
            dwdradar::readRadarFile(file_path),
            error = function(e) {
                if (!.radolan_error_printed) {
                    cat("    [ERROR] dwdradar::readRadarFile failed:", conditionMessage(e), "\n")
                    cat("    [ERROR] File:", file_path, "\n")
                    .radolan_error_printed <<- TRUE
                }
                NULL
            }
        )

        if (!is.null(res) && is.list(res) && "dat" %in% names(res)) {
            if (!.radolan_debug_printed) {
                cat("    [DEBUG] readRadarFile OK, dim:",
                    paste(dim(res$dat), collapse = "x"), "\n")
                .radolan_debug_printed <<- TRUE
            }
            m <- res$dat
            r <- rast(nrows = nrow(m), ncols = ncol(m), vals = as.vector(t(m[nrow(m):1, ])))
            # RADOLAN-Projektion: Stereographische Projektion Deutschland
            # Extent in METERN (nicht km!) – passend zur Kugel mit a=6370040 m
            crs(r) <- "+proj=stere +lat_0=90 +lat_ts=60 +lon_0=10 +a=6370040 +b=6370040 +no_defs"
            ext(r)  <- ext(-523462.2, 376537.8, -4658645, -3758645)
            grid    <- r
        }
    }

    # Fallback: rdwd::readDWD() probieren (falls direkter dwdradar-Aufruf scheiterte)
    if (is.null(grid)) {
        grid_try <- tryCatch(
            readDWD(file_path),
            error = function(e) NULL
        )
        if (!is.null(grid_try)) {
            if (is.list(grid_try) && "dat" %in% names(grid_try)) {
                m <- grid_try$dat
                r <- rast(nrows = nrow(m), ncols = ncol(m), vals = as.vector(t(m[nrow(m):1, ])))
                crs(r) <- "+proj=stere +lat_0=90 +lat_ts=60 +lon_0=10 +a=6370040 +b=6370040 +no_defs"
                ext(r)  <- ext(-523462.2, 376537.8, -4658645, -3758645)
                grid    <- r
            } else if (inherits(grid_try, "SpatRaster")) {
                grid <- grid_try
            }
        }
    }

    if (is.null(grid)) return(invisible(NULL))
    if (nlyr(grid) > 1) grid <- grid[[1]]

    # Zeitstempel aus Dateiname: raa01-{ry|rw}_10000-YYMMDDHHMM-dwd---bin
    ts_str <- gsub(".*-([0-9]{10})-.*", "\\1", basename(file_path))
    ts     <- as.POSIXct(ts_str, format = "%y%m%d%H%M", tz = "UTC")
    if (is.na(ts)) return(invisible(NULL))

    sensor_proj <- project(sensor_vect_wgs84, crs(grid))
    extracted   <- extract(grid, sensor_proj)
    precip_vals <- extracted[, 2]

    # Debug: einmalig extrahierte Rohwerte ausgeben
    if (!.radolan_debug_printed) {
        cat("    [DEBUG] Raw extracted values:", paste(round(precip_vals, 4), collapse = ", "), "\n")
        cat("    [DEBUG] Sensor proj coords (first):",
            paste(round(crds(sensor_proj)[1, ], 1), collapse = ", "), "\n")
        .radolan_debug_printed <<- TRUE
    }

    # No-data / Clutter maskieren
    precip_vals[!is.finite(precip_vals) | precip_vals < 0 | precip_vals >= nodata_threshold] <- NA_real_
    precip_vals[is.na(precip_vals)] <- 0.0

    new_rows <- sensors %>%
        mutate(timestamp = ts, precipitation_mm = precip_vals) %>%
        select(timestamp, station, precipitation_mm)

    write_csv(new_rows, out_file, append = file.exists(out_file))
    invisible(ts)
}

# 3. Resume-Logik: Startdatum je Produkt bestimmen
determine_start_date <- function(out_file, default_start = as.Date("2025-09-01")) {
    if (file.exists(out_file)) {
        existing <- read_csv(out_file, show_col_types = FALSE)
        if (nrow(existing) > 0) {
            # Prüfe ob alle Werte 0 sind (= kaputte Daten durch alten Extent-Bug)
            all_zero <- all(existing$precipitation_mm == 0, na.rm = TRUE)
            if (all_zero) {
                cat("WARNING: All existing precipitation values are 0 — resetting file (likely projection bug).\n")
                df_init <- data.frame(timestamp = as.POSIXct(character()), station = character(), precipitation_mm = numeric())
                write_csv(df_init, out_file)
                return(default_start)
            }
            last_date <- max(as.Date(existing$timestamp))
            return(max(default_start, last_date))
        }
    } else {
        df_init <- data.frame(timestamp = as.POSIXct(character()), station = character(), precipitation_mm = numeric())
        write_csv(df_init, out_file)
    }
    default_start
}

end_date <- Sys.Date()

# ═══════════════════════════════════════════════════════════════════════════════
# PRODUKT A: RADOLAN YW/RY (5-Minuten, nicht stationsgeeicht)
#   DWD stellt 5-min-Daten als tägliche YW-Archive bereit:
#     .../5_minutes/radolan/recent/YW-YYMMDD.tar.gz  (ab Sep 2024 bis heute)
#   Jedes tar.gz enthält 288 Binärdateien (raa01-yw_10000-...-dwd---bin)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat(" RADOLAN YW (5 min) – für Event-Detektion & Visualisierung\n")
cat(strrep("=", 70), "\n")

start_date_ry <- determine_start_date(output_file_ry)
cat("Resume from:", as.character(start_date_ry), "\n")

if (start_date_ry >= end_date) {
    cat("YW data is already up to date.\n")
} else {
    yw_base <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/5_minutes/radolan/recent"

    cat("\n--- Fetching YW (5-min) data:",
        as.character(start_date_ry), "to", as.character(end_date), "---\n")

    for (day in as.character(seq(start_date_ry, end_date, by = "day"))) {
        day_date <- as.Date(day)
        day_str  <- format(day_date, "%y%m%d")  # YYMMDD

        tar_url  <- paste0(yw_base, "/YW-", day_str, ".tar.gz")
        tar_file <- file.path(temp_dir, paste0("YW-", day_str, ".tar.gz"))

        cat("  Downloading YW:", day_str)
        dl <- tryCatch(
            download.file(tar_url, tar_file, mode = "wb", quiet = TRUE),
            error = function(e) { cat(" [FAILED:", e$message, "]\n"); -1L }
        )
        if (dl != 0) next
        cat("\n")

        extract_dir <- file.path(temp_dir, paste0("yw_day_", day_str))
        dir.create(extract_dir, showWarnings = FALSE)
        untar(tar_file, exdir = extract_dir)

        bin_files <- sort(list.files(extract_dir, pattern = "^raa01-yw", full.names = TRUE, recursive = TRUE))

        # Nur Dateien ab start_date_ry verarbeiten (relevant für ersten Tag)
        file_ts <- as.POSIXct(
            gsub(".*-([0-9]{10})-.*", "\\1", basename(bin_files)),
            format = "%y%m%d%H%M", tz = "UTC"
        )
        start_posix <- as.POSIXct(start_date_ry, tz = "UTC")
        bin_files   <- bin_files[!is.na(file_ts) & file_ts >= start_posix]

        cat("    Processing", length(bin_files), "5-min files...\n")
        ok <- 0L
        for (bf in bin_files) {
            res <- process_radolan_file(bf, output_file_ry, nodata_threshold = 100)
            if (!is.null(res)) ok <- ok + 1L
        }
        cat("    Successfully extracted:", ok, "/", length(bin_files), "\n")

        unlink(extract_dir, recursive = TRUE)
        file_delete(tar_file)
    }
}

cat("RADOLAN YW processing complete.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# AGGREGATION: YW 5-min → stündliche Werte
#   DWD RW (stündlich, stationsgeeicht) ist ab 2025 nicht mehr verfügbar.
#   Daher aggregieren wir die YW 5-min-Daten zu Stundensummen.
#   Vorteil: pro Sensor (nicht ein globaler Punkt wie Open-Meteo)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat(" Aggregation: YW 5-min → stündlich (für Lag-Analyse)\n")
cat(strrep("=", 70), "\n")

if (file.exists(output_file_ry)) {
    ry_data <- read_csv(output_file_ry, show_col_types = FALSE)
    if (nrow(ry_data) > 0) {
        hourly_agg <- ry_data %>%
            mutate(
                timestamp = as.POSIXct(timestamp, tz = "UTC"),
                hour      = floor_date(timestamp, "hour")
            ) %>%
            group_by(timestamp = hour, station) %>%
            summarise(precipitation_mm = sum(precipitation_mm, na.rm = TRUE), .groups = "drop") %>%
            arrange(timestamp, station)

        write_csv(hourly_agg, output_file_rw)
        cat("Aggregiert:", nrow(hourly_agg), "Stundenwerte aus", nrow(ry_data), "5-min-Werten.\n")
    } else {
        cat("Keine YW-Daten vorhanden. Stündliche Aggregation übersprungen.\n")
    }
} else {
    cat("YW-Datei nicht gefunden. Stündliche Aggregation übersprungen.\n")
}

cat("\nRADOLAN Fetcher finished.\n")
