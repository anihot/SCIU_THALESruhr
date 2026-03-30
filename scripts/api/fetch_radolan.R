library(rdwd)
library(terra)
library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file   <- "data/processed/precipitation_at_sensors.csv"
temp_dir      <- "data/processed/tmp_radolan"

# RADOLAN RW: 10-Minuten-Komposit, 1 km Rasterauflösung, stationsgeeicht
# Einheit nach rdwd-Skalierung: mm / 10 min
# No-data-Flag (raw >= 4095) wird nach Skalierung als >= 40 mm/10min maskiert
#
# Datenquellen:
#   Historical (>3 Tage alt): DWD OpenData tägliche tar.gz-Archive
#   URL-Muster: .../radolan/reproc/2017_002/bin/{YYYY}/RW{YYYYMMDD}.tar.gz
#   Recent (letzte ~3 Tage):   rdwd::selectDWD / dataDWD (Einzel-Binärdateien)

cat("Starting RADOLAN RW (10 min, stationsgeeicht) Precipitation Fetcher...\n")

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

# 2. Zeitrahmen bestimmen
start_date <- as.Date("2025-09-01")
end_date   <- Sys.Date()

if (file.exists(output_file)) {
    existing_data <- read_csv(output_file, show_col_types = FALSE)
    if (nrow(existing_data) > 0) {
        last_date  <- max(as.Date(existing_data$timestamp))
        start_date <- max(start_date, last_date)
        cat("Existing data found. Resuming from:", as.character(start_date), "\n")
    }
} else {
    cat("No existing precipitation data. Initializing", output_file, "\n")
    df_init <- data.frame(timestamp = POSIXct(), station = character(), precipitation_mm = numeric())
    write_csv(df_init, output_file)
}

if (start_date >= end_date) {
    cat("Precipitation data is already up to date. Nothing to fetch.\n")
    quit(save = "no", status = 0)
}

# 3. Helper: eine einzelne RADOLAN-Binärdatei einlesen und Werte extrahieren
process_radolan_file <- function(file_path) {
    grid <- try(readDWD(file_path), silent = TRUE)
    if (inherits(grid, "try-error") || is.null(grid)) return(invisible(NULL))

    if (!inherits(grid, "SpatRaster")) {
        grid <- try(rast(grid), silent = TRUE)
        if (inherits(grid, "try-error")) return(invisible(NULL))
    }
    if (nlyr(grid) > 1) grid <- grid[[1]]

    # Zeitstempel aus Dateiname: raa01-rw_10000-YYMMDDHHMM-dwd---bin
    ts_str <- gsub(".*-([0-9]{10})-.*", "\\1", basename(file_path))
    ts     <- as.POSIXct(ts_str, format = "%y%m%d%H%M", tz = "UTC")
    if (is.na(ts)) return(invisible(NULL))

    sensor_proj <- project(sensor_vect_wgs84, crs(grid))
    extracted   <- extract(grid, sensor_proj)
    precip_vals <- extracted[, 2]

    # No-data / Clutter maskieren
    precip_vals[!is.finite(precip_vals) | precip_vals < 0 | precip_vals >= 30] <- NA_real_
    precip_vals[is.na(precip_vals)] <- 0.0

    new_rows <- sensors %>%
        mutate(timestamp = ts, precipitation_mm = precip_vals) %>%
        select(timestamp, station, precipitation_mm)

    write_csv(new_rows, output_file, append = file.exists(output_file))
    invisible(ts)
}

# 4. HISTORICAL: tägliche tar.gz-Archive (alles älter als ~3 Tage)
# DWD stellt recent-Feed nur für die letzten ~3 Tage bereit;
# ältere Daten kommen aus dem reproc/2017_002-Archiv.
hist_end_date <- end_date - 3

if (start_date <= hist_end_date) {
    cat("\n--- Fetching HISTORICAL RADOLAN data:",
        as.character(start_date), "to", as.character(hist_end_date), "---\n")

    hist_base <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/10_minutes/radolan/reproc/2017_002/bin"

    for (day in as.character(seq(start_date, hist_end_date, by = "day"))) {
        day_date <- as.Date(day)
        year_str <- format(day_date, "%Y")
        day_str  <- format(day_date, "%Y%m%d")

        tar_url  <- paste0(hist_base, "/", year_str, "/RW", day_str, ".tar.gz")
        tar_file <- file.path(temp_dir, paste0("RW", day_str, ".tar.gz"))

        cat("  Downloading:", day_str)
        dl <- tryCatch(
            download.file(tar_url, tar_file, mode = "wb", quiet = TRUE),
            error = function(e) { cat(" [FAILED:", e$message, "]\n"); -1L }
        )
        if (dl != 0) next
        cat("\n")

        extract_dir <- file.path(temp_dir, paste0("day_", day_str))
        dir.create(extract_dir, showWarnings = FALSE)
        untar(tar_file, exdir = extract_dir)

        bin_files <- sort(list.files(extract_dir, pattern = "^raa01-rw", full.names = TRUE))
        cat("    Processing", length(bin_files), "10-min files...\n")
        for (bf in bin_files) process_radolan_file(bf)

        unlink(extract_dir, recursive = TRUE)
        file_delete(tar_file)
    }

    cat("Historical RADOLAN processing complete.\n")

    # start_date für den recent-Schritt aktualisieren
    start_date <- hist_end_date + 1
}

# 5. RECENT: Einzel-Binärdateien via rdwd (letzte ~3 Tage)
cat("\n--- Fetching RECENT RADOLAN data from:", as.character(start_date), "---\n")

urls_recent <- selectDWD(res = "recent", var = "radolan/rw", per = "10_minutes")

url_ts <- as.POSIXct(
    gsub(".*-([0-9]{10})-.*", "\\1", basename(urls_recent)),
    format = "%y%m%d%H%M", tz = "UTC"
)

start_posix   <- as.POSIXct(start_date, tz = "UTC")
selected_urls <- urls_recent[!is.na(url_ts) & url_ts >= start_posix]

if (length(selected_urls) == 0) {
    cat("Recent precipitation data is already up to date.\n")
} else {
    cat("Processing", length(selected_urls), "RADOLAN RW files (10 min each)...\n")

    for (url in selected_urls) {
        cat("  Processing:", basename(url), "\n")
        file <- try(dataDWD(url, dir = temp_dir, read = FALSE, quiet = TRUE), silent = TRUE)
        if (inherits(file, "try-error")) next
        process_radolan_file(file)
        if (file.exists(file)) file_delete(file)
    }
}

cat("RADOLAN RW processing complete.\n")
