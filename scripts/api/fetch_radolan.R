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
#   Produkt A – RADOLAN RY: 5-Minuten-Komposit, 1 km, NICHT stationsgeeicht
#     → Hohe zeitliche Auflösung für Event-Detektion & Visualisierung
#     → Einheit: mm / 5 min
#     → Historical (reproc): .../5_minutes/radolan/reproc/2017_002/bin/{YYYY}/YW2017.002_{YYYYMM}.tar
#     → Recent (~3 Tage):    rdwd::selectDWD (radolan/ry)
#
#   Produkt B – RADOLAN RW: stündliches Komposit, 1 km, stationsgeeicht
#     → Genauere Niederschlagsmenge als Referenz (Lag-Analyse, Gesamtmengen)
#     → Einheit: mm / h
#     → Historical (reproc): .../hourly/radolan/reproc/2017_002/bin/{YYYY}/RW2017.002_{YYYYMM}.tar.gz
#     → Recent (~3 Tage):    Einzel-.gz unter .../hourly/radolan/recent/bin/
# ═══════════════════════════════════════════════════════════════════════════════

cat("Starting RADOLAN Dual Fetcher (RY 5-min + RW hourly)...\n")

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
process_radolan_file <- function(file_path, out_file, nodata_threshold = 30) {
    grid <- try(readDWD(file_path), silent = TRUE)
    if (inherits(grid, "try-error") || is.null(grid)) return(invisible(NULL))

    if (!inherits(grid, "SpatRaster")) {
        grid <- try(rast(grid), silent = TRUE)
        if (inherits(grid, "try-error")) return(invisible(NULL))
    }
    if (nlyr(grid) > 1) grid <- grid[[1]]

    # Zeitstempel aus Dateiname: raa01-{ry|rw}_10000-YYMMDDHHMM-dwd---bin
    ts_str <- gsub(".*-([0-9]{10})-.*", "\\1", basename(file_path))
    ts     <- as.POSIXct(ts_str, format = "%y%m%d%H%M", tz = "UTC")
    if (is.na(ts)) return(invisible(NULL))

    sensor_proj <- project(sensor_vect_wgs84, crs(grid))
    extracted   <- extract(grid, sensor_proj)
    precip_vals <- extracted[, 2]

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
            last_date <- max(as.Date(existing$timestamp))
            return(max(default_start, last_date))
        }
    } else {
        df_init <- data.frame(timestamp = POSIXct(), station = character(), precipitation_mm = numeric())
        write_csv(df_init, out_file)
    }
    default_start
}

end_date <- Sys.Date()

# ═══════════════════════════════════════════════════════════════════════════════
# PRODUKT A: RADOLAN RY (5-Minuten, nicht stationsgeeicht)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat(" RADOLAN RY (5 min) – für Event-Detektion & Visualisierung\n")
cat(strrep("=", 70), "\n")

start_date_ry <- determine_start_date(output_file_ry)
cat("Resume from:", as.character(start_date_ry), "\n")

if (start_date_ry >= end_date) {
    cat("RY data is already up to date.\n")
} else {
    hist_end_date <- end_date - 3

    # --- RY HISTORICAL: monatliche YW-Archive aus reproc ---
    if (start_date_ry <= hist_end_date) {
        cat("\n--- Fetching HISTORICAL RY data:",
            as.character(start_date_ry), "to", as.character(hist_end_date), "---\n")

        ry_hist_base <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/5_minutes/radolan/reproc/2017_002/bin"

        # Monatliche Archive: YW2017.002_{YYYYMM}.tar
        months_needed <- unique(format(seq(start_date_ry, hist_end_date, by = "day"), "%Y-%m"))

        for (ym in months_needed) {
            year_str  <- substr(ym, 1, 4)
            month_str <- gsub("-", "", ym)  # YYYYMM

            tar_url  <- paste0(ry_hist_base, "/", year_str, "/YW2017.002_", month_str, ".tar")
            tar_file <- file.path(temp_dir, paste0("YW2017.002_", month_str, ".tar"))

            cat("  Downloading RY month:", month_str)
            dl <- tryCatch(
                download.file(tar_url, tar_file, mode = "wb", quiet = TRUE),
                error = function(e) { cat(" [FAILED:", e$message, "]\n"); -1L }
            )
            if (dl != 0) next
            cat("\n")

            extract_dir <- file.path(temp_dir, paste0("ry_month_", month_str))
            dir.create(extract_dir, showWarnings = FALSE)
            untar(tar_file, exdir = extract_dir)

            bin_files <- sort(list.files(extract_dir, pattern = "^raa01-yw", full.names = TRUE, recursive = TRUE))

            # Nur Dateien ab start_date_ry verarbeiten
            file_ts <- as.POSIXct(
                gsub(".*-([0-9]{10})-.*", "\\1", basename(bin_files)),
                format = "%y%m%d%H%M", tz = "UTC"
            )
            start_posix <- as.POSIXct(start_date_ry, tz = "UTC")
            bin_files   <- bin_files[!is.na(file_ts) & file_ts >= start_posix]

            cat("    Processing", length(bin_files), "5-min files...\n")
            for (bf in bin_files) process_radolan_file(bf, output_file_ry, nodata_threshold = 30)

            unlink(extract_dir, recursive = TRUE)
            file_delete(tar_file)
        }

        cat("Historical RY processing complete.\n")
        start_date_ry <- hist_end_date + 1
    }

    # --- RY RECENT: Einzel-Binärdateien via rdwd ---
    cat("\n--- Fetching RECENT RY data from:", as.character(start_date_ry), "---\n")

    urls_recent_ry <- tryCatch(
        selectDWD(res = "recent", var = "radolan/ry", per = "5_minutes"),
        warning = function(w) character(0),
        error   = function(e) character(0)
    )

    if (length(urls_recent_ry) > 0) {
        url_ts <- as.POSIXct(
            gsub(".*-([0-9]{10})-.*", "\\1", basename(urls_recent_ry)),
            format = "%y%m%d%H%M", tz = "UTC"
        )
        start_posix    <- as.POSIXct(start_date_ry, tz = "UTC")
        selected_urls  <- urls_recent_ry[!is.na(url_ts) & url_ts >= start_posix]

        if (length(selected_urls) > 0) {
            cat("Processing", length(selected_urls), "RADOLAN RY files (5 min each)...\n")
            for (url in selected_urls) {
                file <- try(dataDWD(url, dir = temp_dir, read = FALSE, quiet = TRUE), silent = TRUE)
                if (inherits(file, "try-error")) next
                process_radolan_file(file, output_file_ry, nodata_threshold = 30)
                if (file.exists(file)) file_delete(file)
            }
        } else {
            cat("Recent RY data is already up to date.\n")
        }
    } else {
        cat("Warning: selectDWD returned no RY recent URLs. Skipping recent RY.\n")
    }
}

cat("RADOLAN RY processing complete.\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PRODUKT B: RADOLAN RW (stündlich, stationsgeeicht)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 70), "\n")
cat(" RADOLAN RW (hourly, stationsgeeicht) – Referenz für Niederschlagsmengen\n")
cat(strrep("=", 70), "\n")

start_date_rw <- determine_start_date(output_file_rw)
cat("Resume from:", as.character(start_date_rw), "\n")

if (start_date_rw >= end_date) {
    cat("RW data is already up to date.\n")
} else {
    hist_end_date <- end_date - 3

    # --- RW HISTORICAL: monatliche Archive aus reproc ---
    if (start_date_rw <= hist_end_date) {
        cat("\n--- Fetching HISTORICAL RW data:",
            as.character(start_date_rw), "to", as.character(hist_end_date), "---\n")

        rw_hist_base <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/reproc/2017_002/bin"

        months_needed <- unique(format(seq(start_date_rw, hist_end_date, by = "day"), "%Y-%m"))

        for (ym in months_needed) {
            year_str  <- substr(ym, 1, 4)
            month_str <- gsub("-", "", ym)

            tar_url  <- paste0(rw_hist_base, "/", year_str, "/RW2017.002_", month_str, ".tar.gz")
            tar_file <- file.path(temp_dir, paste0("RW2017.002_", month_str, ".tar.gz"))

            cat("  Downloading RW month:", month_str)
            dl <- tryCatch(
                download.file(tar_url, tar_file, mode = "wb", quiet = TRUE),
                error = function(e) { cat(" [FAILED:", e$message, "]\n"); -1L }
            )
            if (dl != 0) next
            cat("\n")

            extract_dir <- file.path(temp_dir, paste0("rw_month_", month_str))
            dir.create(extract_dir, showWarnings = FALSE)
            untar(tar_file, exdir = extract_dir)

            bin_files <- sort(list.files(extract_dir, pattern = "^raa01-rw", full.names = TRUE, recursive = TRUE))

            # Nur Dateien ab start_date_rw
            file_ts <- as.POSIXct(
                gsub(".*-([0-9]{10})-.*", "\\1", basename(bin_files)),
                format = "%y%m%d%H%M", tz = "UTC"
            )
            start_posix <- as.POSIXct(start_date_rw, tz = "UTC")
            bin_files   <- bin_files[!is.na(file_ts) & file_ts >= start_posix]

            cat("    Processing", length(bin_files), "hourly files...\n")
            # RW ist stationsgeeicht → höherer Clutter-Schwellwert
            for (bf in bin_files) process_radolan_file(bf, output_file_rw, nodata_threshold = 100)

            unlink(extract_dir, recursive = TRUE)
            file_delete(tar_file)
        }

        cat("Historical RW processing complete.\n")
        start_date_rw <- hist_end_date + 1
    }

    # --- RW RECENT: Einzel-.gz-Dateien direkt von DWD ---
    cat("\n--- Fetching RECENT RW data from:", as.character(start_date_rw), "---\n")

    recent_base <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/recent/bin"

    # Stündliche Zeitstempel generieren
    start_posix <- as.POSIXct(as.character(start_date_rw), tz = "UTC")
    end_posix   <- as.POSIXct(Sys.time(), tz = "UTC")
    hourly_seq  <- seq(start_posix, end_posix, by = "hour")

    cat("Processing up to", length(hourly_seq), "RADOLAN RW files (hourly)...\n")

    for (ts in hourly_seq) {
        ts_posix   <- as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
        ts_str     <- format(ts_posix, "%y%m%d%H%M")
        file_name  <- paste0("raa01-rw_10000-", ts_str, "-dwd---bin.gz")
        file_url   <- paste0(recent_base, "/", file_name)
        local_file <- file.path(temp_dir, file_name)

        dl <- tryCatch(
            download.file(file_url, local_file, mode = "wb", quiet = TRUE),
            error = function(e) -1L
        )
        if (dl != 0) next

        # .gz entpacken
        bin_file <- sub("\\.gz$", "", local_file)
        tryCatch({
            con_in  <- gzfile(local_file, "rb")
            con_out <- file(bin_file, "wb")
            while (length(chunk <- readBin(con_in, "raw", n = 65536)) > 0) writeBin(chunk, con_out)
            close(con_in)
            close(con_out)
        }, error = function(e) NULL)

        if (file.exists(bin_file)) {
            process_radolan_file(bin_file, output_file_rw, nodata_threshold = 100)
            file_delete(bin_file)
        }
        if (file.exists(local_file)) file_delete(local_file)
    }
}

cat("RADOLAN RW processing complete.\n")
cat("\nDual RADOLAN Fetcher finished.\n")
