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

# RADOLAN RY: 5-Minuten-Komposit, 1 km Rasterauflösung
# Einheit nach rdwd-Skalierung: mm / 5 min
# No-data-Flag (raw >= 4095) wird nach Skalierung als >= 40 mm/5min maskiert

cat("Starting RADOLAN RY (5 min) Precipitation Fetcher...\n")

# 1. Sensor-Koordinaten laden
if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
sensors <- read_csv(metadata_file, show_col_types = FALSE) %>%
    select(station, lat, lon)

if (!dir_exists(temp_dir)) dir_create(temp_dir)

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

# 3. RY-Dateiliste vom DWD holen
cat("Fetching file list from DWD...\n")
urls_recent <- selectDWD(res = "recent", var = "radolan/ry", per = "5_minutes")

# 24h × 12 Dateien/h = 288 Dateien für vollständige 24h-Abdeckung
n_files       <- min(288, length(urls_recent))
selected_urls <- urls_recent[(length(urls_recent) - n_files + 1):length(urls_recent)]

cat("Processing", n_files, "RADOLAN RY files (5 min each)...\n")

# Sensor-Vektor einmalig erstellen (wird in der Schleife wiederverwendet)
sensor_vect_wgs84 <- vect(
    data.frame(lon = sensors$lon, lat = sensors$lat),
    geom = c("lon", "lat"),
    crs  = "EPSG:4326"
)

for (url in selected_urls) {
    cat("  Processing:", basename(url), "\n")

    file <- try(dataDWD(url, dir = temp_dir, read = FALSE, quiet = TRUE), silent = TRUE)
    if (inherits(file, "try-error")) next

    grid <- try(readDWD(file), silent = TRUE)

    if (!inherits(grid, "try-error") && !is.null(grid)) {
        # Normalisieren auf SpatRaster
        if (!inherits(grid, "SpatRaster")) {
            grid <- try(rast(grid), silent = TRUE)
        }

        if (inherits(grid, "SpatRaster")) {
            if (nlyr(grid) > 1) grid <- grid[[1]]

            # Zeitstempel aus Dateiname extrahieren
            # RY-Format: raa01-ry_10000-YYMMDDHHMM-dwd---bin
            ts_str <- gsub(".*-([0-9]{10})-.*", "\\1", basename(file))
            ts     <- as.POSIXct(ts_str, format = "%y%m%d%H%M", tz = "UTC")

            # Sensor-Koordinaten in RADOLAN-CRS projizieren
            sensor_proj <- project(sensor_vect_wgs84, crs(grid))

            # Punktextraktion
            extracted  <- extract(grid, sensor_proj)
            precip_vals <- extracted[, 2]

            # No-data / Clutter maskieren:
            # RY raw no-data = 4095; nach rdwd-Skalierung (/100) ≈ 40.95 mm/5min
            # Physikalisch unmögliche Werte (>= 30 mm / 5 min) ebenfalls maskieren
            precip_vals[!is.finite(precip_vals) | precip_vals < 0 | precip_vals >= 30] <- NA_real_
            precip_vals[is.na(precip_vals)] <- 0.0

            new_rows <- sensors %>%
                mutate(
                    timestamp        = ts,
                    precipitation_mm = precip_vals   # Einheit: mm / 5 min
                ) %>%
                select(timestamp, station, precipitation_mm)

            cat("    Extracted (mm/5min):", paste(round(precip_vals, 3), collapse = ", "), "\n")

            write_csv(new_rows, output_file, append = file.exists(output_file))
        }
    }

    if (file.exists(file)) file_delete(file)
}

cat("RADOLAN RY processing complete.\n")
