library(rdwd)
library(terra)
library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
metadata_file <- "data/metadata/sensor_metadata.csv"
output_file <- "data/processed/precipitation_at_sensors.csv"
temp_dir <- "data/processed/tmp_radolan"

cat("🚀 Starting RADOLAN RW (1h) Precipitation Fetcher...\n")

# 1. Load sensor coordinates
if (!file.exists(metadata_file)) stop("Metadata file not found: ", metadata_file)
sensors <- read_csv(metadata_file, show_col_types = FALSE) %>%
    select(station, lat, lon)

# Create temp dir
if (!dir_exists(temp_dir)) dir_create(temp_dir)

# 2. Determine timeframe
start_date <- as.Date("2025-09-01") # When sensors started having good data
end_date <- Sys.Date()

if (file.exists(output_file)) {
    existing_data <- read_csv(output_file, show_col_types = FALSE)
    if (nrow(existing_data) > 0) {
        last_date <- max(as.Date(existing_data$timestamp))
        start_date <- max(start_date, last_date)
        cat("Existing data found. Resuming from:", as.character(start_date), "\n")
    }
} else {
    # Initialize empty file with headers
    cat("No existing precipitation data. Initializing", output_file, "\n")
    df_init <- data.frame(timestamp = POSIXct(), station = character(), precipitation_mm = numeric())
    write_csv(df_init, output_file)
}


# 3. Fetch data from DWD using rdwd
cat("Fetching file list from DWD...\n")
urls_recent <- selectDWD(res = "recent", var = "radolan/rw", per = "hr")

# To be smart with storage, we process the last 24 available files
n_files <- min(24, length(urls_recent))
selected_urls <- urls_recent[(length(urls_recent) - n_files + 1):length(urls_recent)]

cat("Processing", n_files, "RADOLAN files (one by one to save space)...\n")

for (url in selected_urls) {
    cat("  Processing:", basename(url), "\n")

    # Download file quietly
    file <- try(dataDWD(url, dir = temp_dir, read = FALSE, quiet = TRUE), silent = TRUE)
    if (inherits(file, "try-error")) next

    # readDWD handles the RADOLAN binary format
    # It returns a list or a SpatRaster depending on the version/format
    grid <- try(readDWD(file), silent = TRUE)

    if (!inherits(grid, "try-error") && !is.null(grid)) {
        # Normalize to SpatRaster (rdwd may return different types depending on version)
        if (!inherits(grid, "SpatRaster")) {
            grid <- try(rast(grid), silent = TRUE)
        }

        if (inherits(grid, "SpatRaster")) {
            # Use first layer only (RW files are single-layer: one hour)
            if (nlyr(grid) > 1) grid <- grid[[1]]

            # Extract timestamp from filename (format: RW_YYMMDDHHMM)
            ts_str <- gsub(".*RW_([0-9]{10}).*", "\\1", basename(file))
            ts <- as.POSIXct(ts_str, format = "%y%m%d%H%M", tz = "UTC")

            # Project sensor coordinates (WGS84) into RADOLAN polar stereographic CRS
            sensor_vect <- vect(
                data.frame(lon = sensors$lon, lat = sensors$lat),
                geom = c("lon", "lat"),
                crs = "EPSG:4326"
            )
            sensor_proj <- project(sensor_vect, crs(grid))

            # Extract precipitation values at sensor locations
            extracted <- extract(grid, sensor_proj)
            precip_vals <- extracted[, 2]

            # Mask RADOLAN no-data / clutter values:
            # rdwd scales raw values by /10 → 250 mm/h corresponds to raw 2500 (no-data flag)
            precip_vals[!is.finite(precip_vals) | precip_vals < 0 | precip_vals >= 250] <- NA_real_
            precip_vals[is.na(precip_vals)] <- 0.0

            new_rows <- sensors %>%
                mutate(
                    timestamp = ts,
                    precipitation_mm = precip_vals
                ) %>%
                select(timestamp, station, precipitation_mm)

            cat("    Precipitation extracted:", paste(round(precip_vals, 2), collapse = ", "), "mm/h\n")

            # Append to CSV
            write_csv(new_rows, output_file, append = file.exists(output_file))
        }
    }

    # Delete binary file immediately to save space
    if (file.exists(file)) file_delete(file)
}


# Cleanup
# dir_delete(temp_dir)

cat("✅ RADOLAN processing complete.\n")
