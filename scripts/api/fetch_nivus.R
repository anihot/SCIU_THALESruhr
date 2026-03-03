library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)

# Configuration
api_key <- Sys.getenv("NIVUS_API_KEY")
if (api_key == "") {
    message("Notice: NIVUS_API_KEY environment variable is empty. Using fallback hardcoded key.")
    api_key <- "REtDX0U3RENDMkMwLThDRkMtNDYzRC05RjMwLTYxMzFFQURFMUUyOEBOSVZVU1dFQi5DT006MjJkNjQ2MzctYzRmYy00MzhiLTk0NmQtYmFiNTViZjc3OGNh"
} else {
    # Clean up the key (remove quotes/whitespace)
    api_key <- trimws(gsub("^\"|\"$", "", api_key))
    # Log part of the key for debugging (GitHub will still mask if it matches a secret)
    key_prefix <- substr(api_key, 1, 5)
    key_suffix <- substr(api_key, nchar(api_key) - 4, nchar(api_key))
    message(paste0(
        "Notice: Using API key from environment (Length: ", nchar(api_key),
        ", Format: ", key_prefix, "...", key_suffix, ")"
    ))
}

base_url <- "https://datakiosk-api.nivusweb.com"
export_dir <- "data/raw/sensor_exports"

# Header Configuration
custom_headers <- add_headers(
    `X-API-Key` = api_key,
    `User-Agent` = "R-httr-SCIU-Project"
)

if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
}

# Time range Configuration
HISTORICAL_FETCH <- FALSE # Set to TRUE for a full backfill from 2025

to_date <- now(tzone = "UTC")
if (HISTORICAL_FETCH) {
    from_date <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
    message("!!! HISTORICAL FETCH ENABLED: Fetching from 2025-01-01 !!!")
} else {
    from_date <- to_date - days(30)
}

to_str <- format(to_date, "%Y-%m-%dT%H:%M:%SZ")
from_str <- format(from_date, "%Y-%m-%dT%H:%M:%SZ")

message(paste("Fetching data from", from_str, "to", to_str))

# 1. Fetch Stations
message("Fetching stations list...")
# Print headers for debugging (Masking key)
# message("Headers used:")
# message(paste("  X-API-Key:", paste0(substr(api_key, 1, 5), "...", substr(api_key, nchar(api_key)-5, nchar(api_key)))))

stations_resp <- GET(
    url = paste0(base_url, "/api/v2/stations"),
    custom_headers,
    verbose() # Add verbose output for detailed trace in GitHub Actions
)

if (status_code(stations_resp) != 200) {
    message("Error Status Code: ", status_code(stations_resp))
    message("Error Response Body:")
    print(content(stations_resp, "text"))
    stop("Failed to fetch stations.")
}

stations <- content(stations_resp, "parsed")
message(paste("Found", length(stations), "stations total."))

# 2. Filter stations: exclude those starting with "2407"
stations <- stations %>% keep(~ !grepl("^2407", .x$Name))
message(paste("Processing", length(stations), "stations after filtering (excluded '2407*')."))

# 3. Process each station
for (station in stations) {
    station_id <- station$Id
    station_name <- station$Name

    # Get full station info (including channels)
    message(paste("Processing station:", station_name, "(", station_id, ")..."))
    s_resp <- GET(paste0(base_url, "/api/v2/stations/", station_id), custom_headers)
    if (status_code(s_resp) != 200) next

    s_data <- content(s_resp, "parsed")
    channels <- s_data$Channels

    if (is.null(channels) || length(channels) == 0) {
        message("  No channels found for station.")
        next
    }

    all_channels_data <- list()

    # 3. Fetch data for each channel
    for (channel in channels) {
        c_id <- channel$Id
        c_name <- tolower(channel$Name)

        # Optimization: Only fetch level and distance to save time and space
        if (!c_name %in% c("level", "distance")) next

        message(paste("    Fetching channel:", c_name, "..."))

        d_resp <- GET(
            url = paste0(base_url, "/api/v2/data/history/", c_id),
            query = list(start = from_str, end = to_str),
            custom_headers
        )

        if (status_code(d_resp) == 200) {
            d_vals <- content(d_resp, "parsed")
            if (length(d_vals) > 0) {
                # Convert to data frame
                c_df <- map_df(d_vals, function(x) {
                    list(
                        Timestamp = as.character(x$ValTime),
                        Value = as.numeric(x$Val)
                    )
                })

                # Rename Value column to channel name
                names(c_df)[2] <- c_name
                all_channels_data[[length(all_channels_data) + 1]] <- c_df
                message(paste("      Found", nrow(c_df), "records."))
            }
        }
    }

    # Process results if we have data
    if (length(all_channels_data) > 0) {
        # Use reduce to full_join all channel data frames
        station_df <- reduce(all_channels_data, full_join, by = "Timestamp")

        safe_name <- gsub("[^[:alnum:]]", "_", station_name)
        file_path <- file.path(export_dir, paste0(safe_name, "_merged_export.csv"))

        # Merge with existing file if it exists
        if (file.exists(file_path)) {
            existing_df <- read.csv(file_path, stringsAsFactors = FALSE)
            # Ensure Timestamp formats match for de-duplication
            station_df <- bind_rows(existing_df, station_df) %>%
                distinct(Timestamp, .keep_all = TRUE)
            message(paste("  Merged with existing data. Total records:", nrow(station_df)))
        }

        # Sort by timestamp
        station_df <- station_df %>% arrange(Timestamp)

        write.csv(station_df, file_path, row.names = FALSE)
        message(paste("  SUCCESS! Saved", nrow(station_df), "records to", file_path))
    } else {
        message("  No 'level' or 'distance' data found for this station.")
    }
}

message("Done.")
