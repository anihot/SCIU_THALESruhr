library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)

# Configuration
api_key <- Sys.getenv("NIVUS_API_KEY")
if (api_key == "") {
    # Fallback to hardcoded key if local
    api_key <- "REtDX0U3RENDMkMwLThDRkMtNDYzRC05RjMwLTYxMzFFQURFMUUyOEBOSVZVU1dFQi5DT006MjJkNjQ2MzctYzRmYy00MzhiLTk0NmQtYmFiNTViZjc3OGNh"
}
base_url <- "https://datakiosk-api.nivusweb.com"
export_dir <- "data/sensor_exports"

if (!dir.exists(export_dir)) {
    dir.create(export_dir, recursive = TRUE)
}

# Time range: last 30 days
to_date <- now(tzone = "UTC")
from_date <- to_date - days(30)

to_str <- format(to_date, "%Y-%m-%dT%H:%M:%SZ")
from_str <- format(from_date, "%Y-%m-%dT%H:%M:%SZ")

message(paste("Fetching data from", from_str, "to", to_str))

# 1. Fetch Stations
message("Fetching stations list...")
stations_resp <- GET(
    url = paste0(base_url, "/api/v2/stations"),
    add_headers(`X-API-Key` = api_key)
)

if (status_code(stations_resp) != 200) {
    stop("Failed to fetch stations. Status: ", status_code(stations_resp))
}

stations <- content(stations_resp, "parsed")
message(paste("Found", length(stations), "stations."))

# 2. Process each station
for (station in stations) {
    station_id <- station$Id
    station_name <- station$Name

    # Get full station info (including channels)
    message(paste("Processing station:", station_name, "(", station_id, ")..."))
    s_resp <- GET(paste0(base_url, "/api/v2/stations/", station_id), add_headers(`X-API-Key` = api_key))
    if (status_code(s_resp) != 200) next

    s_data <- content(s_resp, "parsed")
    channels <- s_data$Channels

    if (is.null(channels) || length(channels) == 0) {
        message("  No channels found for station.")
        next
    }

    station_df <- NULL

    # 3. Fetch data for each channel
    for (channel in channels) {
        c_id <- channel$Id
        c_name <- channel$Name

        message(paste("    Fetching channel:", c_name, "..."))

        d_resp <- GET(
            url = paste0(base_url, "/api/v2/data/history/", c_id),
            query = list(start = from_str, end = to_str),
            add_headers(`X-API-Key` = api_key)
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

                # Merge into station_df
                if (is.null(station_df)) {
                    station_df <- c_df
                } else {
                    station_df <- full_join(station_df, c_df, by = "Timestamp")
                }
                message(paste("      Found", nrow(c_df), "records."))
            }
        }
    }

    # 4. Save to CSV
    if (!is.null(station_df)) {
        # Sort by timestamp
        station_df <- station_df %>% arrange(Timestamp)

        safe_name <- gsub("[^[:alnum:]]", "_", station_name)
        file_path <- file.path(export_dir, paste0(safe_name, "_merged_export.csv"))
        write.csv(station_df, file_path, row.names = FALSE)
        message(paste("  SUCCESS! Saved", nrow(station_df), "rows to", file_path))
    } else {
        message("  No data found for any channel in this station.")
    }
}

message("Done.")
