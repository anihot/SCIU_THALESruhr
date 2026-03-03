library(rdwd)
library(terra)
library(readr)
library(dplyr)
library(lubridate)
library(fs)

#' Fetch and Extract RADOLAN RW Data for Specific Coordinates
#' @param sensors_df Dataframe with station, lat, lon
#' @param start_date Date to start fetching from
#' @param end_date Date to end fetching at
#' @param temp_dir Directory for temporary DWD files
#' @return Dataframe with station, timestamp, precipitation_mm
process_radolan_for_sensors <- function(sensors_df, start_date, end_date, temp_dir = "tmp_radolan") {
    if (!dir_exists(temp_dir)) dir_create(temp_dir)

    # 1. Find URLs for RW data
    # RW = Hourly Radar Rainfall (1km grid)
    cat("Searching for RADOLAN RW data between", as.character(start_date), "and", as.character(end_date), "...\n")

    # RADOLAN RW is in grid/radolan/recent/bin/ or grid/radolan/historical/bin/
    # rdwd handles the selection
    urls <- selectDWD(res = "recent", var = "radolan/rw", per = "hr")

    # Filter URLs for the requested timeframe if possible,
    # but rdwd selectDWD for radolan is often just the main directory or recent zip.
    # For historical data, we might need a different approach or manual URL construction.

    # Actually, for RW hourly, rdwd often provides a single archive or many files.
    # Let's use the 'rdwd' high-level function where possible.

    # For this specific task, we'll download the files to the temp_dir
    files <- dataDWD(urls, dir = temp_dir, read = FALSE)

    # Process the downloaded files
    # This part is complex because RADOLAN files are binary and need projection
    # rdwd::readDWD.binary() or direct terra usage

    # Placeholder for local coordinate extraction logic
    # In the main script, we will iterate through the necessary timestamps
}

#' Extract point values from a RADOLAN RW file
#' @param file_path Path to the binary/zipped RW file
#' @param coords Matrix or dataframe with lon, lat
#' @return Vector of precipitation values
extract_rw_values <- function(file_path, coords) {
    # This will use terra or stars to read the binary RADOLAN format
    # and project the sensor coordinates into the RADOLAN grid (Stereographic)
    # then extract the values.
}
