library(dplyr)
library(readr)
library(fs)
library(lubridate)

# Input and Output Directories
input_dir <- "data/raw/sensor_exports"
output_dir <- "data/processed/cleaned_analysis"

# Ensure output directory exists
if (!dir_exists(output_dir)) {
    dir_create(output_dir)
}

# Function to process each file
process_sensor_file <- function(file_path) {
    station_name <- path_ext_remove(path_file(file_path))
    cat("Processing station:", station_name, "\n")

    data <- read_csv(file_path, show_col_types = FALSE)

    if (!"Timestamp" %in% names(data) || !"level" %in% names(data)) {
        cat("Warning: Required columns (Timestamp, level) missing in", file_path, "\n")
        return(NULL)
    }

    df_cleaned <- data %>%
        select(Timestamp, level, any_of("distance")) %>%
        mutate(Timestamp = as_datetime(Timestamp)) %>%
        arrange(Timestamp) %>%
        mutate(
            level_raw = level,
            level = round(level, 3),
            distance = if ("distance" %in% names(.)) round(distance, 3) else NA_real_,
            level_raw = round(level_raw, 3)
        ) %>%
        select(Zeit_Datum = Timestamp, level, distance, level_raw)

    output_file <- path(output_dir, paste0(station_name, "_cleaned.csv"))
    write_csv(df_cleaned, output_file)
    cat("Saved", nrow(df_cleaned), "records to:", output_file, "\n\n")
}

# Main Loop
files <- dir_ls(input_dir, glob = "*.csv")
files <- files[!grepl("Wasserbaulabor__", basename(files))]

for (f in files) {
    process_sensor_file(f)
}

cat("Analysis complete.\n")
