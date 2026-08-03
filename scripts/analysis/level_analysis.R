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
process_sensor_file <- function(file_path, max_level = 0.4) {
    station_name <- path_ext_remove(path_file(file_path))
    cat("Processing station:", station_name, "\n")

    data <- read_csv(file_path, show_col_types = FALSE)

    if (!"Timestamp" %in% names(data) || !"level" %in% names(data)) {
        cat("Warning: Required columns (Timestamp, level) missing in", file_path, "\n")
        return(NULL)
    }

    df <- data %>%
        select(Timestamp, level, any_of("distance")) %>%
        mutate(Timestamp = as_datetime(Timestamp)) %>%
        arrange(Timestamp) %>%
        mutate(
            level_raw = level,
            level_final = ifelse(!is.na(level) & level <= max_level, level, 0)
        )

    n_unrealistic <- sum(!is.na(df$level_raw) & df$level_raw > max_level)
    if (n_unrealistic > 0) {
        cat("  Zeroed", n_unrealistic, "unrealistic values (>",
            round(max_level * 100), "cm) for", station_name, "\n")
    }

    # 3-point rolling median: removes isolated single-point spikes (cars,
    # vibration) while preserving consecutive elevated values (real water).
    n_lvl <- nrow(df)
    level_smoothed <- df$level_final
    if (n_lvl >= 3) {
        lvl_prev <- dplyr::lag(df$level_final)
        lvl_next <- dplyr::lead(df$level_final)
        mid_idx <- 2:(n_lvl - 1)
        level_smoothed[mid_idx] <- mapply(function(a, b, c) median(c(a, b, c)),
                                          lvl_prev[mid_idx],
                                          df$level_final[mid_idx],
                                          lvl_next[mid_idx])
    }
    df$level_final <- level_smoothed

    # Bilateral spike filter: remove points where BOTH neighbours differ by
    # more than 5 cm — these are isolated spikes that survived the median.
    threshold <- 0.05
    df_cleaned <- df %>%
        mutate(
            diff_prev = abs(level_final - lag(level_final)),
            diff_next = abs(level_final - lead(level_final)),
            is_spike = !is.na(diff_prev) & diff_prev > threshold &
                       !is.na(diff_next) & diff_next > threshold
        ) %>%
        filter(!is_spike) %>%
        select(Zeit_Datum = Timestamp, level = level_final, distance, level_raw)

    output_file <- path(output_dir, paste0(station_name, "_cleaned.csv"))
    write_csv(df_cleaned, output_file)
    cat("  Saved", nrow(df_cleaned), "records to:", output_file, "\n\n")
}

# Station-specific max level overrides (default 0.4 m = 40 cm).
STATION_MAX_LEVEL <- c(
)

# Main Loop
files <- dir_ls(input_dir, glob = "*.csv")
files <- files[!grepl("Wasserbaulabor__", basename(files))]

for (f in files) {
    sname <- path_ext_remove(path_file(f))
    max_lvl <- if (sname %in% names(STATION_MAX_LEVEL)) STATION_MAX_LEVEL[[sname]] else 0.4
    process_sensor_file(f, max_level = max_lvl)
}

cat("Analysis complete.\n")
