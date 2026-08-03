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

    # Read data
    data <- read_csv(file_path, show_col_types = FALSE)

    if (!"Timestamp" %in% names(data) || !"level" %in% names(data)) {
        cat("Warning: Required columns (Timestamp, level) missing in", file_path, "\n")
        return(NULL)
    }

    # 1. Selection and Basic Preprocessing
    df <- data %>%
        select(Timestamp, level, any_of("distance")) %>%
        mutate(Timestamp = as_datetime(Timestamp)) %>%
        arrange(Timestamp)

    # 2. Use API-provided level directly.
    #    Zero clearly unrealistic values (sensor errors, obstructions).
    #    max_level is station-specific: default 0.4 m (40 cm).
    df <- df %>%
        mutate(
            level_raw = level,
            level_final = ifelse(!is.na(level), level, NA_real_),
            is_unrealistic = !is.na(level_final) & level_final > max_level,
            level_final = ifelse(is_unrealistic, 0, level_final)
        )

    if (any(df$is_unrealistic, na.rm = TRUE)) {
        num_unrealistic <- sum(df$is_unrealistic, na.rm = TRUE)
        cat("Note: Removed", num_unrealistic, "unrealistic data points (>",
            round(max_level * 100), "cm) for", station_name, "\n")
    }

    # 3. Filter Noise (Cars / isolated single-point spikes)
    # (a) Rolling 3-point median: replaces each point with the median of itself
    #     and its two neighbours. Single spikes disappear; consecutive elevated
    #     values (real water) are preserved.
    # (b) Bilateral spike filter (>5 cm/min on both sides) as fallback.

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

    threshold <- 0.05 # 5cm per minute is quite a lot for normal flooding

    df_cleaned <- df %>%
        mutate(
            diff_prev = abs(level_final - lag(level_final)),
            diff_next = abs(level_final - lead(level_final)),
            is_car = ifelse(!is.na(diff_prev) & diff_prev > threshold & !is.na(diff_next) & diff_next > threshold, TRUE, FALSE)
        ) %>%
        filter(!is_car) %>%
        mutate(level = round(level_final, 3),
               distance = if ("distance" %in% names(.)) round(distance, 3) else NA_real_,
               level_raw = round(level_raw, 3)) %>%
        select(Zeit_Datum = Timestamp, level, distance, level_raw)

    # Save cleaned data
    output_file <- path(output_dir, paste0(station_name, "_cleaned.csv"))
    write_csv(df_cleaned, output_file)

    cat("Saved cleaned data to:", output_file, "\n\n")
}

# Station-specific max level overrides (default 0.4 m = 40 cm).
STATION_MAX_LEVEL <- c(
)

# Main Loop
files <- dir_ls(input_dir, glob = "*.csv")
# Exclude "Wasserbaulabor_" (without "2") from analysis
files <- files[!grepl("Wasserbaulabor__", basename(files))]

for (f in files) {
    sname <- path_ext_remove(path_file(f))
    max_lvl <- if (sname %in% names(STATION_MAX_LEVEL)) STATION_MAX_LEVEL[[sname]] else 0.4
    process_sensor_file(f, max_level = max_lvl)
}

cat("Analysis complete.\n")
