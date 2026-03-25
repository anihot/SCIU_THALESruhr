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

# Function to calculate the mode (most frequent value), ignoring NAs
get_mode <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) {
        return(NA)
    }
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to process each file
process_sensor_file <- function(file_path) {
    station_name <- path_ext_remove(path_file(file_path))
    cat("Processing station:", station_name, "\n")

    # Read data
    data <- read_csv(file_path, show_col_types = FALSE)

    if (!all(c("Timestamp", "distance") %in% names(data))) {
        cat("Warning: Required columns missing in", file_path, "\n")
        return(NULL)
    }

    # 1. Selection and Basic Preprocessing
    df <- data %>%
        select(Timestamp, matches("^level$"), distance) %>%
        mutate(Timestamp = as_datetime(Timestamp)) %>%
        arrange(Timestamp)

    # 2. Determine Baseline(s) (Total Height)
    # Sensors can move or be recalibrated. Use a sliding window (7 days) to find the local baseline.
    # This prevents massive fake levels when a sensor setup changes mid-month.

    df <- df %>%
        mutate(date_window = as.Date(floor_date(Timestamp, "7 days")))

    # Function to get mode for a group
    calculate_window_baseline <- function(dist_vec) {
        if (all(is.na(dist_vec))) {
            return(NA)
        }
        # Coarse/Fine two-step mode
        d_coarse <- round(dist_vec, 1)
        c_mode <- get_mode(d_coarse)
        f_mode <- get_mode(round(dist_vec[round(dist_vec, 1) == c_mode], 3))
        return(f_mode)
    }

    baselines <- df %>%
        filter(!is.na(distance)) %>%
        group_by(date_window) %>%
        summarise(window_baseline = calculate_window_baseline(distance), .groups = "drop")

    df <- df %>%
        left_join(baselines, by = "date_window") %>%
        mutate(
            window_baseline = tidyr::fill(data.frame(window_baseline), .direction = "downup")$window_baseline
        )

    # 3. Calculate Level and Filter Logic
    # Level = Baseline - current distance
    # Apply logical constraints:
    # - Level cannot be negative (sensor precision noise -> 0)
    # - Level > 0.3m is unrealistic for street flooding without rain data -> likely obstacle/error
    MAX_REALISTIC_LEVEL <- 0.4

    df <- df %>%
        mutate(
            calculated_level = window_baseline - distance,
            calculated_level = ifelse(calculated_level < 0, 0, calculated_level),
            is_unrealistic = calculated_level > MAX_REALISTIC_LEVEL,
            level_final = ifelse(is_unrealistic, 0, calculated_level)
        )

    if (any(df$is_unrealistic, na.rm = TRUE)) {
        num_unrealistic <- sum(df$is_unrealistic, na.rm = TRUE)
        cat("Note: Removed", num_unrealistic, "unrealistic data points (> 1m) for", station_name, "\n")
    }

    # 4. Filter Noise (Cars)
    # Sudden spikes (back and forth) are likely cars.
    # Threshold for "spike": e.g., > 10cm jump compared to previous AND next value
    # Or simply a jump larger than expected for rain events.
    threshold <- 0.05 # 5cm per minute is quite a lot for normal flooding

    df_cleaned <- df %>%
        mutate(
            diff_prev = abs(calculated_level - lag(calculated_level)),
            diff_next = abs(calculated_level - lead(calculated_level)),
            is_car = ifelse(!is.na(diff_prev) & diff_prev > threshold & !is.na(diff_next) & diff_next > threshold, TRUE, FALSE)
        ) %>%
        filter(!is_car) %>%
        mutate(level = round(level_final, 3), distance = round(distance, 3)) %>%
        select(Zeit_Datum = Timestamp, level, distance)

    # Save cleaned data
    output_file <- path(output_dir, paste0(station_name, "_cleaned.csv"))
    write_csv(df_cleaned, output_file)

    cat("Saved cleaned data to:", output_file, "\n\n")
}

# Main Loop
files <- dir_ls(input_dir, glob = "*.csv")
# Exclude "Wasserbaulabor_" (without "2") from analysis
files <- files[!grepl("Wasserbaulabor__", basename(files))]

for (f in files) {
    process_sensor_file(f)
}

cat("Analysis complete.\n")
