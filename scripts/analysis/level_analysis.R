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

    # 2. Use API-provided level directly (well-calibrated sensor reference).
    #    The previous approach recomputed level from distance using a sliding-
    #    window mode baseline, but the mode sits at the CENTER of the dry-state
    #    distance distribution (~5.818) instead of the TOP (~5.824). This
    #    systematically dampened real water levels by ~0.5 cm and created
    #    artificial spiky signals when the computed level bounced between 0
    #    and small positive values.
    # Use API-provided level directly. No baseline subtraction — the API
    # firmware has a well-calibrated fixed reference. Subtracting a mode
    # baseline (a) dampened signals by ~0.5 cm, (b) created artificial
    # spike patterns from zero-clamping.
    # Only filter clearly unrealistic values (sensor errors, obstructions).
    MAX_REALISTIC_LEVEL <- 0.4  # 40 cm

    df <- df %>%
        mutate(
            level_final = ifelse(!is.na(level), level, NA_real_),
            is_unrealistic = !is.na(level_final) & level_final > MAX_REALISTIC_LEVEL,
            level_final = ifelse(is_unrealistic, 0, level_final)
        )

    if (any(df$is_unrealistic, na.rm = TRUE)) {
        num_unrealistic <- sum(df$is_unrealistic, na.rm = TRUE)
        cat("Note: Removed", num_unrealistic, "unrealistic data points (>40cm) for", station_name, "\n")
    }

    # 4. Filter Noise (Cars / isolated single-point spikes)
    # (a) Rollierende 3-Punkt-Median: ersetzt jeden Punkt durch den Median aus
    #     sich selbst und seinen beiden Nachbarn. Einzelne Spike-Werte (Auto,
    #     Vibration) verschwinden, weil die Nachbarn meist 0 sind. Mehrere
    #     aufeinanderfolgende erhöhte Werte (echter Wasserstand) bleiben erhalten.
    # (b) Großer Spike-Filter (>5 cm/min zwischen Nachbarpunkten) als Fallback.

    n_lvl <- nrow(df)
    level_smoothed <- df$level_final
    if (n_lvl >= 3) {
        lvl_prev <- dplyr::lag(df$level_final)
        lvl_next <- dplyr::lead(df$level_final)
        # Elementweiser Median über (prev, curr, next); NA an Rändern → Originalwert
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
