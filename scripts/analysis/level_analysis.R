library(dplyr)
library(readr)
library(fs)
library(lubridate)
library(zoo)

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

# ============================================================
# Distance-Baseline-Recomputation für Königsallee
# ============================================================
# Die API liefert level = firmware_reference - distance mit einem festen
# Referenzwert. Bei Sensorverschiebungen (Temperatur, Rekalibration) driftet
# dieser Referenzwert, was Schein-Ereignisse produziert. Gleichzeitig liegt
# der Trockenzustand mancher Tage ÜBER der Firmware-Referenz, sodass echte
# Ereignisse durch Zero-Clamping teilweise verschwinden.
#
# Lösung: level aus einer rollierenden 7-Tage-90th-Perzentile der Distance
# berechnen. Der Unterschied zum alten Ansatz (Mode-Baseline):
#   - Wir verwenden p90 statt Mode → erfasst den trockenen Oberrand,
#     nicht die Mitte der Verteilung → kein systematisches Dämpfen.
#   - Shift-Erkennung: wenn in einer Stunde SOWOHL p90 als auch p10 der
#     Distance signifikant fallen, liegt ein Sensorshift vor (nicht Regen).
#     Bei echtem Regen bleibt der p90 nahe am Trockenwert, nur p10 fällt.

DISTANCE_BASELINE_STATIONS <- c("Königsallee_Springorum_merged_export_cleaned.csv")

recompute_distance_baseline <- function(cleaned_csv_path) {
    station_label <- basename(cleaned_csv_path)
    cat("Distance-baseline recomputation:", station_label, "\n")

    df <- read_csv(cleaned_csv_path, show_col_types = FALSE) %>%
        mutate(Zeit_Datum = as_datetime(Zeit_Datum)) %>%
        arrange(Zeit_Datum)

    if (!"distance" %in% names(df) || nrow(df) < 10) {
        cat("  Skipped: missing distance column or too few rows.\n")
        return(invisible(NULL))
    }

    # ---- Schritt 0: Obstruktions-Distance filtern ----
    # Spikes (Autos, Objekte) erzeugen extrem niedrige Distance-Werte (z.B.
    # 2–3 m statt ~5.82 m). Diese verfälschen p10, Shift-Erkennung und
    # Level-Berechnung. Median ± 3×IQR als robuster Bereich.
    dist_median <- median(df$distance, na.rm = TRUE)
    dist_iqr    <- IQR(df$distance, na.rm = TRUE)
    dist_lower  <- dist_median - 3 * pmax(dist_iqr, 0.01)
    df$distance_clean <- ifelse(
        !is.na(df$distance) & df$distance >= dist_lower,
        df$distance,
        NA_real_
    )
    n_dist_outliers <- sum(!is.na(df$distance) & is.na(df$distance_clean))
    if (n_dist_outliers > 0) {
        cat("  Filtered", n_dist_outliers, "distance outliers (< ", round(dist_lower, 3), "m)\n")
    }

    # ---- Schritt 1: Stündliche p90/p10 der Distance ----
    df_h <- df %>%
        filter(!is.na(distance_clean)) %>%
        mutate(hour = floor_date(Zeit_Datum, "hour")) %>%
        group_by(hour) %>%
        summarise(
            dist_p90 = quantile(distance_clean, 0.90, na.rm = TRUE),
            dist_p10 = quantile(distance_clean, 0.10, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(hour)

    n_h <- nrow(df_h)
    if (n_h < 2) {
        cat("  Skipped: insufficient hourly data.\n")
        return(invisible(NULL))
    }

    # ---- Schritt 2: 7-Tage-rollierende 90th-Perzentile der stündlichen p90 ----
    # = langfristiger Trockenzustand (Referenz für Shift-Erkennung & Level)
    window_h <- min(7L * 24L, n_h)
    raw_baseline <- zoo::rollapply(
        df_h$dist_p90,
        width    = window_h,
        FUN      = function(x) quantile(x, 0.90, na.rm = TRUE),
        fill     = NA,
        align    = "right"
    )
    # Expanding window für die ersten < 7 Tage
    early <- which(is.na(raw_baseline))
    for (i in early) {
        raw_baseline[i] <- quantile(df_h$dist_p90[seq_len(i)], 0.90, na.rm = TRUE)
    }
    df_h$dry_baseline <- raw_baseline

    # ---- Schritt 3: Shift-Erkennung ----
    # Shift-Kandidat: p90 der Stunde > 0.3 cm und p10 > 0.5 cm unter dry_baseline.
    # Echte Regenstunde: nur p10 fällt (trockene Minutenmessungen halten p90 hoch).
    # Bestätigt wenn >= 3 aufeinanderfolgende Shift-Kandidat-Stunden.
    df_h <- df_h %>%
        mutate(
            drop_p90        = dry_baseline - dist_p90,
            drop_p10        = dry_baseline - dist_p10,
            shift_candidate = (drop_p90 > 0.003) & (drop_p10 > 0.005)
        )

    rc      <- rle(df_h$shift_candidate)
    df_h$run_id <- rep(seq_along(rc$lengths), rc$lengths)

    df_h <- df_h %>%
        group_by(run_id) %>%
        mutate(confirmed_shift = shift_candidate & (n() >= 3)) %>%
        ungroup() %>%
        mutate(
            # Onset-Extension: die ersten 1–2 Stunden eines Shifts haben noch
            # kein bestätigtes run_n ≥ 3. Rückwärts verlängern: eine Stunde gilt
            # auch als bestätigt, wenn sie Shift-Kandidat ist UND eine der nächsten
            # 2 Stunden bereits bestätigt ist.
            confirmed_shift = confirmed_shift |
                (shift_candidate & (
                    dplyr::lead(confirmed_shift, 1, default = FALSE) |
                    dplyr::lead(confirmed_shift, 2, default = FALSE)
                ))
        )

    # ---- Schritt 4: Level neu berechnen ----
    df <- df %>%
        mutate(hour = floor_date(Zeit_Datum, "hour")) %>%
        left_join(
            df_h %>% select(hour, dry_baseline, drop_p90, confirmed_shift),
            by = "hour"
        ) %>%
        mutate(
            # Rohes Level relativ zur rollierenden Trockenreferenz
            # Obstruktions-Distance (NA in distance_clean) → Level = 0
            level_raw  = dplyr::if_else(
                is.na(distance_clean),
                0,
                pmax(0, dry_baseline - distance, na.rm = FALSE)
            ),
            # Shift-Korrektur: bei bestätigtem Sensorshift den Drift abziehen
            shift_corr = dplyr::if_else(
                confirmed_shift & !is.na(drop_p90),
                pmax(0, drop_p90),
                0
            ),
            level_new  = pmax(0, level_raw - shift_corr, na.rm = FALSE),
            level_new  = replace(level_new, is.na(level_new), 0),
            level_new  = pmin(level_new, 0.4),  # 40 cm cap (same as process_sensor_file)
            level_new  = round(level_new, 3),
            level      = level_new,
            # Rohpegel (vor Shift-Korrektur) und Baseline für Visualisierung speichern
            level_raw  = round(pmin(replace(level_raw, is.na(level_raw), 0), 0.4), 3)
        ) %>%
        select(Zeit_Datum, level, distance, level_raw)

    write_csv(df, cleaned_csv_path)
    cat("  Done –", nrow(df), "rows recomputed.\n")
    invisible(NULL)
}

cat("\n---- Distance-baseline recomputation ----\n")
for (fname in DISTANCE_BASELINE_STATIONS) {
    fpath <- path(output_dir, fname)
    if (file_exists(fpath)) {
        recompute_distance_baseline(fpath)
    } else {
        cat("Not found, skipping:", fname, "\n")
    }
}
