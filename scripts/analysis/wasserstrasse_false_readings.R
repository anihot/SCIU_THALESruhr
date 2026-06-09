library(readr)
library(dplyr)
library(lubridate)

# --- 1. Load data ---
ws <- read_csv("data/processed/cleaned_analysis/Wasserstraße_Springorum_merged_export_cleaned.csv",
               show_col_types = FALSE)

precip <- read_csv("data/processed/precipitation_at_sensors.csv",
                   show_col_types = FALSE) %>%
    filter(grepl("Wasserstra", station, ignore.case = TRUE))

cat("=== Wasserstraße: Analyse der ~0.82m Distance-Episoden ===\n\n")

# --- 2. Identify episodes where distance drops to ~0.82m ---
# Normal dry state: distance ~7.16m
# Suspicious: distance < 1.0m (clearly not the normal range)
ws_suspicious <- ws %>%
    filter(!is.na(distance), distance < 1.0, distance > 0.1) %>%
    arrange(Zeit_Datum) %>%
    mutate(
        time_diff = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum), units = "mins")),
        episode = cumsum(is.na(time_diff) | time_diff > 60)
    )

cat("Total suspicious readings (distance 0.1-1.0m):", nrow(ws_suspicious), "\n")

# Summarize episodes
episodes <- ws_suspicious %>%
    group_by(episode) %>%
    summarize(
        start = min(Zeit_Datum),
        end = max(Zeit_Datum),
        duration_hours = round(as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "hours")), 1),
        n_readings = n(),
        mean_dist = round(mean(distance), 3),
        min_dist = round(min(distance), 3),
        max_dist = round(max(distance), 3),
        .groups = "drop"
    ) %>%
    filter(n_readings >= 3)  # ignore isolated single readings

cat("Number of episodes (≥3 readings, gap >60min):", nrow(episodes), "\n\n")

# --- 3. Cross-reference with DWD precipitation ---
results <- list()

for (i in seq_len(nrow(episodes))) {
    ep <- episodes[i, ]
    # Check precipitation 3h before start to 1h after end
    window_start <- ep$start - hours(3)
    window_end <- ep$end + hours(1)

    precip_window <- precip %>%
        filter(timestamp >= window_start, timestamp <= window_end)

    total_precip <- sum(precip_window$precipitation_mm, na.rm = TRUE)
    # 5-min intervals → multiply by 12 for mm/h
    max_intensity <- if (nrow(precip_window) > 0) max(precip_window$precipitation_mm, na.rm = TRUE) * 12 else 0

    # Also check a wider window: 6h before to 2h after
    precip_wide <- precip %>%
        filter(timestamp >= (ep$start - hours(6)), timestamp <= (ep$end + hours(2)))
    total_precip_wide <- sum(precip_wide$precipitation_mm, na.rm = TRUE)

    rain_confirmed <- total_precip > 0.5
    rain_confirmed_wide <- total_precip_wide > 0.5

    results[[i]] <- tibble(
        episode = i,
        start = ep$start,
        end = ep$end,
        duration_h = ep$duration_hours,
        n_readings = ep$n_readings,
        mean_dist = ep$mean_dist,
        precip_3h_mm = round(total_precip, 2),
        max_intensity_mm_h = round(max_intensity, 1),
        precip_6h_mm = round(total_precip_wide, 2),
        rain_3h = rain_confirmed,
        rain_6h = rain_confirmed_wide
    )
}

results_df <- bind_rows(results)

# --- 4. Summary ---
cat("=== ERGEBNISSE: Distance ~0.82m Episoden vs. DWD-Niederschlag ===\n\n")

cat(sprintf("%-4s %-20s %-20s %6s %6s %7s %8s %8s %8s %6s\n",
    "Nr", "Start", "Ende", "Dauer", "Mess.", "Dist_m", "DWD_3h", "DWD_6h", "MaxInt", "Regen"))
cat(paste(rep("-", 110), collapse = ""), "\n")

for (i in seq_len(nrow(results_df))) {
    r <- results_df[i, ]
    cat(sprintf("%-4d %-20s %-20s %5.1fh %5d %7.3f %7.2f %7.2f %7.1f %6s\n",
        r$episode,
        format(r$start, "%Y-%m-%d %H:%M"),
        format(r$end, "%Y-%m-%d %H:%M"),
        r$duration_h,
        r$n_readings,
        r$mean_dist,
        r$precip_3h_mm,
        r$precip_6h_mm,
        r$max_intensity_mm_h,
        ifelse(r$rain_3h, "JA", "NEIN")))
}

# Summary stats
n_total <- nrow(results_df)
n_rain <- sum(results_df$rain_3h)
n_no_rain <- n_total - n_rain
n_rain_wide <- sum(results_df$rain_6h)

cat(paste("\n", rep("=", 80), collapse = ""), "\n")
cat("\n=== ZUSAMMENFASSUNG ===\n")
cat(sprintf("Gesamt-Episoden: %d\n", n_total))
cat(sprintf("Mit DWD-Niederschlag (3h-Fenster): %d (%.0f%%)\n", n_rain, 100 * n_rain / n_total))
cat(sprintf("OHNE DWD-Niederschlag (3h-Fenster): %d (%.0f%%)\n", n_no_rain, 100 * n_no_rain / n_total))
cat(sprintf("Mit DWD-Niederschlag (6h-Fenster): %d (%.0f%%)\n", n_rain_wide, 100 * n_rain_wide / n_total))
cat(sprintf("OHNE DWD-Niederschlag (6h-Fenster): %d (%.0f%%)\n\n", n_total - n_rain_wide, 100 * (n_total - n_rain_wide) / n_total))

# Show episodes WITHOUT rain separately
cat("=== EPISODEN OHNE REGEN (potenzielle Sensor-Artefakte) ===\n\n")
no_rain <- results_df %>% filter(!rain_6h)
if (nrow(no_rain) > 0) {
    for (i in seq_len(nrow(no_rain))) {
        r <- no_rain[i, ]
        cat(sprintf("  Episode %d: %s bis %s (%.1fh, %d Messwerte, mittl. Dist: %.3fm)\n",
            r$episode, format(r$start, "%Y-%m-%d %H:%M"), format(r$end, "%Y-%m-%d %H:%M"),
            r$duration_h, r$n_readings, r$mean_dist))
    }
} else {
    cat("  Keine — alle Episoden hatten DWD-bestätigten Niederschlag.\n")
}

cat("\n=== EPISODEN MIT REGEN (wahrscheinlich echt, aber möglicherweise überhöht) ===\n\n")
with_rain <- results_df %>% filter(rain_3h) %>% arrange(desc(precip_3h_mm))
if (nrow(with_rain) > 0) {
    for (i in seq_len(min(20, nrow(with_rain)))) {
        r <- with_rain[i, ]
        cat(sprintf("  Episode %d: %s bis %s | DWD: %.1f mm, Max: %.1f mm/h\n",
            r$episode, format(r$start, "%Y-%m-%d %H:%M"), format(r$end, "%Y-%m-%d %H:%M"),
            r$precip_3h_mm, r$max_intensity_mm_h))
    }
}

# --- 5. Save results to CSV ---
write_csv(results_df, "data/processed/wasserstrasse_082_episodes_vs_dwd.csv")
cat("\n\nErgebnisse gespeichert: data/processed/wasserstrasse_082_episodes_vs_dwd.csv\n")
