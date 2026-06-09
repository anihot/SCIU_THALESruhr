library(readr)
library(dplyr)
library(lubridate)

# ============================================================
# Retrospektive Analyse: Echte Ereignisse an der Wasserstraße?
# ============================================================
# Frage: Gab es RADOLAN-verifizierte Episoden mit Variabilität
# im Sensorwert, die auf echte Wasserstandsänderungen hindeuten
# (im Gegensatz zum konstanten ~0.82m-Artefakt)?
#
# Indikatoren für ECHTE Ereignisse:
#   1. Distance-Variabilität (SD > 0.01m) — Artefakt ist extrem konstant
#   2. Gradueller Anstieg/Abfall statt Sprung
#   3. Korrelation mit Niederschlagsintensität
#   4. Gleichzeitige Events an anderen Sensoren
# ============================================================

baseline <- 7.16

# --- 1. Load data ---
ws <- read_csv("data/processed/cleaned_analysis/Wasserstraße_Springorum_merged_export_cleaned.csv",
               show_col_types = FALSE)

precip <- read_csv("data/processed/precipitation_at_sensors.csv",
                   show_col_types = FALSE) %>%
    filter(grepl("Wasserstra", station, ignore.case = TRUE)) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))

# Andere Sensoren für Quervergleich
events_all <- read_csv("data/processed/detected_events.csv",
                       show_col_types = FALSE)

cat("=== Retrospektive Analyse: Echte Ereignisse an der Wasserstraße ===\n\n")

# --- 2. Identify ALL episodes where distance < 1.0m ---
ws_susp <- ws %>%
    filter(!is.na(distance), distance < 1.0, distance > 0.1) %>%
    arrange(Zeit_Datum) %>%
    mutate(
        time_diff = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum), units = "mins")),
        episode = cumsum(is.na(time_diff) | time_diff > 60)
    )

# --- 3. Detaillierte Analyse pro Episode ---
episodes <- ws_susp %>%
    group_by(episode) %>%
    summarize(
        start = min(Zeit_Datum),
        end = max(Zeit_Datum),
        duration_h = round(as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "hours")), 1),
        n_readings = n(),
        mean_dist = mean(distance),
        sd_dist = sd(distance),
        min_dist = min(distance),
        max_dist = max(distance),
        range_dist = max(distance) - min(distance),
        .groups = "drop"
    ) %>%
    filter(n_readings >= 3)

# Precipitation per episode
episodes$total_precip <- 0
episodes$max_intensity <- 0

for (i in seq_len(nrow(episodes))) {
    ep <- episodes[i, ]
    pw <- precip %>%
        filter(timestamp >= (ep$start - hours(3)), timestamp <= (ep$end + hours(1)))
    episodes$total_precip[i] <- sum(pw$precipitation_mm, na.rm = TRUE)
    episodes$max_intensity[i] <- if (nrow(pw) > 0) max(pw$precipitation_mm, na.rm = TRUE) * 12 else 0
}

rain_eps <- episodes %>% filter(total_precip > 0.5)

# --- 4. Analyse: Gradualität des Anstiegs ---
# Für jede Episode: wie schnell springt die Distance von ~7.16 auf ~0.82?
# Artefakt: sofortiger Sprung (1 Zeitschritt)
# Echt: gradueller Übergang über mehrere Minuten

check_graduality <- function(ep_start, ep_end) {
    # Hole Daten 30min vor Episodenbeginn bis 30min nach Ende
    window <- ws %>%
        filter(Zeit_Datum >= (ep_start - minutes(30)),
               Zeit_Datum <= (ep_end + minutes(30)),
               !is.na(distance)) %>%
        arrange(Zeit_Datum)

    if (nrow(window) < 5) return(list(gradual_rise = FALSE, gradual_fall = FALSE,
                                       intermediate_count = 0, transition_time_min = 0))

    # Zähle Messwerte zwischen 1.0m und 6.0m (Übergangszone)
    intermediate <- window %>% filter(distance > 1.0, distance < 6.0)
    n_intermediate <- nrow(intermediate)

    # Berechne Übergangszeit: von letztem >6m bis erstem <1.5m
    high_readings <- window %>% filter(distance > 6.0)
    low_readings <- window %>% filter(distance < 1.5)

    transition_time <- 0
    if (nrow(high_readings) > 0 && nrow(low_readings) > 0) {
        last_high <- max(high_readings$Zeit_Datum[high_readings$Zeit_Datum < min(low_readings$Zeit_Datum)], na.rm = TRUE)
        first_low <- min(low_readings$Zeit_Datum[low_readings$Zeit_Datum > max(high_readings$Zeit_Datum[1])], na.rm = TRUE)
        if (is.finite(last_high) && is.finite(first_low)) {
            transition_time <- as.numeric(difftime(first_low, last_high, units = "mins"))
        }
    }

    list(
        gradual_rise = n_intermediate >= 3,
        gradual_fall = FALSE,  # analog berechenbar
        intermediate_count = n_intermediate,
        transition_time_min = round(transition_time, 1)
    )
}

# --- 5. Quervergleich mit anderen Sensoren ---
check_other_sensors <- function(ep_start, ep_end) {
    # Events an anderen Stationen die im gleichen Zeitfenster liegen
    overlap <- events_all %>%
        filter(station != "Wasserstraße_Springorum") %>%
        mutate(
            evt_start = as.POSIXct(start_time),
            evt_end = as.POSIXct(end_time)
        ) %>%
        filter(evt_start <= ep_end + hours(2), evt_end >= ep_start - hours(2))

    if (nrow(overlap) > 0) {
        stations <- unique(overlap$station)
        return(list(
            confirmed = TRUE,
            n_events = nrow(overlap),
            stations = paste(stations, collapse = ", "),
            details = overlap %>%
                select(station, start_time, peak_level_cm, event_type) %>%
                head(5)
        ))
    }
    list(confirmed = FALSE, n_events = 0, stations = "", details = NULL)
}

# --- 6. Hauptanalyse ---
cat("Analysiere", nrow(rain_eps), "Regen-Episoden auf Echtheit...\n\n")

results <- list()

for (i in seq_len(nrow(rain_eps))) {
    ep <- rain_eps[i, ]

    grad <- check_graduality(ep$start, ep$end)
    other <- check_other_sensors(ep$start, ep$end)

    # Echtheitskriterien
    has_variability <- !is.na(ep$sd_dist) && ep$sd_dist > 0.01
    has_gradual_transition <- grad$intermediate_count >= 3 || grad$transition_time_min > 3
    has_other_sensors <- other$confirmed
    has_strong_rain <- ep$total_precip > 5 && ep$max_intensity > 3

    # Echtheitsscore (0-4)
    score <- sum(c(has_variability, has_gradual_transition, has_other_sensors, has_strong_rain))

    # Klassifikation
    classification <- case_when(
        score >= 3 ~ "WAHRSCHEINLICH ECHT",
        score == 2 ~ "MÖGLICHERWEISE ECHT",
        score == 1 ~ "EHER ARTEFAKT",
        TRUE ~ "ARTEFAKT"
    )

    results[[i]] <- tibble(
        episode = ep$episode,
        start = ep$start,
        end = ep$end,
        duration_h = ep$duration_h,
        precip_mm = round(ep$total_precip, 1),
        max_int_mm_h = round(ep$max_intensity, 1),
        dist_sd = round(ep$sd_dist, 4),
        dist_range = round(ep$range_dist, 3),
        n_intermediate = grad$intermediate_count,
        transition_min = grad$transition_time_min,
        other_sensors = other$n_events,
        other_stations = other$stations,
        variability = has_variability,
        gradual = has_gradual_transition,
        other_confirmed = has_other_sensors,
        strong_rain = has_strong_rain,
        score = score,
        classification = classification
    )
}

results_df <- bind_rows(results)

# --- 7. Ergebnisse ---
cat("=== KLASSIFIKATION DER REGEN-EPISODEN ===\n\n")

for (cls in c("WAHRSCHEINLICH ECHT", "MÖGLICHERWEISE ECHT", "EHER ARTEFAKT", "ARTEFAKT")) {
    subset <- results_df %>% filter(classification == cls)
    cat(sprintf("--- %s (%d Episoden) ---\n", cls, nrow(subset)))

    if (nrow(subset) > 0) {
        for (j in seq_len(nrow(subset))) {
            r <- subset[j, ]
            cat(sprintf("\n  Episode %s bis %s (%.1fh)\n",
                        format(r$start, "%d.%m.%Y %H:%M"),
                        format(r$end, "%d.%m.%Y %H:%M"),
                        r$duration_h))
            cat(sprintf("    DWD: %.1f mm, max %.1f mm/h\n", r$precip_mm, r$max_int_mm_h))
            cat(sprintf("    Distance SD: %.4f m, Range: %.3f m\n", r$dist_sd, r$dist_range))
            cat(sprintf("    Zwischenwerte (1-6m): %d, Übergangszeit: %.1f min\n",
                        r$n_intermediate, r$transition_min))
            if (nchar(r$other_stations) > 0) {
                cat(sprintf("    Andere Sensoren: %d Events (%s)\n", r$other_sensors, r$other_stations))
            } else {
                cat("    Andere Sensoren: keine parallelen Events\n")
            }
            cat(sprintf("    Score: %d/4 [%s%s%s%s]\n",
                        r$score,
                        ifelse(r$variability, "Variabel ", ""),
                        ifelse(r$gradual, "Graduell ", ""),
                        ifelse(r$other_confirmed, "Querbestätigt ", ""),
                        ifelse(r$strong_rain, "Starkregen", "")))
        }
    }
    cat("\n")
}

# --- 8. Zusammenfassung ---
cat("=== ZUSAMMENFASSUNG ===\n\n")
summary_counts <- results_df %>% count(classification) %>% arrange(desc(n))
for (j in seq_len(nrow(summary_counts))) {
    cat(sprintf("  %s: %d\n", summary_counts$classification[j], summary_counts$n[j]))
}

n_echt <- sum(results_df$classification %in% c("WAHRSCHEINLICH ECHT", "MÖGLICHERWEISE ECHT"))
cat(sprintf("\n  => %d von %d Regen-Episoden sind potenziell echte Ereignisse\n\n",
            n_echt, nrow(results_df)))

# --- 9. Speichern ---
write_csv(results_df, "data/processed/wasserstrasse_episode_klassifikation.csv")

# Auch als .md
md_lines <- c(
    "# Wasserstraße: Retrospektive Ereignisklassifikation",
    "",
    sprintf("**Analysedatum:** %s", Sys.Date()),
    sprintf("**Regen-Episoden analysiert:** %d", nrow(results_df)),
    "",
    "## Methode",
    "",
    "Jede Episode mit DWD-RADOLAN-bestätigtem Niederschlag wurde auf 4 Echtheitsindikatoren geprüft:",
    "",
    "1. **Variabilität** — SD der Distance > 0.01m (Artefakt ist extrem konstant bei ~0.817m)",
    "2. **Gradueller Übergang** — Zwischenwerte zwischen 1m und 6m oder Übergangszeit > 3 min",
    "3. **Querbestätigung** — Gleichzeitige Events an anderen Sensoren (An der Kost, Königsallee etc.)",
    "4. **Starker Niederschlag** — DWD > 5mm gesamt UND max. Intensität > 3 mm/h",
    "",
    "Score 0–1 = Artefakt, 2 = möglicherweise echt, 3–4 = wahrscheinlich echt",
    ""
)

for (cls in c("WAHRSCHEINLICH ECHT", "MÖGLICHERWEISE ECHT", "EHER ARTEFAKT", "ARTEFAKT")) {
    subset <- results_df %>% filter(classification == cls)
    md_lines <- c(md_lines, sprintf("## %s (%d Episoden)", cls, nrow(subset)), "")

    if (nrow(subset) > 0) {
        md_lines <- c(md_lines,
            "| Start | Ende | Dauer | DWD (mm) | Max (mm/h) | Dist SD | Andere Sensoren | Score |",
            "|---|---|---|---|---|---|---|---|")

        for (j in seq_len(nrow(subset))) {
            r <- subset[j, ]
            md_lines <- c(md_lines, sprintf(
                "| %s | %s | %.1fh | %.1f | %.1f | %.4f | %s | %d/4 |",
                format(r$start, "%d.%m.%Y %H:%M"),
                format(r$end, "%d.%m.%Y %H:%M"),
                r$duration_h, r$precip_mm, r$max_int_mm_h,
                r$dist_sd,
                ifelse(nchar(r$other_stations) > 0,
                       sprintf("%d (%s)", r$other_sensors, r$other_stations), "—"),
                r$score
            ))
        }
    } else {
        md_lines <- c(md_lines, "*Keine Episoden in dieser Kategorie.*")
    }
    md_lines <- c(md_lines, "")
}

# Fazit
md_lines <- c(md_lines,
    "## Fazit",
    "",
    sprintf("Von %d RADOLAN-verifizierten Regen-Episoden:", nrow(results_df)),
    ""
)
for (j in seq_len(nrow(summary_counts))) {
    md_lines <- c(md_lines, sprintf("- **%s:** %d", summary_counts$classification[j], summary_counts$n[j]))
}
md_lines <- c(md_lines, "",
    sprintf("**%d Episoden** zeigen Merkmale echter Wasserstandsänderungen.", n_echt),
    "Der Großteil der Distance-~0.82m-Episoden ist jedoch auch bei Regen ein Sensor-Artefakt —",
    "die Distance springt abrupt auf den konstanten Wert ~0.817m ohne graduelle Veränderung.",
    "",
    "---",
    "*Analyse: R-Skript scripts/analysis/wasserstrasse_echte_ereignisse.R*"
)

writeLines(md_lines, "data/processed/wasserstrasse_ereignis_klassifikation.md")
cat("Ergebnisse gespeichert:\n")
cat("  data/processed/wasserstrasse_ereignis_klassifikation.md\n")
