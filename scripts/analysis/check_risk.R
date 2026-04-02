library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
forecast_file  <- "data/processed/weather_forecast.csv"
historical_log <- "data/metadata/historical_verified_events.csv"
risk_report_file <- "data/processed/risk_report.md"

# ═══════════════════════════════════════════════════════════════════════════════
# Erfahrungsbasierte Risikoabschätzung
#
#   Konzept:
#     1. Historische, RADOLAN-verifizierte Events liefern sensorspezifische
#        Niederschlagsprofile (Intensität, Summe, Dauer).
#     2. Die Open-Meteo-Vorhersage wird pro Sensor gegen diese historischen
#        Profile abgeglichen.
#     3. Wenn die vorhergesagten Bedingungen einem bekannten Event-Muster
#        entsprechen, wird eine Risikowarnung ausgelöst.
#
#   Fallback:
#     Wenn noch keine historischen Events vorliegen, werden die
#     DWD-Starkregen-Schwellenwerte (Stufe 2/3/4) als Default genutzt.
# ═══════════════════════════════════════════════════════════════════════════════

cat("Checking weather forecast against historical event patterns...\n")

if (!file_exists(forecast_file)) {
    stop("Forecast file not found. Run fetch_forecast.R first.")
}

# 1. Vorhersage laden (per-Sensor falls verfügbar)
forecast_raw <- read_csv(forecast_file, show_col_types = FALSE) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Berlin")) %>%
    filter(timestamp > now())

has_station_forecast <- "station" %in% names(forecast_raw)

if (nrow(forecast_raw) == 0) {
    cat("No upcoming forecast data available.\n")
    quit(save = "no", status = 0)
}

# 2. Historische Events laden und sensorspezifische Profile erstellen
hist_profiles <- NULL
has_history   <- FALSE

if (file_exists(historical_log)) {
    hist_data <- read_csv(historical_log, show_col_types = FALSE)

    # Nur RADOLAN-verifizierte Events mit Niederschlag verwenden
    hist_verified <- hist_data %>%
        filter(
            !is.na(total_precip_mm), total_precip_mm > 0,
            !is.na(max_intensity_mm_h), max_intensity_mm_h > 0
        )

    if (nrow(hist_verified) >= 1) {
        has_history <- TRUE

        # Pro Station: Schwellenwerte aus der Erfahrung ableiten
        # Konservativ: niedrigstes Profil das historisch ein Event ausgelöst hat
        hist_profiles <- hist_verified %>%
            group_by(station) %>%
            summarise(
                n_events               = n(),
                min_intensity_mm_h     = min(max_intensity_mm_h, na.rm = TRUE),
                min_total_precip_mm    = min(total_precip_mm, na.rm = TRUE),
                median_intensity_mm_h  = median(max_intensity_mm_h, na.rm = TRUE),
                median_total_precip_mm = median(total_precip_mm, na.rm = TRUE),
                median_duration_min    = median(duration_min, na.rm = TRUE),
                # Typische Event-Dauer für Rolling-Window
                typical_window_h       = ceiling(median(duration_min, na.rm = TRUE) / 60),
                .groups = "drop"
            ) %>%
            mutate(
                # Schwelle = niedrigste historische Intensität, mind. 0.5 mm/h
                threshold_mm_h   = pmax(0.5, min_intensity_mm_h),
                # Summen-Schwelle = niedrigste historische Summe, mind. 0.3 mm
                threshold_sum_mm = pmax(0.3, min_total_precip_mm)
            )

        cat("Historical profiles loaded for", nrow(hist_profiles), "station(s):\n")
        for (j in seq_len(nrow(hist_profiles))) {
            p <- hist_profiles[j, ]
            cat("  ", p$station, ": ", p$n_events, " Events, ",
                "Schwelle >= ", round(p$threshold_mm_h, 1), " mm/h",
                " ODER Summe >= ", round(p$threshold_sum_mm, 1), " mm",
                " (Median: ", round(p$median_intensity_mm_h, 1), " mm/h, ",
                round(p$median_total_precip_mm, 1), " mm gesamt)\n", sep = "")
        }
    }
}

# DWD-Schwellenwerte als Fallback (1h UND 6h Kriterien)
DWD_THRESHOLDS <- data.frame(
    level   = c(2, 3, 4),
    label   = c("Markantes Wetter (Stufe 2)", "Unwetter (Stufe 3)", "Extremes Unwetter (Stufe 4)"),
    mm_1h   = c(15, 25, 40),
    mm_6h   = c(20, 35, 60)
)

# 3. Risikoprüfung pro Sensor
all_risks <- list()

if (has_station_forecast) {
    stations <- unique(forecast_raw$station)
} else {
    stations <- if (has_history) unique(hist_profiles$station) else "Alle"
}

for (st in stations) {
    # Vorhersage für diese Station
    if (has_station_forecast) {
        fc <- forecast_raw %>% filter(station == st)
    } else {
        fc <- forecast_raw
    }

    if (nrow(fc) == 0) next

    # Rolling-Summen berechnen (3h und 6h)
    fc <- fc %>%
        arrange(timestamp) %>%
        mutate(
            precip_3h = sapply(seq_len(n()), function(i) {
                sum(precipitation_mm[timestamp >= timestamp[i] & timestamp < (timestamp[i] + hours(3))], na.rm = TRUE)
            }),
            precip_6h = sapply(seq_len(n()), function(i) {
                sum(precipitation_mm[timestamp >= timestamp[i] & timestamp < (timestamp[i] + hours(6))], na.rm = TRUE)
            })
        )

    # --- A) Erfahrungsbasierte Prüfung (pro Station) ---
    station_risk <- NULL

    if (has_history) {
        profile <- hist_profiles %>% filter(station == st)

        if (nrow(profile) == 1) {
            # Typische Event-Dauer als Rolling-Window
            window_h <- max(1, profile$typical_window_h)

            fc <- fc %>%
                mutate(
                    precip_window = sapply(seq_len(n()), function(i) {
                        sum(precipitation_mm[timestamp >= timestamp[i] &
                            timestamp < (timestamp[i] + hours(window_h))], na.rm = TRUE)
                    })
                )

            # Match: Intensität ODER Summe überschreiten historischen Minimalwert
            matched <- fc %>%
                filter(
                    precipitation_mm >= profile$threshold_mm_h |
                    precip_window    >= profile$threshold_sum_mm
                )

            if (nrow(matched) > 0) {
                station_risk <- matched %>%
                    mutate(
                        station     = st,
                        risk_source = "Erfahrungsbasiert",
                        risk_detail = paste0(
                            "Historisch: ", profile$n_events, " Events, ",
                            "Schwelle: ", round(profile$threshold_mm_h, 1), " mm/h / ",
                            round(profile$threshold_sum_mm, 1), " mm Summe"
                        )
                    ) %>%
                    select(station, timestamp, precipitation_mm, probability_percent,
                           precip_3h, precip_6h, risk_source, risk_detail)
            }
        }
    }

    # --- B) DWD-Fallback (wenn keine historischen Daten für diese Station) ---
    if (is.null(station_risk)) {
        dwd_matched <- fc %>%
            filter(
                precipitation_mm >= DWD_THRESHOLDS$mm_1h[1] |
                precip_6h        >= DWD_THRESHOLDS$mm_6h[1]
            )

        if (nrow(dwd_matched) > 0) {
            # DWD-Stufe bestimmen
            dwd_matched <- dwd_matched %>%
                mutate(
                    station     = st,
                    dwd_level   = case_when(
                        precipitation_mm >= 40 | precip_6h >= 60 ~ 4,
                        precipitation_mm >= 25 | precip_6h >= 35 ~ 3,
                        precipitation_mm >= 15 | precip_6h >= 20 ~ 2,
                        TRUE ~ 1
                    ),
                    risk_source = "DWD-Schwellenwert",
                    risk_detail = paste0("DWD Stufe ", dwd_level)
                ) %>%
                select(station, timestamp, precipitation_mm, probability_percent,
                       precip_3h, precip_6h, risk_source, risk_detail)

            station_risk <- dwd_matched
        }
    }

    if (!is.null(station_risk)) {
        all_risks[[st]] <- station_risk
    }
}

# 4. Report generieren
if (length(all_risks) > 0) {
    risk_df <- bind_rows(all_risks) %>% arrange(timestamp)

    cat("\nALERT: Risk detected for", length(all_risks), "station(s)!\n")

    # --- Markdown Report ---
    report_lines <- c(
        "# HOCHWASSER-RISIKOWARNUNG",
        "",
        paste0("*Erstellt: ", format(now(), "%d.%m.%Y %H:%M"), " (Berlin)*"),
        ""
    )

    for (st in unique(risk_df$station)) {
        st_risks <- risk_df %>% filter(station == st)
        source   <- st_risks$risk_source[1]
        detail   <- st_risks$risk_detail[1]

        report_lines <- c(report_lines,
            paste0("## ", st),
            "",
            paste0("**Methode:** ", source),
            paste0("**Grundlage:** ", detail),
            "",
            "| Zeitpunkt | Intensität (mm/h) | Summe 3h (mm) | Summe 6h (mm) | Wahrsch. (%) |",
            "|----|----|----|----|----|"
        )

        # Top 8 kritischste Zeitpunkte pro Station
        top_risks <- st_risks %>%
            arrange(desc(precipitation_mm)) %>%
            head(8) %>%
            arrange(timestamp)

        for (k in seq_len(nrow(top_risks))) {
            r <- top_risks[k, ]
            report_lines <- c(report_lines,
                paste0("| ", format(r$timestamp, "%d.%m. %H:%M"), " | ",
                       round(r$precipitation_mm, 1), " | ",
                       round(r$precip_3h, 1), " | ",
                       round(r$precip_6h, 1), " | ",
                       round(r$probability_percent, 0), " |")
            )
        }

        report_lines <- c(report_lines, "")
    }

    # Erklärung
    if (has_history) {
        report_lines <- c(report_lines,
            "---",
            "",
            "### Methodik",
            "",
            "Die erfahrungsbasierte Risikoabschätzung vergleicht die Open-Meteo-Vorhersage",
            "mit historischen, RADOLAN-verifizierten Sensor-Events. Wenn die vorhergesagten",
            "Bedingungen (Intensität oder Niederschlagssumme) den Werten entsprechen, die",
            "in der Vergangenheit an einem Sensor zu einem verifizierten Ereignis geführt haben,",
            "wird eine Warnung ausgelöst.",
            "",
            paste0("Historische Datenbasis: ", sum(hist_profiles$n_events), " verifizierte Events an ",
                   nrow(hist_profiles), " Station(en).")
        )
    }

    write_lines(report_lines, risk_report_file)
    cat("Risk report saved to", risk_report_file, "\n")

} else {
    cat("No risk detected in the forecast.\n")
    if (file_exists(risk_report_file)) file_delete(risk_report_file)
}
