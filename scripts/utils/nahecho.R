# ============================================================
# Nahecho-Erkennung (z. B. Wasserstraße Springorum)
# ============================================================
# Bei Nässe misst der Radarsensor an der Wasserstraße statt der
# freien Strecke zur Straße (~7,15 m) einen konstanten Abstand von
# ~0,82 m. In diesen Phasen ist kein Wasserstand messbar, der Zustand
# zeigt aber zuverlässig an, dass es am Standort nass ist.
#
# Dieses Modul stellt gemeinsame Funktionen bereit für:
#   - level_analysis.R            (Flag setzen, Pegel auf NA)
#   - event_detection.R           (indirekte Ereignisse)
#   - generate_interactive_map.R  (Phasen im 7-Tage-Popup)
#   - plot_full_timeseries.R      (Phasen in der Gesamtzeitreihe)
# ============================================================

suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
})

# Stationen mit Nahecho-Erkennung: maximaler Abstand (m), ab dem eine
# Messung als Nahecho gilt. Schlüssel = Stationsname ohne "_merged_export".
NAHECHO_STATIONS <- c(
    "Wasserstraße_Springorum" = 1.0
)
NAHECHO_MIN_DIST      <- 0.1   # darunter: Messfehler, kein Nahecho
NAHECHO_MERGE_GAP_MIN <- 60    # Nahecho-Messungen mit Lücken <= 60 min = eine Phase
NAHECHO_MIN_READINGS  <- 3     # kürzere Phasen werden ignoriert

# Indirekte Ereignisse: RADOLAN-Regen während einer Nahecho-Phase
NAHECHO_RAIN_MIN_MM    <- 0.1  # Stunde zählt als nass ab 0,1 mm
NAHECHO_RAIN_GAP_H     <- 3    # Regenstunden mit Lücke <= 3 h = ein Regenereignis
NAHECHO_EVENT_MIN_MM   <- 0.5  # Mindestsumme eines Regenereignisses
NAHECHO_LEAD_IN_H      <- 3    # Regen darf bis 3 h vor Phasenbeginn starten

EVENT_TYPE_NAHECHO <- "Ereignis erkannt (Nahecho, Pegel nicht messbar)"

# Schwellwert für eine Station (NA = keine Nahecho-Erkennung)
nahecho_threshold <- function(station_name) {
    key <- gsub("_merged_export.*$|_cleaned.*$", "", station_name)
    hit <- names(NAHECHO_STATIONS)[startsWith(key, names(NAHECHO_STATIONS))]
    if (length(hit) == 0) NA_real_ else unname(NAHECHO_STATIONS[hit[1]])
}

# Logischer Vektor: welche Messungen sind Nahecho?
flag_nahecho <- function(distance, threshold) {
    if (is.na(threshold) || is.null(distance)) return(rep(FALSE, length(distance)))
    !is.na(distance) & distance > NAHECHO_MIN_DIST & distance < threshold
}

# Zusammenhängende Nahecho-Phasen aus Zeitstempeln + Flag
nahecho_episodes <- function(zeit, flag) {
    empty <- tibble(start = as.POSIXct(character(), tz = "UTC"),
                    end   = as.POSIXct(character(), tz = "UTC"),
                    n_readings = integer())
    if (length(zeit) == 0 || !any(flag, na.rm = TRUE)) return(empty)
    t <- sort(zeit[!is.na(flag) & flag])
    gap <- c(Inf, as.numeric(difftime(t[-1], t[-length(t)], units = "mins")))
    tibble(Zeit = t, ep = cumsum(gap > NAHECHO_MERGE_GAP_MIN)) %>%
        group_by(ep) %>%
        summarise(start = min(Zeit), end = max(Zeit), n_readings = n(), .groups = "drop") %>%
        filter(n_readings >= NAHECHO_MIN_READINGS) %>%
        select(start, end, n_readings)
}

# Regenereignisse aus stündlichem RADOLAN (timestamp = Stundenbeginn)
rain_clusters <- function(precip_hourly) {
    wet <- precip_hourly %>%
        filter(!is.na(precipitation_mm), precipitation_mm >= NAHECHO_RAIN_MIN_MM) %>%
        arrange(timestamp)
    if (nrow(wet) == 0) {
        return(tibble(rain_start = as.POSIXct(character(), tz = "UTC"),
                      rain_end = as.POSIXct(character(), tz = "UTC"),
                      total_precip_mm = numeric(), max_intensity_mm_h = numeric()))
    }
    gap_h <- c(Inf, as.numeric(difftime(wet$timestamp[-1], wet$timestamp[-nrow(wet)], units = "hours")))
    wet %>%
        mutate(cl = cumsum(gap_h > NAHECHO_RAIN_GAP_H + 1)) %>%
        group_by(cl) %>%
        summarise(
            rain_start         = min(timestamp),
            rain_end           = max(timestamp) + hours(1),
            total_precip_mm    = round(sum(precipitation_mm), 2),
            max_intensity_mm_h = round(max(precipitation_mm), 2),
            .groups = "drop"
        ) %>%
        filter(total_precip_mm >= NAHECHO_EVENT_MIN_MM) %>%
        select(-cl)
}

# Indirekte Ereignisse: Regenereignisse, die in eine Nahecho-Phase fallen
# (Regenbeginn zwischen Phasenbeginn - 3 h und Phasenende).
nahecho_events <- function(episodes, precip_hourly) {
    rc <- rain_clusters(precip_hourly)
    if (nrow(episodes) == 0 || nrow(rc) == 0) return(NULL)
    hits <- lapply(seq_len(nrow(rc)), function(i) {
        ep <- episodes %>%
            filter(rc$rain_start[i] >= start - hours(NAHECHO_LEAD_IN_H),
                   rc$rain_start[i] <= end)
        if (nrow(ep) == 0) return(NULL)
        rc[i, ] %>% mutate(nahecho_start = ep$start[1])
    })
    out <- bind_rows(hits)
    if (nrow(out) == 0) NULL else out
}
