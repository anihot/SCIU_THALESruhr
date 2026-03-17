library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(httr)
library(jsonlite)
library(fs)
library(tidyr)
library(knitr)

# Config
events_file  <- "data/processed/detected_events.csv"
cleaned_dir  <- "data/processed/cleaned_analysis"
plots_dir    <- "data/output/plots"
output_file  <- "data/processed/lag_analysis.csv"

LAT              <- 51.48
LON              <- 7.21
LEAD_IN_HOURS    <- 3     # Stunden vor Ereignisbeginn, die auf Regen geprüft werden
MIN_PRECIP_MM_H  <- 0.04  # mm/5min ≈ 0.5 mm/h – ab wann Regen als "Beginn" gilt
MAX_LAG_HOURS    <- 6     # Lags > 6h werden als nicht korreliert gewertet (NA)
MIN_LAG_MINUTES  <- -60   # Lags < -60 min (Sensor vor Regen) werden verworfen

cat("=== Lag-Analyse ===\n")
cat("Berechnet Onset-Lag und Peak-Lag für alle Regenereignisse.\n\n")

# ── 1. Events laden ───────────────────────────────────────────────────────────

if (!file_exists(events_file)) stop("events_file nicht gefunden: ", events_file)

events <- read_csv(events_file, show_col_types = FALSE) %>%
    mutate(
        start_time = as.POSIXct(start_time, tz = "UTC"),
        end_time   = as.POSIXct(end_time,   tz = "UTC"),
        peak_time  = as.POSIXct(peak_time,  tz = "UTC")
    )

# Nur Events mit Regen (kein Indoor-Sensor Wasserbaulabor_2 für Lag-Analyse)
events_rain <- events %>%
    filter(
        !grepl("Wasserbaulabor", station, ignore.case = TRUE),
        !is.na(start_time),
        coalesce(openmeteo_verified, total_precip_mm > 0, FALSE) == TRUE |
            event_type != "Sturzflut-Ereignis"  # Leichter Regen auch einschließen
    )

cat("Ereignisse gesamt:           ", nrow(events), "\n")
cat("Ereignisse für Lag-Analyse:  ", nrow(events_rain), "\n\n")

# ── 2. Stündliche Niederschlagsdaten holen (Open-Meteo Archive) ───────────────

start_date <- format(min(as.Date(events_rain$start_time)) - days(1), "%Y-%m-%d")
end_date   <- format(Sys.Date(), "%Y-%m-%d")

cat("Lade Niederschlagsdaten:", start_date, "bis", end_date, "...\n")

url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", LAT, "&longitude=", LON,
    "&start_date=", start_date, "&end_date=", end_date,
    "&hourly=precipitation&timezone=Europe%2FBerlin"
)

resp <- try(GET(url), silent = TRUE)
if (inherits(resp, "try-error") || status_code(resp) != 200) {
    stop("Open-Meteo Archive konnte nicht abgerufen werden.")
}

raw         <- fromJSON(content(resp, "text", encoding = "UTF-8"))
precip_arch <- data.frame(
    timestamp   = as.POSIXct(raw$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "Europe/Berlin"),
    precip_mm_h = raw$hourly$precipitation
)
cat("Geladen:", nrow(precip_arch), "Stundenwerte.\n\n")

# ── 3. Lag-Berechnung pro Ereignis ────────────────────────────────────────────

compute_lag <- function(station, start_time, end_time, peak_time) {
    # Niederschlagsfenster: lead_in vor Start bis 1h nach Ende
    win_start  <- start_time - hours(LEAD_IN_HOURS)
    win_end    <- end_time   + hours(1)
    precip_win <- precip_arch %>%
        filter(timestamp >= win_start, timestamp <= win_end)

    if (nrow(precip_win) == 0 ||
        all(is.na(precip_win$precip_mm_h)) ||
        all(precip_win$precip_mm_h < MIN_PRECIP_MM_H, na.rm = TRUE)) {
        return(list(
            onset_lag_min     = NA_real_,
            peak_lag_min      = NA_real_,
            precip_onset_time = as.POSIXct(NA),
            precip_peak_time  = as.POSIXct(NA),
            precip_total_mm   = 0,
            precip_max_mm_h   = 0
        ))
    }

    # Onset-Lag: erster Stundenwert >= MIN_PRECIP_MM_H → Sensor-Ereignisbeginn
    first_rain        <- precip_win %>% filter(precip_mm_h >= MIN_PRECIP_MM_H) %>% slice(1)
    precip_onset_time <- first_rain$timestamp[1]
    onset_lag_min     <- as.numeric(difftime(start_time, precip_onset_time, units = "mins"))

    # Lags außerhalb sinnvollem Bereich verwerfen
    if (!is.finite(onset_lag_min) ||
        onset_lag_min > MAX_LAG_HOURS * 60 ||
        onset_lag_min < MIN_LAG_MINUTES) {
        onset_lag_min <- NA_real_
    }

    # Peak-Lag: Stunde mit höchstem Niederschlag → Sensor-Peakzeit
    peak_row         <- precip_win %>% filter(precip_mm_h == max(precip_mm_h, na.rm = TRUE)) %>% slice(1)
    precip_peak_time <- peak_row$timestamp[1]
    peak_lag_min     <- as.numeric(difftime(peak_time, precip_peak_time, units = "mins"))

    if (!is.finite(peak_lag_min) ||
        peak_lag_min > MAX_LAG_HOURS * 60 ||
        peak_lag_min < MIN_LAG_MINUTES) {
        peak_lag_min <- NA_real_
    }

    list(
        onset_lag_min     = onset_lag_min,
        peak_lag_min      = peak_lag_min,
        precip_onset_time = precip_onset_time,
        precip_peak_time  = precip_peak_time,
        precip_total_mm   = sum(precip_win$precip_mm_h, na.rm = TRUE),
        precip_max_mm_h   = max(precip_win$precip_mm_h, na.rm = TRUE)
    )
}

cat("Berechne Lags...\n")

lag_cols <- purrr::pmap_dfr(
    list(events_rain$station, events_rain$start_time, events_rain$end_time, events_rain$peak_time),
    compute_lag
)

events_lag <- bind_cols(events_rain, lag_cols)

n_onset <- sum(!is.na(events_lag$onset_lag_min))
n_peak  <- sum(!is.na(events_lag$peak_lag_min))
cat("Onset-Lags berechnet:", n_onset, "/", nrow(events_lag), "\n")
cat("Peak-Lags berechnet: ", n_peak,  "/", nrow(events_lag), "\n\n")

# ── 4. Exportieren ────────────────────────────────────────────────────────────

write_excel_csv(events_lag, output_file)
cat("Ergebnis gespeichert:", output_file, "\n\n")

# ── 5. Deskriptive Statistik ──────────────────────────────────────────────────

lag_summary <- events_lag %>%
    filter(!is.na(onset_lag_min)) %>%
    group_by(station) %>%
    summarise(
        n             = n(),
        median_min    = round(median(onset_lag_min), 1),
        mean_min      = round(mean(onset_lag_min),   1),
        min_min       = round(min(onset_lag_min),    1),
        max_min       = round(max(onset_lag_min),    1),
        .groups       = "drop"
    ) %>%
    arrange(median_min)

cat("=== Onset-Lag Zusammenfassung (Minuten) ===\n")
print(lag_summary, n = Inf)

# ── 6. Plots ──────────────────────────────────────────────────────────────────

if (!dir_exists(plots_dir)) dir_create(plots_dir)

lag_valid <- events_lag %>% filter(!is.na(onset_lag_min))

if (nrow(lag_valid) == 0) {
    cat("\nKeine gültigen Lags für Visualisierung.\n")
    quit(save = "no", status = 0)
}

# Gemeinsame Plot-Basis
base_theme <- theme_minimal(base_size = 12) +
    theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom"
    )

# ── Plot 1: Onset-Lag je Station (Boxplot + Punkte) ──────────────────────────
p1 <- ggplot(lag_valid, aes(x = reorder(station, onset_lag_min, median), y = onset_lag_min)) +
    geom_boxplot(aes(fill = station), outlier.shape = NA, alpha = 0.7, width = 0.5) +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, color = "grey20") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    coord_flip() +
    base_theme +
    theme(legend.position = "none") +
    labs(
        title    = "Onset-Lag nach Station",
        subtitle = "Zeit zwischen erstem Regen (≥ 0,5 mm/h) und erstem Schwellenübertritt am Sensor\nGestrichelte Linie = synchron (Lag 0 min)",
        x        = NULL,
        y        = "Onset-Lag (Minuten)",
        caption  = paste0("n = ", nrow(lag_valid), " Ereignisse | Niederschlag: Open-Meteo Archive (stündlich, ±30 min Unsicherheit)")
    )

ggsave(file.path(plots_dir, "lag_onset_by_station.png"), p1, width = 10, height = 6, dpi = 300)
cat("Gespeichert: lag_onset_by_station.png\n")

# ── Plot 2: Onset-Lag je Ereignistyp ─────────────────────────────────────────
p2 <- ggplot(lag_valid, aes(x = event_type, y = onset_lag_min, fill = event_type)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, color = "grey20") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
    base_theme +
    theme(legend.position = "none", axis.text.x = element_text(angle = 12, hjust = 1)) +
    labs(
        title    = "Onset-Lag nach Ereignistyp",
        subtitle = "Sturzfluten sollten kürzere Lags aufweisen als Regenereignisse",
        x        = NULL,
        y        = "Onset-Lag (Minuten)"
    )

ggsave(file.path(plots_dir, "lag_onset_by_type.png"), p2, width = 9, height = 5, dpi = 300)
cat("Gespeichert: lag_onset_by_type.png\n")

# ── Plot 3: Lag vs. Regenintensität (Scatter + Regressionslinie) ─────────────
lag_scatter <- lag_valid %>% filter(!is.na(precip_max_mm_h), precip_max_mm_h > 0)

if (nrow(lag_scatter) >= 5) {
    p3 <- ggplot(lag_scatter, aes(x = precip_max_mm_h, y = onset_lag_min)) +
        geom_point(aes(color = event_type, shape = station), size = 2.5, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "grey30",
                    linetype = "dashed", linewidth = 0.8) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
        # DWD-Schwellenlinien
        geom_vline(xintercept = 15, linetype = "dashed", color = "#E69F00", linewidth = 0.5) +
        geom_vline(xintercept = 25, linetype = "dashed", color = "#D55E00", linewidth = 0.5) +
        annotate("text", x = 15.5, y = max(lag_scatter$onset_lag_min, na.rm=TRUE) * 0.95,
                 label = "DWD L2", hjust = 0, size = 3, color = "#E69F00") +
        annotate("text", x = 25.5, y = max(lag_scatter$onset_lag_min, na.rm=TRUE) * 0.85,
                 label = "DWD L3", hjust = 0, size = 3, color = "#D55E00") +
        base_theme +
        labs(
            title    = "Onset-Lag vs. maximale Regenintensität",
            subtitle = "Erwartung: höhere Intensität → kürzerer Lag (negativer Zusammenhang)",
            x        = "Max. Regenintensität (mm/h)",
            y        = "Onset-Lag (Minuten)",
            color    = "Ereignistyp",
            shape    = "Station"
        )

    ggsave(file.path(plots_dir, "lag_vs_intensity.png"), p3, width = 11, height = 6, dpi = 300)
    cat("Gespeichert: lag_vs_intensity.png\n")
}

# ── Plot 4: Peak-Lag Verteilung ───────────────────────────────────────────────
peak_valid <- events_lag %>% filter(!is.na(peak_lag_min))

if (nrow(peak_valid) >= 5) {
    p4 <- ggplot(peak_valid, aes(x = peak_lag_min, fill = event_type)) +
        geom_histogram(binwidth = 15, alpha = 0.8, position = "identity") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        facet_wrap(~station, scales = "free_y", ncol = 2) +
        base_theme +
        labs(
            title    = "Peak-Lag Verteilung je Station",
            subtitle = "Zeit zwischen Niederschlagsmaximum und Sensor-Peak | Negativ = Sensor peakt vor Regen",
            x        = "Peak-Lag (Minuten)",
            y        = "Anzahl Ereignisse",
            fill     = "Ereignistyp"
        )

    ggsave(file.path(plots_dir, "lag_peak_distribution.png"), p4, width = 12, height = 7, dpi = 300)
    cat("Gespeichert: lag_peak_distribution.png\n")
}

# ── 7. Markdown-Bericht ───────────────────────────────────────────────────────

report_file <- "data/processed/lag_report.md"

overall_median_onset <- round(median(lag_valid$onset_lag_min, na.rm = TRUE), 0)
overall_mean_onset   <- round(mean(lag_valid$onset_lag_min,   na.rm = TRUE), 0)

# Zusammenfassung nach Ereignistyp
type_summary <- lag_valid %>%
    group_by(event_type) %>%
    summarise(
        n                 = n(),
        `Median-Lag (min)` = round(median(onset_lag_min), 0),
        `Mittel-Lag (min)` = round(mean(onset_lag_min), 0),
        `Min (min)`        = round(min(onset_lag_min), 0),
        `Max (min)`        = round(max(onset_lag_min), 0),
        .groups = "drop"
    )

# Lineare Regression: Lag ~ Intensität
reg_data <- lag_valid %>% filter(!is.na(precip_max_mm_h), precip_max_mm_h > 0)
lm_result <- if (nrow(reg_data) >= 5) lm(onset_lag_min ~ precip_max_mm_h, data = reg_data) else NULL

report_lines <- c(
    "# Lag-Analyse: Reaktionszeiten der Sensoren",
    paste0("*Erstellt: ", format(now(), "%Y-%m-%d %H:%M"), "*"),
    "",
    "## Methodik",
    "- **Onset-Lag**: Zeit zwischen erstem Stundenwert ≥ 0,5 mm/h und erstem Schwellenübertritt am Sensor (1,5 cm)",
    "- **Peak-Lag**: Zeit zwischen Niederschlagsmaximum und Sensor-Peakwert",
    "- Niederschlagsquelle: Open-Meteo Archive (stündlich) – Messungenauigkeit ±30 min",
    paste0("- Lags außerhalb [−60 min, ", MAX_LAG_HOURS * 60, " min] werden als nicht korreliert verworfen"),
    "",
    "## Gesamtergebnis",
    paste0("- Ereignisse analysiert: **", nrow(lag_valid), "**"),
    paste0("- Median Onset-Lag (alle Stationen): **", overall_median_onset, " min**"),
    paste0("- Mittlerer Onset-Lag: **", overall_mean_onset, " min**"),
    ""
)

if (!is.null(lm_result)) {
    coef_intensity <- round(coef(lm_result)[["precip_max_mm_h"]], 2)
    r2             <- round(summary(lm_result)$r.squared, 3)
    report_lines <- c(report_lines,
        "## Zusammenhang Lag ~ Regenintensität",
        paste0("Lineare Regression: Onset-Lag ~ max. Intensität (mm/h)"),
        paste0("- Koeffizient: **", coef_intensity, " min pro mm/h** (negativ = höherer Regen → kürzerer Lag)"),
        paste0("- R² = ", r2),
        ""
    )
}

report_lines <- c(report_lines,
    "## Nach Ereignistyp",
    "",
    paste(knitr::kable(type_summary, format = "markdown"), collapse = "\n"),
    "",
    "## Nach Station",
    "",
    paste(knitr::kable(lag_summary %>% rename(
        `Median-Lag (min)` = median_min,
        `Mittel-Lag (min)` = mean_min,
        `Min (min)` = min_min,
        `Max (min)` = max_min
    ), format = "markdown"), collapse = "\n"),
    "",
    "## Plots",
    "- `data/output/plots/lag_onset_by_station.png` – Boxplot Onset-Lag je Station",
    "- `data/output/plots/lag_onset_by_type.png` – Boxplot nach Ereignistyp",
    "- `data/output/plots/lag_vs_intensity.png` – Scatter Lag vs. Regenintensität",
    "- `data/output/plots/lag_peak_distribution.png` – Histogramm Peak-Lag je Station"
)

write_lines(report_lines, report_file)
cat("Bericht gespeichert:", report_file, "\n")

cat("\n✅ Lag-Analyse abgeschlossen.\n")
