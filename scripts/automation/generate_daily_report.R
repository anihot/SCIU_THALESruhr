library(readr)
library(dplyr)
library(lubridate)
library(knitr)
library(fs)

# Config
forecast_file <- "data/processed/weather_forecast.csv"
events_file <- "data/processed/detected_events.csv"
output_file <- "data/processed/daily_summary.md"

cat("📊 Generating daily summary report...\n")

# 1. Weather Outlook (Next 24h)
weather_summary <- "Keine Wetterdaten verfügbar."
if (file_exists(forecast_file)) {
    forecast <- read_csv(forecast_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Berlin")) %>%
        filter(timestamp >= now(), timestamp <= now() + hours(24))

    if (nrow(forecast) > 0) {
        max_precip <- max(forecast$precipitation_mm, na.rm = TRUE)
        total_precip <- sum(forecast$precipitation_mm, na.rm = TRUE)

        weather_summary <- paste0(
            "Vorhersage für die nächsten 24h:\n",
            "- Erwarteter Niederschlag: **", round(total_precip, 1), " mm**\n",
            "- Maximale Intensität: **", round(max_precip, 1), " mm/h**\n"
        )

        if (max_precip > 0) {
            weather_summary <- paste0(weather_summary, "- Wahrscheinlichkeit: Bis zu **", max(forecast$probability_percent), "%**\n")
        }
    }
}

# 2. Sensor Events
event_summary <- "Keine Ereignisse in den letzten 24 Stunden verzeichnet."
if (file_exists(events_file)) {
    # Suppress output safely
    try({
        events <- read_csv(events_file, show_col_types = FALSE)
        if (nrow(events) > 0 && "start_time" %in% colnames(events)) {
            events_recent <- events %>%
                mutate(start_time = as.POSIXct(start_time, tz = "UTC")) %>%
                filter(start_time >= now() - hours(24))
                
            if (nrow(events_recent) > 0) {
                event_summary <- paste0(
                    "In den letzten 24 Stunden wurden **", nrow(events_recent), "** Sensor-Ereignisse registriert.\n",
                    "Details dazu im Dashboard oder im aktuellen Log auf GitHub."
                )
            }
        }
    }, silent = TRUE)
}

# 3. Sensor Health Status
health_file <- "data/processed/sensor_health_report.md"
health_summary <- "Keine Sensor-Status-Daten verfügbar."
if (file_exists(health_file)) {
    health_summary <- paste(read_lines(health_file), collapse = "\n")
}

# 4. Assemble Report
report <- c(
    paste0("# 📅 SCIU Daily Status Report - ", format(now(), "%d.%m.%Y")),
    "",
    "## ☀️ Wetter-Check",
    weather_summary,
    "",
    "## 🌊 Sensor-Aktivität (Letzte 24h)",
    event_summary,
    "",
    "## 🛠 System-Status",
    health_summary,
    "",
    "---",
    paste0("*Bericht automatisch erstellt am ", format(now(), "%Y-%m-%d %H:%M:%S"), " (Berlin)*")
)

write_lines(report, output_file)
cat("SUCCESS: Daily summary saved to", output_file, "\n")
