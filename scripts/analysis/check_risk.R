library(readr)
library(dplyr)
library(lubridate)
library(fs)

# Config
forecast_file <- "data/processed/weather_forecast.csv"
historical_log <- "data/metadata/historical_verified_events.csv"
risk_report_file <- "data/processed/risk_report.md"

# Thresholds
DEFAULT_STARKREGEN_MM_H <- 15 # DWD Level 2

cat("🧐 Checking weather forecast for potential sensor triggers...\n")

if (!file_exists(forecast_file)) {
    stop("Forecast file not found. Run fetch_forecast.R first.")
}

# 1. Load Forecast
forecast <- read_csv(forecast_file, show_col_types = FALSE) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Berlin")) %>%
    filter(timestamp > now())

if (nrow(forecast) == 0) {
    cat("No upcoming forecast data available.\n")
    quit(save = "no", status = 0)
}

# 2. Determine Risk Thresholds
# We use the minimum intensity that historically caused an event as a danger mark
risk_threshold <- DEFAULT_STARKREGEN_MM_H

if (file_exists(historical_log)) {
    hist_data <- read_csv(historical_log, show_col_types = FALSE)
    if (nrow(hist_data) > 0 && "max_intensity_mm_h" %in% names(hist_data)) {
        min_hist_intensity <- min(hist_data$max_intensity_mm_h[hist_data$max_intensity_mm_h > 0], na.rm = TRUE)
        if (is.finite(min_hist_intensity)) {
            # Use the historical minimum if it's lower than DWD (more sensitive)
            # but cap it at 2mm/h to avoid spamming for trivial rain
            risk_threshold <- max(2, min_hist_intensity)
            cat("Using sensitive historical threshold:", risk_threshold, "mm/h\n")
        }
    }
} else {
    cat("Using default DWD Starkregen threshold:", risk_threshold, "mm/h\n")
}

# 3. Identify High Risk Windows
high_risk <- forecast %>%
    filter(precipitation_mm >= risk_threshold)

if (nrow(high_risk) > 0) {
    cat("⚠️ ALERT: High precipitation risk detected!\n")

    # Generate Report
    report_lines <- c(
        "# ⚠️ HOCHWASSER-RISIKOWARNUNG",
        "",
        paste0("Basierend auf der aktuellen Vorhersage besteht ein erhöhtes Risiko für Ereignisse an den Sensoren."),
        paste0("- Festgelegter Schwellenwert: **", round(risk_threshold, 1), " mm/h**"),
        "",
        "## Kritische Zeitfenster (Vorhersage):",
        ""
    )

    risk_table <- high_risk %>%
        arrange(timestamp) %>%
        select(timestamp, precipitation_mm, probability_percent) %>%
        head(10) # Top 10 critical points

    library(knitr)
    report_lines <- c(report_lines, kable(risk_table, format = "markdown"), "", "*Bitte die Sensoren im Auge behalten.*")

    write_lines(report_lines, risk_report_file)
    cat("Risk report saved to", risk_report_file, "\n")

    # We exit with a specific status or signal if we want the GitHub Action to branch?
    # Actually, we'll just check if the file exists in the workflow.
} else {
    cat("✅ No immediate risk detected in the 7-day forecast.\n")
    if (file_exists(risk_report_file)) file_delete(risk_report_file)
}
