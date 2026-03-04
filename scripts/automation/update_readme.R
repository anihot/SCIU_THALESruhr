library(readr)
library(dplyr)
library(lubridate)
library(fs)
library(knitr)

# Config
events_file <- "data/processed/detected_events.csv"
forecast_file <- "data/processed/weather_forecast.csv"
readme_file <- "README.md"
plots_dir <- "data/output/plots"

cat("Updating README with latest events...\n")

if (!file_exists(events_file)) {
    stop("Events file not found.")
}

# 1. Read events
if (file_exists(events_file)) {
    events <- read_csv(events_file, show_col_types = FALSE)
} else {
    events <- tibble(station = character(), start_time = POSIXct(), end_time = POSIXct(), peak_level_cm = numeric(), duration_min = numeric())
}

# 2. Filter for recent events (last 30 hours)
current_time <- with_tz(Sys.time(), tzone = "UTC")

# Handle empty events list gracefully
if (nrow(events) > 0) {
    events_recent <- events %>%
        mutate(start_time = as.POSIXct(start_time, tz = "UTC")) %>%
        filter(start_time > (current_time - hours(30))) %>%
        arrange(desc(start_time))
} else {
    events_recent <- events # Should be empty anyway
}

# 3. Prepare Markdown section
if (nrow(events_recent) > 0) {
    header_text <- paste0(
        "\n## 🔔 Aktuelle Ereignisse (Letzte 24-30h)\n",
        "*Stand: ", format(current_time, "%Y-%m-%d %H:%M:%S UTC"), "*\n\n",
        "Es wurden **", nrow(recent_events), "** neue potenzielle Ereignisse erkannt.\n\n"
    )

    # Table of recent events
    event_table <- kable(recent_events %>% select(station, start_time, peak_level_cm, duration_min), format = "markdown")
    event_table <- paste(event_table, collapse = "\n")

    # Matching plots
    unique_stations <- unique(recent_events$station)
    plot_links <- "\n### 📈 Aktuelle Plots der betroffenen Stationen\n"

    for (st in unique_stations) {
        # Construct expected plot filename
        # Pattern from plot_sensor_data.R: paste0(station_name, ".png")
        # where station_name is "Herzogstraße_merged_export_cleaned"
        # Wait, the event_detection script cleans the name to "Herzogstraße"
        # But plot_sensor_data.R uses the filename from cleaned_dir which is "Herzogstraße_merged_export_cleaned"

        plot_filename <- paste0(st, "_merged_export_cleaned.png")
        plot_path <- path(plots_dir, plot_filename)

        if (file_exists(plot_path)) {
            plot_links <- paste0(
                plot_links, "#### Station: ", st, "\n",
                "![Plot ", st, "](", plot_path, ")\n\n"
            )
        } else {
            cat("Warning: Plot not found for station", st, "at", plot_path, "\n")
        }
    }

    new_content <- paste0(header_text, event_table, "\n", plot_links, "\n---\n")
} else {
    new_content <- paste0("\n## ✅ Keine neuen Ereignisse in den letzten 24h\n*Stand: ", format(current_time, "%Y-%m-%d %H:%M:%S UTC"), "*\n\n---\n")
}

# 3b. Weather Outlook
weather_content <- ""
if (file_exists(forecast_file)) {
    forecast <- read_csv(forecast_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Berlin"))

    # Check for rain in next 48 hours
    upcoming_rain <- forecast %>%
        filter(timestamp > now(), timestamp < (now() + hours(48)), precipitation_mm > 0)

    if (nrow(upcoming_rain) > 0) {
        max_rain <- max(upcoming_rain$precipitation_mm)
        total_rain_48h <- sum(upcoming_rain$precipitation_mm)

        # Determine intensity icon
        intensity_icon <- "🔹"
        if (max_rain > 2) intensity_icon <- "🟡"
        if (max_rain > 5) intensity_icon <- "🔴"

        weather_content <- paste0(
            "## 🌦 Wetterausblick (Nächste 48h)\n",
            intensity_icon, " **Regen erwartet:** Summe ca. **", round(total_rain_48h, 1), " mm**. ",
            "Maximale Intensität: **", round(max_rain, 1), " mm/h**.\n\n",
            "*Datenquelle: Open-Meteo*\n\n---\n"
        )
    } else {
        weather_content <- "## ☀️ Wetterausblick (Nächste 48h)\nKein nennenswerter Regen vorhergesagt.\n\n---\n"
    }
}

# 4. Inject into README
# Combine events and weather
total_new_content <- paste0(weather_content, "\n", new_content)
readme_txt <- read_file(readme_file)

start_mark <- "<!-- LATEST_EVENTS_START -->"
end_mark <- "<!-- LATEST_EVENTS_END -->"

pattern <- paste0(start_mark, "(?s:.*?)", end_mark)
replacement <- paste0(start_mark, "\n", total_new_content, "\n", end_mark)

updated_readme <- gsub(pattern, replacement, readme_txt, perl = TRUE)

# write_file with explicit UTF-8 to avoid mangling emojis or characters on Windows
write_file(updated_readme, readme_file)

cat("SUCCESS: README.md updated with latest events.\n")
