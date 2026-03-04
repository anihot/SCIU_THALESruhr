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

# 3b. Weather Outlook (DWD-compliant criteria)
weather_content <- ""
if (file_exists(forecast_file)) {
    forecast <- read_csv(forecast_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp, tz = "Europe/Berlin"))

    # Calculate rolling 6h sum for DWD criteria
    # Using a simple row-wise sum for the next 6 hours at each point
    forecast <- forecast %>%
        rowwise() %>%
        mutate(
            precip_6h = sum(forecast$precipitation_mm[which(forecast$timestamp >= timestamp & forecast$timestamp < (timestamp + hours(6)))], na.rm = TRUE)
        ) %>%
        ungroup()

    # Look at next 48 hours
    outlook_window <- forecast %>%
        filter(timestamp > now(), timestamp < (now() + hours(48)))

    if (nrow(outlook_window) > 0) {
        max_1h <- max(outlook_window$precipitation_mm, na.rm = TRUE)
        max_6h <- max(outlook_window$precip_6h, na.rm = TRUE)
        total_48h <- sum(outlook_window$precipitation_mm, na.rm = TRUE)

        # DWD Thresholds
        # 1. Extremes Unwetter (Stufe 4): > 40mm/1h or > 60mm/6h
        # 2. Unwetter (Stufe 3): > 25mm/1h or > 35mm/6h
        # 3. Markantes Wetter (Stufe 2): > 15mm/1h or > 20mm/6h

        warn_level <- 0
        warn_msg <- "Kein nennenswerter Regen vorhergesagt."
        warn_icon <- "☀️"

        if (max_1h > 0.5) {
            warn_level <- 1
            warn_msg <- "Leichter bis mäßiger Regen vorhergesagt."
            warn_icon <- "🔹"
        }

        if (max_1h > 15 || max_6h > 20) {
            warn_level <- 2
            warn_msg <- "**Warnung vor markantem Starkregen** (DWD Stufe 2)"
            warn_icon <- "🟡"
        }

        if (max_1h > 25 || max_6h > 35) {
            warn_level <- 3
            warn_msg <- "**UNWETTERWARNUNG vor Starkregen** (DWD Stufe 3)"
            warn_icon <- "🔴"
        }

        if (max_1h > 40 || max_6h > 60) {
            warn_level <- 4
            warn_msg <- "**⚠️ EXTREME UNWETTERWARNUNG vor Starkregen** (DWD Stufe 4)"
            warn_icon <- "🟣"
        }

        weather_content <- paste0(
            "### ", warn_icon, " Wetterausblick (48h)\n",
            "**", warn_msg, "**\n\n",
            "- Summe: **", round(total_48h, 1), " mm** | Max: **", round(max_1h, 1), " mm/h**\n",
            "- Max. 6h: **", round(max_6h, 1), " mm**\n\n",
            "*Quelle: Open-Meteo (DWD)*\n\n"
        )
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

cat("SUCCESS: README.md updated with latest events and detailed weather outlook.\n")
