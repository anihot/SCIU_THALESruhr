library(readr)
library(ggplot2)
library(dplyr)
library(fs)
library(lubridate)

# Define directories
cleaned_dir <- "data/processed/cleaned_analysis"
plots_dir <- "data/output/plots"

if (!dir_exists(plots_dir)) {
    dir_create(plots_dir)
}

# Get list of cleaned files
cleaned_files <- dir_ls(cleaned_dir, glob = "*.csv")
# Exclude "Wasserbaulabor_" (without "2") from plotting
cleaned_files <- cleaned_files[!grepl("Wasserbaulabor__", basename(cleaned_files))]
precip_file <- "data/processed/precipitation_at_sensors.csv"

# Load RADOLAN precipitation data
if (file_exists(precip_file)) {
    precip_data <- read_csv(precip_file, show_col_types = FALSE) %>%
        mutate(timestamp = with_tz(as.POSIXct(timestamp, tz = "UTC"), "Europe/Berlin"))
} else {
    precip_data <- NULL
}

# Load Open-Meteo forecast as fallback (used when RADOLAN has no data for a station/window)
forecast_file <- "data/processed/weather_forecast.csv"
precip_fallback <- NULL
if (file_exists(forecast_file)) {
    precip_fallback <- read_csv(forecast_file, show_col_types = FALSE) %>%
        mutate(timestamp = with_tz(timestamp, tzone = "Europe/Berlin"))
    # Abwärtskompatibel: falls keine station-Spalte, für alle Sensoren nutzen
    if (!"station" %in% names(precip_fallback)) {
        precip_fallback <- precip_fallback %>% select(timestamp, precipitation_mm)
    }
}

for (file_path in cleaned_files) {
    file_name <- path_file(file_path)
    # Extract station name (assumes format StationName_merged_export_cleaned.csv)
    # The event detection script and others use the base station name
    station_name <- gsub("_merged_export_cleaned", "", path_ext_remove(file_name))

    cat("Plotting station:", station_name, "\n")

    # Load data
    df <- read_csv(file_path, show_col_types = FALSE)

    if (!"Zeit_Datum" %in% names(df) || !"level" %in% names(df)) {
        cat("Warning: Required columns missing in", file_path, "- skipping\n")
        next
    }

    df <- df %>% mutate(Zeit_Datum = with_tz(Zeit_Datum, "Europe/Berlin"))

    if (nrow(df) == 0) {
        cat("Warning: No data to plot for", station_name, "\n")
        next
    }

    # Filter for the last 24 hours
    current_time <- now(tzone = "Europe/Berlin")
    start_time <- current_time - hours(24)

    df_24h <- df %>%
        filter(Zeit_Datum >= start_time)

    if (nrow(df_24h) == 0) {
        cat("Warning: No data in the last 24 hours for", station_name, "\n")
        next
    }

    # Prepare precipitation overlay: RADOLAN first, Open-Meteo as fallback
    p_precip      <- NULL
    precip_source <- ""
    scale_factor  <- 1

    if (!is.null(precip_data)) {
        radolan_station <- precip_data %>%
            filter(station == station_name, timestamp >= start_time)
        if (nrow(radolan_station) > 0 && any(radolan_station$precipitation_mm > 0, na.rm = TRUE)) {
            p_precip      <- radolan_station
            precip_source <- "RADOLAN (DWD)"
        }
    }

    if (is.null(p_precip) && !is.null(precip_fallback)) {
        # Per-Sensor filtern falls station-Spalte vorhanden
        if ("station" %in% names(precip_fallback)) {
            p_precip <- precip_fallback %>%
                filter(station == station_name, timestamp >= start_time) %>%
                select(timestamp, precipitation_mm)
        } else {
            p_precip <- precip_fallback %>% filter(timestamp >= start_time)
        }
        precip_source <- "Open-Meteo (Vorhersage)"
    }

    if (!is.null(p_precip) && nrow(p_precip) > 0) {
        max_level  <- max(df_24h$level * 100, na.rm = TRUE)
        max_precip <- max(p_precip$precipitation_mm, na.rm = TRUE)

        # If level is flat near 0, use a fixed 10 cm reference so precip bars are visible
        if (!is.finite(max_level) || max_level <= 0) max_level <- 10
        if (!is.finite(max_precip) || max_precip == 0) {
            scale_factor <- 1
        } else {
            scale_factor <- max_level / max_precip
        }
    }


    # Create plot
    p <- ggplot() +
        geom_line(data = df_24h, aes(x = Zeit_Datum, y = level * 100, color = "Wasserstand"), linewidth = 0.8)

    if (!is.null(p_precip) && nrow(p_precip) > 0) {
        p <- p +
            geom_col(
                data = p_precip, aes(x = timestamp, y = precipitation_mm * scale_factor, fill = "Niederschlag"),
                alpha = 0.3, width = 300
            ) + # 300s = 5min width (RADOLAN RY)
            scale_y_continuous(
                name = "Wasserstand (cm)",
                sec.axis = sec_axis(~ . / scale_factor, name = "Niederschlag (mm/5min)")  # RADOLAN RY
            ) +
            scale_fill_manual(values = c("Niederschlag" = "#56B4E9"), name = "")
    } else {
        p <- p + scale_y_continuous(name = "Wasserstand (cm)")
    }

    p <- p +
        scale_color_manual(values = c("Wasserstand" = "#0072B2"), name = "") +
        theme_minimal() +
        labs(
            title = paste("Sensor Data (Last 24h):", station_name),
            subtitle = paste("Window:", format(min(df_24h$Zeit_Datum), "%Y-%m-%d %H:%M"), "to", format(max(df_24h$Zeit_Datum), "%Y-%m-%d %H:%M")),
            x = "Time",
            caption = paste0("Niederschlagsquelle: ", if (nchar(precip_source) > 0) precip_source else "keine Daten",
                         " | Generiert: ", format(now(), "%Y-%m-%d %H:%M:%S"))
        ) +
        theme(
            plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
        )

    # Save plot
    output_plot <- path(plots_dir, paste0(station_name, "_merged_export_cleaned.png"))
    ggsave(output_plot, plot = p, width = 10, height = 6, dpi = 300)

    cat("Saved overlay plot to:", output_plot, "\n")
}


cat("\nPlotting complete.\n")
