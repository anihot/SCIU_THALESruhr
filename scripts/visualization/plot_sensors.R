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
precip_file <- "data/processed/precipitation_at_sensors.csv"

# Load precipitation data if available
if (file_exists(precip_file)) {
    precip_data <- read_csv(precip_file, show_col_types = FALSE) %>%
        mutate(timestamp = as.POSIXct(timestamp))
} else {
    precip_data <- NULL
}

for (file_path in cleaned_files) {
    file_name <- path_file(file_path)
    # Extract station name (assumes format StationName_merged_export_cleaned.csv)
    # The event detection script and others use the base station name
    station_name <- gsub("_merged_export_cleaned", "", path_ext_remove(file_name))

    cat("Plotting station:", station_name, "\n")

    # Load data
    df <- read_csv(file_path, show_col_types = FALSE)

    if (nrow(df) == 0) {
        cat("Warning: No data to plot for", station_name, "\n")
        next
    }

    # Filter for the last 24 hours
    current_time <- now(tzone = lubridate::tz(df$Zeit_Datum))
    start_time <- current_time - hours(24)

    df_24h <- df %>%
        filter(Zeit_Datum >= start_time)

    if (nrow(df_24h) == 0) {
        cat("Warning: No data in the last 24 hours for", station_name, "\n")
        next
    }

    # Prepare precipitation overlay
    p_precip <- NULL
    scale_factor <- 1
    if (!is.null(precip_data)) {
        p_precip <- precip_data %>%
            filter(station == station_name, timestamp >= start_time)

        if (nrow(p_precip) > 0) {
            # Rescale factor for secondary axis (e.g., max precip in 24h to max level)
            max_level <- max(df_24h$level * 100, na.rm = TRUE)
            max_precip <- max(p_precip$precipitation_mm, na.rm = TRUE)

            # Handle cases where max_precip is 0, -Inf (empty), or NA
            if (is.na(max_precip) || !is.finite(max_precip) || max_precip == 0) {
                scale_factor <- 1
            } else {
                if (max_level <= 0) max_level <- 1 # Avoid division by zero/negative
                scale_factor <- max_level / max_precip
            }
        }
    }


    # Create plot
    p <- ggplot() +
        geom_line(data = df_24h, aes(x = Zeit_Datum, y = level * 100, color = "Wasserstand"), linewidth = 0.8)

    if (!is.null(p_precip) && nrow(p_precip) > 0) {
        p <- p +
            geom_col(
                data = p_precip, aes(x = timestamp, y = precipitation_mm * scale_factor, fill = "Niederschlag"),
                alpha = 0.3, width = 3600
            ) + # 3600s = 1h width
            scale_y_continuous(
                name = "Wasserstand (cm)",
                sec.axis = sec_axis(~ . / scale_factor, name = "Niederschlag (mm/h)")
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
            caption = paste("Source: Nivus & DWD RADOLAN | Generated:", format(now(), "%Y-%m-%d %H:%M:%S"))
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
