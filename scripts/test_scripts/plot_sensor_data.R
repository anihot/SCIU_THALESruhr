library(readr)
library(ggplot2)
library(dplyr)
library(fs)
library(lubridate)

# Define directories
cleaned_dir <- "data/cleaned_analysis"
plots_dir <- "data/plots"

if (!dir_exists(plots_dir)) {
    dir_create(plots_dir)
}

# Get list of cleaned files
cleaned_files <- dir_ls(cleaned_dir, glob = "*.csv")

for (file_path in cleaned_files) {
    file_name <- path_file(file_path)
    station_name <- path_ext_remove(file_name)

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

    # Create plot
    p <- ggplot(df_24h, aes(x = Zeit_Datum, y = level)) +
        geom_line(color = "#0072B2", linewidth = 0.8) +
        theme_minimal() +
        labs(
            title = paste("Water Level (Last 24h):", station_name),
            subtitle = paste("Window:", format(min(df_24h$Zeit_Datum), "%Y-%m-%d %H:%M"), "to", format(max(df_24h$Zeit_Datum), "%Y-%m-%d %H:%M")),
            x = "Time",
            y = "Water Level (m)",
            caption = paste("Source: Nivus Sensor Data | Generated:", format(now(), "%Y-%m-%d %H:%M:%S"))
        ) +
        theme(
            plot.title = element_text(face = "bold", size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )

    # Save plot
    output_plot <- path(plots_dir, paste0(station_name, ".png"))
    ggsave(output_plot, plot = p, width = 10, height = 6, dpi = 300)

    cat("Saved plot to:", output_plot, "\n")
}

cat("\nPlotting complete.\n")
