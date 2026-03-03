library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load data
df <- read_csv("data/processed/cleaned_analysis/Wasserbaulabor_2_merged_export_cleaned.csv", show_col_types = FALSE)

# Filter for today's experiment (2026-02-18)
today_data <- df %>% filter(as.Date(Zeit_Datum) == as.Date("2026-02-18"))

# Identify activity (level > 0.5cm)
activity <- today_data %>% filter(level > 0.005)

if (nrow(activity) > 0) {
    # Metrics
    t_start <- min(activity$Zeit_Datum, na.rm = TRUE)
    t_end <- max(activity$Zeit_Datum, na.rm = TRUE)

    max_level <- max(activity$level, na.rm = TRUE)
    t_peak <- activity$Zeit_Datum[which.max(activity$level)]

    duration_total <- as.numeric(difftime(t_end, t_start, units = "mins"))
    duration_rise <- as.numeric(difftime(t_peak, t_start, units = "mins"))
    duration_fall <- as.numeric(difftime(t_end, t_peak, units = "mins"))

    rise_rate <- (max_level * 100) / duration_rise # cm/min

    # Statistical baseline (before experiment) - check if we have 5 mins of pre-data
    pre_exp <- today_data %>% filter(Zeit_Datum < t_start - minutes(2))
    if (nrow(pre_exp) > 5) {
        baseline_noise <- sd(pre_exp$level, na.rm = TRUE) * 100 # cm
    } else {
        baseline_noise <- 0
    }

    cat("--- EXPERIMENT REPORT ---\n")
    cat("Start Time: ", format(t_start, "%H:%M:%S"), " UTC\n")
    cat("Peak Time:  ", format(t_peak, "%H:%M:%S"), " UTC\n")
    cat("End Time:   ", format(t_end, "%H:%M:%S"), " UTC\n\n")

    cat("Max Level:  ", round(max_level * 100, 2), " cm\n")
    cat("Total Time: ", round(duration_total, 1), " min\n")
    cat("Rise Phase: ", round(duration_rise, 1), " min (Gradient: ", round(rise_rate, 3), " cm/min)\n")
    cat("Fall Phase: ", round(duration_fall, 1), " min\n")
    cat("Noise (SD): ", round(baseline_noise, 4), " cm\n")

    # Zoomed Plot
    p <- ggplot(
        today_data %>% filter(Zeit_Datum >= (t_start - minutes(15)) & Zeit_Datum <= (t_end + minutes(15))),
        aes(x = Zeit_Datum, y = level * 100)
    ) +
        geom_line(color = "#D55E00", size = 1) +
        geom_point(alpha = 0.5, size = 1) +
        theme_minimal() +
        labs(
            title = "High-Resolution Experiment Analysis: Wasserbaulabor 2",
            subtitle = paste("Event detected on", as.Date(t_start), "| Sampling rate: ~15s"),
            x = "Time (UTC)",
            y = "Water Level (cm)",
            caption = paste("Peak detected at", format(t_peak, "%H:%M"), "UTC")
        ) +
        theme(
            plot.title = element_text(face = "bold", size = 14),
            axis.title = element_text(face = "bold")
        ) +
        scale_x_datetime(date_labels = "%H:%M", date_breaks = "10 mins")

    ggsave("data/output/plots/experiment_detail_zoom.png", p, width = 10, height = 6, dpi = 300)
    cat("\nHigh-res plot saved to: data/output/plots/experiment_detail_zoom.png\n")
} else {
    cat("No significant activity detected for today.\n")
}
