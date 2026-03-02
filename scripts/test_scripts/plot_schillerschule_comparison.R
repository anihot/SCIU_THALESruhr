library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Config
rain_yard_file <- "c:/Users/Anika Hotzel/Desktop/04_R/schillerschule/schillerschule_yard.csv"
rain_garden_file <- "c:/Users/Anika Hotzel/Desktop/04_R/schillerschule/schillerschule_garden.csv"
events_file <- "data/schillerschule_analysiert.csv"
plots_dir <- "data/plots"

if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

cat("🚀 Starting Comparison Plotting (Aggregated)...\n")

# 1. Load and Aggregate Rain Data
load_rain <- function(file_path, label) {
    cat("Reading", label, "data...\n")
    read_csv(file_path,
        col_select = c(TIMESTAMP, Rain_mm_Tot),
        col_types = cols(
            TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S%z"),
            Rain_mm_Tot = col_double()
        )
    )
}

rain_yard <- load_rain(rain_yard_file, "Yard")
rain_garden <- load_rain(rain_garden_file, "Garden")

# Combine and take Max to represent "Schillerschule Site"
rain_combined <- full_join(rain_yard, rain_garden, by = "TIMESTAMP", suffix = c("_yard", "_garden")) %>%
    mutate(Rain_mm = pmax(Rain_mm_Tot_yard, Rain_mm_Tot_garden, na.rm = TRUE)) %>%
    select(TIMESTAMP, Rain_mm) %>%
    filter(!is.na(Rain_mm))

# 2. Overview Plot - Daily Totals
cat("Generating Overview Plot...\n")
rain_daily <- rain_combined %>%
    mutate(Date = as.Date(TIMESTAMP)) %>%
    group_by(Date) %>%
    summarise(Daily_Rain = sum(Rain_mm, na.rm = TRUE), .groups = "drop")

p_overview <- ggplot(rain_daily, aes(x = Date, y = Daily_Rain)) +
    geom_col(fill = "#3498DB") +
    theme_minimal() +
    labs(
        title = "Niederschlagsganglinie Schillerschule (Aggregiert)",
        subtitle = "Tägliche Summen (Max aus Garten & Hof)",
        x = "Datum", y = "Regen (mm)"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

ggsave(file.path(plots_dir, "schillerschule_rain_overview.png"), p_overview, width = 12, height = 6, dpi = 300)

# 3. Detailed Comparison Plot for Top Events
cat("Loading Events and Sensor Data...\n")
events <- read_csv(events_file, show_col_types = FALSE) %>%
    filter(rain_verified) %>%
    arrange(desc(total_rain_mm))

# Take top 3 events for comparison
top_events <- head(events, 3)

for (i in seq_len(nrow(top_events))) {
    event <- top_events[i, ]
    station_name <- event$station
    station_clean <- gsub("_[S|R].*", "", station_name)
    start_dt <- event$start_time
    end_dt <- event$end_time

    cat("Plotting Event:", station_name, "at", as.character(start_dt), "\n")

    # Load raw sensor data
    sensor_file <- paste0("data/sensor_exports/", station_name, "_merged_export.csv")
    if (!file.exists(sensor_file)) next

    sensor_data <- read_csv(sensor_file, show_col_types = FALSE) %>%
        mutate(Timestamp = ymd_hms(Timestamp)) %>%
        filter(Timestamp >= (start_dt - hours(1)), Timestamp <= (end_dt + hours(3)))

    # Load rain data for same window
    rain_window <- rain_combined %>%
        filter(TIMESTAMP >= (start_dt - hours(1)), TIMESTAMP <= (end_dt + hours(3)))

    # Dual Axis Plot
    p_comp <- ggplot() +
        # Rain Bar (Blue)
        geom_col(data = rain_window, aes(x = TIMESTAMP, y = Rain_mm * 10), fill = "#3498DB", alpha = 0.5) +
        # Sensor Level Line (Red)
        geom_line(data = sensor_data, aes(x = Timestamp, y = level), color = "#E74C3C", linewidth = 1) +
        scale_y_continuous(
            name = "Wasserstand (cm)",
            sec.axis = sec_axis(~ . / 10, name = "Niederschlag (mm)")
        ) +
        theme_minimal() +
        labs(
            title = paste("Ereignis-Analyse:", station_clean),
            subtitle = paste("Datum:", format(start_dt, "%d.%m.%Y"), "| Peak:", round(event$peak_level_cm, 1), "cm"),
            x = "Zeit"
        ) +
        theme(
            axis.title.y = element_text(color = "#E74C3C", face = "bold"),
            axis.title.y.right = element_text(color = "#3498DB", face = "bold")
        )

    safe_name <- gsub(" ", "_", tolower(station_clean))
    date_str <- format(start_dt, "%Y%m%d_%H%M")
    ggsave(file.path(plots_dir, paste0("comparison_", safe_name, "_", date_str, ".png")), p_comp, width = 10, height = 6, dpi = 300)
}

cat("✅ All plots generated in:", plots_dir, "\n")
