library(readr)
library(dplyr)
library(fs)
library(lubridate)

# Config
input_dir <- "data/raw/sensor_exports"
output_csv <- "data/processed/sensor_health_status.csv"
output_md <- "data/processed/sensor_health_report.md"
HEALTH_THRESHOLD_HOURS <- 24

cat("🔍 Checking sensor health (data freshness)...\n")

if (!dir_exists(input_dir)) {
    stop("Input directory not found: ", input_dir)
}

# List all sensor export files
files <- dir_ls(input_dir, glob = "*.csv")
# Exclude "Wasserbaulabor_" (without "2") from health checks
files <- files[!grepl("Wasserbaulabor__", basename(files))]
# Exclude Herzogstraße (sensor currently inactive/decommissioned)
files <- files[!grepl("Herzogstra", basename(files))]

health_data <- list()

for (f in files) {
    station_name <- gsub("_merged_export", "", path_ext_remove(path_file(f)))

    # Check if file is nearly empty or actually has data
    if (file_info(f)$size < 100) {
        health_data[[length(health_data) + 1]] <- tibble(
            station = station_name,
            last_seen = NA,
            hours_since = Inf,
            status = "No Data"
        )
        next
    }

    tryCatch(
        {
            last_line <- tail(readLines(f, warn = FALSE), 1)
            ts_str <- sub('^"([^"]*)".*', '\\1', last_line)
            latest_time <- as_datetime(ts_str)

            if (!is.na(latest_time)) {
                diff_hours <- as.numeric(difftime(now(tzone = "UTC"), latest_time, units = "hours"))
                status <- ifelse(diff_hours > HEALTH_THRESHOLD_HOURS, "Inactive", "Healthy")

                health_data[[length(health_data) + 1]] <- tibble(
                    station = station_name,
                    last_seen = latest_time,
                    hours_since = round(diff_hours, 1),
                    status = status
                )
            }
        },
        error = function(e) {
            cat("Error processing", station_name, ":", e$message, "\n")
        }
    )
}

# Combine results
health_df <- bind_rows(health_data)

# Save CSV
write_csv(health_df, output_csv)

# Generate Markdown Report
report_lines <- c("### 🛠 Sensor-Status-Check", "")

if (any(health_df$status != "Healthy")) {
    inactive <- health_df %>% filter(status != "Healthy")
    report_lines <- c(
        report_lines,
        "⚠️ **Achtung: Inaktive Sensoren erkannt!**",
        "*(Hinweis: Dies kann auch durch einen Fehler im automatisierten Download-Prozess verursacht werden)*",
        ""
    )
    for (i in seq_len(nrow(inactive))) {
        report_lines <- c(report_lines, paste0("- 🔴 **", inactive$station[i], "**: Letzte Daten vor ", inactive$hours_since[i], " Stunden (", format(inactive$last_seen[i], "%d.%m. %H:%M"), ")"))
    }
} else {
    report_lines <- c(report_lines, "✅ Alle Sensoren senden planmäßig Daten (letzte 24h).")
}

write_lines(report_lines, output_md)

cat("SUCCESS: Sensor health report saved to", output_md, "\n")
print(health_df)
