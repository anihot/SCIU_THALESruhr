# Plot for Wasserbaulabor 2 Experiment
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(fs)

# 1. Load data
data_file <- "data/processed/cleaned_analysis/Wasserbaulabor_2_merged_export_cleaned.csv"
if (!file_exists(data_file)) stop("Data not found")
df <- read_csv(data_file)
df$Zeit_Datum <- as.POSIXct(df$Zeit_Datum, tz = "UTC")

# 2. Extract Event 1: Feb 21st (approx 19:30 to 22:30)
event1 <- df %>%
  filter(Zeit_Datum >= as.POSIXct("2025-02-21 19:00:00", tz="UTC") & Zeit_Datum <= as.POSIXct("2025-02-21 23:00:00", tz="UTC"))

# 3. Extract Event 2: Feb 23rd (approx 08:15 to 09:45)  
event2 <- df %>%
  filter(Zeit_Datum >= as.POSIXct("2025-02-23 07:45:00", tz="UTC") & Zeit_Datum <= as.POSIXct("2025-02-23 10:15:00", tz="UTC"))

# 4. Extract Event 3: Feb 26th (approx 09:30 to 09:45)
event3 <- df %>%
  filter(Zeit_Datum >= as.POSIXct("2025-02-26 09:00:00", tz="UTC") & Zeit_Datum <= as.POSIXct("2025-02-26 10:15:00", tz="UTC"))

# Function to plot
plot_event <- function(data, title, filename) {
  p <- ggplot(data, aes(x = Zeit_Datum, y = level)) +
    geom_line(color = "#007BFF", size = 1) +
    geom_point(color = "#0056b3", size = 2) +
    labs(title = title,
         x = "Uhrzeit (UTC)",
         y = "Wasserstand (cm)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  ggsave(filename, plot = p, width = 10, height = 5, bg = "white")
}

dir_create("output_experiments")
if(nrow(event1) > 0) plot_event(event1, "Wasserbaulabor 2: Experiment am 21.02.2025", "output_experiments/wasserbaulabor2_21feb.png")
if(nrow(event2) > 0) plot_event(event2, "Wasserbaulabor 2: Experiment am 23.02.2025", "output_experiments/wasserbaulabor2_23feb.png")
if(nrow(event3) > 0) plot_event(event3, "Wasserbaulabor 2: Experiment am 26.02.2025", "output_experiments/wasserbaulabor2_26feb.png")

cat("Plots generated successfully in output_experiments directory\n")
