# Plot for Wasserbaulabor 2 Experiment - Feb 18th
library(dplyr)
library(readr)
library(ggplot2)

df <- read_csv("data/processed/cleaned_analysis/Wasserbaulabor_2_merged_export_cleaned.csv")
df$Zeit_Datum <- as.POSIXct(df$Zeit_Datum, tz = "UTC")

# Extract Event: Feb 18th (09:00 to 10:00)
# Start time from summary: "2026/02/18 09:18:15"
# Note: The year in the summary says 2026, which is either a typo in data or future date, 
# falling back to 2026 for filtering just in case, but expanding the window
event_feb18 <- df %>%
  filter(
    (Zeit_Datum >= as.POSIXct("2026-02-18 09:00:00", tz="UTC") & Zeit_Datum <= as.POSIXct("2026-02-18 10:00:00", tz="UTC"))
  )

if (nrow(event_feb18) > 0) {
  p <- ggplot(event_feb18, aes(x = Zeit_Datum, y = level)) +
    geom_line(color = "#007BFF", linewidth = 1) +
    geom_point(color = "#0056b3", size = 2) +
    labs(title = "Wasserbaulabor 2: Experiment am 18.02.",
         x = "Uhrzeit (UTC)",
         y = "Wasserstand (cm)") +
    coord_cartesian(xlim = c(min(na.omit(event_feb18$Zeit_Datum)), max(na.omit(event_feb18$Zeit_Datum)))) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  ggsave("C:/Users/Anika Hotzel/.gemini/antigravity/brain/2f14420d-cfb5-4fab-b5db-5e48a0da74d6/plots/wasserbaulabor2_18feb.png", plot = p, width = 10, height = 5, bg = "white")
  cat("Plot saved successfully.\n")
} else {
  cat("No data found for this timeframe.\n")
}
