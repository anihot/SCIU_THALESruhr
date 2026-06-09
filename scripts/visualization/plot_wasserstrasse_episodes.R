library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(fs)

# --- Config ---
plots_dir <- "data/output/plots/wasserstrasse_episodes"
if (!dir_exists(plots_dir)) dir_create(plots_dir, recurse = TRUE)

# --- 1. Load data ---
ws <- read_csv("data/processed/cleaned_analysis/Wasserstraße_Springorum_merged_export_cleaned.csv",
               show_col_types = FALSE)

precip <- read_csv("data/processed/precipitation_at_sensors.csv",
                   show_col_types = FALSE) %>%
    filter(grepl("Wasserstra", station, ignore.case = TRUE)) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))

cat("Sensor data:", nrow(ws), "rows\n")
cat("Precip data:", nrow(precip), "rows\n\n")

# --- 2. Identify rain episodes (distance < 1.0m with DWD confirmation) ---
ws_suspicious <- ws %>%
    filter(!is.na(distance), distance < 1.0, distance > 0.1) %>%
    arrange(Zeit_Datum) %>%
    mutate(
        time_diff = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum), units = "mins")),
        episode = cumsum(is.na(time_diff) | time_diff > 60)
    )

episodes <- ws_suspicious %>%
    group_by(episode) %>%
    summarize(
        start = min(Zeit_Datum),
        end = max(Zeit_Datum),
        duration_h = round(as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "hours")), 1),
        n_readings = n(),
        mean_dist = round(mean(distance), 3),
        .groups = "drop"
    ) %>%
    filter(n_readings >= 3)

# Check precipitation for each episode
episodes$total_precip <- 0
episodes$max_intensity <- 0

for (i in seq_len(nrow(episodes))) {
    ep <- episodes[i, ]
    window_start <- ep$start - hours(3)
    window_end <- ep$end + hours(1)
    pw <- precip %>% filter(timestamp >= window_start, timestamp <= window_end)
    episodes$total_precip[i] <- sum(pw$precipitation_mm, na.rm = TRUE)
    episodes$max_intensity[i] <- if (nrow(pw) > 0) max(pw$precipitation_mm, na.rm = TRUE) * 12 else 0
}

rain_episodes <- episodes %>% filter(total_precip > 0.5) %>% arrange(start)
cat("Rain-confirmed episodes:", nrow(rain_episodes), "\n\n")

# --- 3. Create plots ---
for (i in seq_len(nrow(rain_episodes))) {
    ep <- rain_episodes[i, ]

    # Time window: 3h before to 2h after
    plot_start <- ep$start - hours(3)
    plot_end <- ep$end + hours(2)

    # Sensor data in window — use level (computed from distance)
    # level = baseline - distance, so higher level = more water
    # For readings with NA level but valid distance, compute level from baseline ~7.16m
    baseline <- 7.16
    ws_window <- ws %>%
        filter(Zeit_Datum >= plot_start, Zeit_Datum <= plot_end) %>%
        mutate(
            level_plot = ifelse(!is.na(level), level,
                         ifelse(!is.na(distance), baseline - distance, NA_real_))
        ) %>%
        filter(!is.na(level_plot))

    # Precipitation in window
    precip_window <- precip %>%
        filter(timestamp >= plot_start, timestamp <= plot_end)

    if (nrow(ws_window) == 0) next

    # --- Build dual-axis plot ---
    # Primary y: Level (m) — higher = more water
    # Secondary y: Precipitation (mm/5min) — bars from top

    max_precip <- if (nrow(precip_window) > 0) max(precip_window$precipitation_mm, na.rm = TRUE) else 0.1
    if (max_precip == 0) max_precip <- 0.1

    # Y-axis: level range, include 0 and enough headroom
    level_max_data <- max(ws_window$level_plot, na.rm = TRUE)
    y_max <- max(level_max_data * 1.3, 0.5)  # at least 0.5m headroom

    # If artifact values present (~6.34m), use fixed range
    if (level_max_data > 5) y_max <- 7.5

    # Reserve top 25% for precip bars
    precip_zone_top <- y_max
    precip_zone_height <- y_max * 0.25
    scale_factor <- precip_zone_height / max_precip

    # Episode time label
    ep_label <- sprintf("Episode %s bis %s (%.1fh)",
                        format(ep$start, "%d.%m.%Y %H:%M"),
                        format(ep$end, "%d.%m.%Y %H:%M"),
                        ep$duration_h)

    p <- ggplot()

    # Episode time range (subtle background shading)
    p <- p +
        annotate("rect",
                 xmin = ep$start, xmax = ep$end,
                 ymin = -Inf, ymax = Inf,
                 fill = "#FFE0E0", alpha = 0.4)

    # Precipitation bars (hanging from top)
    if (nrow(precip_window) > 0 && max(precip_window$precipitation_mm, na.rm = TRUE) > 0) {
        precip_plot <- precip_window %>%
            filter(precipitation_mm > 0) %>%
            mutate(bar_top = precip_zone_top,
                   bar_bottom = precip_zone_top - precipitation_mm * scale_factor)

        if (nrow(precip_plot) > 0) {
            p <- p +
                geom_rect(
                    data = precip_plot,
                    aes(xmin = timestamp - 150, xmax = timestamp + 150,
                        ymin = bar_bottom, ymax = bar_top),
                    fill = "#56B4E9", alpha = 0.45
                )
        }
    }

    # Raw level data — simple line + points
    p <- p +
        geom_line(
            data = ws_window,
            aes(x = Zeit_Datum, y = level_plot),
            color = "#2C3E50", linewidth = 0.5, alpha = 0.8
        ) +
        geom_point(
            data = ws_window,
            aes(x = Zeit_Datum, y = level_plot),
            color = "#2C3E50", size = 0.2, alpha = 0.4
        )

    # Secondary axis: precipitation labels
    sec_breaks <- pretty(c(0, max_precip), n = 4)

    p <- p +
        scale_y_continuous(
            name = "Wasserstand / Level (m)",
            limits = c(-0.1, y_max),
            sec.axis = sec_axis(
                ~ (precip_zone_top - .) / scale_factor,
                name = "Niederschlag (mm / 5 min)",
                breaks = sec_breaks
            )
        ) +
        theme_minimal(base_size = 11) +
        labs(
            title = sprintf("Wasserstraße: Wasserstand + DWD-Niederschlag"),
            subtitle = ep_label,
            x = "Zeit",
            caption = sprintf(
                "DWD RADOLAN: %.1f mm gesamt, max. %.1f mm/h | %d Messwerte | Level = Baseline (7.16m) - Distance",
                ep$total_precip, ep$max_intensity, ep$n_readings
            )
        ) +
        theme(
            plot.title = element_text(face = "bold", size = 13),
            plot.subtitle = element_text(size = 10, color = "grey30"),
            plot.caption = element_text(size = 8, color = "grey50"),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
            axis.title.y.right = element_text(color = "#56B4E9"),
            axis.text.y.right = element_text(color = "#56B4E9"),
            legend.position = "none",
            panel.grid.minor = element_blank()
        )

    # Save
    fname <- sprintf("episode_%03d_%s.png", i,
                     format(ep$start, "%Y%m%d_%H%M"))
    output_path <- path(plots_dir, fname)
    ggsave(output_path, plot = p, width = 12, height = 6, dpi = 200)
    cat(sprintf("[%d/%d] %s — %.1f mm Niederschlag\n",
                i, nrow(rain_episodes), fname, ep$total_precip))
}

# --- 4. Overview plot: all episodes on timeline ---
cat("\nGeneriere Übersichtsplot...\n")

overview_data <- episodes %>%
    mutate(
        rain = total_precip > 0.5,
        category = ifelse(rain, "Mit Regen", "Ohne Regen (Artefakt)"),
        mid_time = start + (end - start) / 2
    )

p_overview <- ggplot(overview_data) +
    geom_segment(
        aes(x = start, xend = end, y = total_precip, yend = total_precip,
            color = category),
        linewidth = 1.5, alpha = 0.7
    ) +
    geom_point(
        aes(x = mid_time, y = total_precip, color = category, size = duration_h),
        alpha = 0.6
    ) +
    scale_color_manual(
        values = c("Mit Regen" = "#2196F3", "Ohne Regen (Artefakt)" = "#E63946"),
        name = "Kategorie"
    ) +
    scale_size_continuous(name = "Dauer (h)", range = c(1, 6)) +
    theme_minimal(base_size = 11) +
    labs(
        title = "Wasserstraße: Alle Distance-~0.82m-Episoden vs. DWD-Niederschlag",
        subtitle = sprintf("136 Episoden gesamt — %d mit Regen (blau), %d ohne Regen (rot)",
                           sum(overview_data$rain), sum(!overview_data$rain)),
        x = "Datum",
        y = "DWD-Niederschlag im Zeitfenster (mm)",
        caption = "Rote Punkte bei 0 mm = Sensor-Artefakte ohne Niederschlag"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom"
    )

ggsave(path(plots_dir, "00_uebersicht_alle_episoden.png"),
       plot = p_overview, width = 14, height = 7, dpi = 200)

cat("\nAlle Plots gespeichert in:", plots_dir, "\n")
