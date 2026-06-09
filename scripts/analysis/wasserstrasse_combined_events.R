library(readr)
library(dplyr)
library(lubridate)
library(fs)

# ============================================================
# Wasserstraße: Kombinierte Ereigniserkennung
# ============================================================
# Kombiniert:
#   A) Reguläre Event-Detection (Level-basiert) — aus detected_events.csv
#   B) Artefakt-Episoden (~0.82m Distance) die mit DWD-Niederschlag
#      und Events an anderen Sensoren korrelieren
#
# Ergebnis: Eine einheitliche Ereignisliste mit Quellenangabe
# ============================================================

baseline <- 7.16

# --- 1. Load data ---
ws <- read_csv("data/processed/cleaned_analysis/Wasserstraße_Springorum_merged_export_cleaned.csv",
               show_col_types = FALSE)

precip <- read_csv("data/processed/precipitation_at_sensors.csv",
                   show_col_types = FALSE) %>%
    filter(grepl("Wasserstra", station, ignore.case = TRUE)) %>%
    mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))

events_all <- read_csv("data/processed/detected_events.csv",
                       show_col_types = FALSE)

cat("=== Wasserstraße: Kombinierte Ereigniserkennung ===\n\n")

# --- A) Reguläre Events (aus bestehender Pipeline) ---
ws_regular <- events_all %>%
    filter(grepl("Wasserstra", station, ignore.case = TRUE))

cat("A) Reguläre Event-Detection: ", nrow(ws_regular), " Ereignis(se)\n")
if (nrow(ws_regular) > 0) print(ws_regular)
cat("\n")

# --- B) Artefakt-Episoden analysieren ---
# Identifiziere Perioden wo Distance auf ~0.82m springt
ws_artifact <- ws %>%
    filter(!is.na(distance), distance < 1.0, distance > 0.1) %>%
    arrange(Zeit_Datum) %>%
    mutate(
        time_diff = as.numeric(difftime(Zeit_Datum, lag(Zeit_Datum), units = "mins")),
        episode = cumsum(is.na(time_diff) | time_diff > 60)
    )

episodes <- ws_artifact %>%
    group_by(episode) %>%
    summarize(
        start = min(Zeit_Datum),
        end = max(Zeit_Datum),
        duration_min = as.numeric(difftime(max(Zeit_Datum), min(Zeit_Datum), units = "mins")),
        n_readings = n(),
        mean_dist = mean(distance),
        sd_dist = sd(distance),
        .groups = "drop"
    ) %>%
    filter(n_readings >= 3)

# Für jede Episode: DWD-Niederschlag + Events an anderen Sensoren
results <- list()

for (i in seq_len(nrow(episodes))) {
    ep <- episodes[i, ]

    # DWD Niederschlag (3h Vorlauf, 1h Nachlauf)
    pw <- precip %>%
        filter(timestamp >= (ep$start - hours(3)),
               timestamp <= (ep$end + hours(1)))
    total_precip <- sum(pw$precipitation_mm, na.rm = TRUE)
    max_intensity <- if (nrow(pw) > 0) max(pw$precipitation_mm, na.rm = TRUE) * 12 else 0

    # Nur DWD-Niederschlag während der Episode selbst (nicht Vorlauf)
    pw_during <- precip %>%
        filter(timestamp >= ep$start, timestamp <= ep$end)
    precip_during <- sum(pw_during$precipitation_mm, na.rm = TRUE)

    # Events an anderen Sensoren im gleichen Zeitfenster (±2h)
    other_events <- events_all %>%
        filter(!grepl("Wasserstra", station, ignore.case = TRUE)) %>%
        mutate(
            evt_start = as.POSIXct(start_time),
            evt_end = as.POSIXct(end_time)
        ) %>%
        filter(evt_start <= ep$end + hours(2), evt_end >= ep$start - hours(2))

    other_stations <- if (nrow(other_events) > 0) {
        paste(unique(gsub("_merged_export|_Springorum", "", other_events$station)), collapse = ", ")
    } else ""

    other_peak <- if (nrow(other_events) > 0) max(other_events$peak_level_cm, na.rm = TRUE) else 0
    other_types <- if (nrow(other_events) > 0) {
        paste(unique(other_events$event_type), collapse = ", ")
    } else ""

    # --- Klassifikation ---
    has_rain <- total_precip > 0.5
    has_strong_rain <- total_precip > 5 && max_intensity > 3
    has_other_events <- nrow(other_events) > 0

    # DWD-Warnstufen
    dwd_level <- case_when(
        max_intensity > 40 ~ 4,  # Violett: >40 mm/h
        max_intensity > 25 ~ 3,  # Rot: >25 mm/h
        max_intensity > 15 ~ 2,  # Gelb: >15 mm/h
        TRUE ~ 1                 # Unter Warnschwelle
    )

    # Ereignistyp basierend auf DWD + Quervergleich
    event_type <- case_when(
        !has_rain ~ NA_character_,  # Kein Regen → kein Ereignis
        has_strong_rain & has_other_events ~ case_when(
            max_intensity > 25 | (any(grepl("Sturzflut", other_events$event_type))) ~
                "Sturzflut-Ereignis (DWD+Quervergleich)",
            total_precip > 10 ~
                "Regenereignis (DWD+Quervergleich)",
            TRUE ~
                "Leichter Regen (DWD+Quervergleich)"
        ),
        has_strong_rain ~
            "Regenereignis (nur DWD-bestätigt)",
        has_other_events & has_rain ~
            "Leichter Regen (Quervergleich)",
        has_rain ~
            "Leichter Regen (nur DWD)",
        TRUE ~ NA_character_
    )

    results[[i]] <- tibble(
        start_time = ep$start,
        end_time = ep$end,
        duration_min = round(ep$duration_min, 1),
        source = "Artefakt-Rekonstruktion",
        total_precip_mm = round(total_precip, 2),
        precip_during_mm = round(precip_during, 2),
        max_intensity_mm_h = round(max_intensity, 1),
        dwd_warnstufe = dwd_level,
        other_sensor_events = nrow(other_events),
        other_stations = other_stations,
        other_peak_cm = round(other_peak, 2),
        other_event_types = other_types,
        event_type = event_type,
        is_real_event = !is.na(event_type),
        confidence = case_when(
            has_strong_rain & has_other_events ~ "hoch",
            has_strong_rain | has_other_events ~ "mittel",
            has_rain ~ "niedrig",
            TRUE ~ NA_character_
        )
    )
}

artifact_df <- bind_rows(results)

# Nur echte Ereignisse behalten
artifact_events <- artifact_df %>%
    filter(is_real_event) %>%
    arrange(start_time)

cat("B) Artefakt-Rekonstruktion:", nrow(artifact_events), "Ereignisse aus",
    nrow(episodes), "Artefakt-Episoden\n\n")

# --- C) Reguläre Events hinzufügen ---
# DWD-Mindest-Niederschlag: reguläre Events nur als "direkt" einstufen
# wenn DWD >= 0.5mm, sonst als "nicht DWD-bestätigt" markieren
MIN_DWD_PRECIP <- 0.5

regular_formatted <- ws_regular %>%
    transmute(
        start_time = as.POSIXct(start_time),
        end_time = as.POSIXct(end_time),
        duration_min = duration_min,
        source = "Reguläre Event-Detection",
        total_precip_mm = total_precip_mm,
        precip_during_mm = NA_real_,
        max_intensity_mm_h = max_intensity_mm_h,
        dwd_warnstufe = NA_integer_,
        other_sensor_events = NA_integer_,
        other_stations = NA_character_,
        other_peak_cm = NA_real_,
        other_event_types = NA_character_,
        event_type = ifelse(total_precip_mm >= MIN_DWD_PRECIP,
                            event_type,
                            paste0(event_type, " (NICHT DWD-bestätigt — nur ",
                                   round(total_precip_mm, 2), " mm)")),
        is_real_event = total_precip_mm >= MIN_DWD_PRECIP,
        confidence = ifelse(total_precip_mm >= MIN_DWD_PRECIP,
                            "direkt (Level-basiert)",
                            "verworfen (kein DWD-Niederschlag)")
    )

# Nicht-DWD-bestätigte reguläre Events melden
rejected_regular <- regular_formatted %>% filter(!is_real_event)
if (nrow(rejected_regular) > 0) {
    cat("C) ACHTUNG: Reguläre Events ohne DWD-Bestätigung (verworfen):\n")
    for (j in seq_len(nrow(rejected_regular))) {
        r <- rejected_regular[j, ]
        cat(sprintf("   %s bis %s — %s (DWD: nur %.2f mm)\n",
            format(r$start_time, "%d.%m.%Y %H:%M"),
            format(r$end_time, "%d.%m.%Y %H:%M"),
            ws_regular$event_type[j],
            r$total_precip_mm))
    }
    cat("\n")
}

# Zusammenführen und Duplikate entfernen
# (reguläre Events haben Vorrang, Artefakt-Events nur wenn kein reguläres im gleichen Zeitfenster)
combined <- bind_rows(regular_formatted, artifact_events)

# Duplikat-Check: Artefakt-Event entfernen wenn reguläres Event ±2h überlappt
if (nrow(regular_formatted) > 0 && nrow(artifact_events) > 0) {
    drop_idx <- c()
    for (j in seq_len(nrow(artifact_events))) {
        ae <- artifact_events[j, ]
        overlap <- regular_formatted %>%
            filter(start_time <= ae$end_time + hours(2),
                   end_time >= ae$start_time - hours(2))
        if (nrow(overlap) > 0) {
            # Finde den Index im combined df
            idx <- which(combined$start_time == ae$start_time &
                         combined$source == "Artefakt-Rekonstruktion")
            drop_idx <- c(drop_idx, idx)
        }
    }
    if (length(drop_idx) > 0) {
        combined <- combined[-drop_idx, ]
    }
}

combined <- combined %>% arrange(start_time)

# --- D) Ausgabe ---
cat("=== KOMBINIERTE EREIGNISLISTE: Wasserstraße ===\n\n")
cat(sprintf("%-20s %-20s %6s %7s %7s %5s %-10s %-35s\n",
    "Start", "Ende", "Dauer", "DWD_mm", "Max_I", "Konf.", "Quelle", "Ereignistyp"))
cat(paste(rep("-", 125), collapse = ""), "\n")

for (j in seq_len(nrow(combined))) {
    r <- combined[j, ]
    cat(sprintf("%-20s %-20s %5.0fm %6.1f %6.1f %-10s %-10s %s\n",
        format(r$start_time, "%d.%m.%Y %H:%M"),
        format(r$end_time, "%d.%m.%Y %H:%M"),
        r$duration_min,
        r$total_precip_mm,
        r$max_intensity_mm_h,
        ifelse(is.na(r$confidence), "—", r$confidence),
        ifelse(r$source == "Reguläre Event-Detection", "Level", "DWD+Quer"),
        r$event_type))
}

# --- E) Zusammenfassung ---
cat(sprintf("\n\nGesamt: %d Ereignisse\n", nrow(combined)))
cat(sprintf("  Davon reguläre Level-Events: %d\n",
            sum(combined$source == "Reguläre Event-Detection")))
cat(sprintf("  Davon rekonstruiert (DWD+Quervergleich): %d\n",
            sum(combined$source == "Artefakt-Rekonstruktion")))
cat("\nNach Konfidenz:\n")
print(combined %>% count(confidence, sort = TRUE))
cat("\nNach Ereignistyp:\n")
print(combined %>% count(event_type, sort = TRUE))

# --- F) Speichern ---
# CSV
write_csv(combined, "data/processed/wasserstrasse_combined_events.csv")

# Markdown
md <- c(
    "# Wasserstraße: Kombinierte Ereignisliste",
    "",
    sprintf("**Analysedatum:** %s", Sys.Date()),
    sprintf("**Zeitraum:** %s bis %s",
            format(min(combined$start_time), "%d.%m.%Y"),
            format(max(combined$end_time), "%d.%m.%Y")),
    "",
    "## Methode",
    "",
    "Da der Sensor an der Wasserstraße durch eine nicht-orthogonale Ausrichtung",
    "regelmäßig auf ein Sekundärziel bei ~0,82m Distance springt, liefert die",
    "reguläre Level-basierte Event-Detection kaum Ergebnisse. Diese kombinierte",
    "Analyse ergänzt die reguläre Detection um **rekonstruierte Ereignisse**:",
    "",
    "1. **Reguläre Event-Detection** (Level-basiert) — Schwellenwert auf bereinigten Level-Daten",
    "2. **Artefakt-Rekonstruktion** — Artefakt-Episoden werden mit DWD-RADOLAN-Niederschlag",
    "   und Events an anderen Sensoren (An der Kost, Königsallee, Wasserbaulabor) abgeglichen",
    "",
    "### Konfidenz-Stufen",
    "",
    "| Konfidenz | Kriterien |",
    "|---|---|",
    "| **hoch** | DWD > 5mm + Intensität > 3mm/h + parallele Events an anderen Sensoren |",
    "| **mittel** | Entweder starker DWD-Niederschlag ODER parallele Events |",
    "| **niedrig** | Nur schwacher DWD-Niederschlag (> 0,5mm), keine Querbestätigung |",
    "| **direkt** | Reguläre Level-basierte Detection (kein Artefakt) |",
    "",
    sprintf("## Ergebnis: %d Ereignisse", nrow(combined)),
    "",
    sprintf("- Reguläre Level-Events: **%d**", sum(combined$source == "Reguläre Event-Detection")),
    sprintf("- Rekonstruierte Events: **%d**", sum(combined$source == "Artefakt-Rekonstruktion")),
    sprintf("  - davon hoch: %d", sum(combined$confidence == "hoch", na.rm = TRUE)),
    sprintf("  - davon mittel: %d", sum(combined$confidence == "mittel", na.rm = TRUE)),
    sprintf("  - davon niedrig: %d", sum(combined$confidence == "niedrig", na.rm = TRUE)),
    ""
)

# Tabelle nach Konfidenz
for (conf in c("direkt (Level-basiert)", "hoch", "mittel", "niedrig")) {
    subset <- combined %>% filter(confidence == conf)
    if (nrow(subset) == 0) next

    label <- case_when(
        conf == "direkt (Level-basiert)" ~ "Reguläre Events (direkt erkannt)",
        conf == "hoch" ~ "Hohe Konfidenz (DWD + Quervergleich)",
        conf == "mittel" ~ "Mittlere Konfidenz",
        conf == "niedrig" ~ "Niedrige Konfidenz (nur schwacher DWD-Niederschlag)"
    )

    md <- c(md,
        sprintf("### %s (%d)", label, nrow(subset)),
        "",
        "| Start | Ende | Dauer | DWD (mm) | Max (mm/h) | Andere Sensoren | Ereignistyp |",
        "|---|---|---|---|---|---|---|"
    )

    for (j in seq_len(nrow(subset))) {
        r <- subset[j, ]
        other_info <- ifelse(is.na(r$other_stations) || nchar(r$other_stations) == 0,
                             "—", sprintf("%d (%s)", r$other_sensor_events, r$other_stations))
        md <- c(md, sprintf(
            "| %s | %s | %.0f min | %.1f | %.1f | %s | %s |",
            format(r$start_time, "%d.%m.%Y %H:%M"),
            format(r$end_time, "%d.%m.%Y %H:%M"),
            r$duration_min, r$total_precip_mm, r$max_intensity_mm_h,
            other_info, r$event_type
        ))
    }
    md <- c(md, "")
}

md <- c(md,
    "## Fazit",
    "",
    "Der Sensor an der Wasserstraße kann aufgrund der Fehlausrichtung keine direkten",
    "Wasserstandsmessungen liefern. Durch DWD-Niederschlagsdaten und Quervergleich mit",
    "anderen Sensoren lassen sich jedoch Niederschlagsereignisse rekonstruieren.",
    "Die Events mit hoher Konfidenz sind meteorologisch bestätigt und können für die",
    "Gesamtanalyse des Einzugsgebiets genutzt werden.",
    "",
    "**Empfehlung:** Sensor orthogonal ausrichten, danach reguläre Event-Detection nutzen.",
    "",
    "---",
    "*Analyse: scripts/analysis/wasserstrasse_combined_events.R*"
)

writeLines(md, "data/processed/wasserstrasse_combined_events.md")

cat("\n\nGespeichert:\n")
cat("  data/processed/wasserstrasse_combined_events.md\n")
cat("  data/processed/wasserstrasse_combined_events.csv\n")
