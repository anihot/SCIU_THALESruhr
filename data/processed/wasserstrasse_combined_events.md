# Wasserstraße: Kombinierte Ereignisliste

**Analysedatum:** 2026-06-09
**Zeitraum:** 21.09.2025 bis 06.06.2026

## Methode

Da der Sensor an der Wasserstraße durch eine nicht-orthogonale Ausrichtung
regelmäßig auf ein Sekundärziel bei ~0,82m Distance springt, liefert die
reguläre Level-basierte Event-Detection kaum Ergebnisse. Diese kombinierte
Analyse ergänzt die reguläre Detection um **rekonstruierte Ereignisse**:

1. **Reguläre Event-Detection** (Level-basiert) — Schwellenwert auf bereinigten Level-Daten
2. **Artefakt-Rekonstruktion** — Artefakt-Episoden werden mit DWD-RADOLAN-Niederschlag
   und Events an anderen Sensoren (An der Kost, Königsallee, Wasserbaulabor) abgeglichen

### Konfidenz-Stufen

| Konfidenz | Kriterien |
|---|---|
| **hoch** | DWD > 5mm + Intensität > 3mm/h + parallele Events an anderen Sensoren |
| **mittel** | Entweder starker DWD-Niederschlag ODER parallele Events |
| **niedrig** | Nur schwacher DWD-Niederschlag (> 0,5mm), keine Querbestätigung |
| **direkt** | Reguläre Level-basierte Detection (kein Artefakt) |

## Ergebnis: 57 Ereignisse

- Reguläre Level-Events: **1**
- Rekonstruierte Events: **56**
  - davon hoch: 13
  - davon mittel: 23
  - davon niedrig: 20

### Reguläre Events (direkt erkannt) (1)

| Start | Ende | Dauer | DWD (mm) | Max (mm/h) | Andere Sensoren | Ereignistyp |
|---|---|---|---|---|---|---|
| 06.06.2026 16:56 | 06.06.2026 21:48 | 292 min | 0.0 | 0.1 | — | Sturzflut-Ereignis |

### Hohe Konfidenz (DWD + Quervergleich) (13)

| Start | Ende | Dauer | DWD (mm) | Max (mm/h) | Andere Sensoren | Ereignistyp |
|---|---|---|---|---|---|---|
| 03.10.2025 21:07 | 04.10.2025 11:56 | 889 min | 9.1 | 9.7 | 3 (An_der_Kost, Königsallee) | Leichter Regen (DWD+Quervergleich) |
| 15.11.2025 00:22 | 20.11.2025 22:28 | 8526 min | 8.2 | 6.6 | 4 (An_der_Kost) | Leichter Regen (DWD+Quervergleich) |
| 02.12.2025 03:24 | 02.12.2025 16:03 | 759 min | 9.0 | 4.9 | 1 (An_der_Kost) | Leichter Regen (DWD+Quervergleich) |
| 06.12.2025 03:28 | 09.12.2025 10:42 | 4754 min | 10.9 | 5.8 | 2 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 15.02.2026 18:50 | 18.02.2026 15:35 | 4125 min | 15.6 | 7.4 | 1 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 19.02.2026 11:09 | 25.02.2026 11:55 | 8686 min | 16.8 | 7.0 | 4 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 13.03.2026 10:14 | 15.03.2026 13:21 | 3067 min | 14.5 | 3.6 | 3 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 29.03.2026 21:46 | 31.03.2026 09:41 | 2154 min | 5.9 | 7.1 | 2 (An_der_Kost) | Leichter Regen (DWD+Quervergleich) |
| 18.04.2026 14:24 | 19.04.2026 16:48 | 1584 min | 89.1 | 19.4 | 1 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 05.05.2026 19:30 | 07.05.2026 11:13 | 2384 min | 79.3 | 12.4 | 1 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 11.05.2026 00:07 | 11.05.2026 15:10 | 903 min | 104.3 | 9.1 | 1 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 13.05.2026 12:22 | 15.05.2026 09:40 | 2718 min | 123.7 | 8.4 | 1 (An_der_Kost) | Regenereignis (DWD+Quervergleich) |
| 31.05.2026 04:50 | 31.05.2026 10:23 | 332 min | 9.1 | 3.6 | 1 (An_der_Kost) | Leichter Regen (DWD+Quervergleich) |

### Mittlere Konfidenz (23)

| Start | Ende | Dauer | DWD (mm) | Max (mm/h) | Andere Sensoren | Ereignistyp |
|---|---|---|---|---|---|---|
| 23.10.2025 01:22 | 23.10.2025 08:38 | 436 min | 4.0 | 9.1 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 23.10.2025 14:00 | 24.10.2025 04:23 | 863 min | 1.3 | 1.9 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 27.10.2025 00:18 | 29.10.2025 14:05 | 3707 min | 1.4 | 1.8 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 29.10.2025 21:43 | 30.10.2025 11:03 | 800 min | 5.2 | 8.2 | — | Regenereignis (nur DWD-bestätigt) |
| 01.11.2025 10:43 | 02.11.2025 10:44 | 1441 min | 2.7 | 8.4 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 28.11.2025 06:04 | 29.11.2025 12:56 | 1852 min | 5.8 | 3.6 | — | Regenereignis (nur DWD-bestätigt) |
| 08.01.2026 09:31 | 10.01.2026 06:17 | 2686 min | 5.6 | 1.9 | 2 (An_der_Kost, Königsallee) | Leichter Regen (Quervergleich) |
| 12.01.2026 07:52 | 16.01.2026 03:18 | 5486 min | 1.4 | 1.8 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 24.01.2026 03:11 | 25.01.2026 15:28 | 2177 min | 0.6 | 0.8 | 2 (Königsallee) | Leichter Regen (Quervergleich) |
| 26.01.2026 12:56 | 29.01.2026 10:22 | 4166 min | 6.6 | 2.6 | 1 (Königsallee) | Leichter Regen (Quervergleich) |
| 03.02.2026 11:22 | 04.02.2026 20:03 | 1961 min | 2.0 | 2.0 | 1 (Königsallee) | Leichter Regen (Quervergleich) |
| 10.02.2026 23:03 | 13.02.2026 15:30 | 3867 min | 7.3 | 5.3 | — | Regenereignis (nur DWD-bestätigt) |
| 11.03.2026 11:57 | 12.03.2026 10:09 | 1332 min | 3.8 | 4.2 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 25.03.2026 10:06 | 27.03.2026 09:57 | 2871 min | 5.6 | 4.3 | — | Regenereignis (nur DWD-bestätigt) |
| 28.03.2026 00:31 | 29.03.2026 11:48 | 2117 min | 8.5 | 4.8 | — | Regenereignis (nur DWD-bestätigt) |
| 11.04.2026 18:44 | 12.04.2026 10:32 | 948 min | 2.4 | 5.3 | 1 (An_der_Kost) | Leichter Regen (Quervergleich) |
| 18.05.2026 04:37 | 18.05.2026 11:37 | 420 min | 19.1 | 6.6 | — | Regenereignis (nur DWD-bestätigt) |
| 18.05.2026 18:25 | 19.05.2026 09:39 | 914 min | 5.4 | 10.7 | — | Regenereignis (nur DWD-bestätigt) |
| 19.05.2026 20:01 | 20.05.2026 13:29 | 1048 min | 20.4 | 4.3 | — | Regenereignis (nur DWD-bestätigt) |
| 29.05.2026 17:54 | 30.05.2026 05:56 | 722 min | 20.8 | 9.0 | — | Regenereignis (nur DWD-bestätigt) |
| 02.06.2026 10:38 | 03.06.2026 08:08 | 1290 min | 30.8 | 5.8 | — | Regenereignis (nur DWD-bestätigt) |
| 04.06.2026 14:28 | 04.06.2026 23:44 | 556 min | 43.8 | 67.6 | — | Regenereignis (nur DWD-bestätigt) |
| 05.06.2026 15:29 | 06.06.2026 07:59 | 990 min | 5.8 | 3.4 | — | Regenereignis (nur DWD-bestätigt) |

### Niedrige Konfidenz (nur schwacher DWD-Niederschlag) (20)

| Start | Ende | Dauer | DWD (mm) | Max (mm/h) | Andere Sensoren | Ereignistyp |
|---|---|---|---|---|---|---|
| 21.09.2025 01:33 | 21.09.2025 04:12 | 159 min | 1.0 | 3.7 | — | Leichter Regen (nur DWD) |
| 19.10.2025 21:01 | 19.10.2025 23:19 | 138 min | 0.8 | 1.3 | — | Leichter Regen (nur DWD) |
| 25.10.2025 06:14 | 26.10.2025 16:15 | 2041 min | 2.2 | 7.3 | — | Leichter Regen (nur DWD) |
| 02.11.2025 16:08 | 02.11.2025 21:55 | 347 min | 3.1 | 7.4 | — | Leichter Regen (nur DWD) |
| 09.11.2025 03:14 | 09.11.2025 03:16 | 2 min | 1.9 | 9.2 | — | Leichter Regen (nur DWD) |
| 09.11.2025 05:22 | 09.11.2025 21:05 | 943 min | 2.6 | 9.2 | — | Leichter Regen (nur DWD) |
| 10.11.2025 21:29 | 11.11.2025 09:54 | 745 min | 1.3 | 8.4 | — | Leichter Regen (nur DWD) |
| 24.11.2025 13:28 | 27.11.2025 10:27 | 4139 min | 2.5 | 6.7 | — | Leichter Regen (nur DWD) |
| 02.12.2025 17:50 | 02.12.2025 18:36 | 46 min | 1.8 | 1.6 | — | Leichter Regen (nur DWD) |
| 02.12.2025 23:00 | 03.12.2025 07:35 | 515 min | 1.0 | 2.0 | — | Leichter Regen (nur DWD) |
| 03.12.2025 22:13 | 04.12.2025 11:31 | 798 min | 1.0 | 1.2 | — | Leichter Regen (nur DWD) |
| 28.12.2025 15:28 | 03.01.2026 04:42 | 7994 min | 0.8 | 1.2 | — | Leichter Regen (nur DWD) |
| 19.02.2026 02:55 | 19.02.2026 04:47 | 112 min | 1.2 | 1.3 | — | Leichter Regen (nur DWD) |
| 10.03.2026 16:50 | 11.03.2026 04:17 | 686 min | 0.7 | 4.8 | — | Leichter Regen (nur DWD) |
| 11.03.2026 06:13 | 11.03.2026 06:18 | 6 min | 0.7 | 4.8 | — | Leichter Regen (nur DWD) |
| 16.03.2026 00:27 | 16.03.2026 12:57 | 750 min | 1.7 | 3.6 | — | Leichter Regen (nur DWD) |
| 13.04.2026 11:41 | 14.04.2026 09:32 | 1312 min | 8.1 | 0.6 | — | Leichter Regen (nur DWD) |
| 04.05.2026 16:58 | 05.05.2026 14:00 | 1262 min | 14.7 | 1.7 | — | Leichter Regen (nur DWD) |
| 13.05.2026 02:00 | 13.05.2026 08:47 | 406 min | 0.8 | 0.5 | — | Leichter Regen (nur DWD) |
| 15.05.2026 14:38 | 16.05.2026 11:10 | 1232 min | 1.7 | 2.8 | — | Leichter Regen (nur DWD) |

## Fazit

Der Sensor an der Wasserstraße kann aufgrund der Fehlausrichtung keine direkten
Wasserstandsmessungen liefern. Durch DWD-Niederschlagsdaten und Quervergleich mit
anderen Sensoren lassen sich jedoch Niederschlagsereignisse rekonstruieren.
Die Events mit hoher Konfidenz sind meteorologisch bestätigt und können für die
Gesamtanalyse des Einzugsgebiets genutzt werden.

**Empfehlung:** Sensor orthogonal ausrichten, danach reguläre Event-Detection nutzen.

---
*Analyse: scripts/analysis/wasserstrasse_combined_events.R*
