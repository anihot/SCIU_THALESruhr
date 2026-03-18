# SCIU THALESruhr - Sensor Data Management

<table>
<tr>
<td width="60%" valign="top">

<!-- LATEST_EVENTS_START -->
### ☀️ Wetterausblick (48h)
**Kein nennenswerter Regen vorhergesagt.**

*(Zeitraum: 18.03. 09:00 bis 20.03. 08:00)*

- Temperatur: **8.6°C** (Min: 4.7°C | Max: 16°C)
- Summe: **0 mm** | Max: **0 mm/h**
- Max. 6h: **0 mm**

*Quelle: Open-Meteo (DWD)*



### 🛠 Sensor-Status-Check

⚠️ **Achtung: Inaktive Sensoren erkannt!**
*(Hinweis: Dies kann auch durch einen Fehler im automatisierten Download-Prozess verursacht werden)*

- 🔴 **Wasserbaulabor_**: Letzte Daten vor 460.3 Stunden (18.02. 09:22)

### Lag-Analyse: Reaktionszeiten der Sensoren
*Onset-Lag = Zeit zwischen erstem Regen (≥ 0,5 mm/h) und erstem Schwellenübertritt am Sensor*

Über alle Stationen: **Median 139 min** | n = 409 Ereignisse

|station                 | Ereignisse (n)| Median-Lag (min)| Min (min)| Max (min)|
|:-----------------------|--------------:|----------------:|---------:|---------:|
|An_der_Kost             |              2|               93|        90|        96|
|Königsallee_Springorum  |              7|              126|        57|       174|
|Wasserstraße_Springorum |            112|              139|       -37|       179|
|Herzogstraße            |            288|              140|       -46|       180|

*Quelle: Open-Meteo Archive (stündlich, ±30 min Messungenauigkeit)*



## 🔔 Aktuelle Ereignisse (Letzte 24-30h)
*Stand: 2026-03-18 07:08:33 UTC*

Es wurden **6** neue potenzielle Ereignisse erkannt.

|station                 |start_time          | peak_level_cm| duration_min|
|:-----------------------|:-------------------|-------------:|------------:|
|Wasserstraße_Springorum |2026-03-18 05:07:30 |           4.9|            0|
|Herzogstraße            |2026-03-17 15:50:00 |          20.6|          711|
|Herzogstraße            |2026-03-17 13:28:00 |           5.7|           24|
|Herzogstraße            |2026-03-17 12:21:00 |          12.2|           26|
|Wasserstraße_Springorum |2026-03-17 10:47:30 |           4.4|            0|
|Herzogstraße            |2026-03-17 09:49:00 |           3.7|           54|

### 📈 Aktuelle Plots der betroffenen Stationen
#### Station: Wasserstraße_Springorum
![Plot Wasserstraße_Springorum](data/output/plots/Wasserstraße_Springorum_merged_export_cleaned.png)

#### Station: Herzogstraße
![Plot Herzogstraße](data/output/plots/Herzogstraße_merged_export_cleaned.png)


---

<!-- LATEST_EVENTS_END -->

</td>
<td width="40%" valign="top">

<p align="center"><b>📍 Sensor-Netzwerk</b></p>

<img src="data/output/sensor_static_map.png" alt="Static Sensor Map" width="100%">

<p align="center">🔗 <b><a href="https://htmlpreview.github.io/?https://github.com/anihot/SCIU_THALESruhr/blob/main/data/output/sensor_map.html" target="_blank">Interaktive Karte anzeigen</a></b><br>(mit 24h Sensor-Daten Plots)</p>

</td>
</tr>
</table>

---

This repository manages the automated collection, processing, and analysis of sensor data from the Nivus DataKiosk for the SCIU project.

## 🛠 Project Structure

- **`.github/workflows/`**: GitHub Actions for daily automated data processing.
- **`data/`**:
    - `raw/`: Raw CSV data fetched directly from the Nivus API.
    - `processed/`: Processed data where levels are calculated and filtered.
    - `output/`: Final results like plots, maps, and summaries.
    - `metadata/`: Sensor coordinates and station configurations.
- **`scripts/`**:
    - `api/`: Scripts for fetching data from Nivus and RADOLAN.
    - `analysis/`: Data cleaning, level calculation, and event detection.
    - `visualization/`: Generation of plots and maps.
    - `automation/`: Tools for README updates and local automation.

## 🤖 Automated Pipeline (GitHub Actions)

The data is automatically processed every day at **08:00 UTC**. The workflow:
1. **Fetch**: Downloads new data points for all monitored stations.
2. **Clean**: Filters noise and calculates "level" from distance readings.
3. **Plot**: Updates visual charts with the latest data.
4. **Detect**: Scans for new events above the defined threshold.
5. **Sync**: Commits all updates back to the repository.

### Setup (Secrets)
To enable the automated fetch, ensure the `NIVUS_API_KEY` is set as a repository secret in GitHub.

## 💻 Local Usage & Automation

### Running the Full Pipeline
You can trigger the entire fetch-and-analyze process manually on Windows:
```powershell
.\scripts\automation\automate_fetch.ps1
```

### Automated Local Sync
To keep your local computer in sync with GitHub changes automatically (e.g., hourly), use the `sync_git.ps1` script with the **Windows Task Scheduler**.

1. Create a task in Task Scheduler.
2. Trigger: Daily, repeating every 1 hour.
3. Action: Start a program `powershell.exe`.
4. Arguments: `-ExecutionPolicy Bypass -File "C:\Path\To\Repo\scripts\sync_git.ps1"`

## 📊 Data Details

- **Levels**: Calculated as `Distance_to_Sensor - Reading`.
- **Filtering**: Automatic removal of spikes and non-water-related noise.
- **Event Detection**: Logged in `detected_events.csv` when levels exceed the specified threshold.
