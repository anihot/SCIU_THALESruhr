# SCIU THALESruhr - Sensor Data Management

**[Live-Dashboard & Interaktive Karte](https://anihot.github.io/SCIU_THALESruhr/)**

<table>
<tr>
<td width="60%" valign="top">

<!-- LATEST_EVENTS_START -->
### ☀️ Wetterausblick (48h)
**Kein nennenswerter Regen vorhergesagt.**

*(Zeitraum: 23.06. 05:00 bis 25.06. 04:00)*

- Temperatur: **19.6°C** (Min: 19.5°C | Max: 37.6°C)
- Summe: **0 mm** | Max: **0 mm/h**
- Max. 6h: **0 mm**

*Quelle: Open-Meteo (DWD)*



### 🛠 Sensor-Status-Check

✅ Alle Sensoren senden planmäßig Daten (letzte 24h).


## ✅ Keine neuen Ereignisse in den letzten 24h
*Stand: 2026-06-23 02:59:07 UTC*

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
