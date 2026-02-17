# SCIU THALESruhr - Sensor Data Management

This repository manages the automated collection, storage and analysis of sensor data from the Nivus DataKiosk for the SCIU project.

## Project Structure

- **`.github/workflows/`**: Contains the GitHub Actions workflow for automated data fetching.
- **`data/sensor_exports/`**: Contains the sensor data exported as CSV files, organized by station.
- **`scripts/`**:
    - `fetch_nivus_rest.R`: The core R script that fetches data from the Nivus REST API.
    - `automate_fetch.ps1`: A PowerShell script for running the fetch process locally on Windows.

## Automated Data Fetching

The data is automatically updated every day at **08:00 UTC** via GitHub Actions. The workflow:
1. Sets up an R environment.
2. Installs necessary packages (`httr`, `jsonlite`, `lubridate`, `dplyr`, `purrr`, `tidyr`).
3. Executes the fetch script using the stored API key.
4. Commits and pushes any new data back to this repository.

### Configuration (One-Time Setup)

To enable the automated fetch in a new environment or repository:
1. Add the Nivus API Key as a secret in GitHub:
   - Go to **Settings** > **Secrets and variables** > **Actions**.
   - Create a new repository secret named `NIVUS_API_KEY`.
   - Set the value to your Nivus REST API key.

## Local Usage

To run the data fetching script locally:
1. Ensure R is installed and available in your PATH.
2. Run the script from the root of the repository:
   ```bash
   Rscript scripts/fetch_nivus_rest.R
   ```
   *Note: The script has a fallback API key for local testing, but it is recommended to set the `NIVUS_API_KEY` environment variable.*

## Data Details

The sensor files in `data/sensor_exports/` are merged CSVs containing timestamps and sensor values for various channels at each station.
