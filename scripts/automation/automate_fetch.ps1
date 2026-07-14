# PowerShell script to automate Nivus sensor data fetching and processing

$RepoRoot = "c:\Users\Anika Hotzel\Documents\SCIU_THALESruhr"

# Change directory to repo root so relative paths in R work correctly
Set-Location -Path $RepoRoot

Write-Host "🚀 Starting SCIU THALESruhr Pipeline... $(Get-Date)" -ForegroundColor Cyan

$RscriptPath = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

# 1. Fetching Data
Write-Host "1/7: Fetching Nivus data..." -ForegroundColor Yellow
& $RscriptPath "scripts/api/fetch_nivus.R"

Write-Host "2/7: Fetching RADOLAN precipitation..." -ForegroundColor Yellow
& $RscriptPath "scripts/api/fetch_radolan.R"

# 2. Analysis
Write-Host "3/7: Cleaning and analyzing sensor levels..." -ForegroundColor Yellow
& $RscriptPath "scripts/analysis/level_analysis.R"

Write-Host "4/7: Detecting events..." -ForegroundColor Yellow
& $RscriptPath "scripts/analysis/event_detection.R"

# 3. Visualization
Write-Host "5/7: Generating 24h plots (for popups)..." -ForegroundColor Yellow
& $RscriptPath "scripts/visualization/plot_sensors.R"

Write-Host "6/7: Generating static map..." -ForegroundColor Yellow
& $RscriptPath "scripts/visualization/generate_static_map.R"

Write-Host "7/8: Generating interactive map..." -ForegroundColor Yellow
& $RscriptPath "scripts/visualization/generate_interactive_map.R"

Write-Host "8/8: Generating full timeseries plots..." -ForegroundColor Yellow
& $RscriptPath "scripts/visualization/plot_full_timeseries.R"

# 4. Schillerschule comparison (local only)
Write-Host "9/9: Schillerschule rain gauge comparison..." -ForegroundColor Yellow
& $RscriptPath "scripts/analysis/schillerschule_comparison.R"

# 5. Final Updates
Write-Host "Finalizing: Updating README with latest data..." -ForegroundColor Cyan
& $RscriptPath "scripts/automation/update_readme.R"

Write-Host "✅ Pipeline complete! All maps and plots updated." -ForegroundColor Green
