# PowerShell script to automate Nivus sensor data fetching

$RepoRoot = "c:\Users\Anika Hotzel\Documents\SCIU_THALESruhr"
$ScriptPath = "$RepoRoot\scripts\fetch_nivus_rest.R"

# Change directory to repo root so relative paths in R work correctly
Set-Location -Path $RepoRoot

Write-Host "Starting Nivus Data Kiosk Fetch... $(Get-Date)"

$RscriptPath = "C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

# Run R scripts
Write-Host "1/3: Fetching data..."
& $RscriptPath "$ScriptPath"

if ($LASTEXITCODE -eq 0) {
    Write-Host "2/3: Cleaning and analyzing data..." -ForegroundColor Cyan
    & $RscriptPath "$RepoRoot\scripts\test_scripts\sensor_level_analysis.R"
    
    Write-Host "3/4: Generating plots..." -ForegroundColor Cyan
    & $RscriptPath "$RepoRoot\scripts\test_scripts\plot_sensor_data.R"
    
    Write-Host "4/5: Running event detection..." -ForegroundColor Cyan
    & $RscriptPath "$RepoRoot\scripts\test_scripts\event_detection.R"
    
    Write-Host "5/5: Generating static map..." -ForegroundColor Cyan
    & $RscriptPath "$RepoRoot\scripts\test_scripts\generate_static_map.R"
    
    Write-Host "Success! Data fetched, cleaned, plotted, events detected and map updated." -ForegroundColor Green
}
else {
    Write-Warning "Error occurred while running the R script pipeline."
}
