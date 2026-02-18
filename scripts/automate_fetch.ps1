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
    
    Write-Host "3/3: Generating plots..." -ForegroundColor Cyan
    & $RscriptPath "$RepoRoot\scripts\test_scripts\plot_sensor_data.R"
    
    Write-Host "Success! Data fetched, cleaned, and plotted." -ForegroundColor Green
}
else {
    Write-Warning "Error occurred while running the R script pipeline."
}
