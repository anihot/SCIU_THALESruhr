# PowerShell script to automate Nivus sensor data fetching

$RepoRoot = "c:\Users\Anika Hotzel\Documents\SCIU_THALESruhr"
$ScriptPath = "$RepoRoot\scripts\fetch_nivus_rest.R"

# Change directory to repo root so relative paths in R work correctly
Set-Location -Path $RepoRoot

Write-Host "Starting Nivus Data Kiosk Fetch... $(Get-Date)"

# Run R script
# Note: Ensure Rscript.exe is in PATH or provide full path
Rscript "$ScriptPath"

if ($LASTEXITCODE -eq 0) {
    Write-Host "Success! Data fetched and saved to data/sensor_exports/" -ForegroundColor Green
} else {
    Write-Warning "Error occurred while running the R script."
}
