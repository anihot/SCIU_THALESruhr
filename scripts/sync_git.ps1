# PowerShell script for automated Git synchronization

$RepoRoot = "c:\Users\Anika Hotzel\Documents\SCIU_THALESruhr"

# Change directory to repo root
Set-Location -Path $RepoRoot

Write-Host "--- Git Sync Started: $(Get-Date) ---"

# 1. Pull latest changes
Write-Host "Pulling from remote..."
git pull --rebase

# 2. Check for local changes
$status = git status --porcelain
if ($status) {
    Write-Host "Local changes detected. Committing and pushing..."
    git add .
    git commit -m "Auto-sync: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"
    git push
}
else {
    Write-Host "No local changes to push."
}

Write-Host "--- Git Sync Finished ---"
