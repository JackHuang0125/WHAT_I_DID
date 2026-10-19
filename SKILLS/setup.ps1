# 在新電腦上設定 Claude Skills 同步
# 用法：先 clone 本 repo，然後在 PowerShell 執行：
#   powershell -ExecutionPolicy Bypass -File <repo>\SKILLS\setup.ps1
#
# 作用：把 %USERPROFILE%\.claude\skills 建成指向本資料夾的 Junction，
#       之後只要 git pull，Claude Code 就會讀到最新的 Skills。

$skillsSrc = $PSScriptRoot
$claudeDir = Join-Path $env:USERPROFILE ".claude"
$target    = Join-Path $claudeDir "skills"

if (-not (Test-Path $claudeDir)) {
    New-Item -ItemType Directory -Path $claudeDir | Out-Null
}

if (Test-Path $target) {
    $item = Get-Item $target -Force
    if ($item.LinkType -eq "Junction" -or $item.LinkType -eq "SymbolicLink") {
        Write-Host "已存在連結：$target -> $($item.Target)"
        return
    }
    # 已有實體資料夾：先備份，避免覆蓋既有 Skills
    $backup = "$target.backup-$(Get-Date -Format yyyyMMddHHmmss)"
    Rename-Item $target $backup
    Write-Host "原本的 skills 資料夾已備份到：$backup（請手動把裡面的 Skill 搬進 $skillsSrc）"
}

New-Item -ItemType Junction -Path $target -Target $skillsSrc | Out-Null
Write-Host "完成：$target -> $skillsSrc"
