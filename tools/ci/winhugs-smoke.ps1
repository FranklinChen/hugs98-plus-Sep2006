$ErrorActionPreference = 'Stop'
Add-Type @'
using System;
using System.Runtime.InteropServices;
using System.Text;
public static class WinHugsUI {
    [DllImport("user32.dll")] public static extern IntPtr GetDlgItem(IntPtr window, int id);
    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern IntPtr SendMessage(IntPtr window, uint message, IntPtr wParam, StringBuilder text);
    [DllImport("user32.dll", EntryPoint = "SendMessageW")]
    public static extern IntPtr SendValue(IntPtr window, uint message, IntPtr wParam, IntPtr lParam);
}
'@
$root = (Resolve-Path "$PSScriptRoot/../..").Path
$env:HUGSDIR = Join-Path $root 'hugsdir'
Remove-Item Env:HUGSFLAGS -ErrorAction SilentlyContinue
$app = Start-Process -FilePath "$root/src/winhugs/build/winhugs.exe" -WorkingDirectory $root -PassThru
function Read-Transcript([IntPtr] $control) {
    $text = [System.Text.StringBuilder]::new(65536)
    [void][WinHugsUI]::SendMessage($control, 0x000D, [IntPtr]65536, $text)
    return $text.ToString()
}
try {
    $deadline = (Get-Date).AddSeconds(60)
    $control = [IntPtr]::Zero
    do {
        if ($app.HasExited) { throw 'WinHugs exited before showing its prompt.' }
        $app.Refresh()
        $control = [WinHugsUI]::GetDlgItem($app.MainWindowHandle, 1006) # ID_RTF
        if ($control -ne [IntPtr]::Zero) {
            $transcript = Read-Transcript $control
            if ($transcript -match 'ERROR') { throw $transcript }
            if ($transcript -match 'Prelude>') { break }
        }
        Start-Sleep -Milliseconds 250
    } while ((Get-Date) -lt $deadline)
    if ($control -eq [IntPtr]::Zero -or $transcript -notmatch 'Prelude>') {
        throw 'WinHugs did not load Prelude and show a prompt within 60 seconds.'
    }
    # Append a real command to the rich edit control, then invoke the Run menu.
    [void][WinHugsUI]::SendValue($control, 0x00B1, [IntPtr](-1), [IntPtr](-1)) # EM_SETSEL
    foreach ($character in ('"GUI_CI_OK:" ++ show (6 * 7 :: Int)').ToCharArray()) {
        [void][WinHugsUI]::SendValue($control, 0x0102, [IntPtr][int]$character, [IntPtr]::Zero) # WM_CHAR
    }
    [void][WinHugsUI]::SendValue($app.MainWindowHandle, 0x0111, [IntPtr]301, [IntPtr]::Zero) # WM_COMMAND, ID_RUN
    $deadline = (Get-Date).AddSeconds(30)
    do {
        if ($app.HasExited) { throw 'WinHugs exited during evaluation.' }
        $transcript = Read-Transcript $control
        if ($transcript -match 'ERROR') { throw $transcript }
        if ($transcript -match 'GUI_CI_OK:42') {
            $transcript | Set-Content "$root/ci-logs/winhugs-transcript.txt"
            Write-Output 'WinHugs loaded Prelude and evaluated a GUI command.'
            return
        }
        Start-Sleep -Milliseconds 250
    } while ((Get-Date) -lt $deadline)
    throw "WinHugs did not evaluate the GUI command within 30 seconds.`n$transcript"
} finally {
    if (-not $app.HasExited) {
        [void]$app.CloseMainWindow()
        if (-not $app.WaitForExit(5000)) { $app.Kill() }
    }
}
