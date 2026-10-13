$ErrorActionPreference = 'Stop'
Add-Type @'
using System;
using System.Runtime.InteropServices;
using System.Text;
public static class WinHugsUI {
    public delegate bool EnumWindowProc(IntPtr window, IntPtr data);
    [DllImport("user32.dll")] static extern bool EnumWindows(EnumWindowProc callback, IntPtr data);
    [DllImport("user32.dll")] static extern bool EnumChildWindows(IntPtr parent, EnumWindowProc callback, IntPtr data);
    [DllImport("user32.dll")] static extern uint GetWindowThreadProcessId(IntPtr window, out uint process);
    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    static extern int GetClassName(IntPtr window, StringBuilder name, int count);
    [DllImport("user32.dll")] public static extern IntPtr GetDlgItem(IntPtr window, int id);
    [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    static extern IntPtr SendMessageTimeout(IntPtr window, uint message, IntPtr wParam,
        StringBuilder text, uint flags, uint timeout, out IntPtr result);
    [DllImport("user32.dll", EntryPoint = "SendMessageW")]
    public static extern IntPtr SendValue(IntPtr window, uint message, IntPtr wParam, IntPtr lParam);
    public static string ReadText(IntPtr window) {
        var text = new StringBuilder(65536);
        IntPtr result;
        if (SendMessageTimeout(window, 0x000D, (IntPtr)text.Capacity, text, 2, 2000, out result) == IntPtr.Zero)
            return "<window did not respond to WM_GETTEXT>";
        return text.ToString();
    }
    public static IntPtr FindMainWindow(uint process) {
        IntPtr found = IntPtr.Zero;
        EnumWindows((window, data) => {
            uint owner;
            GetWindowThreadProcessId(window, out owner);
            if (owner == process && GetDlgItem(window, 1006) != IntPtr.Zero) {
                found = window;
                return false;
            }
            return true;
        }, IntPtr.Zero);
        return found;
    }
    static string DescribeWindow(IntPtr window) {
        var name = new StringBuilder(256);
        GetClassName(window, name, name.Capacity);
        return window + " [" + name + "] " + ReadText(window);
    }
    public static string Diagnostics(uint process) {
        var report = new StringBuilder();
        EnumWindows((window, data) => {
            uint owner;
            GetWindowThreadProcessId(window, out owner);
            if (owner == process) {
                report.AppendLine(DescribeWindow(window));
                EnumChildWindows(window, (child, unused) => {
                    report.AppendLine(DescribeWindow(child));
                    return true;
                }, IntPtr.Zero);
            }
            return true;
        }, IntPtr.Zero);
        return report.ToString();
    }
}
'@
$root = (Resolve-Path "$PSScriptRoot/../..").Path
$env:HUGSDIR = Join-Path $root 'hugsdir'
Remove-Item Env:HUGSFLAGS -ErrorAction SilentlyContinue
$app = Start-Process -FilePath "$root/src/winhugs/build/winhugs.exe" -WorkingDirectory $root -PassThru `
    -ArgumentList '-pCI_READY>', '-P{Hugs}/packages/*'
function Read-Transcript([IntPtr] $control) {
    return [WinHugsUI]::ReadText($control)
}
try {
    $deadline = (Get-Date).AddSeconds(60)
    $control = [IntPtr]::Zero
    do {
        if ($app.HasExited) { throw 'WinHugs exited before showing its prompt.' }
        $window = [WinHugsUI]::FindMainWindow($app.Id)
        $control = [WinHugsUI]::GetDlgItem($window, 1006) # ID_RTF
        if ($control -ne [IntPtr]::Zero) {
            $transcript = Read-Transcript $control
            if ($transcript -match 'ERROR') { throw $transcript }
            if ($transcript -match 'CI_READY>') { break }
        }
        Start-Sleep -Milliseconds 250
    } while ((Get-Date) -lt $deadline)
    if ($control -eq [IntPtr]::Zero -or $transcript -notmatch 'CI_READY>') {
        throw 'WinHugs did not load Prelude and show a prompt within 60 seconds.'
    }
    # Append a real command to the rich edit control, then invoke the Run menu.
    [void][WinHugsUI]::SendValue($control, 0x00B1, [IntPtr](-1), [IntPtr](-1)) # EM_SETSEL
    foreach ($character in ('"GUI_CI_OK:" ++ show (6 * 7 :: Int)').ToCharArray()) {
        [void][WinHugsUI]::SendValue($control, 0x0102, [IntPtr][int]$character, [IntPtr]::Zero) # WM_CHAR
    }
    [void][WinHugsUI]::SendValue($window, 0x0111, [IntPtr]301, [IntPtr]::Zero) # WM_COMMAND, ID_RUN
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
    if ($control -ne [IntPtr]::Zero) {
        Read-Transcript $control | Set-Content "$root/ci-logs/winhugs-transcript.txt"
    }
    [WinHugsUI]::Diagnostics($app.Id) | Set-Content "$root/ci-logs/winhugs-windows.txt"
    if (-not $app.HasExited) {
        [void]$app.CloseMainWindow()
        if (-not $app.WaitForExit(5000)) { $app.Kill() }
    }
}
