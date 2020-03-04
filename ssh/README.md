# ssh

## keys

### Windows

On Windows, ssh is available natively and maintained as a Windows service.

### WSL

Use [wsl-ssh-agent](https://github.com/rupor-github/wsl-ssh-agent) to expose the native ssh-agent to
ssh clients in WSL.

#### Setup

Download the latest release of wsl-ssh-agent [here](https://github.com/rupor-github/wsl-ssh-agent/releases).

Extract the contained `wsl-ssh-agent-gui.exe` to `%USERPROFILE%\bin`

Execute this in Powershell to configure the WSL environment and
auto-start on login:

```powershell
# Keep track of the location of our SSH agent socket file in environment
# variables, which are common to our shortcut and WSL sessions
$AuthSockPath = (Join-Path "%USERPROFILE%" .ssh auth_sock)
[System.Environment]::SetEnvironmentVariable("WSL_SSH_AUTH_SOCK", $AuthSockPath, [System.EnvironmentVariableTarget]::User)
# https://docs.microsoft.com/en-us/windows/wsl/interop#share-environment-variables-between-windows-and-wsl
# u - share from Win32 to WSL
# p - translate path between WSL/Linux format and Win32 format
[System.Environment]::SetEnvironmentVariable("WSLENV", "WSL_SSH_AUTH_SOCK/up", [System.EnvironmentVariableTarget]::User)

# Create shortcut so the utility is started on login
$AppData = [Environment]::GetFolderPath("ApplicationData")
$Target = (Join-Path $AppData Microsoft Windows "Start Menu" Programs Startup wsl-ssh-agent.lnk)

$WScriptShell = New-Object -ComObject WScript.Shell
$Shortcut = $WScriptShell.CreateShortcut($Target)
$Shortcut.TargetPath = (Join-Path "%USERPROFILE%" bin wsl-ssh-agent-gui.exe)
# TODO: Fix this, because environment variables in shortcuts aren't expanded
# recursively.
$Shortcut.Arguments = "--socket %WSL_SSH_AUTH_SOCK%"
$Shortcut.Save()
```
