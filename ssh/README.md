# ssh

## keys

### Windows

On Windows, ssh is available natively and ssh-agent is maintained as a Windows service.
`ssh` knows to try for `ssh-agent` via a named pipe.

### WSL

Use [wsl-ssh-agent](https://github.com/rupor-github/wsl-ssh-agent) to expose the native ssh-agent to
ssh clients in WSL.

#### Setup

Download the latest release of wsl-ssh-agent [here](https://github.com/rupor-github/wsl-ssh-agent/releases).

Extract the contained `wsl-ssh-agent-gui.exe` to `%USERPROFILE%\bin`

Execute this in Powershell to configure the WSL environment and
auto-start on login:

```powershell
function Set-WSL-SSH-Env {
  $AuthSockPath = (Join-Path $Env:USERPROFILE ".ssh\auth_sock")
  # Environment variable to be shared with WSL.
  [System.Environment]::SetEnvironmentVariable("WSL_SSH_AUTH_SOCK", $AuthSockPath, [System.EnvironmentVariableTarget]::User)
  # Configure environment variable to be propagated to WSL sessions.
  # https://docs.microsoft.com/en-us/windows/wsl/interop#share-environment-variables-between-windows-and-wsl
  # u - share from Win32 to WSL
  # p - translate path between WSL/Linux format and Win32 format
  [System.Environment]::SetEnvironmentVariable("WSLENV", "WSL_SSH_AUTH_SOCK/up", [System.EnvironmentVariableTarget]::User)
}

function Install-WSL-SSH-Shortcut {
  $AppData = [Environment]::GetFolderPath("ApplicationData")
  $Target = (Join-Path $AppData "Microsoft\Windows\Start Menu\Programs\Startup\wsl-ssh-agent.lnk")

  $WScriptShell = New-Object -ComObject WScript.Shell
  $Shortcut = $WScriptShell.CreateShortcut($Target)
  $Shortcut.TargetPath = (Join-Path $Env:USERPROFILE "bin\wsl-ssh-agent-gui.exe")
  $Shortcut.Arguments = "--socket $Env:WSL_SSH_AUTH_SOCK"
  $Shortcut.Save()
}

Set-WSL-SSH-Env
Install-WSL-SSH-Shortcut
```

### Ubuntu

On native Ubuntu, run ssh-agent as a user-mode systemd service, like

```
[Unit]
Description=SSH Agent

[Service]
Type=simple
# %h - User-mode home directory
Environment=SSH_AUTH_SOCK=%h/.ssh/auth_sock
ExecStart=/usr/bin/ssh-agent -D -a "$SSH_AUTH_SOCK"
# In case ssh-agent was killed or the system restarted
ExecStartPre=/bin/rm -rf "$SSH_AUTH_SOCK"

[Install]
WantedBy=default.target
```

in `$HOME/.local/share/systemd/user/ssh-agent.service`, installed with

```
systemctl --user enable ssh-agent && systemctl --user start ssh-agent
```

so `SSH_AUTH_SOCK` is just set to `$HOME/.ssh/auth_sock`.

### Linux (remote)

1. SSH agent forwarding is configured
2. For interactive sessions, tmux is used almost exclusively on the remote side,
   so having a single location as the authoritative agent socket is important,
   since existing shell sessions can't be refreshed.
3. Rarely, if ever, do I have more than one active connection that has agent
   forwarding enabled. If I connect again it's OK to take the last connection's
   `SSH_AUTH_SOCK`. In case that changes it would be nice to switch easily.

## Testing

```
Given we are running in Ubuntu (with KDE)
And the ssh-agent has been set up using the instructions above
And dotfiles have been installed
And the computer has been restarted
When apps are started with KRunner
Then they will have SSH_AUTH_SOCK set in their environment
And SSH_AUTH_SOCK will be set to $HOME/.ssh/auth_sock

Given we are running in Ubuntu (with KDE)
And the ssh-agent has been set up using the instructions above
And dotfiles have been installed
When alacritty is started via KRunner
Then the resulting shell will have SSH_AUTH_SOCK set to $HOME/.ssh/auth_sock (resolved)
And sh -c 'echo $SSH_AUTH_SOCK' will output $HOME/.ssh/auth_sock (resolved)
```
