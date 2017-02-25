# vi-mode available in Windows 10 by default.
if ($host.name -eq 'ConsoleHost')
{
  Import-Module PSReadline
  Set-PSReadlineOption -EditMode Vi
}
