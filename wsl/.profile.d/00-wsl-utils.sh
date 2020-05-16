# Set IS_WSL=1 if on WSL, IS_WSL= otherwise.
read _wsl_osrelease </proc/sys/kernel/osrelease
if [ -z "${_wsl_osrelease%%*-Microsoft}" ]; then
    IS_WSL=1
else
    IS_WSL=
fi
