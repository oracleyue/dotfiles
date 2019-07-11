#!/bin/sh

# From larbs's ".profile"
export PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export EDITOR="vim"
export TERMINAL="st"
export BROWSER="chromium"
export READER="zathura"
export FILE="vifm"
export SUDO_ASKPASS="$HOME/.local/bin/tools/dmenupass"
export WMEmacs="i3"

# From larbs's ".xprofile"
setbg &		# Set the background
sxhkd &	        # Bind keys
xset r rate 300 50 &	# Speed xrate up
unclutter &		# Remove mouse when idle
xcompmgr &		# xcompmgr for transparency
dunst &			# dunst for notifications

# Source Xprofile
# [ -f "$HOME/.xprofile" ] && . ~/.xprofile

xmodmap ~/.Xmodmap
xrdb ~/.Xresources

# Gnome keyring
dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Auto-start i3
exec i3
# Manually start i3 in TTY: startx /usr/bin/i3
