#!/bin/sh
# For i3, used by "startx"

export PATH=$PATH:$HOME/bin/i3cmds
export EDITOR="vim"
export TERMINAL="konsole"
export BROWSER="chromium --password-store=kwallet5"
export VIEWER="zathura"
export FILE="vifm"
export WMEmacs="i3"  # help Emacs adjust its config

# Support processes
sxhkd &	                # Bind keys
xset r rate 300 50 &	# Speed xrate up
unclutter & 		# Remove mouse when idle
xcompmgr &		# xcompmgr for transparency
dunst & 		# dunst for notifications

# X settings
[ -f "$HOME/.xprofile" ] && . ~/.xprofile
xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources

# Themes
export XDG_CURRENT_DESKTOP="KDE"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
xwallpaper --zoom ~/.config/wall.png

# Autostart apps
fcitx &
dropbox &

# Auto-start i3
exec i3
# Manually start i3 in TTY: startx /usr/bin/i3
