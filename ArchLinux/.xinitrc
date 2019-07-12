#!/bin/sh
# Used only for "startx" and thus for "i3".

# From LARBS's ".profile"
export PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export EDITOR="vim"
export TERMINAL="konsole"
export BROWSER="chromium --password-store=kwallet5"
export READER="zathura"
export FILE="vifm"
# Themes
export XDG_CURRENT_DESKTOP="KDE"
export QT_STYLE_OVERRIDE="Breeze"
# Emacs
export WMEmacs="i3"

# From larbs's ".xprofile"
setbg &		# Set the background
sxhkd &	        # Bind keys
xset r rate 300 50 &	# Speed xrate up
unclutter &		# Remove mouse when idle
xcompmgr &		# xcompmgr for transparency
dunst &			# dunst for notifications

# X settings
[ -f "$HOME/.xprofile" ] && . ~/.xprofile
xmodmap ~/.Xmodmap
xrdb ~/.Xresources

# start fcitx for i3
fcitx &

# Auto-start i3
exec i3
# Manually start i3 in TTY: startx /usr/bin/i3
