#!/bin/sh
# For i3, used by "startx"

export PATH=$PATH:$HOME/bin/i3cmds
export EDITOR="vim"
export TERMINAL="urxvt"  #konsole
#export BROWSER="chromium --password-store=kwallet5"
export BROWSER="chromium"
export VIEWER="zathura"
export FILE="ranger"
export WMEmacs="i3"  # help Emacs adjust its config

# X settings
[ -f "$HOME/.xprofile" ] && . ~/.xprofile
xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources

# Themes
export XDG_CURRENT_DESKTOP="KDE"
export KDE_SESSION_VERSION="5"  # use "kde-open" instead of "kfmclient" for xdg-open
export SSH_ASKPASS="/usr/bin/ksshaskpass"
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
xwallpaper --zoom ~/.config/i3/wallpaper.jpg

# Support processes
sxhkd &	                # Bind keys
xset r rate 300 50 &	# Speed xrate up
unclutter & 		# Remove mouse when idle
xcompmgr &		# xcompmgr for transparency
eval $(ssh-agent -s)    # start ssh-agent to work with ssh-add.sh

# Autostart apps
fcitx &
dropbox &
mpd >/dev/null 2>&1 &
mpc update >/dev/null 2>&1

# Start i3 (manually start i3 in TTY: startx /usr/bin/i3)
exec i3
