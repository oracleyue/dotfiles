#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
    # start tmux on every shell login
    # [[ -z "$TMUX" ]] && exec tmux


# oracleyue-added $PATH
PATH="$PATH":/opt/android-sdk/tools:/opt/android-sdk/platform-tools:/home/oracleyue/Programs/bin
#export TERM=xterm-256color
#export TERM=screen-256color-bce

# oracleyue-configured QT (using qtchooser)
QT_SELECT=4

# oracleyue-defined alias
alias ls='ls --color=always'
alias ll='ls --color=always -Alh'
alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias dmesg='dmesg --color=always'
alias vi='vim'
alias view='vim -R'
alias emacs='env LC_CTYPE=zh_CN.UTF-8 emacs'
alias cp='cp -r -i'
alias du='du -h -d 1'
alias df='df -h'
alias rsync='rsync -aP'
alias battery_level='upower --show-info /org/freedesktop/UPower/devices/battery_BAT0'
alias chrome="/usr/bin/google-chrome-stable"
alias android-connect="jmtpfs ~/VirtualMachine/mtp"
alias android-disconnect="fusermount -u ~/VirtualMachine/mtp"
alias android-docs="chrome http://developer.android.com/guide/index.html"
alias java-docs="chrome http://docs.oracle.com/javase/7/docs/api/"
alias emacs="emacs -nw"
alias tmux="env TERM=xterm-256color tmux"

# oracleyue-defined startups

# oracleyue-defined commands/functions
function gpgless() { gpg -d "$1" | less ;}
 # atlas: mount the remote folder on atlas server
 #    usage: "atlas [-u]" -  mount/umount by sudo.
#function rsync-pull() { rsync -avz gaia-cluster:"$1" "$2"; }
#function rsync-push() { rsync -avz --progress --partial "$1" gaia-cluster:"$2"; }

# oracleyue-defined environment variables
export EDITOR="vim"

# oracleyue: setting default JVM used in MATLAB
export MATLAB_JAVA=/usr/lib/jvm/java-7-openjdk/jre
#source ~/.kde4/env/matlab_java.sh

# oracleyue: for /wine/
#export WINEPREFIX=/home/oracleyue/.wine # any path to a writable folder on your home directory will do
#export WINEARCH="win32"

# oracleyue: settings for /python/(incl. /pymacs/)
export PYMACS_PYTHON=python2
export PYTHONDOCS=/usr/share/doc/python2/html/

# oracleyue: setting shortcuts for working paths
export prj=~/Workspace/matlab/Feng_prj_HPC
export orgnote=~/Public/Dropbox/oracleyue/OrgNote
export gitrepo=~/FileArchives/arch
export ATLAS=~/Public/ATLAS/prj.backup
export gaiapath=gaia:/work/users/zyue/matlab/Feng_prj_HPC

#export yPATHdisk=/run/media/oracleyue/

# oracleyue: Using /less/ to colorize man page
# man() {
#	env \
#		LESS_TERMCAP_mb=$(printf "\e[1;37m") \
#		LESS_TERMCAP_md=$(printf "\e[1;37m") \
#		LESS_TERMCAP_me=$(printf "\e[0m") \
#		LESS_TERMCAP_se=$(printf "\e[0m") \
#		LESS_TERMCAP_so=$(printf "\e[1;47;30m") \
#		LESS_TERMCAP_ue=$(printf "\e[0m") \
#		LESS_TERMCAP_us=$(printf "\e[0;36m") \
#			man "$@"
# }
    # Using /less/ colored output
export LESS=-R
export LESS_TERMCAP_me=$(printf '\e[0m')
export LESS_TERMCAP_se=$(printf '\e[0m')
export LESS_TERMCAP_ue=$(printf '\e[0m')
export LESS_TERMCAP_mb=$(printf '\e[1;32m')
export LESS_TERMCAP_md=$(printf '\e[1;34m')
export LESS_TERMCAP_us=$(printf '\e[1;32m')
export LESS_TERMCAP_so=$(printf '\e[1;44;1m')
    # Enable /less/ code syntax coloring
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"


## Using vim as alternative pager
#alias less='/usr/share/vim/vim74/macros/less.sh'


# Configurations for /git/
source /usr/share/git/completion/git-completion.bash
source /usr/share/git/completion/git-prompt.sh


# ---------------- PS1 config --------------------------------------------
#PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w $\[\033[00m\] '
# ------------------------------------------------------
# color and PS1 config for Bash
#   For detailed explanations, refer to: 
#       config_Arch.git/_unused/bash.bashrc
# ------------------------------------------------------
# If not running interactively, don't do anything!
[[ $- != *i* ]] && return

# Enable checkwinsize to check the terminal size when bash regains control.
shopt -s checkwinsize

# Enable history appending instead of overwriting.
shopt -s histappend

case ${TERM} in
        xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
                PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
                ;;
        screen*)
                PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
                ;;
esac

# sanitize TERM:
safe_term=${TERM//[^[:alnum:]]/?}
match_lhs=""

[[ -f ~/.dir_colors ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs} ]] \
        && type -P dircolors >/dev/null \
        && match_lhs=$(dircolors --print-database)

if [[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] ; then
        # we have colors :-)
        # Enable colors for ls, etc. Prefer ~/.dir_colors
        if type -P dircolors >/dev/null ; then
                if [[ -f ~/.dir_colors ]] ; then
                        eval $(dircolors -b ~/.dir_colors)
                elif [[ -f /etc/DIR_COLORS ]] ; then
                        eval $(dircolors -b /etc/DIR_COLORS)
                fi
        fi

        PS1="$(if [[ ${EUID} == 0 ]]; then echo '\[\033[01;31m\]\h'; else echo '\[\e[1;35m\][\t] \[\033[01;32m\]\u@\h'; fi)\[\033[01;34m\] \w \$([[ \$? != 0 ]] && echo \"\[\033[01;31m\]:(\[\033[01;34m\] \")\\$\[\033[00m\] "

else
        # show root@ when we do not have colors
        # # PS1="\[\e[1;34m\][\t] \u@\h \w \$([[ \$? != 0 ]] && echo \":( \")\$ "
        # # PS1="\[\e[1;35m\][\t] \[\033[01;32m\]\u@\h \[\033[01;34m\] \w \$([[ \$? != 0 ]] && echo \"\[\033[01;31m\]:(\[\033[01;34m\] \")\\$\[\033[00m\] " 
        PS1="\[\033[01;32m\]\u@\h \[\033[01;34m\]\w \$([[ \$? != 0 ]] && echo \"\[\033[01;31m\]:(\[\033[01;34m\] \")\\$\[\033[00m\] " 
fi

PS2="> "
PS3="> "
PS4="+ "

# Try to keep environment pollution down, EPA loves us.
    unset safe_term match_lhs

# Try to enable the auto-completion (type: "pacman -S bash-completion" to install it).
    [ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Try to enable the "Command not found" hook ("pacman -S pkgfile" to install it).
    [ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash
# ------------------------------------------------------------------------
