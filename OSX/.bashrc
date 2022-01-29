# The system of OS X use .profile to configure its console. However, .bashrc in
# Mac OS X will also be used by emacs for its term, xterm, etc.

# -------------------------------------------------------------------
# Basics & Editors
# -------------------------------------------------------------------
: ${HOME=~}
: ${LOGNAME=$(id -un)}
: ${UNAME=$(uname)}

# complete hostnames from this file
: ${HOSTFILE=~/.ssh/known_hosts}

# suppress Catalina warning on default shell moving to zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

# local
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# vim
export EDITOR=vim
export GPG_TTY=`tty`    # gnupg for vim
alias  view='vim -R'

# emacs
source $HOME/bin/emacs-cmds-osx.sh

# install
#alias brew="ALL_PROXY=socks5://127.0.0.1:7890 brew"

# -------------------------------------------------------------------
# PATH
# -------------------------------------------------------------------
# environment (export PATH and MANPATH at the end of .bashrc)
PATH="$HOME/bin:$PATH"
export MANPATH="/usr/local/share/man:/usr/local/man:$MANPATH"

# development
export C_INCLUDE_PATH=/usr/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=/usr/local/include:$CPLUS_INCLUDE_PATH
export LIBRARY_PATH=/usr/local/lib:$LIBRARY_PATH

# latex
export BSTINPUTS=$(kpsepath bst)

# texinfo (makeinfo)
PATH="/usr/local/opt/texinfo/bin:$PATH"

# CUDA
#PATH=${PATH}:/Developer/NVIDIA/CUDA-9.0/bin

# llvm (clangd)
PATH="/usr/local/opt/llvm/bin:$PATH"

# dotnet
export DOTNET_ROOT="/usr/local/opt/dotnet/libexec"

# stardicts for sdcv
export STARDICT_DATA_DIR="$HOME/Programs/stardicts"

# -------------------------------------------------------------------
# Aliases
# -------------------------------------------------------------------
# alias ll='ls -AlhB'
# alias la='ls -ahB'

# use /exa/ to replace ls
# --------------------------------
alias lss='exa --icons'
alias ls1='exa --icons -1'
alias lsd='exa --icons -D'
alias la='exa --icons -a'
alias ll='exa --icons --header --long --reverse --sort=cr'
alias lh='exa --icons --header --long --reverse --sort=cr -ld .?*'
alias lt='exa --icons -T -L'

alias rm='rm -i' # use =trash= more to delete files
alias mv='mv -i'
alias cp='cp -r -i'
alias tree='tree -N'  # -N to allow print Chinese in UTF
alias du='du -h -d 1'
alias df='df -h'
alias rsync='rsync -aP --exclude=.DS_Store'
# alias rsync='rsync -rlptD -P --exclude=.DS_Store'

alias zip='zip -r'
alias tar='COPYFILE_DISABLE=1 tar'

alias grep='grep -i'
alias sed='gsed'  # use GNU sed
alias ack='ack -Hni'
#alias grep='grep -Hn -i --colour=always'
#alias ack='ack -Hn --no-group --no-color'

alias updatedb='/usr/libexec/locate.updatedb'
alias lsblk='diskutil list'
alias htopMe='htop -u nobody'

alias shred='gshred -n 5'
alias trash='trash -v'

# use FASD
eval "$(fasd --init auto)"
# a: any; s: show/search/select; d: directory; f: file
# sd: interactive directory selection; sf: interactive file selection
# z: quick cd; zz: cd with interactive selection

# use FZF
source /usr/local/opt/fzf/shell/key-bindings.bash
source /usr/local/opt/fzf/shell/completion.bash
# CTRL-T: paste the selected files or directories onto the commandline
# CTRL-R: paste the selected command from history onto the commandline
# ALT-C:  cd into the selected directory
alias fzf='fzf --layout=reverse'

# use tldr as alternative to man ("npm install -g tldr")
# e.g., $ tldr tar
alias men='tldr'

# latex tools
# ldf='latexdiff --flatten'

# apps aliases
alias matlab-tty='matlab -nosplash -nodesktop'
alias ipy='screen -d -m jupyter qtconsole --style=monokai'

# ssh server
#alias jp='ssh -NL 8080:localhost:8888 mylab &'
alias jp='tmux new -d -s jupyter "ssh -NL 8080:localhost:8888 mylab"'
export remote='mylab:/home/lab8888'

# -------------------------------------------------------------------
# Colorize bash
# -------------------------------------------------------------------
export CLICOLOR=1

# /less/ to colorize man page
export LESS=-R
export LESS_TERMCAP_me=$(printf '\e[0m')
export LESS_TERMCAP_se=$(printf '\e[0m')
export LESS_TERMCAP_ue=$(printf '\e[0m')
export LESS_TERMCAP_mb=$(printf '\e[1;32m')
export LESS_TERMCAP_md=$(printf '\e[1;34m')
export LESS_TERMCAP_us=$(printf '\e[1;32m')
export LESS_TERMCAP_so=$(printf '\e[1;44;1m')
# enable /less/ code syntax coloring
# require: brew install source-highlight
LESSPIPE=`which src-hilite-lesspipe.sh`
export LESSOPEN="| ${LESSPIPE} %s"
export LESS=' -R -X -F '

# ----------------------------------------------------------------------
# BASH HISTORY
# ----------------------------------------------------------------------

# Increase the history size
HISTSIZE=10000
HISTFILESIZE=20000

# Lines which begin with a space character are not saved in the history list.
HISTCONTROL=ignorespace

# Add date and time to the history
zyue_BLUE="\033[0;34m" #Blue
zyue_NOCOLOR="\033[0m"
HISTTIMEFORMAT=`echo -e ${zyue_BLUE}[%F %T] $zyue_NOCOLOR `

# ----------------------------------------------------------------------
# Version Control
# ----------------------------------------------------------------------

# === CVS ===
export CVS_RSH='ssh'

# === SVN ===
export SVN_EDITOR=$EDITOR

## display the current subversion revision (to be used later in the prompt)
__svn_ps1() {
  (
    local svnversion
    svnversion=$(svnversion | sed -e "s/[:M]//g")
    # Continue if $svnversion is numerical
    let $svnversion
    if [[ "$?" -eq "0" ]]
    then
        printf " (svn:%s)" "$(svnversion)"
    fi
  ) 2>/dev/null
}

# === git ===
# render __git_ps1 even better so as to show activity in a git repository
#
# see https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh for
# configuration info of the GIT_PS1* variables

# fix error with __git_ps1 in screen
source /usr/local/etc/bash_completion.d/git-prompt.sh



export GIT_PS1_SHOWDIRTYSTATE=1      # unstaged (*) and staged (+) changes
export GIT_PS1_SHOWSTASHSTATE=1      # sth. stashed ($)
export GIT_PS1_SHOWUNTRACKEDFILES=1  # having untracked files (%)
export GIT_PS1_SHOWUPSTREAM="auto"   # difference between HEAD and its upstream:
                                     # "<"  you're behind;  ">"  you're ahead
                                     # "<>" you're diverge; "="  no difference

# ----------------------------------------------------------------------
# PROMPT
# ----------------------------------------------------------------------

# Define some colors to use in the prompt
RESET_COLOR="\[\e[0m\]"
BOLD_COLOR="\[\e[1m\]"
# B&W
WHITE="\[\e[0;37m\]"
# RGB
RED="\[\e[0;31m\]"
GREEN="\[\e[0;32m\]"
BLUE="\[\e[34;1m\]"
# other
YELLOW="\[\e[0;33m\]"
LIGHT_CYAN="\[\e[36;1m\]"
CYAN_UNDERLINE="\[\e[4;36m\]"

# Configure user color and prompt type depending on whoami
if [ "$LOGNAME" = "root" ]; then
    COLOR_USER="${RED}"
    P="#"
else
    COLOR_USER="${WHITE}"
    P=""
fi

# Configure a set of useful variables for the prompt
if [[ -e /proc/sys/kernel/hostname ]] ; then
    DOMAIN=$(cat /proc/sys/kernel/hostname | cut -d '.' -f 2)
elif [[ "$(echo $UNAME | grep -c -i -e '^.*bsd$')" == "1" ]] ; then
    DOMAIN=$(hostname | cut -d '.' -f 2)
else
    DOMAIN=$(hostname -f | cut -d '.' -f 2)
fi

# get virtualization information
XENTYPE=""
if [ -f "/sys/hypervisor/uuid" ]; then
    if [ "$(</sys/hypervisor/uuid)" == "00000000-0000-0000-0000-000000000000" ]; then
        XENTYPE=",Dom0"
    else
        XENTYPE=",DomU"
    fi
fi
# Test the PS1_EXTRA variable
if [ -z "${PS1_EXTRA}" -a -f "/proc/cmdline" ]; then
    # Here PS1_EXTRA is not set and/or empty, check additionally if it has not
    # been set via kernel comment
    kernel_ps1_extra="$(grep PS1_EXTRA /proc/cmdline)"
    if [ -n "${kernel_ps1_extra}" ]; then
        PS1_EXTRA=$( sed -e "s/.*PS1_EXTRA=\"\?\([^ ^\t^\"]\+\)\"\?.*/\1/g" /proc/cmdline )
    fi
fi
PS1_EXTRAINFO="${BOLD_COLOR}${DOMAIN}${XENTYPE}${RESET_COLOR}"
if [ -n "${PS1_EXTRA}" ]; then
    PS1_EXTRAINFO="${PS1_EXTRAINFO},${YELLOW}${PS1_EXTRA}${RESET_COLOR}"
fi


# This function is called from a subshell in $PS1, to provide the colorized
# exit status of the last run command.
# Exit status 130 is also considered as good as it corresponds to a CTRL-D
__colorized_exit_status() {
    printf -- "\$(status=\$? ; if [[ \$status = 0 || \$status = 130  ]]; then \
                                echo -e '\[\e[01;32m\]'\$status;              \
                              else                                            \
                                echo -e '\[\e[01;31m\]'\$status; fi)"
}

###########
# my prompt; the format is as follows:
#
#    [hh:mm:ss]:$?: username@hostname(domain[,xentype][,extrainfo]) workingdir(svn/git status)$>
#    `--------'  ^  `------' `------'         `--------'`--------------'
#       cyan     |  root:red   cyan              light     green
#                |           underline            blue   (absent if not relevant)
#           exit code of
#        the previous command
#
# The git/svn status part is quite interesting: if you are in a directory under
# version control, you have the following information in the prompt:
#   - under GIT: current branch name, followed by a '*' if the repository has
#                uncommitted changes, followed by a '+' if some elements were
#                'git add'ed but not commited.
#   - under SVN: show (svn:XX[M]) where XX is the current revision number,
#                followed by 'M' if the repository has uncommitted changes
#
# `domain` reflect the current domain of the machine that run the prompt
# (guessed from hostname -f)
# `xentype` is DOM0 or domU depending if the machine is a Xen dom0 or domU
# Finally, is the environment variable PS1_EXTRA is set (or passed to the
# kernel), then its content is displayed here.
#
# This prompt is perfect for terminal with black background, in my case the
# Vizor color set (see http://visor.binaryage.com/) or iTerm2
__set_my_prompt() {
    # Default ULHPC
    # PS1="$(__colorized_exit_status) ${LIGHT_CYAN}[\t]${RESET_COLOR} ${COLOR_USER}\u${RESET_COLOR}@${CYAN_UNDERLINE}\h${RESET_COLOR}(${PS1_EXTRAINFO}) ${BLUE}\W${RESET_COLOR}${GREEN}\$(__git_ps1 \" (%s)\")\$(__svn_ps1)${RESET_COLOR}${P}> "

    # Another simple prompt
    # PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w $\[\033[00m\] '

    # Another fancy but noninformative prompt
    # source: https://bbs.archlinux.org/viewtopic.php?pid=1068202#p1068202
    # PS1="\[\033[0;37m\]\342\224\214\342\224\200\$([[ \$? != 0 ]] && echo \"[\[\033[0;31m\]\342\234\227\[\033[0;37m\]]\342\224\200\")[$(if [[ ${EUID} == 0 ]]; then echo '\[\033[0;31m\]\h'; else echo '\[\033[0;33m\]\u\[\033[0;37m\]@\[\033[0;96m\]$(scutil --get ComputerName)'; fi)\[\033[0;37m\]]\342\224\200[\[\033[0;32m\]\w\[\033[0;37m\]]\n\[\033[0;37m\]\342\224\224\342\224\200\342\224\200\342\225\274 \[\033[0m\]"

    # Simplified from ULHPC by zyue:
    # PS1="$(__colorized_exit_status) ${RESET_COLOR}${COLOR_USER}\u${RESET_COLOR}@${CYAN_UNDERLINE}\h${RESET_COLOR}(${PS1_EXTRAINFO}) ${BLUE}\W${RESET_COLOR}${GREEN}\$(__git_ps1 \" (%s)\")${RESET_COLOR}\$ "

    # Customized by zyue:
    PS1="\[\033[0;37m\]\342\224\214\342\224\200\$([[ \$? != 0 ]] && echo \"[\[\033[0;31m\]\342\234\227\[\033[0;37m\]]\342\224\200\")[$(if [[ ${EUID} == 0 ]]; then echo '\[\033[0;31m\]\h'; else echo '\[\033[0;33m\]\u\[\033[0;37m\]@\[\033[0;96m\]$(scutil --get ComputerName)'; fi)\[\033[0;37m\]]\342\224\200[\[\033[0;32m\]\w\[\033[0;37m\]]${RED}\$(__git_ps1 \" (%s)\")${RESET_COLOR}\n\[\033[0;37m\]\342\224\224\342\224\200\342\224\200\342\225\274 \[\033[0m\]"
}

# --------------------------------------------------------------------
# PATH MANIPULATION FUNCTIONS (thanks rtomayko ;) )
# --------------------------------------------------------------------
######
# Remove duplicate entries from a PATH style value while retaining
# the original order. Use PATH if no <path> is given.
# Usage: puniq [<path>]
#
# Example:
#   $ puniq /usr/bin:/usr/local/bin:/usr/bin
#   /usr/bin:/usr/local/bin
###
puniq () {
    # echo "$1" |tr : '\n' |nl |sort -u -k 2,2 |sort -n |
    #     cut -f 2- |tr '\n' : |sed -e 's/:$//' -e 's/^://'

    # use awk
    printf "%s" "$1" | awk -v RS=':' '!a[$1]++ { if (NR > 1) printf RS; printf $1 }'
}

# -------------------------------------------------------------------
# USER SHELL ENVIRONMENT
# -------------------------------------------------------------------

# Set the color prompt by default when interactive
if [ -n "$PS1" ]; then
    __set_my_prompt
    export PS1
fi

# MOTD
test -n "$INTERACTIVE" -a -n "$LOGIN" && {
    uname -npsr
    uptime
}

# export PKG_CONFIG_PATH
# export C_INCLUDE_PATH   CPLUS_INCLUDE_PATH   LIBRARY_PATH   DYLD_FALLBACK_LIBRARY_PATH

# condense PATH entries
PATH="$(puniq "$PATH")"
export PATH MANPATH
