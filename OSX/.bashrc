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
alias  view='vim -R'
export GPG_TTY=`tty`    # gnupg for vim

# emacs
source $HOME/bin/emacs-cmds-osx.sh

# install
alias set_proxy_sock="export ALL_PROXY=socks5://127.0.0.1:7890"
alias set_proxy_http="export ALL_PROXY=http://127.0.0.1:7890"
# alias brew-proxy="ALL_PROXY=socks5://127.0.0.1:7890 brew"
alias brew-proxy="ALL_PROXY=http://127.0.0.1:7890 brew"

# -------------------------------------------------------------------
# PATH
# -------------------------------------------------------------------
# environment (export PATH and MANPATH at the end of .bashrc)
export PATH="$HOME/bin:/usr/local/sbin:$PATH"
export MANPATH="/usr/local/share/man:/usr/local/man:$MANPATH"

# development
export C_INCLUDE_PATH=/usr/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=/usr/local/include:$CPLUS_INCLUDE_PATH
export LIBRARY_PATH=/usr/local/lib:$LIBRARY_PATH

# qt
export PATH="/usr/local/opt/qt@5/bin:$PATH"

# latex
export BSTINPUTS=$(kpsepath bst)

# texinfo (makeinfo)
export PATH="/usr/local/opt/texinfo/bin:$PATH"

# stardicts for sdcv
export STARDICT_DATA_DIR="$HOME/Programs/stardicts"

# -------------------------------------------------------------------
# Alternative tools for better performance or display
# -------------------------------------------------------------------
# cd:       use "z" or "zz" from fasd
# cat:      use "bat"
# find:     use "fd"
# grep:     use "rg" (ripgrep)
# du:       use "gdu" (gdu-go) or "dust"
# top/htop: use "btm" (bottom)

# cd/vim **<TAB>   use "fzf" to find files/directories
# git with interactive interface: lazygit

# having been aliased to replace original commands
# ls:       use "exa"
# men:      use "tldr"

# -------------------------------------------------------------------
# Aliases
# -------------------------------------------------------------------

# use /exa/ to replace ls
alias ls='exa --icons'
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
# alternative of "du": use "gdu" or "dust" for better display
alias du='du -h -d 1'
alias dus='du -h -d 1 . | sort -h'
alias df='df -h'
alias rsync='rsync -aP --exclude=.DS_Store'

alias zip='zip -r'
alias tar='COPYFILE_DISABLE=1 tar'

alias grep='grep -i'
alias sed='gsed'  # use GNU sed

alias updatedb='/usr/libexec/locate.updatedb'
alias lsblk='diskutil list'

alias shred='gshred -n 5'
alias trash='trash -v'

# use FASD for recent files/directories/commands
eval "$(fasd --init auto)"
# a: any; d: directory; f: file; s: interactive select
# z: quick cd; zz: cd with interactive selection
# sd: interactive directory selection; sf: interactive file selection
# Examples:
# $ vim `sf INPUTS`  OR  $ sf -e vim INPUTS
# $ cd `sd INPUTS`

# use FZF
alias fzf='fzf --layout=reverse'
# use: cd/vim **<TAB>
source /usr/local/opt/fzf/shell/completion.bash
# use "fd", which respect .gitignore or .fdignore
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
# use default keybindings:
source /usr/local/opt/fzf/shell/key-bindings.bash
# CTRL-T: paste files or directories from search
# CTRL-R: paste recent commands from history from search
# ALT-C:  cd into the selected directory

# fzf search then save result into clipboard
alias fzfcopy='fzf | pbcopy'

# use tldr as alternative to man ("npm install -g tldr")
# e.g., $ tldr tar
alias men='tldr'

# latex tools
# ldf='latexdiff --flatten'

# apps aliases
alias matlab-tty='matlab -nosplash -nodesktop'
alias ipy='screen -d -m jupyter qtconsole --style=monokai'

# ssh server
alias start-tunnel='ssh -NL 8080:localhost:4096 gpu-server &'
#alias jp='tmux new -d -s jupyter "ssh -NL 8080:localhost:4096 gpu-server"'

# -------------------------------------------------------------------
# colorize bash
# -------------------------------------------------------------------
export CLICOLOR=1

# basic prompt setup
export PROMPT_DIRTRIM=3
source /usr/local/etc/bash_completion.d/git-prompt.sh
# export PS1="\\w\$(__git_ps1 '(%s)') \$ "
# export PS1="\[\033[1;34m\]\\u@\\h:\[\033[0;32m\]\\w\[\033[0m\] \[\033[0;31m\]\$(__git_ps1 '(%s)')\[\033[0m\]\$ "
export PS1="\[\033[1;32m\]\\w\[\033[0m\]\[\033[1;31m\]\$(__git_ps1 ' (%s)')\[\033[0m\] \$ "

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

# -------------------------------------------------------------------
# Miniconda
# -------------------------------------------------------------------
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/zyue/miniconda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/zyue/miniconda/etc/profile.d/conda.sh" ]; then
        . "/Users/zyue/miniconda/etc/profile.d/conda.sh"
    else
        export PATH="/Users/zyue/miniconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# -------------------------------------------------------------------
# autostart tmux
# -------------------------------------------------------------------
# if command -v tmux &> /dev/null && [[ -z "$TMUX" ]]; then
    # tmux attach-session -t default || tmux new-session -s default
    # exit
# fi
