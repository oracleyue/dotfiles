# The system of OS X use .profile to configure its console. However, .bashrc in
# Mac OS X will also be used by emacs for its term, xterm, etc.

# set the locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# set for gnupg for vim
GPG_TTY=`tty`
export GPG_TTY

# environment variables
export EDITOR=vim
# pathes
export PATH=/Users/oracleyue/bin:${PATH}
export MANPATH=${MANPATH}:/usr/local/man
# Python
export PYTHONPATH=/Users/oracleyue/.local/lib/python3.7/site-packages
# Latex
# export BSTINPUTS=$(kpsepath bst)
# CUDA
export PATH=${PATH}:/Developer/NVIDIA/CUDA-9.0/bin
export DYLD_LIBRARY_PATH=/Developer/NVIDIA/CUDA-9.0/lib:$DYLD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATH
# gtags
export GTAGSLABEL=pygments          # "gtags" in GNU global
export GTAGSLIBPATH=$HOME/.gtags/   # "gtags" create tags for system libs

# aliases
alias ll='ls -Alh'
alias la='ls -a'
alias view='vim -R'
#alias em='emacs -nw'
source $HOME/bin/emacs-cmds-osx.sh
alias rm='rm -i' # use =trash= more to delete files
alias mv='mv -i'
alias cp='cp -r -i'
alias du='du -h -d 1'
alias df='df -h'
#alias rsync='rsync -aP'
alias rsync='rsync -rlptD -P --exclude=.DS_Store'
alias zip='zip -r'
alias tar='COPYFILE_DISABLE=1 tar'
#alias grep='grep -Hn -i --colour=always'
alias grep='grep -i'
#alias ack='ack -Hn --no-group --no-color'
alias ack='ack -Hni'
alias gitup='git add . && git ci -m "regular update" && git push'
alias updatedb='/usr/libexec/locate.updatedb'
alias shred='gshred -n 5'
alias trash='trash -v'
alias lsblk='diskutil list'
alias htopi='htop -u nobody'

# alias commands for development
alias gcc='gcc-7'
alias g++='g++-7'
alias gcc_stdlib_path='gcc -xc++ -E -v -'
alias clang_complete="CXX='cc_args.py g++' cmake .. && make && mv .clang_complete .."

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

# aliases for convenience
alias matlab-tty='matlab -nosplash -nodesktop'
alias blog="cd $BLOGPATH; hexo list post"

# colorize the bash
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
# export PS1='\[\033[0;32m\]\u@\h\[\033[00m\]:\[\033[0;34m\]\w\[\033[00m\]\$ '
# Source: https://bbs.archlinux.org/viewtopic.php?pid=1068202#p1068202
PS1="\[\033[0;37m\]\342\224\214\342\224\200\$([[ \$? != 0 ]] && echo \"[\[\033[0;31m\]\342\234\227\[\033[0;37m\]]\342\224\200\")[$(if [[ ${EUID} == 0 ]]; then echo '\[\033[0;31m\]\h'; else echo '\[\033[0;33m\]\u\[\033[0;37m\]@\[\033[0;96m\]$(scutil --get ComputerName)'; fi)\[\033[0;37m\]]\342\224\200[\[\033[0;32m\]\w\[\033[0;37m\]]\n\[\033[0;37m\]\342\224\224\342\224\200\342\224\200\342\225\274 \[\033[0m\]"

# using /less/ to colorize man page
export LESS=-R
export LESS_TERMCAP_me=$(printf '\e[0m')
export LESS_TERMCAP_se=$(printf '\e[0m')
export LESS_TERMCAP_ue=$(printf '\e[0m')
export LESS_TERMCAP_mb=$(printf '\e[1;32m')
export LESS_TERMCAP_md=$(printf '\e[1;34m')
export LESS_TERMCAP_us=$(printf '\e[1;32m')
export LESS_TERMCAP_so=$(printf '\e[1;44;1m')
# enable /less/ code syntax coloring
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
