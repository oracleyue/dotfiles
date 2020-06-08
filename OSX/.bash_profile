# keep everything in .bashrc
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# Suppress Catalina warning on default shell moving to zsh
export BASH_SILENCE_DEPRECATION_WARNING=1
