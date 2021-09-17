# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

if [ -z "$PS1" ]; then
  exit
fi

export PATH=.:$HOME/install/bin:$HOME/.local/bin:$PATH

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
# [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
export LESS='FiRX'

export EDITOR='emacs -nw'

PS1='\u@\h:\w\$ '

# Alias definitions.
# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -AlFh'
alias lt='ls -AlFhtr'
alias la='ls -A'
alias l='ls -CF'

alias df='df -h'
alias rm='rm -i'
alias e='emacs'
alias ew='emacs -nw'
alias sd='screen -dR -S dev'

function cdf {
  local dir=$(find -type d -name $1 | head -n1)
  if [ -n "$dir" ] ; then
    cd $dir
  else
    echo "Unable to find $1"
    return 1
  fi
}

# enable programmable completion features
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi

# Source bash settings unique to an environment
if [ -f ~/.bash_unique ]; then
  source ~/.bash_unique
fi
