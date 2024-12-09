#       __                         __
#      |  \                       |  \
#      | ▓▓____   ______   _______| ▓▓____   ______   _______
#      | ▓▓    \ |      \ /       \ ▓▓    \ /      \ /       \
#      | ▓▓▓▓▓▓▓\ \▓▓▓▓▓▓\  ▓▓▓▓▓▓▓ ▓▓▓▓▓▓▓\  ▓▓▓▓▓▓\  ▓▓▓▓▓▓▓
#      | ▓▓  | ▓▓/      ▓▓\▓▓    \| ▓▓  | ▓▓ ▓▓   \▓▓ ▓▓
#      | ▓▓__/ ▓▓  ▓▓▓▓▓▓▓_\▓▓▓▓▓▓\ ▓▓  | ▓▓ ▓▓     | ▓▓_____
#      | ▓▓    ▓▓\▓▓    ▓▓       ▓▓ ▓▓  | ▓▓ ▓▓      \▓▓     \
#       \▓▓▓▓▓▓▓  \▓▓▓▓▓▓▓\▓▓▓▓▓▓▓ \▓▓   \▓▓\▓▓       \▓▓▓▓▓▓▓
#
#
#---------------------------------------------------------------------------

# if not running interactively, return
[[ $- != *i* ]] && return

### shell options ###
shopt -s autocd 	# change to named directory
shopt -s cdspell 	# autocorrects cd misspellings
shopt -s cmdhist 	# save multi-line commands as single line
shopt -s histappend 	# do not overwrite history
shopt -s dotglob
#shopt -s globstar
shopt -s expand_aliases

# enable TAB completion for bash
[ -r /usr/share/bash-completion/bash_completion   ] &&
	. /usr/share/bash-completion/bash_completion

HISTSIZE=1000
HISTFILESIZE=2000

# checks term size when bash regains control
[[ $DISPLAY ]] && shopt -s checkwinsize

### minimal prompt ###
export PROMPT_DIRTRIM=2
#export PS1="\[\e[34m\]\w\[\e[m\]\[\e[32m\] $ \[\e[m\]"

### bash-alias ###
alias ls="ls --color=auto --group-directories-first"
alias ll="exa -ls  extension  --group-directories-first"
alias la="exa -als extension --group-directories-first"
alias grep="grep --color=auto"
alias q="exit"
alias ec="emacsclient -ta 'emacs -nw'"
alias yta='yt-dlp -x --embed-metadata --replace-in-metadata "title" " *(\(?[Oo]fficial)?.*([Vv]ideo|[Aa]udio|[Vv]isualizer)\)?" "" '

# git bare repository setup for dotfiles management
alias dotfile="git --git-dir=$HOME/.local/dotfiles.git --work-tree=$HOME"

### below config is taken from default debian .bashrc ###

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# powerline
if [ -f /usr/share/powerline/bindings/bash/powerline.sh ]; then
	source /usr/share/powerline/bindings/bash/powerline.sh
fi
