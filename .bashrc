# px86's .bashrc
#
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
export PS1="\[\e[34m\]\w\[\e[m\]\[\e[32m\] $ \[\e[m\]"

### bash-alias ###
alias ls="ls --color=auto --group-directories-first"
alias ll="ls -ohX"
alias la="ll -A"
alias grep="grep --color=auto"
alias q="exit"
alias ec="emacsclient -ta 'emacs -nw'"
alias yta="youtube-dl -x -f bestaudio/best"

# git bare repository setup for dotfiles management
alias dotfile="git --git-dir=$HOME/.local/dotfiles.git --work-tree=$HOME"

### END ###
