# px86's .bash_profile/.profile
#                                   ______  __ __          
#                                  /      \|  \  \         
#        ______   ______   ______ |  ▓▓▓▓▓▓\\▓▓ ▓▓ ______  
#       /      \ /      \ /      \| ▓▓_  \▓▓  \ ▓▓/      \ 
#      |  ▓▓▓▓▓▓\  ▓▓▓▓▓▓\  ▓▓▓▓▓▓\ ▓▓ \   | ▓▓ ▓▓  ▓▓▓▓▓▓\
#      | ▓▓  | ▓▓ ▓▓   \▓▓ ▓▓  | ▓▓ ▓▓▓▓   | ▓▓ ▓▓ ▓▓    ▓▓
#      | ▓▓__/ ▓▓ ▓▓     | ▓▓__/ ▓▓ ▓▓     | ▓▓ ▓▓ ▓▓▓▓▓▓▓▓
#      | ▓▓    ▓▓ ▓▓      \▓▓    ▓▓ ▓▓     | ▓▓ ▓▓\▓▓     \
#      | ▓▓▓▓▓▓▓ \▓▓       \▓▓▓▓▓▓ \▓▓      \▓▓\▓▓ \▓▓▓▓▓▓▓
#      | ▓▓                                                
#      | ▓▓                                                
#       \▓▓                                                
#      
#----------------------------------------------------------------------------

### PATH ###
[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.local/scripts" ] && PATH="$HOME/.local/scripts:$PATH"
export PATH

### clean-up the HOME directory ###
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_CONFIG_HOME="$HOME"/.config
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2/gtkrc
export WGETRC="$XDG_CONFIG_HOME"/wget/wgetrc
export LESSHISTFILE=-
export HISTFILE="$XDG_CACHE_HOME"/bash_history
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export NODE_REPL_HISTORY="$XDG_CACHE_HOME"/node_repl_history
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/python_startup.py

export _JAVA_AWT_WM_NONREPARENTING=1
export WORKON_HOME="$XDG_DATA_HOME"/virtualenvs/

### make 'less' colorful ###
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

### applications ###
export EDITOR="emacsclient -ta 'nvim'"
export VISUAL="emacsclient -ca 'emacs'"
export GIT_EDITOR="emacsclient -ta 'nvim'"
export TERMINAL="xterm"
export BROWSER="firefox"
export READER="zathura"

# start xserver if logged on /dev/tty1
if [ $(tty) = '/dev/tty1' ]
then
    exec startx ~/.config/X11/xinitrc
fi
