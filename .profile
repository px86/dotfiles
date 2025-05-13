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
[ -d "$HOME/.local/bin" ]     && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.local/scripts" ] && PATH="$HOME/.local/scripts:$PATH"
[ -d "/opt/clojure/bin" ]     && PATH="/opt/clojure/bin:$PATH"
export PATH

### clean-up the HOME directory ###
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_CONFIG_HOME="$HOME"/.config
export WGETRC="$XDG_CONFIG_HOME"/wget/wgetrc
export LESSHISTFILE=-
export HISTFILE="$XDG_CACHE_HOME"/bash_history
export PYTHON_HISTORY="$XDG_CACHE_HOME"/python_history
export NODE_REPL_HISTORY="$XDG_CACHE_HOME"/node_repl_history
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
#export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/python_startup.py
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc

export MOZ_ENABLE_WAYLAND=1
export QT_QPA_PLATFORM=wayland
export SDL_VIDEODRIVER=wayland
export XDG_SESSION_TYPE=wayland
export XKB_DEFAULT_OPTIONS=caps:ctrl_modifier

export GOPATH="$HOME/src/go"

[ -d '/usr/lib/jvm/default' ] && export JAVA_HOME='/usr/lib/jvm/default'
export _JAVA_AWT_WM_NONREPARENTING=1
export RANGER_LOAD_DEFAULT_RC='FALSE'

export WORKON_HOME="$XDG_DATA_HOME"/virtualenvs/
[ -d "$WORKON_HOME" ] || mkdir -p "$WORKON_HOME"

### make 'less' colorful ###
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'
export MANPAGER='less -R --use-color -Dd+b -Dum -DSky'
export MANROFFOPT='-P -c'

### applications ###
export EDITOR='nvim'
export VISUAL='emacsclient -ca ""'
export GIT_EDITOR='nvim'
export TERMINAL='footclient'
export BROWSER='firefox'
export READER='zathura'

if [ $(tty) = '/dev/tty1' ]
then
    exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) Hyprland
fi
