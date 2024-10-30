#!/bin/sh

emacs --daemon &
foot --server &
kanshi &
dunst &
systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP BEMENU_OPTS
dbus-update-activation-environment --system WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=wlr

# gsettings set org.gnome.desktop.interface gtk-theme dracula-gtk-theme
# gsettings set org.gnome.desktop.wm.preferences theme dracula-gtk-theme

# gsettings set org.gnome.desktop.interface icon-theme dracula-icons

# gsettings set org.gnome.desktop.interface font-name "Andika Bold 11"
# gsettings set org.gnome.desktop.interface monospace-font-name "Cascadia Code Regular 11"
