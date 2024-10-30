import os


BRIGHTNESS_UP_CMD = "brightnessctl -c backlight set 1%+"
BRIGHTNESS_DOWN_CMD = "brightnessctl -c backlight set 1%-"

VOLUME_TOGGLE_MUTE_CMD = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
VOLUME_UP_CMD = "pactl set-sink-volume @DEFAULT_SINK@ +1%"
VOLUME_DOWN_CMD = "pactl set-sink-volume @DEFAULT_SINK@ -1%"

TERMINAL_CMD = "footclient"
EDITOR_CMD = "emacsclient -ca ''"

BROWSER_CMD = "firefox --new-tab"
BROWSER_PRIVATE_CMD = "firefox --private-window"

FILEMANAGER_CMD = "pcmanfm"

MESSENGER_CMD = "telegram"

MUSIC_CMD = "deadbeef"

DOCUMENTS_MENU_CMD = "dox-bm"
BOOKMARKS_MENU_CMD = "bmark-bm"
WIFI_MENU_CMD = "mymenu wifi"

WALLPAPERS_DIR_CMD = (
    f"emacsclient -c -e ({os.path.expanduser('~/Pictures/Wallpapers')!r})"
)
SCREENSHOTS_DIR_CMD = (
    f"emacsclient -c -e ({os.path.expanduser('~/Pictures/Screenshots')!r})"
)

NETWORKING_ON_CMD = "nmcli networking on"
NETWORKING_OFF_CMD = "nmcli networking off"

TASK_MANAGER_CMD = "mymenu btop --preset 1"
AUDIO_MANAGER_CMD = "pavucontrol"

SCREENSHOT_CMD = ""
SCREENSHOT_WITH_SELECTION_CMD = ""

SCREENLOCK_CMD = ""

QTILE_CONFIG_RELOAD_CMD = "/usr/bin/kill -s SIGUSR1 $(pgrep qtile)"
