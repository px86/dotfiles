import os


apps = [
    "nmcli",
    "xterm",
    "pavucontrol",
    "rofi",
    "brightnessctl",
    "pactl",
    "amixer",
    "pcmanfm",
    "sxiv",
    "foot",
]


class App:
    """Default applications exported via environment variables."""

    TERMINAL = os.environ.get("TERMINAL", "foot")
    BROWSER = os.environ.get("BROWSER", "firefox")
    BROWSER_PRIVATE = (
        BROWSER + " --private-window" if BROWSER == "firefox" else " --incognito"
    )
    WALLPAPER = os.path.expanduser("~/.local/data/wallpaper")


class Font:
    """Fonts to be uesd in widgets and bar."""

    SANS = "Open Sans Bold"
    MONO = "CaskaydiaCove Nerd Font Bold"
    ICON = "Font Awesome 6 Free"


class BackgroundColor:
    """Background colors for bar, widget, and border."""

    BAR = "#000000"  # "#272a36"
    WIDGET = "#181818"  # "#3b4252",
    BORDER = "#bf616a"


class ForegroundColor:
    """Foreground colors for widgets."""

    BACKLIGHT = "#81a1c1"
    BATTERY = "#8fbcbb"
    CHORD = "#a3be8c"
    CLOCK = "#8fbcbb"
    CPU = "#81a1c1"
    MEMORY = "#a3be8c"
    PROMPT = "#a3be8c"
    QUICKEXIT = "#bf616a"
    VOLUME = "#81a1c1"
    WINDOWCOUNT = "#f8f8f8"
    WLAN = "#8fbcbb"
