"""px86's qtile configuration."""

import os
import re
import subprocess
from functools import cache
from collections.abc import Callable

from libqtile import layout
from libqtile import bar
from libqtile import widget
from libqtile import qtile
from libqtile import hook
from libqtile.lazy import lazy
from libqtile.config import Click, Drag, Screen, Match, Group, KeyChord, EzKey as Key
from libqtile.backend.wayland.inputs import InputConfig

import command as cmd
import unicodeicon as icon
from custom import (
    Font as font,
    ForegroundColor as fg,
    BackgroundColor as bg,
    App as app,
)

wl_input_rules = {
    "type:touchpad": InputConfig(tap=True, natural_scroll=True),
    # "2:14:ETPS/2 Elantech Touchpad": InputConfig(tap=True, natural_scroll=True),
    # "type:keyboard": InputConfig(kb_options="ctrl:nocaps,compose:ralt"),
}


class WindowClassName:
    """Window class names for applications."""

    WEB = ["firefox", "Firefox-esr", "brave-browser", "chromium"]
    DOC = ["Zathura", "libreoffice", re.compile(r"zathura")]
    MUSIC = ["deadbeef"]
    VID = ["mpv"]
    PICS = ["gimp.bin"]
    COMM = ["walc", "TelegramDesktop", re.compile(r".*[Tt]elegram.*")]


wcls = WindowClassName

groups: list[Group] = [
    Group(name="1", label=icon.EMACS_ICON),
    Group(
        name="2", label=icon.FIREFOX_ICON, matches=[Match(wm_class=s) for s in wcls.WEB]
    ),
    Group(
        name="3",
        label=icon.FLASK_ICON,
        matches=[Match(wm_class=s) for s in wcls.DOC],
    ),
    Group(
        name="4", label=icon.MUSIC_ICON, matches=[Match(wm_class=s) for s in wcls.MUSIC]
    ),
    Group(name="5", label=icon.VIDEO_CAMERA_ICON),
    Group(
        name="6",
        label=icon.PICTURE_ICON,
        layout="max",
        matches=[Match(wm_class=s) for s in wcls.PICS],
    ),
    Group(
        name="7",
        label=icon.TELEGRAM_ICON,
        matches=[Match(wm_class=s) for s in wcls.COMM],
    ),
    # ScratchPad(
    #     name="scratchpad",
    #     dropdowns=[
    #         DropDown(
    #             name="term",
    #             cmd="foot",
    #             x=0,
    #             y=0,
    #             width=1,
    #             height=1,
    #             opacity=0.95,
    #         ),
    #     ],
    # ),
]

layouts = [
    layout.MonadTall(
        align=layout.MonadTall._left,
        border_focus=bg.BORDER,
        border_normal=bg.BAR,
        border_width=2,
        ratio=0.60,
        change_ratio=0.05,
        change_size=20,
        max_ratio=0.75,
        min_ratio=0.25,
        min_secondary_size=85,
        new_client_position="after_current",
        single_border_width=0,
        margin=0,
        single_margin=0,
    ),
    layout.Columns(
        border_focus=bg.BORDER,
        border_normal=bg.BAR,
        fair=False,
        num_columns=2,
        border_width=2,
        border_on_single=False,
        margin=0,
        margin_on_single=0,
    ),
    layout.Max(),
]

float_wm_classes = [
    "gnu octave",
    "gcolor3",
    "confirmreset",
    "makebranch",
    "maketag",
    "ssh-askpass",
    "pinentry",
    "pinentry-gtk-2",
    "branchdialog",
    "pavucontrol",
    "float-me",
]


def should_float(client) -> bool:
    """Decide if a window should float or not."""

    wininfo = client.info()
    title = wininfo["name"].lower()
    wm_class = wininfo["wm_class"][0].lower()

    if (
        wm_class in float_wm_classes
        or title in float_wm_classes
        or (
            wm_class == "pcmanfm"
            and title in ("rename file", "run a command", "select filter")
        )
    ):
        client.move_to_top()
        return True

    return False


floating_layout = layout.Floating(
    border_width=3,
    border_focus=bg.BORDER,
    border_normal=bg.BAR,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(func=should_float),
    ],
)

widget_defaults = dict(
    font=font.MONO,
    fontsize=13,
    padding=4,
)


@cache
def _decoration(content, foreground, background):
    """TextBox widget for arrow style decoration."""
    return widget.TextBox(
        text=content,
        font=font.ICON,
        fontsize=18,
        padding=-0.1,
        foreground=foreground,
        background=background,
    )


def arrow_right_dark():
    return _decoration(icon.ARROW_RIGHT_ICON, bg.BAR, bg.WIDGET)


def arrow_right_light():
    return _decoration(icon.ARROW_RIGHT_ICON, bg.WIDGET, bg.BAR)


def arrow_left_dark():
    return _decoration(icon.ARROW_LEFT_ICON, bg.BAR, bg.WIDGET)


def arrow_left_light():
    return _decoration(icon.ARROW_LEFT_ICON, bg.WIDGET, bg.BAR)


def widget_groupbox():
    return widget.GroupBox(
        font=font.ICON,
        fontsize=16,
        padding=-4,
        disable_drag=True,
        active="aaaaaa",
        inactive="888888",
        highlight_method="line",
        urgent_alert_method="text",
        # this_current_screen_border="ffffff",
        # this_screen_border="303030",
        # other_current_screen_border="00038c",
        # urgent_border="8f0000",
        # urgent_text="8f0000",
    )


@cache
def widget_chord():
    return widget.Chord(
        fmt="CHORD: {} ",
        foreground=fg.CHORD,
    )


def widget_currentlayouticon():
    return widget.CurrentLayoutIcon(scale=0.75, background=bg.WIDGET)


def widget_windowcount():
    return widget.WindowCount(background=bg.WIDGET, foreground=fg.WINDOWCOUNT)


@cache
def widget_prompt():
    return widget.Prompt(
        ignore_dups_history=True,
        fontsize=14,
        prompt="spawn: ",
        foreground=fg.PROMPT,
        background=bg.WIDGET,
    )


@cache
def widget_volume():
    return widget.Volume(
        fmt=icon.VOLUME_ICON + " {}",
        mouse_callbacks={
            "Button1": lambda: qtile.cmd_spawn(cmd.VOLUME_TOGGLE_MUTE_CMD),
            "Button3": lambda: qtile.cmd_spawn(cmd.AUDIO_MANAGER_CMD),
        },
        margin=6,
        update_interval=1.5,
        foreground=fg.VOLUME,
    )


@cache
def widget_backlight(backlight_name: str = "intel_backlight"):
    return widget.Backlight(
        backlight_name=backlight_name,
        backlight_file="brightness",
        max_brightness_file="max_brightness",
        change_command="brightnessctl -c backlight set {}%",
        step=2,
        format="{percent:2.0%}",
        fmt=icon.BRIGHTNESS_ICON + " {}",
        margin=6,
        update_interval=2,
        foreground=fg.BACKLIGHT,
    )


@cache
def widget_wlan():
    return widget.Wlan(
        interface="wlp2s0",
        format=icon.WIFI_CONNECTED_ICON + " {essid}:{percent:2.0%}",
        disconnected_message=icon.AIRPLANE_ICON,
        mouse_callbacks={
            "Button1": lambda: qtile.cmd_spawn(cmd.NETWORKING_ON_CMD),
            "Button3": lambda: qtile.cmd_spawn(cmd.NETWORKING_OFF_CMD),
        },
        update_interval=10,
        foreground=fg.WLAN,
        background=bg.WIDGET,
    )


@cache
def widget_cpu():
    return widget.CPU(
        format="{freq_current}GHz {load_percent}%",
        fmt=icon.EXTINGUISHER_ICON + " {}",
        padding=6,
        mouse_callbacks={"Button1": lambda: qtile.cmd_spawn(cmd.TASK_MANAGER_CMD)},
        update_interval=8,
        foreground=fg.CPU,
    )


@cache
def widget_memory():
    return widget.Memory(
        measure_mem="M",
        format="{MemUsed:.0f}mb",
        fmt=icon.BOMB_ICON + " {}",
        mouse_callbacks={"Button1": lambda: qtile.cmd_spawn(cmd.TASK_MANAGER_CMD)},
        update_interval=10,
        foreground=fg.MEMORY,
    )


@cache
def widget_battery(battery: str = "BAT0"):
    return widget.Battery(
        battery=battery,
        charge_char="+",
        discharge_char="",
        format="{percent:2.0%} {char}",
        fmt=icon.PLUG_ICON + " {}",
        foreground=fg.BATTERY,
        background=bg.WIDGET,
    )


@cache
def widget_clock():
    return widget.Clock(
        format="%a %d %b %H:%M",
        font=font.SANS,
        fontsize=12,
        foreground=fg.CLOCK,
    )


@cache
def widget_systray():
    """Use this only once."""
    return widget.Systray()


@cache
def widget_quickexit():
    return widget.QuickExit(
        default_text=icon.POWER_OFF_ICON,
        countdown_format="{}",
        padding=6,
        foreground=fg.QUICKEXIT,
    )


def widget_spacer():
    return widget.Spacer()


widget_layout_primary = [
    widget_groupbox,
    widget_chord,
    arrow_right_dark,
    widget_currentlayouticon,
    widget_windowcount,
    widget_prompt,
    arrow_right_light,
    widget_spacer,  # spacer
    widget_volume,
    widget_backlight,
    arrow_left_light,
    widget_wlan,
    arrow_left_dark,
    widget_cpu,
    widget_memory,
    arrow_left_light,
    widget_battery,
    arrow_left_dark,
    widget_clock,
    # widget_systray,
    widget_quickexit,
]

widget_layout_secondary = widget_layout_primary.copy()
# widget_layout_secondary.pop(-2)  # remove systray
widget_layout_secondary.pop(9)  # remove backlight


def create_bar(widget_layout: list[Callable]) -> bar.Bar:
    return bar.Bar(
        size=18,
        opacity=0.97,
        border_color=bg.BAR,
        border_width=2,
        # margin=[8, 10, 0, 10],
        background=bg.BAR,
        widgets=[w() for w in widget_layout],
    )


# Laptop
screens = [
    Screen(
        wallpaper=app.WALLPAPER,
        wallpaper_mode="fill",
        top=create_bar(widget_layout_primary),
    ),
    Screen(
        wallpaper=app.WALLPAPER,
        wallpaper_mode="fill",
        top=create_bar(widget_layout_secondary),
    ),
]

modifier_keys = {
    "M": "mod4",
    "A": "mod1",
    "S": "shift",
    "C": "control",
}

MOD = "mod4"

keys = [
    # Shift focus between windows
    Key("M-h", lazy.layout.left()),
    Key("M-j", lazy.layout.down()),
    Key("M-k", lazy.layout.up()),
    Key("M-l", lazy.layout.right()),
    Key("M-<space>", lazy.layout.next()),
    Key("M-<Left>", lazy.screen.prev_group(skip_empty=True)),
    Key("M-<Right>", lazy.screen.next_group(skip_empty=True)),
    Key("M-<tab>", lazy.screen.toggle_group()),
    Key("M-f", lazy.window.toggle_maximize()),
    Key("M-S-<space>", lazy.window.toggle_floating()),
    Key("M-S-q", lazy.shutdown()),
    Key("M-C-r", lazy.spawn(cmd.QTILE_CONFIG_RELOAD_CMD, shell=True)),
    Key("M-S-c", lazy.window.kill()),
    Key("M-b", lazy.hide_show_bar()),
    Key("M-n", lazy.next_layout()),
    Key("M-p", lazy.spawncmd()),
    Key("M-<Return>", lazy.spawn(cmd.TERMINAL_CMD)),
    # SHIFT MODE #
    # move windows around with vim keys
    KeyChord(
        [MOD],
        "w",
        [
            Key("h", lazy.layout.shuffle_left()),
            Key("j", lazy.layout.shuffle_down()),
            Key("k", lazy.layout.shuffle_up()),
            Key("l", lazy.layout.shuffle_right()),
            Key("<space>", lazy.layout.next()),
        ],
        name="SHIFT",
        mode=True,
    ),
    # RESIZE MODE #
    # resize windows with vim keys
    KeyChord(
        [MOD],
        "s",
        [
            Key(
                "h",
                lazy.layout.grow_left().when(layout="columns"),
                lazy.layout.shrink_main().when(layout="monadtall"),
            ),
            Key(
                "l",
                lazy.layout.grow_right().when(layout="columns"),
                lazy.layout.grow_main().when(layout="monadtall"),
            ),
            Key(
                "j",
                lazy.layout.grow_down().when(layout="columns"),
                lazy.layout.grow().when(layout="monadtall"),
            ),
            Key(
                "k",
                lazy.layout.grow_up().when(layout="columns"),
                lazy.layout.shrink().when(layout="monadtall"),
            ),
            Key(
                "n",
                lazy.layout.normalize().when(layout="columns"),
                lazy.layout.reset().when(layout="monadtall"),
            ),
            Key("m", lazy.layout.maximize()),
            Key("<space>", lazy.layout.next()),
        ],
        name="RESIZE",
        mode=True,
    ),
    # Media keys
    Key("<XF86MonBrightnessUp>", lazy.spawn(cmd.BRIGHTNESS_UP_CMD)),
    Key("<XF86MonBrightnessDown>", lazy.spawn(cmd.BRIGHTNESS_DOWN_CMD)),
    Key("<XF86AudioMute>", lazy.spawn(cmd.VOLUME_TOGGLE_MUTE_CMD)),
    Key("<XF86AudioRaiseVolume>", lazy.spawn(cmd.VOLUME_UP_CMD)),
    Key("<XF86AudioLowerVolume>", lazy.spawn(cmd.VOLUME_DOWN_CMD)),
    Key("<Print>", lazy.spawn(cmd.SCREENSHOT_CMD)),
    Key("M-<Print>", lazy.spawn(cmd.SCREENSHOT_WITH_SELECTION_CMD)),
    KeyChord(
        [MOD],
        "o",
        [
            Key("e", lazy.spawn(cmd.EDITOR_CMD)),
            Key("h", lazy.spawn(cmd.FILEMANAGER_CMD)),
            Key("w", lazy.spawn(cmd.BROWSER_CMD)),
            Key("p", lazy.spawn(cmd.BROWSER_PRIVATE_CMD)),
            Key("i", lazy.spawn(cmd.WALLPAPERS_DIR_CMD)),
            Key("s", lazy.spawn(cmd.SCREENSHOTS_DIR_CMD)),
            Key("m", lazy.spawn(cmd.MUSIC_CMD)),
            Key("t", lazy.spawn(cmd.MESSENGER_CMD)),
            Key("b", lazy.spawn(cmd.BOOKMARKS_MENU_CMD)),
            Key("d", lazy.spawn(cmd.DOCUMENTS_MENU_CMD)),
            Key("n", lazy.spawn(cmd.WIFI_MENU_CMD)),
        ],
        name="LAUNCH",
    ),
    Key("M-S-l", lazy.spawn(cmd.SCREENLOCK_CMD)),
]


def switch_to_group_or_toggle(group: Group):
    """Switch to group or toggle to previous group, if already there."""
    if group.name == qtile.current_group.name:
        qtile.current_group.screen.previous_group.cmd_toscreen()
    else:
        qtile.groups_map[group.name].cmd_toscreen()


for _group in groups:
    if not _group.name.isdigit():
        continue
    keys.extend(
        [
            Key(
                f"M-{_group.name}",
                lazy.group[_group.name].function(switch_to_group_or_toggle),
                desc=f"Switch to group '{_group.name}' or toggle to previous",
            ),
            Key(
                f"M-S-{_group.name}",
                lazy.window.togroup(_group.name, switch_group=False),
                desc=f"Move window to group '{_group.name}'",
            ),
        ]
    )

mouse = [
    Drag(
        [MOD],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [MOD],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    Click(
        [MOD],
        "Button2",
        lazy.window.bring_to_front(),
    ),
]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = "floating_only"
reconfigure_screens = True
cursor_warp = True
extension_defaults = widget_defaults.copy()
auto_fullscreen = True
auto_minimize = True
focus_on_window_activation = "smart"
wmname = "LG3D"


@hook.subscribe.startup_once
def autostart():
    """Launch applications on startup."""
    script = os.path.expanduser("~/.config/qtile/autostart.sh")
    if os.access(script, os.X_OK):
        subprocess.Popen([script])
