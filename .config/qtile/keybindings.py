"""All the keybindings go here."""

from libqtile import qtile
from libqtile.config import KeyChord
from libqtile.config import EzKey as Key
from libqtile.config import Group
from libqtile.lazy import lazy

from groups import groups

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
    Key("M-C-r", lazy.restart()),
    Key("M-S-q", lazy.shutdown()),
    Key("M-S-c", lazy.window.kill()),
    Key("M-b", lazy.hide_show_bar()),
    Key("M-n", lazy.next_layout()),
    Key("M-p", lazy.spawncmd()),
    Key("M-S-p", lazy.spawn("rofi -show drun")),
    Key("M-<Return>", lazy.spawn("xterm")),
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
    # toggle dropdown terminal
    Key("M-<grave>", lazy.group["scratchpad"].dropdown_toggle("term")),
    # Media keys
    Key("<XF86MonBrightnessUp>", lazy.spawn("brightnessctl -c backlight set 1%+")),
    Key("<XF86MonBrightnessDown>", lazy.spawn("brightnessctl -c backlight set 1%-")),
    Key("<XF86AudioMute>", lazy.spawn("pulsemixer --toggle-mute")),
    Key("<XF86AudioRaiseVolume>", lazy.spawn("pulsemixer --change-volume +2")),
    Key("<XF86AudioLowerVolume>", lazy.spawn("pulsemixer --change-volume -2")),
    Key("<Print>", lazy.spawn("screenshot")),
    Key("M-<Print>", lazy.spawn("screenshot -s")),
    # Launch applications
    KeyChord(
        [MOD],
        "o",
        [
            Key("e", lazy.spawn('emacsclient -ca ""')),
            Key("h", lazy.spawn("spacefm")),
            Key("w", lazy.spawn("brave-browser")),
            Key("p", lazy.spawn("brave-browser --incognito")),
            Key("i", lazy.spawn("sxiv -bt ~/Pictures/Wallpapers/", shell=True)),
            Key("s", lazy.spawn("sxiv -bt ~/Pictures/Screenshots/", shell=True)),
            Key("m", lazy.spawn("deadbeef")),
            Key("f", lazy.spawn("walc")),
            Key("t", lazy.spawn("telegram")),
            Key("b", lazy.spawn("bmark")),
            Key("d", lazy.spawn("dox")),
        ],
        name="LAUNCH",
    ),
    # Lock the screen
    Key("M-S-l", lazy.spawn('slock -m "Locked at $(date)"', shell=True)),
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
