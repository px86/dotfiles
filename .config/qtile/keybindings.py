"""All the keybindings go here."""

from libqtile.config import KeyChord, EzKey as Key
from libqtile.lazy import lazy

from groups import groups

modifier_keys = {
   'M': 'mod4',
   'A': 'mod1',
   'S': 'shift',
   'C': 'control',
}

Mod = 'mod4'

keys = [
    # Shift focus between windows
    Key('M-h', lazy.layout.left()),
    Key('M-j', lazy.layout.down()),
    Key('M-k', lazy.layout.up()),
    Key('M-l', lazy.layout.right()),
    Key('M-<space>', lazy.layout.next()),

    Key('M-<Left>',  lazy.screen.prev_group(skip_empty=True)),
    Key('M-<Right>', lazy.screen.next_group(skip_empty=True)),

    Key('M-f', lazy.window.toggle_maximize()),
    Key('M-S-<space>', lazy.window.toggle_floating()),

    Key('M-C-r', lazy.restart()),
    Key('M-S-q', lazy.shutdown()),
    Key('M-S-c', lazy.window.kill()),

    Key('M-b', lazy.hide_show_bar()),
    Key('M-n', lazy.next_layout()),

    Key('M-p', lazy.spawncmd()),
    Key('M-S-p', lazy.spawn('rofi -show drun')),
    Key('M-<Return>', lazy.spawn('xterm')),

    # SHIFT MODE #
    # move windows around with vim keys
    KeyChord([Mod], 'w', [
        Key('h', lazy.layout.shuffle_left()),
        Key('j', lazy.layout.shuffle_down()),
        Key('k', lazy.layout.shuffle_up()),
        Key('l', lazy.layout.shuffle_right()),
        Key('<space>', lazy.layout.next()),
        ], mode='SHIFT',
    ),

    # RESIZE MODE #
    # resize windows with vim keys
    KeyChord([Mod], 's', [
        Key('h',
            lazy.layout.grow_left().when(layout='columns'),
            lazy.layout.shrink_main().when(layout='monadtall')),
        Key('l',
            lazy.layout.grow_right().when(layout='columns'),
            lazy.layout.grow_main().when(layout='monadtall')),
        Key('j',
            lazy.layout.grow_down().when(layout='columns'),
            lazy.layout.grow().when(layout='monadtall')),
        Key('k',
            lazy.layout.grow_up().when(layout='columns'),
            lazy.layout.shrink().when(layout='monadtall')),
        Key('n',
            lazy.layout.normalize().when(layout='columns'),
            lazy.layout.reset().when(layout='monadtall')),
        Key('m', lazy.layout.maximize()),
        Key('<space>', lazy.layout.next()),
        ], mode='RESIZE',
    ),

    # toggle dropdown terminal
    Key('M-<grave>', lazy.group['scratchpad'].dropdown_toggle('term')),

    # Media keys
    Key('<XF86MonBrightnessUp>',   lazy.spawn('brightnessctl -c backlight set 1%+')),
    Key('<XF86MonBrightnessDown>', lazy.spawn('brightnessctl -c backlight set 1%-')),

    Key('<XF86AudioMute>',        lazy.spawn('pulsemixer --toggle-mute')),
    Key('<XF86AudioRaiseVolume>', lazy.spawn('pulsemixer --change-volume +2')),
    Key('<XF86AudioLowerVolume>', lazy.spawn('pulsemixer --change-volume -2')),

    Key('<Print>',   lazy.spawn('screenshot')),
    Key('M-<Print>', lazy.spawn('screenshot -s')),

    # Launch applications
    KeyChord([Mod], 'o', [
        Key('e', lazy.spawn('emacsclient -ca ""')),
        Key('h', lazy.spawn('spacefm')),
        Key('w', lazy.spawn('brave-browser')),
        Key('p', lazy.spawn('brave-browser --incognito')),
        Key('i', lazy.spawn('sxiv -bt ~/Pictures/Wallpapers/',  shell=True)),
        Key('s', lazy.spawn('sxiv -bt ~/Pictures/Screenshots/', shell=True)),
        Key('m', lazy.spawn('deadbeef')),
        Key('f', lazy.spawn('walc')),
        Key('t', lazy.spawn('telegram')),
        Key('b', lazy.spawn('bmark')),
        Key('d', lazy.spawn('dox')),
    ]),
    # Lock the screen
    Key('M-S-l', lazy.spawn('slock -m "Locked at $(date)"', shell=True)),
]

for group in groups:
    i = group.name
    if len(i) == 1:
        print(f'Group name: {i}')
        keys.extend([
            Key(f'M-{i}',   lazy.group[i].toscreen()),
            Key(f'M-S-{i}', lazy.window.togroup(i, switch_group=False))
        ])
