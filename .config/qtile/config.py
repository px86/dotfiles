"""px86's qtile configuration."""

#
#     __ _| |_(_) | ___
#    / _` | __| | |/ _ \
#   | (_| | |_| | |  __/
#    \__, |\__|_|_|\___|
#       |_|
#

# ---------------------------------------------------------------------------
# IMPORTS
# ---------------------------------------------------------------------------

from libqtile import bar, layout, widget
from libqtile.lazy import lazy
from libqtile.config import (Click, Drag, Screen, Key, KeyChord,
                             Group, Match, DropDown, ScratchPad)

# ---------------------------------------------------------------------------
# SOME VARIABLES
# ---------------------------------------------------------------------------

MOD = "mod4"
TERMINAL = "xterm"

# fonts
SANS = 'Source Sans Pro Bold'
MONO = 'Caskaydia Cove Nerd Font'

# dark colors
CD1 = '#282A36'
CD2 = '#2e3440'
CD3 = '#3b4252'

# light colors
CL1 = '#a9def9'
CL2 = '#5af78e'

# ---------------------------------------------------------------------------
# GROUPS (workspaces)
# ---------------------------------------------------------------------------

groups = [
    ScratchPad(
        name="scratchpad",
        dropdowns=[
            DropDown(
                name="term",
                cmd="xterm -T dropdown",
                x=0,
                y=0,
                width=1.0,
                height=0.6,
                opacity=0.95,
            ),
        ]),
    Group("1", label='', layout='max'),
    Group("2", label='', matches=[
        Match(wm_class=[
            "Zathura",
            "libreoffice",
        ]),
    ]),
    Group("3", label='', layout='max', matches=[
        Match(wm_class=[
            "firefox",
            "Firefox-esr",
            "Brave-browser",
        ])
    ]),
    Group("4", label='', matches=[
        Match(wm_class=[
            "deadbeef",
        ])
    ]),
    Group("5", label=''),
    Group("6", label='', matches=[
        Match(wm_class=[
            "gimp.bin",
        ]),
    ]),
    Group("7", label='', matches=[
        Match(wm_class=[
            "walc",
            "TelegramDesktop",
        ]),
    ]),
    Group("8", label=''),
]  # 'groups' end

# ---------------------------------------------------------------------------
# KEY BINDINGS
# ---------------------------------------------------------------------------

keys = [
    # Shift focus between windows
    Key([MOD], "h", lazy.layout.left()),
    Key([MOD], "j", lazy.layout.down()),
    Key([MOD], "k", lazy.layout.up()),
    Key([MOD], "l", lazy.layout.right()),
    Key([MOD], "space", lazy.layout.next()),

    Key([MOD], "Left",  lazy.screen.prev_group(skip_empty=False)),
    Key([MOD], "Right", lazy.screen.next_group(skip_empty=False)),

    Key([MOD], "f", lazy.window.toggle_fullscreen()),
    Key([MOD, "shift"], "space", lazy.window.toggle_floating()),

    Key([MOD, "control"], "r", lazy.restart()),
    Key([MOD, "shift"],   "q", lazy.shutdown()),
    Key([MOD, "shift"],   "c", lazy.window.kill()),

    Key([MOD], "b", lazy.hide_show_bar()),
    Key([MOD], "n", lazy.next_layout()),

    Key([MOD], "p", lazy.spawncmd()),
    Key([MOD], "Return", lazy.spawn(TERMINAL)),

    # SHIFT MODE #
    # move windows around with vim keys
    KeyChord([MOD], "w", [
        Key([], "h", lazy.layout.shuffle_left()),
        Key([], "j", lazy.layout.shuffle_down()),
        Key([], "k", lazy.layout.shuffle_up()),
        Key([], "l", lazy.layout.shuffle_right()),
        Key([], "space", lazy.layout.next()),
        ], mode="SHIFT",
    ),

    # RESIZE MODE #
    # resize windows with vim keys
    KeyChord([MOD], "s", [
        Key([], "h", lazy.layout.grow_left()),
        Key([], "j", lazy.layout.grow_down()),
        Key([], "k", lazy.layout.grow_up()),
        Key([], "l", lazy.layout.grow_right()),
        Key([], "n", lazy.layout.normalize()),
        Key([], "space", lazy.layout.next()),
        ], mode="RESIZE",
    ),

    # toggle dropdown terminal
    Key([MOD], 'grave',
        lazy.group['scratchpad'].dropdown_toggle('term')),
]

# switching groups, moving windows to other groups
for group in groups:
    i = group.name
    if len(i)==1:
        keys.extend([
            Key([MOD], i, lazy.group[i].toscreen()),
            
            Key([MOD, "shift"], i,
                lazy.window.togroup(i, switch_group=False)),
        ])

# -----------------------------------------------------------------------
# LAYOUTS
# -----------------------------------------------------------------------

layouts = [
    layout.Columns(
        fair=False,
        num_columns=2,
        border_width=2,
        border_focus='#ff3377',
        border_normal=CD1,
        border_on_single=False,
        margin=0,
        margin_on_single=0,
    ),
    layout.Max(),
    layout.Floating(
        border_width=2,
        border_focus='#ff0077',
    ),
]

# --------------------------------------------------------------------------
# widgets and bar
# --------------------------------------------------------------------------

# widget defaults #
widget_defaults = dict(
    font=MONO,
    fontsize=14,
    padding=4,
)

extension_defaults = widget_defaults.copy()

# --------------------------------------------------------------------------

def decoration(fg, bg):
    '''TextBox widget for arrow style decoration.'''
    return widget.TextBox(
            text='',
            fontsize=18,
            padding=-0.1,
            foreground=fg,
            background=bg,
        )


main_bar = bar.Bar(
    size=20,
    opacity=1.0,
    background=CD1,

    # WIDGETS #
    widgets=[
        widget.GroupBox(
            font=MONO,
            fontsize=18,
            padding=1,
            disable_drag=True,
            active='#bbbbbb',
            inactive='#888888',
            this_current_screen_border='#ffffff',
            highlight_method='text',
            urgent_alert_method='text',
        ),

        widget.CurrentLayoutIcon(scale=0.75),

        widget.WindowCount(),

        widget.Chord(
            fmt='CHORD: {} ',
            font=f'{MONO} Bold',
            foreground=CL2,
        ),

        widget.Prompt(
            ignore_dups_history=True,
            font=f'{MONO} Bold',
            prompt='>_ ',
            foreground=CL1,
        ),

        widget.Spacer(),

        decoration(fg=CD3, bg=CD1),

        widget.Wlan(
            interface='wlp2s0',
            format='直 {essid}:{quality}/70',
            disconnected_message='睊 none',
            font=f'{MONO} Bold',
            update_interval=10,
            foreground=CL1,
            background=CD3,
        ),

        decoration(bg=CD3, fg=CL1),

        widget.Memory(
            measure_mem='M',
            format='{MemUsed:.0f}mb',
            fmt=' {}',
            update_interval=10.0,
            foreground=CD1,
            background=CL1,
        ),

        widget.Battery(
            battery='BAT0',
            charge_char='+',
            discharge_char='',
            format='{percent:2.0%} {char}',
            fmt=' {}',
            update_interval=60,
            foreground=CD1,
            background=CL1,
        ),

        decoration(fg=CD1, bg=CL1),

        widget.Clock(
            format='%A %d %b %H:%M',
            font=SANS,
            foreground=CL1,
        ),

        widget.Systray(),

        widget.QuickExit(
            default_text='⏻',
            countdown_format='{}',
            padding=6,
            foreground='#bf616a', 
        ),
    ],
)

# SCREEN #
screens = [Screen(top=main_bar)]

# --------------------------------------------------------------------------

mouse = [
    Drag([MOD], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([MOD], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([MOD], "Button2", lazy.window.bring_to_front()),
]

# --------------------------------------------------------------------------

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True

# --------------------------------------------------------------------------

floating_layout = layout.Floating(float_rules=[
    *layout.Floating.default_float_rules,
    Match(wm_class='GNU Octave'),
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),    # gitk
    Match(wm_class='maketag'),       # gitk
    Match(wm_class='ssh-askpass'),   # ssh-askpass
    Match(title='branchdialog'),     # gitk
    Match(title='pinentry'),         # GPG key password entry
])

# --------------------------------------------------------------------------

auto_fullscreen = True
focus_on_window_activation = "smart"
wmname = "LG3D"

# --------------------------------------------------------------------------
# END
# --------------------------------------------------------------------------
