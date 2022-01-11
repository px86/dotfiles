'''px86's qtile configuration.'''

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

from libqtile import layout, hook
from libqtile.lazy import lazy
from libqtile.config import EzKey as Key
from libqtile.config import Click, Drag, Screen, Match

from keybindings import keys, Mod, modifier_keys
from groups import groups
from styles import background as bg
from bar import bar, widget_defaults, extension_defaults

# -----------------------------------------------------------------------
# LAYOUTS
# -----------------------------------------------------------------------

layouts = [
    layout.MonadTall(
        align=layout.MonadTall._left,
        border_focus=bg['border'],
        border_normal='#000000',
        border_width=2,
        ratio=0.65,
        change_ratio=0.05,
        change_size=20,
        margin=0,
        max_ratio=0.75,
        min_ratio=0.25,
        min_secondary_size=85,
        new_client_position='after_current',
        single_border_width=0,
        single_margin=None,
    ),
    layout.Columns(
        border_focus=bg['border'],
        border_normal='#000000',
        fair=False,
        num_columns=2,
        border_width=2,
        border_on_single=False,
        margin=0,
        margin_on_single=0,
    ),
    layout.Max(),
]

# SCREEN #
screens = [Screen(top=bar)]

mouse = [
    Drag([Mod], 'Button1', lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([Mod], 'Button3', lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([Mod], 'Button2', lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True

floating_layout = layout.Floating(
    border_width=3,
    border_focus='#cd69c9',
    border_normal='#000000',
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class='GNU Octave'),
        Match(wm_class='Gcolor3'),
        Match(wm_class='confirmreset'),  # gitk
        Match(wm_class='makebranch'),    # gitk
        Match(wm_class='maketag'),       # gitk
        Match(wm_class='ssh-askpass'),   # ssh-askpass
        Match(title='branchdialog'),     # gitk
        Match(title='pinentry'),         # GPG key password entry
    ])

auto_fullscreen = True
focus_on_window_activation = 'smart'
wmname = 'LG3D'

# --------------------------------------------------------------------------
# END
# --------------------------------------------------------------------------
