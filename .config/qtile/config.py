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
from bar import bar, widget_defaults

# -----------------------------------------------------------------------
# LAYOUTS
# -----------------------------------------------------------------------

layouts = [
    layout.MonadTall(
        align=layout.MonadTall._left,
        border_focus=bg['border'],
        border_normal=bg['bar'],
        border_width=2,
        ratio=0.60,
        change_ratio=0.05,
        change_size=20,
        max_ratio=0.75,
        min_ratio=0.25,
        min_secondary_size=85,
        new_client_position='after_current',
        single_border_width=0,
        margin=0,
        single_margin=0,
    ),
    layout.Columns(
        border_focus=bg['border'],
        border_normal=bg['bar'],
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

floating_layout = layout.Floating(
    border_width=3,
    border_focus=bg['border'],
    border_normal=bg['bar'],
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class='GNU Octave'),
        Match(wm_class='Gcolor3'),
        Match(wm_class='confirmreset'),  # gitk
        Match(wm_class='makebranch'),    # gitk
        Match(wm_class='maketag'),       # gitk
        Match(wm_class='ssh-askpass'),   # ssh-askpass
        Match(wm_class='pinentry'),      # GPG key password entry
        Match(title='branchdialog'),     # gitk
        Match(title='float-me'),         # Float certain TUI applications
    ])

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = 'floating_only'
reconfigure_screens = True
cursor_warp = True
extension_defaults = widget_defaults.copy()
auto_fullscreen = True
auto_minimize = True
focus_on_window_activation = 'smart'
wmname = 'LG3D'

# --------------------------------------------------------------------------
# END
# --------------------------------------------------------------------------
