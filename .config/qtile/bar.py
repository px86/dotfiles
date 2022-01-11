'''Defines the bar.'''

from libqtile import bar, widget, qtile
from styles import font, background as bg, foreground as fg

widget_defaults = dict(
    font=font['monospace'],
    fontsize=13,
    padding=4,
)

extension_defaults = widget_defaults.copy()

def _decoration(fg, bg, icon=''):
    '''TextBox widget for arrow style decoration.'''
    return widget.TextBox(
        text=icon,
        fontsize=18,
        padding=-0.1,
        foreground=fg,
        background=bg,
    )

_widgets_left = [
    widget.GroupBox(
        font=font['icon'],
        fontsize=16,
        padding=-2,
        disable_drag=True,
        active='bbbbbb',
        inactive='888888',
        this_current_screen_border='ffffff',
        highlight_method='text',
        urgent_alert_method='text',
    ),

    widget.Chord(
        fmt='CHORD: {} ',
        foreground=fg['chord'],
    ),

    _decoration(icon='', bg=bg['widget'], fg=bg['bar']),

    widget.CurrentLayoutIcon(scale=0.75,background=bg['widget']),

    widget.WindowCount(background=bg['widget'], foreground=fg['windowcount']),

    widget.Prompt(
        ignore_dups_history=True,
        prompt='spawn: ',
        foreground=fg['prompt'],
        background=bg['widget'],
    ),

    _decoration(icon='', fg=bg['widget'], bg=bg['bar']),
]

_widgets_right = [
    widget.Volume(
        fmt='墳 {}',
        mouse_callbacks={
            'Button3': lambda: qtile.cmd_spawn('pavucontrol')
        },
        margin=6,
        update_interval=2,
        foreground=fg['volume'],
    ),

    widget.Backlight(
        backlight_name='intel_backlight',
        backlight_file='brightness',
        max_brightness_file='max_brightness',
        change_command='brightnessctl -c backlight set {}%',
        step=2,
        format='{percent:2.0%}',
        fmt=' {}',
        margin=6,
        update_interval=2,
        foreground=fg['backlight'],
    ),

    _decoration(fg=bg['widget'], bg=bg['bar']),

    widget.Wlan(
        interface='wlp2s0',
        format='直 {essid}:{quality}/70',
        disconnected_message='睊 ',
        mouse_callbacks={
            'Button1': lambda: qtile.cmd_spawn('nmcli networking on'),
            'Button3': lambda: qtile.cmd_spawn('nmcli networking off'),
        },
        update_interval=10,
        foreground=fg['wlan'],
        background=bg['widget'],
    ),

    _decoration(bg=bg['widget'], fg=bg['bar']),

    widget.CPU(
        format='{freq_current}GHz {load_percent}%',
        fmt=' {}',
        padding=6,
        update_interval=8,
        foreground=fg['cpu'],
    ),

    widget.Memory(
        measure_mem='M',
        format='{MemUsed:.0f}mb',
        fmt=' {}',
        mouse_callbacks={
            'Button1': lambda: qtile.cmd_spawn('xterm -e btop')
        },
        update_interval=10,
        foreground=fg['memory'],
    ),

    _decoration(bg=bg['bar'], fg=bg['widget']),

    widget.Battery(
        battery='BAT0',
        charge_char='+',
        discharge_char='',
        format='{percent:2.0%} {char}',
        fmt=' {}',
        foreground=fg['battery'],
        background=bg['widget'],
    ),

    _decoration(fg=bg['bar'], bg=bg['widget']),

    widget.Clock(
        format='%a %d %b %H:%M',
        font=font['sans-serif'],
        fontsize=12,
        foreground=fg['clock'],
    ),

    widget.Systray(),

    widget.QuickExit(
        default_text='⏻',
        countdown_format='{}',
        padding=6,
        foreground=fg['quickexit'],
    ),
]

bar = bar.Bar(
    size=18,
    opacity=1.0,
    background=bg['bar'],
    widgets=_widgets_left + [widget.Spacer()] + _widgets_right,
)
