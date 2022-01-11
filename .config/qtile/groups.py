'''Groups and dropdowns for my Qtile WM session.'''

from libqtile.config import Group, ScratchPad, DropDown, Match

# Window class names
_browsers = ['firefox', 'Firefox-esr', 'brave-browser', 'chromium']
_communication = ['walc', 'TelegramDesktop']

groups = [
    Group('1', label=''),
    Group('2', label='', matches=[
        Match(wm_class=_browsers)
    ]),
    Group('3', label='', matches=[
        Match(wm_class=['Zathura', 'libreoffice']),
    ]),
    Group('4', label='', matches=[
        Match(wm_class=['deadbeef'])
    ]),
    Group('5', label=''),
    Group('6', label='', layout='max', matches=[
        Match(wm_class=['gimp.bin']),
    ]),
    Group('7', label='', matches=[
        Match(wm_class=_communication),
    ]),
    ScratchPad(name='scratchpad', dropdowns=[
        DropDown(name='term', cmd='xterm -T dropdown', x=0.1, y=0.1,
                 width=0.8, height=0.8, opacity=0.90),
    ]),
]
