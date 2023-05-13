"""Groups and dropdowns for my Qtile configuration."""

from libqtile.config import Group
from libqtile.config import ScratchPad
from libqtile.config import DropDown
from libqtile.config import Match

# Window class names
_web    = ["firefox", "Firefox-esr", "brave-browser", "chromium"]
_docs   = ["Zathura", "libreoffice"]
_music  = ["deadbeef"]
_pics   = ["gimp.bin"]
_comm   = ["walc", "TelegramDesktop"]

groups: list[Group] = [
    Group("1", label=""),
    Group("2", label="", matches=[Match(wm_class=_web)]),
    Group(
        "3",
        label="",
        matches=[Match(wm_class=_docs)],
    ),
    Group("4", label="", matches=[Match(wm_class=_music)]),
    Group("5", label=""),
    Group(
        "6",
        label="",
        layout="max",
        matches=[Match(wm_class=_pics)],
    ),
    Group(
        "7",
        label="",
        matches=[Match(wm_class=_comm)],
    ),
    ScratchPad(
        name="scratchpad",
        dropdowns=[
            DropDown(
                name="term",
                cmd="xterm -T dropdown -fullscreen",
                x=0,
                y=0,
                width=1,
                height=1,
                opacity=0.95,
            ),
        ],
    ),
]
