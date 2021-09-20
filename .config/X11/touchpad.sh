#!/bin/sh

TOUCHPAD_DEVICE="$(xinput list --name-only | grep -i 'Touchpad')"

[ -z "$TOUCHPAD_DEVICE" ] && exit 1

xinput enable "$TOUCHPAD_DEVICE" 
xinput --set-prop "$TOUCHPAD_DEVICE" "libinput Tapping Enabled" 1
xinput --set-prop "$TOUCHPAD_DEVICE" "libinput Natural Scrolling Enabled" 1
xinput --set-prop "$TOUCHPAD_DEVICE" "libinput Accel Speed" 0.5
