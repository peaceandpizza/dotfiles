xsetroot -solid midnightblue
xsetroot -cursor_name left_ptr
setxkbmap -layout br
xrandr --output VGA1 --above LVDS1
mpd &
exec xmonad
