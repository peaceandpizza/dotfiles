import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Hooks.SetWMName
import XMonad.Actions.SpawnOn
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import System.IO
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers

myLayout = tiled ||| tiled2 ||| tiled3 ||| Mirror tiled ||| Full
 where
       -- default tiling algorithm partitions the screen into two panes
       tiled = spacing 2 $ Tall nmaster delta ratio
       tiled2 = spacing 2 $ Tall 2 delta ratio
       tiled3 = spacing 2 $ ThreeColMid nmaster delta (1/3)
                 
       -- The default number of windows in the master pane
       nmaster = 1
                                
       -- Default proportion of screen occupied by master pane
       ratio = toRational (2/(1 + sqrt 5 :: Double)) 
                                               
       -- Percent of screen to increment by when resizing panes
       delta = 5/100

myManageHook = composeOne
    [ className =? "Steam"          -?> doF (W.shift "4")
    , className =? "Deluge"          -?> doF (W.shift "9")
    ]

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/pedro/.xmobarrc"
xmonad $ defaultConfig
    {borderWidth = 2
    ,normalBorderColor = "#7A7A7A"
    ,focusedBorderColor = "#C9F76F"
    }
    { startupHook = do
	setWMName "LG3D"
	spawnOn "1" "feh --bg-center /home/pedro/.xmonad/monsoon.jpg"
    }
    { manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> insertPosition End Newer <+> manageHook defaultConfig <+> myManageHook
    , layoutHook = avoidStruts $ myLayout
    , logHook = dynamicLogWithPP xmobarPP


                        { ppOutput = hPutStrLn xmproc
                        ,   ppCurrent = xmobarColor "#679B00" "#9FEE00" . pad
                        , ppVisible = xmobarColor "#9FEE00" "#679B00" . pad
                        , ppHidden  = xmobarColor "#9FEE00" "#679B00" . pad
                        , ppSep = " "
                        , ppTitle = xmobarColor "#C9F76F" "" . shorten 90
			                  , ppOrder = \(ws:_:t:_) -> [ws, t]
                        }
        , modMask = mod4Mask
        , terminal = "xfce4-terminal"
        
    } `additionalKeys`
    [((0, 0x1008FF02), spawn "xbacklight +20")
	, ((0, 0x1008FF03), spawn "xbacklight -20")  	
	, ((0, xK_Print), spawn "scrot -q 100 /home/pedro/Pictures/Screenshots/screenshot$(date +%m%d%Y%H%M%S).jpg")
	, ((0                     , 0x1008FF11), spawn "amixer set Master 2%-"),
	((0                     , 0x1008FF13), spawn "amixer set Master 2%+"),
	((0                     , 0x1008FF12), spawn "amixer -c 0 sset Master toggle"),
	((mod4Mask, xK_Left), spawn "xrandr --output VGA1 --rotate left"),
	((0                     , 0x1008FF14), spawn "mpc toggle"),
	((0                     , 0x1008FF15), spawn "mpc stop"),
	((0                     , 0x1008FF16), spawn "mpc prev"),
	((0                     , 0x1008FF17), spawn "mpc next"),
	((mod4Mask, xK_Up), spawn "xrandr --output VGA1 --rotate normal"),

	((0                     , 0x1008FFA9), spawn "sh ~/.xmonad/toggletouchpad.sh"),
  ((mod4Mask, xK_p), spawn "dmenu_run -i -f -sf '#679B00' -sb '#9FEE00' -nf '#C9F76F'")
    ]
