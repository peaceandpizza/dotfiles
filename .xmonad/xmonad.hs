import XMonad
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
import XMonad.Hooks.Place
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Operations
import System.Exit
import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import Data.Ratio ((%))
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ defaultConfig
      { startupHook = do
      setWMName "LG3D"
      spawnOn "1" "feh --bg-center /home/pedro/.xmonad/bruce.png"
      }
      {manageHook = placeHook simpleSmart <+> manageDocks <+> insertPosition End Newer <+> manageHook'
      , layoutHook = avoidStruts $ myLayout
      , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      , modMask = mod4Mask
      , terminal = "xfce4-terminal"
      , borderWidth = 2
      , normalBorderColor = colorNormalBorder
      , focusedBorderColor = colorFocusedBorder
}`additionalKeys`
      [((0, 0x1008FF02), spawn "xbacklight +20")
      , ((0, 0x1008FF03), spawn "xbacklight -20")
      , ((0, xK_Print), spawn "scrot -q 100 /home/pedro/Pictures/Screenshots/screenshot$(date +%m%d%Y%H%M%S).jpg")
      , ((mod1Mask , xK_F11), spawn "amixer set Master 2%-"),
      ((mod1Mask , xK_F12), spawn "amixer set Master 2%+"),
      ((mod1Mask , xK_F10), spawn "amixer -c 0 sset Master toggle"),
      ((mod4Mask , xK_Left), spawn "xrandr --output VGA1 --rotate left"),
      ((mod1Mask , xK_Home), spawn "mpc toggle"),
      ((mod1Mask , xK_End), spawn "mpc stop"),
      ((mod1Mask , xK_Page_Up), spawn "mpc prev"),
      ((mod1Mask , xK_Page_Down), spawn "mpc next"),
      ((mod4Mask , xK_Up), spawn "xrandr --output VGA1 --rotate normal"),
      ((0 , 0x1008FFA9), spawn "sh ~/.xmonad/toggletouchpad.sh"),
      ((mod4Mask, xK_p), spawn "dmenu_run -i -f -sf '#A64D00' -sb '#FF7600' -nf '#34C6CD'")
      ]

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

myXmonadBar = "dzen2 -x '0' -y '0' -e 'button2=;' -h '15' -w '1000' -ta 'l' -fg '#34C6CD' -bg '#383838' -fn 'Clean-10'"
myStatusBar = "conky -c ~/.xmonad/.conky_dzen | dzen2 -e 'button2=;' -x '1000' -w '920' -h '15' -ta 'r' -bg '#383838' -fg '#34C6CD' -y '0' -fn 'Clean-10'"

manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 
 
    where
 
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        -- classnames
        myFloats  = ["Downloads"]
 
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
 
        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#A64D00" "#FF7600" . pad
      , ppVisible           =   dzenColor "#FF7600" "#A64D00" . pad
      , ppHidden            =   dzenColor "#FF7600" "#A64D00" . pad
      , ppUrgent            =   dzenColor "#FF7600" "#A64D00" . pad
      , ppWsSep             =   " "
      , ppSep               =   " "
      , ppLayout            =   dzenColor "#ebac54" "#383838" .
                                (\x -> case x of
                                    _                           ->      "-"
                                )
      , ppTitle             =   (" " ++) . dzenColor "#34C6CD" "#383838" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

colorOrange         = "#FF7600"
colorDarkGray       = "#383838"
colorPink           = "#BF3330"
colorGreen          = "#5DC8DC"
colorBlue           = "#34C6CD"
colorYellow         = "#FFAB00"
colorWhite          = "#FFB473"
 
colorNormalBorder   = "#7A7A7A"
colorFocusedBorder  = "#FF7600"
 
barFont  = "inconsolata:size=9"
barXFont = "inconsolata:size=9"
xftFont = "xft: inconsolata-9"

mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }
 
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 22
                }