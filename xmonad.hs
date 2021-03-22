import System.IO (hPutStrLn)

import XMonad

import XMonad.Config.Desktop (desktopConfig)

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Hooks.SetWMName

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)


myTerminal = "termonad"
myModMask = mod4Mask
myBorderWidth = 2
myNormalBorderColor = "#292d3e"
myFocusedBorderColor = "#ffb86c"
myFont = "xft:mononoki Nerd Font Mono:regular:pixelsize=16"
myLockScreenCommand = "~/.local/bin/betterlockscreen -l"

main :: IO ()
main = do
    xmobarProc <- spawnPipe "xmobar -x 0 ~/.config/xmobar/.xmobarrc"

    xmonad $ desktopConfig
        { logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmobarProc
            , ppTitle = xmobarColor "#ffa500" "" . shorten 20
            }
            , modMask = myModMask
            , terminal = myTerminal
            , startupHook = myStartupHook
            , borderWidth = myBorderWidth
            , normalBorderColor  = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , layoutHook = myLayout
            , manageHook = myManageHook
            , workspaces = myWorkspaces
        } `additionalKeysP` myKeys

myStartupHook = do
    -- You'll need to set your WM name to "LG3D" for Java Swing apps to work
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr
    spawnOnce "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,bg -variant ,phonetic &"
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x292d3e --height 18 &"
    spawnOnce "nextcloud-client &"
    spawnOnce "feh --bg-scale $HOME/wallpaper.png &"

-- this is to make enough room for xmobar on the screen
myLayout = avoidStruts $ layoutHook desktopConfig
myManageHook = composeAll
    [ className =? "Brave-browser" --> doShift ( myWorkspaces !! 1 )
    , className =? "Firefox"       --> doShift ( myWorkspaces !! 1 )
    , className =? "vlc"           --> doShift ( myWorkspaces !! 4 )
    ] <+> manageHook desktopConfig <+> manageDocks

myKeys =
    [ ("M-<Return>", spawn myTerminal)
    , ("M-d", spawn "dmenu_run -p 'dmenu:'")
    , ("M-l", spawn myLockScreenCommand)
    , ("M-r", spawn "rofi -show run")
    , ("M-w", spawn "rofi -show window")
    ]

myWorkspaces = [ "dev", "www", "code", "emacs", "vpn", "slack", "chat", "book", "game" ]
