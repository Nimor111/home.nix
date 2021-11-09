import System.IO (hPutStrLn)

import XMonad (
  logHook,
  modMask,
  terminal,
  startupHook,
  borderWidth,
  normalBorderColor,
  focusedBorderColor,
  layoutHook,
  manageHook,
  workspaces,
  mod4Mask,
  xmonad,
  composeAll,
  className,
  (=?),
  (-->),
  doShift,
  (<+>),
  spawn )

import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Hooks.SetWMName (setWMName)

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
    spawnOnce "feh --bg-scale $HOME/wallpaper.jpg &"
    spawnOnce "xautolock -time 5 -locker \"betterlockscreen -l\" &"

-- this is to make enough room for xmobar on the screen
myLayout = avoidStruts $ layoutHook desktopConfig
myManageHook = composeAll
    [ className =? "Brave-browser"          --> doShift ( myWorkspaces !! 1 )
    , className =? "Firefox"                --> doShift ( myWorkspaces !! 1 )
    , className =? "Google-chrome"          --> doShift ( myWorkspaces !! 1 )
    , className =? "vlc"                    --> doShift ( myWorkspaces !! 4 )
    , className =? "jetbrains-idea"         --> doShift ( myWorkspaces !! 2 )
    , isFullscreen                          --> doFullFloat
    ] <+> manageHook desktopConfig <+> manageDocks

myKeys =
    [ ("M-<Return>", spawn myTerminal)
    , ("M-d", spawn "dmenu_run -p 'dmenu:'")
    , ("M-S-l", spawn myLockScreenCommand)
    , ("M-r", spawn "rofi -show run")
    , ("M-w", spawn "rofi -show window")
    , ("M-p", spawn "gopass ls --flat | rofi -dmenu | xargs --no-run-if-empty gopass show -c")
    -- you have to install the light ( or lux ) package for this to work, if on non-nixos system it doesn't work when installed with nix
    , ("<XF86MonBrightnessUp>",   spawn "lux -a 10%")
    , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ("<Print>", spawn "scrot -s")
    ]

myWorkspaces = [ "dev", "www", "code", "emacs", "vpn", "slack", "chat", "book", "game" ]
