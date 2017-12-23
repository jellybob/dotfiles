{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Concurrent
import qualified Data.Map as M
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Pass
import XMonad.StackSet as W
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedScratchpad

main :: IO ()
main = do
  xmobar <- spawnPipe "xmobar"
  xmonad $ docks $ fullscreenSupport $ defaultConfig
    { terminal = myTerminal
    , layoutHook = myLayout
    , handleEventHook = myEventHook
    , manageHook = manageDocks <+> namedScratchpadManageHook scratchpads <+> myManageHook
    , modMask = modMask'
    , logHook = myLogHook xmobar
    , focusedBorderColor = "#268bd2"
    , keys = \c -> myKeys c `M.union` keys defaultConfig c }

-- Basic Config
modMask' :: KeyMask
modMask' = mod4Mask

myTerminal = "konsole"

myXPConfig = defaultXPConfig
  { position = Bottom
  , font = "xft:Fira Code Medium-16"
  , height = 28
  }

-- Key Bindings
myKeys conf@(XConfig {modMask = modm}) = M.fromList $
  [
    ((modm, xK_p), shellPrompt myXPConfig),
    ((modm .|. shiftMask, xK_r), renameWorkspace myXPConfig),
    ((modm, xK_w), gotoMenuArgs ["-fn", "'Fira Code Medium-16'", "-b"]),
    ((modm .|. shiftMask, xK_d), namedScratchpadAction scratchpads "Daylog"),
    ((modm .|. shiftMask, xK_s), namedScratchpadAction scratchpads "Scratch Terminal"),
    ((modm .|. shiftMask, xK_l), unsafeSpawn "xlock -mode blank & systemctl suspend"),
    ((modm .|. shiftMask, xK_t), sendMessage ToggleStruts),
    ((modm, xK_u), prevScreen),
    ((modm .|. shiftMask, xK_u), shiftPrevScreen >> prevScreen),
    ((modm, xK_i), nextScreen),
    ((modm .|. shiftMask, xK_i), shiftNextScreen >> nextScreen),
    ((modm, xK_b), unsafeSpawn "google-chrome"),
    ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ '-1.5%'"),
    ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ '+1.5%'"),
    ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ((0, xF86XK_AudioPlay), spawn "dbus-send --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
  ]

-- Named Scratchpads
scratchpads = 
  [
    NS "Scratch Terminal" "gnome-terminal --role 'Scratch'" (role =? "Scratch") centeredFloating,
    NS "Ping" "gnome-terminal -e 'ping 8.8.8.8' --role 'Ping'" (role =? "Ping") centeredFloating
  ] where role = stringProperty "WM_WINDOW_ROLE"
centeredFloating = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

-- Layouts
myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

-- Window Management
myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , className =? "ffplay" --> doFloatAt (1/50) (1/50) ]

-- Log Output

myLogHook h = workspaceNamesPP xmobarPP
  { ppOutput           = hPutStrLn h
  , ppSort             = getSortByXineramaRule
  , ppCurrent          = wrap "<fc=#859900>" "</fc>"
  , ppVisible          = wrap "<fc=#268bd2>" "</fc>"
  , ppHidden           = wrap "" ""
  , ppTitle            = wrap "<fc=#93a1a1>" "</fc>"
  , ppSep              = " "
  , ppLayout           = hideLogSection
  } >>= dynamicLogWithPP
  where
    hideLogSection s = ""

-- Utility Stuff
myStartupHook = do
  startupHook defaultConfig
  -- Make Java apps happier
  setWMName "LG3D"

myEventHook = do
  docksEventHook
  XMonad.Hooks.EwmhDesktops.fullscreenEventHook
