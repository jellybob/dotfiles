{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Concurrent
import qualified Data.Map as M
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WorkspaceNames
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
import XMonad.StackSet as W
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedScratchpad

main :: IO ()
main = do
  statusline <- spawnPipe lemonbar
  powerline <- spawnPipe powerline
  xmonad $ fullscreenSupport $ defaultConfig
    { terminal = myTerminal
    , layoutHook = myLayout
    , handleEventHook = myEventHook
    , manageHook = manageDocks <+> namedScratchpadManageHook scratchpads <+> myManageHook
    , modMask = modMask'
    , logHook = myLogHook statusline
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
    ((modm .|. shiftMask, xK_d), namedScratchpadAction scratchpads "Daylog"),
    ((modm .|. shiftMask, xK_e), namedScratchpadAction scratchpads "Enpass"),
    ((modm .|. shiftMask, xK_s), namedScratchpadAction scratchpads "Scratch Terminal"),
    ((modm .|. shiftMask, xK_l), unsafeSpawn "xlock -mode blank"),
    ((modm .|. shiftMask, xK_n), unsafeSpawn "/home/jon/bin/play-neocam"),
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
    NS "Daylog" "gnome-terminal -e 'vim /home/jon/Neos/daylog.md' --role 'Daylog'" (role =? "Daylog") centeredFloating,
    NS "Enpass" "/home/jon/.Enpass/runenpass.sh" (title =? "Enpass") centeredFloating,
    NS "Scratch Terminal" "gnome-terminal --role 'Scratch'" (role =? "Scratch") centeredFloating,
    NS "Ping" "gnome-terminal -e 'ping 8.8.8.8' --role 'Ping'" (role =? "Ping") centeredFloating
  ] where role = stringProperty "WM_WINDOW_ROLE"
centeredFloating = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

-- Status bar
powerline = "python3 /usr/lib/python3.5/site-packages/powerline/bindings/lemonbar/powerline-lemonbar.py -- -f 'Fira Code Medium:size=16' -b -g 1000x28+1560+0"
lemonbar = "lemonbar -f 'Fira Code Medium:size=16' -b -g '1000x28+0+0'"

-- Layouts
myLayout = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

-- Window Management
myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , className =? "ffplay" --> doFloatAt (1/50) (1/50) ]

-- Log Output

-- TODO: Powerline style bars with some text replacement. The idea is to replace
-- each combination of characters with the appropriate colour/character pairs.
--
-- ex - :::: gets remove entirely as we need no seperator between hidden workspaces
--    - !! gets replaced with a "green background, white foreground" code
--    - ``:: gets replaced with "blue foreground, grey background", "pointer", "grey background, black foreground" (or whatever)
--

myLogHook h = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ defaultPP
  { ppOutput           = hPutStrLn h
  , ppSort             = getSortByXineramaRule
  , ppLayout           = hideLogSection
  , ppCurrent          = wrap "<" ">"
  , ppVisible          = wrap "[" "]"
  , ppHidden           = wrap "" ""
  , ppHiddenNoWindows  = hideLogSection
  , ppWsSep            = " " 
  , ppSep              = " | " 
  }
  where
    hideLogSection s = ""

-- Utility Stuff
myStartupHook = do
  startupHook defaultConfig
  -- Make Java apps happier
  setWMName "LG3D"
  -- Work around struts not being handled on workspace 1
  takeTopFocus
  liftIO $ Control.Concurrent.threadDelay 1000000
  sendMessage ToggleStruts
  sendMessage ToggleStruts

myEventHook = do
  docksEventHook
  XMonad.Hooks.EwmhDesktops.fullscreenEventHook
