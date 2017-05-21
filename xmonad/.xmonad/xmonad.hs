{-# LANGUAGE OverloadedStrings #-}

import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

myTerminal = "gnome-terminal"

modMask' :: KeyMask
modMask' = mod4Mask

myXmonadBar = "/usr/local/bin/lemonbar"

main :: IO ()
main = do
  leftBar <- spawnPipe myXmonadBar
  xmonad $ defaultConfig
    { terminal = myTerminal
    , layoutHook = customLayout
    , manageHook = myManageHook
    , modMask = modMask'
    , logHook = myLogHook leftBar }

customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppOutput = hPutStrLn h
  , ppSort = getSortByXineramaRule }

myManageHook = composeAll [
  resource =? "lemonbar" --> doIgnore ]
