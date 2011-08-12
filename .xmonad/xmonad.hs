import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import System.IO

import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Util.Run
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders
import XMonad.Prompt.AppendFile
import XMonad.Actions.DwmPromote
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
-- Shift & View
import Control.Monad (liftM2)
import XMonad.Actions.GridSelect
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window

modMask'    = mod4Mask	-- Rebind Mod(ALT) to Windows Key
terminal'   = "gnome-terminal"
workspaces' = ["dev","www","doc"] ++ map show [4..7] ++ ["mov","im"]
layoutHook' = smartBorders (avoidStruts (windowNavigation (ResizableTall 1 (3/100) (1/2) []) ||| tabbed shrinkText defaultTheme ||| Full ))


myManageHook = composeAll
    [
    className =? "Gimp"      --> doF (W.shift "dev")
    -- Browser
    , className =? "Chromium" --> doF (W.shift "dev") -- Chrome
    , className =? "Firefox" --> doF (W.shift "www")
    , className =? "Opera" --> doF (W.shift "www")
    -- DOC
    , className =? "Evince" --> viewShift "doc"
    -- MUSIC
    , className =? "Rhythmbox" --> doF (W.shift "mov")
    , className =? "Totem" --> viewShift "mov"
    , className =? "MPlayer" --> viewShift "mov"
    -- IM
    , className =? "Pidgin" --> doF (W.shift "im")
    , className =? "Skype" --> doF (W.shift "im")
    , className =? "Xchat" --> doF (W.shift "im")
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.conf"
  unsafeSpawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x000000 --height 19 &"
  unsafeSpawn "if [ -x /usr/bin/gnome-terminal ] ; then gnome-terminal & fi"
  unsafeSpawn "if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi"
  unsafeSpawn "if [ -x /usr/bin/gnome-power-manager ] ; then gnome-power-manager & fi"
  unsafeSpawn "if [ -x /usr/bin/xscreensaver ] ; then xscreensaver & fi"
  xmonad $ defaultConfig
    { manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
    , layoutHook = layoutHook'
    , logHook    = dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle  = xmobarColor "green" "" . shorten 50
        }
    , modMask  = modMask'
    , terminal = terminal'
    , borderWidth = 1
    , workspaces = workspaces'
    }
    `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")
    , ("M-o", windows W.focusDown >> kill)
    , ("M-<Return>", dwmpromote >>  windows W.focusDown )	-- Swap the focused window and the master window
    , ("M-p", shellPrompt defaultXPConfig)
    , ("M-<F1>", xmonadPrompt defaultXPConfig)
    , ("M-<F3>", appendFilePrompt defaultXPConfig "/home/jinzhu/TODO")
    , ("M-<F4>", spawn "xscreensaver-command -lock")
    , ("M-S-f", shiftTo Next EmptyWS)
    , ("M-C-S-s", spawn "sudo pm-suspend")

    , ("M-l", sendMessage $ Go R)
    , ("M-h", sendMessage $ Go L)
    , ("M-k", sendMessage $ Go U)
    , ("M-j", sendMessage $ Go D)
    , ("M-S-l", sendMessage Expand)
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-k", sendMessage MirrorExpand)
    , ("M-S-j", sendMessage MirrorShrink)
    , ("M-C-l", sendMessage $ Swap R)
    , ("M-C-h", sendMessage $ Swap L)
    , ("M-C-k", sendMessage $ Swap U)
    , ("M-C-j", sendMessage $ Swap D)
    , ("M-<Right>", nextWS)
    , ("M-<Left>", prevWS)
    , ("M-<Up>", toggleWS)
    , ("M-<Down>", toggleWS)
    , ("M-S-<Right>", shiftToNext >> nextWS)
    , ("M-S-<Left>", shiftToPrev >> prevWS)
    , ("M-g", goToSelected defaultGSConfig)
    , ("M-S-g", windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } )
    , ("M-S-b", windowPromptBring defaultXPConfig)
    ]
