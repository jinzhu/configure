import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Util.Run
import XMonad.Util.Dmenu
-- import XMonad.Util.Loggers
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Prompt.AppendFile
import XMonad.Actions.DwmPromote
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout
import XMonad.Layout.Tabbed


modMask'    = mod4Mask	-- Rebind Mod(ALT) to Windows Key
terminal'   = "gnome-terminal"
workspaces' = ["dev","www","doc"] ++ map show [4..7] ++ ["mov","im"]
layoutHook' = avoidStruts (tall ||| tabbed shrinkText defaultTheme ||| Full )

tall = Tall 1 (3/100) (1/2)
myManageHook = composeAll
    [ className =? "Gimp"      --> doCenterFloat
    -- Browser
    , className =? "Gran Paradiso" --> doF (W.shift "www") -- Firefox On Arch
    , className =? "Firefox" --> doF (W.shift "www")
    , className =? "Opera" --> doF (W.shift "www")
    -- DOC
    , className =? "Evince" --> doF (W.shift "doc")
    -- MUSIC
    , className =? "Rhythmbox" --> doF (W.shift "mov")
    , className =? "Totem" --> doF (W.shift "mov")
    , className =? "MPlayer" --> doF (W.shift "mov")
    -- IM
    , className =? "Pidgin" --> doF (W.shift "im")
    , className =? "Xchat" --> doF (W.shift "im")
    ]

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.conf"
	unsafeSpawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x000000 --height 19 &"
	unsafeSpawn "if [ -x /usr/bin/gnome-terminal ] ; then gnome-terminal & fi"
	unsafeSpawn "if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi"
	unsafeSpawn "if [ -x /usr/bin/gnome-power-manager ] ; then gnome-power-manager & fi"
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
		} `additionalKeys`
		[ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
		, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
		, ((mod4Mask .|. shiftMask, xK_Return), spawn terminal')
		, ((mod4Mask, xK_o), windows W.focusDown >> kill)
		, ((mod4Mask, xK_Return), dwmpromote >>  windows W.focusDown )	-- Swap the focused window and the master window
		, ((0, xK_Print), spawn "scrot")
		, ((mod4Mask, xK_c), kill)
		, ((mod4Mask, xK_F1), xmonadPrompt defaultXPConfig)
		, ((mod4Mask, xK_F2), shellPrompt defaultXPConfig)
		, ((mod4Mask, xK_F3), appendFilePrompt defaultXPConfig "/home/mvp/TODO")
		, ((mod4Mask, xK_Right), nextWS)
		, ((mod4Mask, xK_Left), prevWS)
		, ((mod4Mask, xK_Up), toggleWS)
		, ((mod4Mask, xK_Down), toggleWS)
		, ((mod4Mask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
		, ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
		, ((mod4Mask .|. shiftMask, xK_f), shiftTo Next EmptyWS)
		]
