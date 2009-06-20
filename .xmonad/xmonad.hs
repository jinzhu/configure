import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Prompt.AppendFile
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS

terminal'   = "gnome-terminal"
tall = Tall 1 (3/100) (1/2)
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "firefox" --> doF (W.shift "www")
    ]

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.conf"
	unsafeSpawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x000000 --height 19 &"
	unsafeSpawn "if [ -x /usr/bin/gnome-terminal ] ; then gnome-terminal & fi"
	unsafeSpawn "if [ -x /usr/bin/nm-applet ] ; then nm-applet --sm-disable & fi"
	xmonad $ defaultConfig
		{ manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
		, layoutHook = avoidStruts (tall ||| Full)
		, logHook    = dynamicLogWithPP $ xmobarPP
				{ ppOutput = hPutStrLn xmproc
				, ppTitle  = xmobarColor "green" "" . shorten 50
        , ppOrder = \(ws:l:t:_) -> [ws,l]
				}
		, modMask  = mod4Mask	-- Rebind Mod(ALT) to Windows Key
		, terminal = terminal'
	  , borderWidth = 1
	  , workspaces = ["dev","news","www","4","5","6","7","8","music"]
		} `additionalKeys`
		[ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
		, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
		, ((mod4Mask, xK_Return), spawn terminal')
		, ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)	-- Swap the focused window and the master window
		, ((0, xK_Print), spawn "scrot")
		, ((mod4Mask, xK_c), kill)
		, ((mod4Mask, xK_F2), shellPrompt defaultXPConfig)
		, ((mod4Mask, xK_F3), appendFilePrompt defaultXPConfig "/home/mvp/TODO")
		, ((mod4Mask, xK_Right), nextWS)
		, ((mod4Mask, xK_Left), prevWS)
		, ((mod4Mask, xK_Up), toggleWS)
		, ((mod4Mask, xK_Down), toggleWS)
		, ((mod1Mask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
		, ((mod1Mask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
		, ((mod1Mask .|. shiftMask, xK_f), shiftTo Next EmptyWS)
		]
