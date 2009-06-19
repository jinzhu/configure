import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO

terminal'   = "gnome-terminal"
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    ]

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.conf"
	xmonad $ defaultConfig
		{ manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
		, layoutHook = avoidStruts  $  layoutHook defaultConfig
		, logHook    = dynamicLogWithPP $ xmobarPP
				{ ppOutput = hPutStrLn xmproc
				, ppTitle  = xmobarColor "green" "" . shorten 50
				}
		, modMask  = mod4Mask	-- Rebind Mod(ALT) to Windows Key
		, terminal = terminal'
	  -- , workspaces = ["e-mail","news","www","4","5","6","7","8","music"]
		} `additionalKeys`
		[ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
		, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
		, ((mod4Mask, xK_Return), spawn terminal')
		, ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)	-- Swap the focused window and the master window
		, ((0, xK_Print), spawn "scrot")
		]
