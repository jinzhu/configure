"--------------------------------------------------------------------------
"
" Vim script to launch/control browsers.
"
" Currently supported browsers:
"  - Firefox  (remote [new window / new tab] / launch)  [1]
"  - Mozilla  (remote [new window / new tab] / launch)  [1]
"  - Netscape (remote [new window] / launch)            [1]
"  - Opera    (remote [new window / new tab] / launch)
"  - Lynx     (Under the current TTY if not running the GUI, or a new xterm
"              window if DISPLAY is set.)
"  - w3m      (Under the current TTY if not running the GUI, or a new xterm
"              window if DISPLAY is set.)
"
" TODO:
"
"  Support more browsers?
"   - links  (text browser)
"
"  Defaulting to lynx if the the GUI isn't available may be undesirable.
"
"  Note: Various browsers such as galeon, nautilus, phoenix, &c use the
"  same HTML rendering engine as mozilla/firefox, so supporting them isn't
"  as important.
"   
"
"  BUGS:
"  * [1] The remote control for firefox/mozilla/netscape will probably
"    default to firefox if more than one is running.
"
"  * Since the commands to start the browsers are run in the backgorund
"    there's no way to actually get v:shell_error, so execution errors
"    aren't actually seen when not issuing a command to an already running
"    browser.
"
"  * The code is a mess.  Oh well.
"
"--------------------------------------------------------------------------


let s:BrowsersExist = ''
let s:FirefoxPath = system("which firefox")
if v:shell_error == 0
	let s:BrowsersExist = s:BrowsersExist . 'f'
else
	unlet s:FirefoxPath
endif
let s:MozillaPath = system("which mozilla")
if v:shell_error == 0
	let s:BrowsersExist = s:BrowsersExist . 'm'
else
	unlet s:MozillaPath
endif
let s:NetscapePath = system("which netscape")
if v:shell_error == 0
	let s:BrowsersExist = s:BrowsersExist . 'n'
else
	unlet s:NetscapePath
endif
let s:OperaPath = system("which opera")
if v:shell_error == 0
	let s:BrowsersExist = s:BrowsersExist . 'o'
else
	unlet s:OperaPath
endif
let s:LynxPath = system("which lynx")
if v:shell_error == 0
	let s:BrowsersExist = s:BrowsersExist . 'l'
else
	unlet s:LynxPath
endif
let s:w3mPath = system("which w3m")
if v:shell_error == 0
	let s:BrowsersExist = s:BrowsersExist . 'w'
else
	unlet s:w3mPath
endif

let s:NetscapeRemoteCmd = system("which netscape-remote")
if v:shell_error != 0
	if exists('s:FirefoxPath')
		let s:NetscapeRemoteCmd = s:FirefoxPath
	elseif exists('s:MozillaPath')
		let s:NetscapeRemoteCmd = s:MozillaPath
	elseif exists('s:NetscapePath')
		let s:NetscapeRemoteCmd = s:NetscapePath
	else
		"echohl ErrorMsg
		"echomsg "Can't set up remote-control preview code.\n(netscape-remote/firefox/mozilla/netscape not installed?)"
		"echohl None
		"finish
		let s:NetscapeRemoteCmd = 'false'
	endif
endif
let s:NetscapeRemoteCmd = substitute(s:NetscapeRemoteCmd, "\n$", "", "")


if exists("*LaunchBrowser")
	finish
endif


" Usage:
"  :call LaunchBrowser({[nolmf]},{[012]},[url])
"    The first argument is which browser to launch:
"      f - Firefox
"      m - Mozilla
"      n - Netscape
"      o - Opera
"      l - Lynx
"      w - w3m
"
"      default - This launches the first browser that was actually found.
"    The second argument is whether to launch a new window:
"      0 - No
"      1 - Yes
"      2 - New Tab (or new window if the browser doesn't provide a way to
"                   open a new tab)
"    The optional third argument is an URL to go to instead of loading the
"    current file.
"
" Return value:
"  0 - Failure (No browser was launched/controlled.)
"  1 - Success
"
" A special case of no arguments returns a character list of what browsers
" were found.
function! LaunchBrowser(...)

	let err = 0

	if a:0 == 0
		return s:BrowsersExist
	elseif a:0 >= 2
		let which = a:1
		let new = a:2
	else
		let err = 1
	endif

	let file = 'file://' . expand("%:p")

	if a:0 == 3
		let file = a:3
	elseif a:0 > 3
		let err = 1
	endif

	if err
		echohl ErrorMsg
		echomsg 'E119: Wrong number of arguments for function: LaunchBrowser'
		echohl None
		return 0
	endif

	if which ==? 'default'
		let which = strpart(s:BrowsersExist, 0, 1)
	endif

	if ((! strlen($DISPLAY)) || which ==? 'l' )

		if s:BrowsersExist !~? 'l'
			echohl ErrorMsg | echomsg "Lynx isn't found in $PATH." | echohl None
			return 0
		endif

		echohl Todo | echo "Launching lynx..." | echohl None

		if (has("gui_running") || new) && strlen($DISPLAY)
			call system("xterm -T Lynx -e lynx " . file . " &")

			if shell_error
				echohl ErrorMsg | echo "Unable to launch lynx in an xterm." | echohl None
				return 0
			endif
		else
			sleep 1
			execute "!lynx " . file

			if shell_error
				echohl ErrorMsg | echo "Unable to launch lynx." | echohl None
				return 0
			endif
		endif

		return 1
	endif


	if (which ==? 'w')

		if s:BrowsersExist !~? 'w'
			echohl ErrorMsg | echomsg "w3m isn't found in $PATH." | echohl None
			return 0
		endif

		echohl Todo | echo "Launching w3m..." | echohl None

		if (has("gui_running") || new) && strlen($DISPLAY)
			call system("xterm -T w3m -e w3m " . file . " &")

			if shell_error
				echohl ErrorMsg | echo "Unable to launch w3m in an xterm." | echohl None
				return 0
			endif
		else
			sleep 1
			execute "!w3m " . file

			if shell_error
				echohl ErrorMsg | echo "Unable to launch w3m." | echohl None
				return 0
			endif
		endif

		return 1
	endif

	if (which ==? 'o')

		if s:BrowsersExist !~? 'o'
			echohl ErrorMsg | echomsg "Opera isn't found in $PATH." | echohl None
			return 0
		endif

		if new == 2
			echohl Todo | echo "Opening new Opera tab..." | echohl None
			call system("sh -c \"trap '' HUP; opera -remote 'openURL(" . file . ",new-page)' &\"")

			if shell_error
				echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
				return 0
			endif
		elseif new
			echohl Todo | echo "Opening new Opera window..." | echohl None
			call system("sh -c \"trap '' HUP; opera -remote 'openURL(" . file . ",new-window)' &\"")

			if shell_error
				echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
				return 0
			endif
		else
			echohl Todo | echo "Sending remote command to Opera..." | echohl None
			call system("sh -c \"trap '' HUP; opera " . file . " &\"")

			if shell_error
				echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
				return 0
			endif
		endif

		return 1
	endif

	let windows = system("xwininfo -root -children | egrep \"[Ff]irefox|[Nn]etscape|[Mm]ozilla\"; return 0")
	if windows =~? 'firefox'
		let FirefoxRunning = 1
	else
		let FirefoxRunning = 0
	endif
	if windows =~? 'mozilla'
		let MozillaRunning = 1
	else
		let MozillaRunning = 0
	endif
	if windows =~? 'netscape'
		let NetscapeRunning = 1
	else
		let NetscapeRunning = 0
	endif

	if (which ==? 'f')

		if s:BrowsersExist !~? 'f'
			echohl ErrorMsg | echomsg "Firefox isn't found in $PATH." | echohl None
			return 0
		endif

		if ! FirefoxRunning
			echohl Todo | echo "Launching firefox, please wait..." | echohl None
			call system("sh -c \"trap '' HUP; firefox " . file . " &\"")

			if shell_error
				echohl ErrorMsg | echo "Unable to launch firefox." | echohl None
				return 0
			endif
		else
			if new == 2
				echohl Todo | echo "Firefox is running, opening new tab..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ",new-tab)\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			elseif new
				echohl Todo | echo "Firefox is running, opening new window..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ",new-window)\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			else
				echohl Todo | echo "Firefox is running, issuing remote command..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ")\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			endif
		endif

		return 1
	endif

	if (which ==? 'm')

		if s:BrowsersExist !~? 'm'
			echohl ErrorMsg | echomsg "Mozilla isn't found in $PATH." | echohl None
			return 0
		endif

		if ! MozillaRunning
			echohl Todo | echo "Launching mozilla, please wait..." | echohl None
			call system("sh -c \"trap '' HUP; mozilla " . file . " &\"")

			if shell_error
				echohl ErrorMsg | echo "Unable to launch mozilla." | echohl None
				return 0
			endif
		else
			if new == 2
				echohl Todo | echo "Mozilla is running, opening new tab..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ",new-tab)\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			elseif new
				echohl Todo | echo "Mozilla is running, opening new window..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ",new-window)\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			else
				echohl Todo | echo "Mozilla is running, issuing remote command..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ")\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			endif
		endif

		return 1
	endif

	if (which ==? 'n')

		if s:BrowsersExist !~? 'n'
			echohl ErrorMsg | echomsg "Netscape isn't found in $PATH." | echohl None
			return 0
		endif

		if ! NetscapeRunning
			echohl Todo | echo "Launching netscape, please wait..." | echohl None
			call system("sh -c \"trap '' HUP; netscape " . file . " &\"")

			if shell_error
				echohl ErrorMsg | echo "Unable to launch netscape." | echohl None
				return 0
			endif
		else
			if new
				echohl Todo | echo "Netscape is running, opening new window..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ",new-window)\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			else
				echohl Todo | echo "Netscape is running, issuing remote command..." | echohl None
				call system(s:NetscapeRemoteCmd . " -remote \"openURL(" . file . ")\"")

				if shell_error
					echohl ErrorMsg | echo "Unable to issue remote command." | echohl None
					return 0
				endif
			endif
		endif

		return 1
	endif

	echohl ErrorMsg | echo "Unknown browser ID." | echohl None
	return 0
endfunction

" vim: set ts=2 sw=2 ai nu tw=75 fo=croq2:
