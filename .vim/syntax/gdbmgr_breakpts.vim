" gdbmgr_breakpts.vim
"   Author: Charles E. Campbell, Jr.
"   Date:   Oct 26, 2010
" ---------------------------------------------------------------------
" Remove any old syntax stuff hanging around
syn clear

syn match GdbMgrBreakpts_count		'^#\d\+'
syn match GdbMgrBreakpts_breakpt	'\s\zsbreakpoint\>'
syn match GdbMgrBreakpts_tmpbreakpt	'\<tmp-breakpoint\>'
syn match GdbMgrBreakpts_colon		':'

" Highlighting
if !exists("did_gdbmgr_breakpts_syntax")
 let did_gdbmgr_breakpts_syntax= 1
 hi link GdbMgrBreakpts_count		Number
 hi link GdbMgrBreakpts_breakpt		Identifier
 hi link GdbMgrBreakpts_tmpbreakpt 	Type
 hi link GdbMgrBreakpts_colon		Delimiter
endif
