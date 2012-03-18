" CtagExpl.vim
"   Author: Charles E. Campbell, Jr.
"   Date:   Jan 22, 2009
"   Version: 1a	ASTRO-ONLY
" ---------------------------------------------------------------------
" Remove any old syntax stuff hanging around
syntax clear

syn match ctagexplLabel	'^\h\w*'			nextgroup=ctagexplColon
syn match ctagexplColon	':'		contained
syn match ctagexplFold	'{{{\d\+'

if !exists("did_ctagexpl_syntax_inits")
 let did_ctagexpl_syntax_inits = 1
 hi def link ctagexplLabel	Statement
 hi def link ctagexplColon	Delimiter
 hi def link ctagexplFold	Ignore
endif

" ---------------------------------------------------------------------
"  vim: ts=8
