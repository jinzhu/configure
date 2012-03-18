" hdrtag.vim  -- provides support for the Hdrtag command
"   Author: Charles E. Campbell, Jr.
"   Date:   Aug 12, 2008
" ---------------------------------------------------------------------
" Remove any old syntax stuff hanging around
syntax clear

syn match hdrtagType				'^\S.*\ze: {{{1$'			nextgroup=hdrtagColon
syn match hdrtagColon	contained	':'				skipwhite	nextgroup=hdrtagFdm
syn match hdrtagFdm		contained	'{{{1'

if !exists("did_hdrtag_syntax_inits")
 let did_hdrtag_syntax_inits = 1
 hi default link hdrtagType		Title
 hi default link hdrtagColon	Statement
 hi default link hdrtagSelect	PreProc
 hi default link hdrtagFdm		Ignore
endif
