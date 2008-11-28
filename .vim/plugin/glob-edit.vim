
" Author       : Jonas Kramer
" Copyright    : Copyright (C) 2008 by Jonas Kramer. Published under the terms
"                of the GNU General Public License (GPL).
" Name Of File : glob-edit.vim
" Description  : Open lots of files with a single command.
" Maintainer   : Jonas Kramer (jkramer at nex dot scrapping dot cc)
" Last Changed : 2008-07-12
" Version      : 0.01
" Usage        : Save this file in your plugin directory.

fu! Edit(...)
	" Don't do anything without arguments.
	if(a:0 > 0) 
		let i = a:0 

		" White there are arguments left...
		while(i > 0)
			" Glob the current argument.
			exe 'let pattern = a:' . i 
			let globbed = glob(pattern)

			" If there are files matching the glob pattern...
			if(strlen(globbed) > 0)
				" ...edit them all.
				for file in split(globbed, "\n")
					exe 'tabedit ' . file 
				endfor
			else
				" Otherwise open a new file with the pattern as name.
				exe 'edit ' . pattern
			endif

			let i = i - 1 
		endw
	endi 
endf

com! -nargs=* -complete=file Edit call Edit(<f-args>) 

cab edit Edit
