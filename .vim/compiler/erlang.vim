" Vim compiler file
" Compiler:        erl -make
" Maintainer:      Oscar Hellström <oscar@oscarh.net>  
" URL:             http://personal.oscarh.net  
" Latest Revision: 2006-05-24
" ------------------------------------------------------------------------------
"  Options:
" To set compiler to use put ether
" let g:erlangCompiler="erlc"
" let g:erlangCompiler="erlc-debug"
" let g:erlangCompiler="emake"
" in your vimrc file
"
" ------------------------------------------------------------------------------

if exists("current_compiler")
  finish
endif
let current_compiler = "erlang"

if exists(":CompilerSet") != 2          " older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

let s:cpo_save = &cpo
set cpo-=C

if (! exists("g:erlangCompiler")) || g:erlangCompiler == "emake"
	CompilerSet makeprg=cd\ ..&&\erl\ -make

	CompilerSet errorformat=src/%f:%l:\ %m,
				\src/%f:%l:\ Warning:\ %m,
				\%f:%l:\ %m,
				\%f:%l:\ Warning:\ %m,
elseif g:erlangCompiler == "erlc-debug"
	CompilerSet makeprg=erlc\ -I\ ../include\ +debug_info\ %

	CompilerSet errorformat=%f:%l:\ %m,
				\%f:%l:\ Warning:\ %m
elseif g:erlangCompiler == "erlc"
	CompilerSet makeprg=erlc\ -I\ ../include\ %

	CompilerSet errorformat=%f:%l:\ %m,
				\%f:%l:\ Warning:\ %m
endif

let &cpo = s:cpo_save
unlet s:cpo_save
