" SudoEdit.vim - Use sudo/su for writing/reading files with Vim
" ---------------------------------------------------------------
" Version:  0.6
" Authors:  Christian Brabandt <cb@256bit.org>
" Last Change: 2009/07/08
" Script:  http://www.vim.org/scripts/script.php?script_id=2709 
" License: VIM License
" GetLatestVimScripts: 2709 4 :AutoInstall: SudoEdit.vim

" Configuration:"{{{
" Exit quickly when:
" - this plugin was already loaded
" - when 'compatible' is set
if exists('loaded_sudowrite') || &cp
    finish
endif

if v:version < 700 || ( v:version == 700 && !has("patch111"))
  echomsg 'sudowrite: You need at least Vim 7.0 with patch111'
  finish
endif

let loaded_sudowrite=0.6

" Which Tool for super-user access to use"{{{
" Will be tried in order, first tool that is found will be used
" (e.g. you could use ssh)
" You can specify one in your .vimrc using the
" global variable g:sudoAuth
let s:sudoAuth=" sudo su "
if exists("g:sudoAuth")
    let s:sudoAuth = g:sudoAuth . s:sudoAuth
endif
"}}}

" Specify the parameter to use for the auth tool e.g. su uses "-c", but
" for su, it will be autodetected, sudo does not need one, for ssh use 
" "root@localhost"
"
" You can also use this parameter if you do not want to become root 
" but any other user
"
" You can specify this parameter in your .vimrc using the
" global variable g:sudoAuthArg
if !exists("g:sudoAuthArg")
    let s:sudoAuthArg=""
else
    let s:sudoAuthArg=g:sudoAuthArg
endif
"}}}




" Functions:"{{{

fu! <SID>LocalSettings(setflag)
    if a:setflag
	" Set shellrediraction temporarily
	" This is used to get su working right!
	let s:o_srr=&srr
	let &srr='>'
    else
	" Reset old settings
	" shellredirection
	let &srr=s:o_srr
    endif
endfu

fu! <SID>CheckAuthTool(Authlist)"{{{
    for tool in a:Authlist
	if executable(tool)
	    return [tool]
	endif
    endfor
    echoerr "No tool found for authentication. Is sudo/su installed and in your $PATH?"
    echoerr "Try setting g:sudoAuth and g:sudoAuthArg"
    return []
endfu"}}}

let s:AuthTool=<SID>CheckAuthTool(split(s:sudoAuth, '\s'))"{{{
if empty(s:AuthTool)
    finish
endif"}}}

if s:AuthTool[0] == "su" && empty(s:sudoAuthArg)
    let s:sudoAuthArg="-c"
endif
call add(s:AuthTool, s:sudoAuthArg . " ")

fu! <SID>SudoRead(file)
    %d
"    let cmd=':0r !' . join(s:AuthTool, ' ') . ' cat ' . a:file . ' 2>/dev/null '
    let cmd='cat ' . shellescape(a:file,1) . ' 2>/dev/null'
    if  s:AuthTool[0] =~ '^su$'
        let cmd='"' . cmd . '" --'
    endif
    let cmd=':0r! ' . join(s:AuthTool, ' ') . cmd
    silent! exe cmd
    $d 
    exe ":f " . a:file
    filetype detect
    set nomod
endfu

fu! <SID>SudoWrite(file) range
    if  s:AuthTool[0] =~ '^su$'
	" Workaround since su cannot be run with :w !
	    let tmpfile = tempname()
	    exe a:firstline . ',' . a:lastline . 'w ' . tmpfile
	    let cmd=':!' . join(s:AuthTool, ' ') . '"mv ' . tmpfile . ' ' . a:file . '" --'
    else
	let cmd='tee >/dev/null ' . a:file
	let cmd=a:firstline . ',' . a:lastline . 'w !' . join(s:AuthTool, ' ') . cmd
    endif
    silent exe cmd
    if v:shell_error
	throw "writeError"
    endif
    exe ":f " . a:file
    set nomod
endfu

fu! <SID>Stats(file)
    ":w echoes a string like this by default:
    ""SudoEdit.vim" 108L, 2595C geschrieben
    return '"' . a:file . '" ' . line('$') . 'L, ' . getfsize(expand(a:file)) . 'C written'
endfu



fu! <SID>SudoDo(readflag, file) range
    call <SID>LocalSettings(1)
    let file = !empty(a:file) ? a:file : expand("%")
    if empty(file)
	throw "emptyfile"
    endif
    if a:readflag
	call <SID>SudoRead(file)
    else
	try
	    exe a:firstline . ',' . a:lastline . 'call <SID>SudoWrite(' . shellescape(file,1) . ')'
	    echo <SID>Stats(file)
	catch /emptyfile/
	    echoerr "Cannot write file. Please enter filename for writing!"
	catch /writeError/
	    let a=v:errmsg
	    echoerr "There was an error writing the file!"
	    echoerr a
	finally
	    call <SID>LocalSettings(0)
	    redraw!
	endtry
	sleep
    endif
    if v:shell_error
	echoerr "Error " . ( a:readflag ? "reading " : "writing to " )  . file . "! Password wrong?"
    endif
    call <SID>LocalSettings(0)
    redraw!
endfu
"}}}"}}}

" Define User-Commands"{{{
com! -complete=file -range=% -nargs=? SudoWrite :<line1>,<line2>call <SID>SudoDo(0, <q-args>)
com! -complete=file -nargs=? SudoRead  :call <SID>SudoDo(1, <q-args>)
"}}}


" Modeline {{{1
" vim: set fdm=marker fdl=0 :  }}}
