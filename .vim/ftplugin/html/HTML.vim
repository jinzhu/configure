" ---- Author & Copyright: ---------------------------------------------- {{{1
"
" Author:      Christian J. Robinson <infynity@onewest.net>
" URL:         http://www.infynity.spodzone.com/vim/HTML/
" Last Change: April 26, 2008
" Version:     0.33.1
" Original Concept: Doug Renze
"
"
" The original Copyright goes to Doug Renze, although nearly all of his
" efforts have been modified in this implementation.  My changes and additions
" are Copyrighted by me, on the dates marked in the ChangeLog.
"
" (Doug Renze has authorized me to place the original "code" under the GPL.)
"
" ----------------------------------------------------------------------------
"
" This program is free software; you can redistribute it and/or modify it
" under the terms of the GNU General Public License as published by the Free
" Software Foundation; either version 2 of the License, or (at your option)
" any later version.
"
" This program is distributed in the hope that it will be useful, but WITHOUT
" ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
" FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
" more details.
"
" ---- Original Author's Notes: ----------------------------------------------
"
" HTML Macros
"        I wrote these HTML macros for my personal use.  They're
"        freely-distributable and freely-modifiable.
"
"        If you do make any major additions or changes, or even just
"        have a suggestion for improvement, feel free to let me
"        know.  I'd appreciate any suggestions.
"
"        Credit must go to Eric Tilton, Carl Steadman and Tyler
"        Jones for their excellent book "Web Weaving" which was
"        my primary source.
"
"        Doug Renze
"
" ---- TODO: ------------------------------------------------------------ {{{1
"
" - Specific browser mappings for Win32 with "start <browser> ..." ?
" - Find a way to make "gv" after executing a visual mapping re-select the
"   right text.  (Currently my extra code that wraps around the visual
"   mappings can tweak the selected area significantly.)
"   + This should probably exclude the newly created tags, so things like
"     visual selection ;ta, then gv and ;tr, then gv and ;td work.
" - Add :HTMLmappingsreload/html/xhtml to the HTML menu?
"
" ---- RCS Information: ------------------------------------------------- {{{1
" $Id: HTML.vim,v 1.177 2008/04/26 22:36:28 infynity Exp $
" ----------------------------------------------------------------------- }}}1

" ---- Initialization: -------------------------------------------------- {{{1

if v:version < 600
  echoerr "HTML.vim no longer supports Vim versions prior to 6."
  sleep 2
  finish
elseif v:version < 700
  let s:tmp =
    \ "The HTML macros support for Vim versions prior to 7\n" .
    \ "will be abandoned in future versions.\n\n" .
    \ "You should seriously consider upgrading your version of Vim."
  call confirm(s:tmp, "&Dismiss", 1, 'Warning')
  unlet s:tmp
endif

" Save cpoptions and remove some junk that will throw us off (reset at the end
" of the script):
let s:savecpo = &cpoptions
set cpoptions&vim

let s:doing_internal_html_mappings = 1

if ! exists("b:did_html_mappings_init")
let b:did_html_mappings_init = 1

setlocal matchpairs+=<:>

" ---- Init Functions: -------------------------------------------------- {{{2

" s:BoolVar()  {{{3
"
" Given a string, test to see if a variable by that string name exists, and if
" so, whether it's set to 1|true|yes / 0|false|no   (Actually, anything not
" listed here also returns as 1.)
"
" Arguments:
"  1 - String:  The name of the variable to test (not its value!)
" Return Value:
"  1/0
"
" Limitations:
"  This /will not/ work on function-local variable names.
function! s:BoolVar(var)
  if a:var =~ '^[bgstvw]:'
    let var = a:var
  else
    let var = 'g:' . a:var
  endif

  if s:IsSet(var)
    execute "let varval = " . var
    return s:Bool(varval)
  else
    return 0
  endif
endfunction

" s:Bool() {{{3
"
" Helper to s:BoolVar() -- Test the string passed to it and return true/false
" based on that string.
"
" Arguments:
"  1 - String:  1|true|yes / 0|false|no
" Return Value:
"  1/0
function! s:Bool(str)
  return a:str !~? '^no$\|^false$\|^0$\|^$'
endfunction

" SetIfUnset()  {{{3
"
" Set a variable if it's not already set.
"
" Arguments:
"  1       - String:  The variable name
"  2 ... N - String:  The default value to use, "-" for the null string
" Return Value:
"  0  - The variable already existed
"  1  - The variable didn't exist and was set
"  -1 - An error occurred
function! SetIfUnset(var, ...)
  if a:var =~ '^[bgstvw]:'
    let var = a:var
  else
    let var = 'g:' . a:var
  endif

  if a:0 == 0
    echohl ErrorMsg
    echomsg "E119: Not enough arguments for function: SetIfUnset"
    echohl None
    return -1
  else
    let i = 2
    let val = a:1
    while i <= a:0
      execute "let val = val . ' ' . a:" . i
      let i = i + 1
    endwhile
  endif

  if ! s:IsSet(var)
    if (val == "-")
      execute "let " . var . "= \"\""
    else
      execute "let " . var . "= val"
    endif
    return 1
  endif
  return 0
endfunction

" s:IsSet() {{{3
"
" Given a string, test to see if a variable by that string name exists.
"
" Arguments:
"  1 - String:  The variable name
" Return Value:
"  1/0
function! s:IsSet(str)
  execute "let varisset = exists(\"" . a:str . "\")"
  return varisset
endfunction  "}}}3

" ----------------------------------------------------------------------- }}}2

command! -nargs=+ SetIfUnset call SetIfUnset(<f-args>)

SetIfUnset g:html_bgcolor         #FFFFFF
SetIfUnset g:html_textcolor       #000000
SetIfUnset g:html_linkcolor       #0000EE
SetIfUnset g:html_alinkcolor      #FF0000
SetIfUnset g:html_vlinkcolor      #990066
SetIfUnset g:html_tag_case        uppercase
SetIfUnset g:html_map_leader      ;
SetIfUnset g:html_default_charset iso-8859-1
" No way to know sensible defaults here so just make sure the
" variables are set:
SetIfUnset g:html_authorname  -
SetIfUnset g:html_authoremail -

if exists('b:html_tag_case')
  let b:html_tag_case_save = b:html_tag_case
endif

" Detect whether to force uppper or lower case:  {{{2
if &filetype ==? "xhtml"
      \ || s:BoolVar('g:do_xhtml_mappings')
      \ || s:BoolVar('b:do_xhtml_mappings')
  let b:do_xhtml_mappings = 1
else
  let b:do_xhtml_mappings = 0

  if s:BoolVar('g:html_tag_case_autodetect')
        \ && (line('$') != 1 || getline(1) != "")
    let s:byteoffset = line2byte(line('.')) + col('.') - 1

    silent! go 1
    let s:found_upper = search('\C<\(\s*/\)\?\s*\u\+\_[^<>]*>', 'w')
    silent! go 1
    let s:found_lower = search('\C<\(\s*/\)\?\s*\l\+\_[^<>]*>', 'w')

    if s:found_upper && ! s:found_lower
      let b:html_tag_case = 'uppercase'
    elseif ! s:found_upper && s:found_lower
      let b:html_tag_case = 'lowercase'
    endif

    if s:byteoffset == -1
      go 1
    else
      execute ':go ' . s:byteoffset
    endif

    unlet s:byteoffset s:found_upper s:found_lower
  endif
endif

if s:BoolVar('b:do_xhtml_mappings')
  let b:html_tag_case = 'lowercase'
endif
" }}}2

call SetIfUnset('b:html_tag_case', g:html_tag_case)

let s:thisfile = expand("<sfile>:p")
" ----------------------------------------------------------------------------


" ---- Functions: ------------------------------------------------------- {{{1

" HTMLencodeString()  {{{2
"
" Encode the characters in a string into their HTML &#...; representations.
"
" Arguments:
"  1 - String:  The string to encode.
"  2 - String:  Optional, whether to decode rather than encode the string:
"                d/decode: Decode the &#...; elements of the provided string
"                anything else: Encode the string (default)
" Return Value:
"  String:  The encoded string.
function! HTMLencodeString(string, ...)
  let out = ''

  if a:0 > 0
    if a:1 =~? '^d\(ecode\)\=$'
      let out = substitute(a:string, '&#\(\d\+\);', '\=nr2char(submatch(1))', 'g')
      let out = substitute(out, '%\(\x\{2}\)', '\=nr2char("0x".submatch(1))', 'g')
      return out
    elseif a:1 == '%'
      if v:version >= 700
        let out = substitute(a:string, '\(.\)', '\=printf("%%%02X", char2nr(submatch(1)))', 'g')
      else
        let out = substitute(a:string, '\(.\)', '\="%".ConvertToBase(char2nr(submatch(1)), 16)', 'g')
      endif
      return out
    endif
  endif

  if v:version >= 700
    let string = split(a:string, '\zs')
    for c in string
      let out = out . '&#' . char2nr(c) . ';'
    endfor
  else
    let len = strlen(a:string)
    let c = 0
    while c < len
      let out = out . '&#' . char2nr(a:string[c]) . ';'
      let c = c + 1
    endwhile
  endif

  return out
endfunction

" HTMLmap()  {{{2
"
" Define the HTML mappings with the appropriate case, plus some extra stuff:
"
" Arguments:
"  1 - String:  Which map command to run.
"  2 - String:  LHS of the map.
"  3 - String:  RHS of the map.
"  4 - Integer: Optional, applies only to visual maps:
"                -1: Don't add any extra special code to the mapping.
"                 0: Mapping enters insert mode.
"               Applies only when filetype indenting is on:
"                 1: re-selects the region, moves down a line, and re-indents.
"                 2: re-selects the region and re-indents.
"                 (Don't use these two arguments for maps that enter insert
"                 mode!)
let s:modes{'n'} = 'normal'
let s:modes{'v'} = 'visual'
let s:modes{'o'} = 'operator-pending'
let s:modes{'i'} = 'insert'
let s:modes{'c'} = 'command-line'
let s:modes{'l'} = 'langmap'
function! HTMLmap(cmd, map, arg, ...)
  let mode = strpart(a:cmd, 0, 1)
  if exists('s:modes{mode}') && maparg(a:map, mode) != ''
    echohl WarningMsg
    echomsg "WARNING: A mapping to \"" . a:map . "\" already exists for " . s:modes{mode} . " mode."
    echohl None
  endif

  let arg = s:HTMLconvertCase(a:arg)
  if ! s:BoolVar('b:do_xhtml_mappings')
    let arg = substitute(arg, ' />', '>', 'g')
  endif

  let map = substitute(a:map, '^<lead>\c', g:html_map_leader, '')

  if mode == 'v'
    " If 'selection' is "exclusive" all the visual mode mappings need to
    " behave slightly differently:
    let arg = substitute(arg, "`>a\\C", "`>i<C-R>=<SID>VI()<CR>", 'g')

    if a:0 >= 1 && a:1 < 0
      execute a:cmd . " <buffer> <silent> " . map . " " . arg
    elseif a:0 >= 1 && a:1 >= 1
      execute a:cmd . " <buffer> <silent> " . map . " <C-C>:call <SID>TO(0)<CR>gv" . arg
        \ . ":call <SID>TO(1)<CR>m':call <SID>HTMLreIndent(line(\"'<\"), line(\"'>\"), " . a:1 . ")<CR>``"
    elseif a:0 >= 1
      execute a:cmd . " <buffer> <silent> " . map . " <C-C>:call <SID>TO(0)<CR>gv" . arg
        \ . "<C-O>:call <SID>TO(1)<CR>"
    else
      execute a:cmd . " <buffer> <silent> " . map . " <C-C>:call <SID>TO(0)<CR>gv" . arg
        \ . ":call <SID>TO(1)<CR>"
    endif
  else
    execute a:cmd . " <buffer> <silent> " . map . " " . arg
  endif

  if exists('s:modes{mode}')
    let b:HTMLclearMappings = b:HTMLclearMappings . ':' . mode . "unmap <buffer> " . map . "\<CR>"
  else
    let b:HTMLclearMappings = b:HTMLclearMappings . ":unmap <buffer> " . map . "\<CR>"
  endif

  call s:HTMLextraMappingsAdd(':call HTMLmap("' . a:cmd . '", "' . escape(a:map, '"\')
        \ . '", "' . escape(a:arg, '"\') . (a:0 >= 1 ? ('", ' . a:1) : '"' ) . ')')
endfunction

" HTMLmapo()  {{{2
"
" Define a map that takes an operator to its corresponding visual mode
" mapping:
"
" Arguments:
"  1 - String:  The mapping.
"  2 - Boolean: Whether to enter insert mode after the mapping has executed.
"               (A value greater than 1 tells the mapping not to move right one
"               character.)
function! HTMLmapo(map, insert)
  if v:version < 700
    return
  endif

  let map = substitute(a:map, "^<lead>", g:html_map_leader, '')

  execute 'nnoremap <buffer> <silent> ' . map
    \ . " :let b:htmltagaction='" . map . "'<CR>"
    \ . ":let b:htmltaginsert=" . a:insert . "<CR>"
    \ . ':set operatorfunc=<SID>WR<CR>g@'

  let b:HTMLclearMappings = b:HTMLclearMappings . ":nunmap <buffer> " . map . "\<CR>"
  call s:HTMLextraMappingsAdd(':call HTMLmapo("' . escape(a:map, '"\') . '", ' . a:insert . ')')
endfunction

" s:WR()  {{{2
" Function set in 'operatorfunc' for mappings that take an operator:
function! s:WR(type)
  let sel_save = &selection
  let &selection = "inclusive"

  if a:type == 'line'
    execute "normal `[V`]" . b:htmltagaction
  elseif a:type == 'block'
    execute "normal `[\<C-V>`]" . b:htmltagaction
  else
    execute "normal `[v`]" . b:htmltagaction
  endif

  let &selection = sel_save

  if b:htmltaginsert
    if b:htmltaginsert < 2
      execute "normal \<Right>"
    endif
    startinsert
  endif

  " Leave these set so .-repeating of operator mappings works:
  "unlet b:htmltagaction b:htmltaginsert
endfunction

" s:HTMLextraMappingsAdd()  {{{2
"
" Add to the b:HTMLextraMappings variable if necessary:
"
" Arguments:
"  1 - String: The command necessary to re-define the mapping.
function! s:HTMLextraMappingsAdd(arg)
  if ! exists('s:doing_internal_html_mappings')
    if ! exists('b:HTMLextraMappings')
      let b:HTMLextraMappings = ''
    endif
    let b:HTMLextraMappings = b:HTMLextraMappings . a:arg . ' |'
  endif
endfunction

" s:TO()  {{{2
"
" Used to make sure the 'showmatch', 'indentexpr', and 'formatoptions' options
" are off temporarily to prevent the visual mappings from causing a
" (visual)bell or inserting improperly:
"
" Arguments:
"  1 - Integer: 0 - Turn options off.
"               1 - Turn options back on, if they were on before.
function! s:TO(s)
  if a:s == 0
    let s:savesm=&l:sm | let &l:sm=0
    let s:saveinde=&l:inde | let &l:inde=''
    let s:savefo=&l:fo | let &l:fo=''

    " A trick to make leading indent on the first line of visual-line
    " selections is handled properly (turn it into a character-wise
    " selection and exclude the leading indent):
    if visualmode() ==# 'V'
      let s:visualmode_save = visualmode()
      exe "normal `<^v`>\<C-C>"
    endif
  else
    let &l:sm=s:savesm | unlet s:savesm
    let &l:inde=s:saveinde | unlet s:saveinde
    let &l:fo=s:savefo | unlet s:savefo

    " Restore the last visual mode if it was changed:
    if exists('s:visualmode_save')
      exe "normal gv" . s:visualmode_save . "\<C-C>"
      unlet s:visualmode_save
    endif
  endif
endfunction

" s:TC()  {{{2
"
" Used to make sure the 'comments' option is off temporarily to prevent
" certain mappings from inserting unwanted comment leaders:
"
" Arguments:
"  1 - Integer: 0 - Turn options off.
"               1 - Turn options back on, if they were on before.
function! s:TC(s)
  if a:s == 0
    let s:savecom=&l:com | let &l:com=''
  else
    let &l:com=s:savecom | unlet s:savecom
  endif
endfunction

" s:VI() {{{2
"
" Used by HTMLmap() to enter insert mode in Visual mappings in the right
" place, depending on what 'selection' is set to:
"
" Arguments:
"   None
" Return Value:
"   The proper movement command based on the value of 'selection'.
function! s:VI()
  if &selection == 'inclusive'
    return "\<right>"
  else
    return "\<C-O>`>"
  endif
endfunction

" s:HTMLconvertCase()  {{{2
"
" Convert special regions in a string to the appropriate case determined by
" b:html_tag_case
"
" Arguments:
"  1 - String: The string with the regions to convert surrounded by [{...}].
" Return Value:
"  The converted string.
function! s:HTMLconvertCase(str)
  if (! exists('b:html_tag_case')) || b:html_tag_case =~? 'u\(pper\(case\)\?\)\?' || b:html_tag_case == ''
    let str = substitute(a:str, '\[{\(.\{-}\)}\]', '\U\1', 'g')
  elseif b:html_tag_case =~? 'l\(ower\(case\)\?\)\?'
    let str = substitute(a:str, '\[{\(.\{-}\)}\]', '\L\1', 'g')
  else
    echohl WarningMsg
    echomsg "b:html_tag_case = '" . b:html_tag_case . "' invalid, overriding to 'upppercase'."
    echohl None
    let b:html_tag_case = 'uppercase'
    let str = s:HTMLconvertCase(a:str)
  endif
  return str
endfunction

" s:HTMLreIndent()  {{{2
"
" Re-indent a region.  (Usually called by HTMLmap.)
"  Nothing happens if filetype indenting isn't enabled or 'indentexpr' is
"  unset.
"
" Arguments:
"  1 - Integer: Start of region.
"  2 - Integer: End of region.
"  3 - Integer: 1: Add an extra line below the region to re-indent.
"               *: Don't add an extra line.
function! s:HTMLreIndent(first, last, extraline)
  " To find out if filetype indenting is enabled:
  let save_register = @x
  redir @x | silent! filetype | redir END
  let filetype_output = @x
  let @x = save_register

  if filetype_output =~ "indent:OFF" && &indentexpr == ''
    return
  endif

  " Make sure the range is in the proper order:
  if a:last >= a:first
    let firstline = a:first
    let lastline = a:last
  else
    let lastline = a:first
    let firstline = a:last
  endif

  " Make sure the full region to be re-indendted is included:
  if a:extraline == 1
    if firstline == lastline
      let lastline = lastline + 2
    else
      let lastline = lastline + 1
    endif
  endif

  execute firstline . ',' . lastline . 'norm =='
endfunction

" HTMLnextInsertPoint()  {{{2
"
" Position the cursor at the next point in the file that needs data.
"
" Arguments:
"  1 - Character: Optional, the mode the function is being called from. 'n'
"                 for normal, 'i' for insert.  If 'i' is used the function
"                 enables an extra feature where if the cursor is on the start
"                 of a closing tag it places the cursor after the tag.
"                 Default is 'n'.
" Return Value:
"  None.
" Known problems:
"  Due to the necessity of running the search twice (why doesn't Vim support
"  cursor offset positioning in search()?) this function
"    a) won't ever position the cursor on an "empty" tag that starts on the
"       first character of the first line of the buffer
"    b) won't let the cursor "escape" from an "empty" tag that it can match on
"       the first line of the buffer when the cursor is on the first line and
"       tab is successively pressed
function! HTMLnextInsertPoint(...)
  let saveerrmsg  = v:errmsg
  let v:errmsg    = ""
  let saveruler   = &ruler   | let &ruler=0
  let saveshowcmd = &showcmd | let &showcmd=0
  let byteoffset  = line2byte(line('.')) + col('.') - 1

  " Tab in insert mode on the beginning of a closing tag jumps us to
  " after the tag:
  if a:0 >= 1 && a:1 == 'i'
    if strpart(getline(line('.')), col('.') - 1, 2) == '</'
      normal %
      let done = 1
    elseif strpart(getline(line('.')), col('.') - 1, 4) =~ ' *-->'
      normal f>
      let done = 1
    else
      let done = 0
    endif

    if done == 1
      if col('.') == col('$') - 1
        startinsert!
      else
        normal l
      endif

      return
    endif
  endif


  normal 0

  " Running the search twice is inefficient, but it squelches error
  " messages and the second search puts my cursor where it's needed...
  if search('<\([^ <>]\+\)\_[^<>]*>\_s*<\/\1>\|<\_[^<>]*""\_[^<>]*>\|<!--\_s*-->', 'w') == 0
    if byteoffset == -1
      go 1
    else
      execute ':go ' . byteoffset
      if a:0 >= 1 && a:1 == 'i' && col('.') == col('$') - 1
        startinsert!
      endif
    endif
  else
    normal 0
    silent! execute ':go ' . line2byte(line('.')) + col('.') - 2
    execute 'silent! normal! /<\([^ <>]\+\)\_[^<>]*>\_s*<\/\1>\|<\_[^<>]*""\_[^<>]*>\|<!--\_s*-->/;/>\_s*<\|""\|<!--\_s*-->/e' . "\<CR>"

    " Handle cursor positioning for comments and/or open+close tags spanning
    " multiple lines:
    if getline('.') =~ '<!-- \+-->'
      execute "normal F\<space>"
    elseif getline('.') =~ '^ *-->' && getline(line('.')-1) =~ '<!-- *$'
      normal 0
      normal t-
    elseif getline('.') =~ '^ *-->' && getline(line('.')-1) =~ '^ *$'
      normal k$
    elseif getline('.') =~ '^ *<\/[^<>]\+>' && getline(line('.')-1) =~ '^ *$'
      normal k$
    endif

    call histdel('search', -1)
    let @/ = histget('search', -1)
  endif

  let v:errmsg = saveerrmsg
  let &ruler   = saveruler
  let &showcmd = saveshowcmd
endfunction

" s:tag()  {{{2
"
" Causes certain tags (such as bold, italic, underline) to be closed then
" opened rather than opened then closed where appropriate, if syntax
" highlighting is on.
"
" Arguments:
"  1 - String: The tag name.
"  2 - Character: The mode:
"                  'i' - Insert mode
"                  'v' - Visual mode
" Return Value:
"  The string to be executed to insert the tag.

" -----------------------------------------------------------------------
" s:HTMLtags{tag}{mode}{open/close} = keystrokes                      {{{
"  tag        - The literal tag, without the <>'s
"  mode       - i = insert, v = visual
"  open/close - c = When inside an equivalent tag, close then open it
"               o = When not inside an equivalent tag
"  keystrokes - The mapping keystrokes to execute
let s:HTMLtags{'i'}{'i'}{'o'} = "<[{I></I}]>\<C-O>F<"
let s:HTMLtags{'i'}{'i'}{'c'} = "<[{/I><I}]>\<C-O>F<"
let s:HTMLtags{'i'}{'v'}{'o'} = "`>a</[{I}]>\<C-O>`<<[{I}]>"
let s:HTMLtags{'i'}{'v'}{'c'} = "`>a<[{I}]>\<C-O>`<</[{I}]>"
let s:HTMLtags{'em'}{'i'}{'o'} = "<[{EM></EM}]>\<C-O>F<"
let s:HTMLtags{'em'}{'i'}{'c'} = "<[{/EM><EM}]>\<C-O>F<"
let s:HTMLtags{'em'}{'v'}{'o'} = "`>a</[{EM}]>\<C-O>`<<[{EM}]>"
let s:HTMLtags{'em'}{'v'}{'c'} = "`>a<[{EM}]>\<C-O>`<</[{EM}]>"
let s:HTMLtags{'b'}{'i'}{'o'} = "<[{B></B}]>\<C-O>F<"
let s:HTMLtags{'b'}{'i'}{'c'} = "<[{/B><B}]>\<C-O>F<"
let s:HTMLtags{'b'}{'v'}{'o'} = "`>a</[{B}]>\<C-O>`<<[{B}]>"
let s:HTMLtags{'b'}{'v'}{'c'} = "`>a<[{B}]>\<C-O>`<</[{B}]>"
let s:HTMLtags{'u'}{'i'}{'o'} = "<[{U></U}]>\<C-O>F<"
let s:HTMLtags{'u'}{'i'}{'c'} = "<[{/U><U}]>\<C-O>F<"
let s:HTMLtags{'u'}{'v'}{'o'} = "`>a</[{U}]>\<C-O>`<<[{U}]>"
let s:HTMLtags{'u'}{'v'}{'c'} = "`>a<[{U}]>\<C-O>`<</[{U}]>"
let s:HTMLtags{'comment'}{'i'}{'o'} = "<!--  -->\<C-O>F "
let s:HTMLtags{'comment'}{'i'}{'c'} = " --><!-- \<C-O>F<"
let s:HTMLtags{'comment'}{'v'}{'o'} = "`>a -->\<C-O>`<<!-- "
let s:HTMLtags{'comment'}{'v'}{'c'} = "`>a<!-- \<C-O>`< -->"
let s:HTMLtags{'strong'}{'i'}{'o'} = "<[{STRONG></STRONG}]>\<C-O>F<"
let s:HTMLtags{'strong'}{'i'}{'c'} = "<[{/STRONG><STRONG}]>\<C-O>F<"
let s:HTMLtags{'strong'}{'v'}{'o'} = "`>a</[{STRONG}]>\<C-O>`<<[{STRONG}]>"
let s:HTMLtags{'strong'}{'v'}{'c'} = "`>a<[{STRONG}]>\<C-O>`<</[{STRONG}]>"
" ------------------------------------------------------------------- }}}
function! s:tag(tag, mode)
  let attr=synIDattr(synID(line('.'), col('.') - 1, 1), "name")
  if ( a:tag == 'i' && attr =~? 'italic' )
        \ || ( a:tag == 'em' && attr =~? 'italic' )
        \ || ( a:tag == 'b' && attr =~? 'bold' )
        \ || ( a:tag == 'strong' && attr =~? 'bold' )
        \ || ( a:tag == 'u' && attr =~? 'underline' )
        \ || ( a:tag == 'comment' && attr =~? 'comment' )
    let ret=s:HTMLconvertCase(s:HTMLtags{a:tag}{a:mode}{'c'})
  else
    let ret=s:HTMLconvertCase(s:HTMLtags{a:tag}{a:mode}{'o'})
  endif
  if a:mode == 'v'
    " If 'selection' is "exclusive" all the visual mode mappings need to
    " behave slightly differently:
    let ret = substitute(ret, "`>a\\C", "`>i" . s:VI(), 'g')
  endif
  return ret
endfunction

" s:HTMLdetectCharset()  {{{2
"
" Detects the HTTP-EQUIV Content-Type charset based on Vim's current
" encoding/fileencoding.
"
" Arguments:
"  None
" Return Value:
"  The value for the Content-Type charset based on 'fileencoding' or
"  'encoding'.
function! s:HTMLdetectCharset()

  if exists("g:html_charset")
    return g:html_charset
  endif

  " TODO: This table needs to be expanded:
  let charsets{'latin1'}    = 'iso-8859-1'
  let charsets{'utf_8'}     = 'UTF-8'
  let charsets{'utf_16'}    = 'UTF-16'
  let charsets{'shift_jis'} = 'Shift_JIS'
  let charsets{'euc_jp'}    = 'EUC-JP'
  let charsets{'cp950'}     = 'Big5'
  let charsets{'big5'}      = 'Big5'

  if &fileencoding != ''
    let enc=tolower(&fileencoding)
  else
    let enc=tolower(&encoding)
  endif

  " The iso-8859-* encodings are valid for the Content-Type charset header:
  if enc =~? '^iso-8859-'
    return enc
  endif

  let enc=substitute(enc, '\W', '_', 'g')

  if charsets{enc} != ''
    return charsets{enc}
  endif

  return g:html_default_charset
endfunction

" HTMLgenerateTable()  {{{2
"
" Interactively creates a table.
"
" Arguments:
"  None
" Return Value:
"  None
function! HTMLgenerateTable()
    let byteoffset = line2byte(line('.')) + col('.') - 1

    let rows    = inputdialog("Number of rows: ") + 0
    let columns = inputdialog("Number of columns: ") + 0

    if (! (rows > 0 && columns > 0))
        echo "Rows and columns must be integers."
        return
    endif

    let border = inputdialog("Border width of table [none]: ") + 0

    let r = 0
    let c = 0

    if (border)
        execute s:HTMLconvertCase("normal o<[{TABLE BORDER}]=" . border . ">\<ESC>")
    else
        execute s:HTMLconvertCase("normal o<[{TABLE}]>\<ESC>")
    endif

    while r < rows
        let r = r + 1
        let c = 0

        execute s:HTMLconvertCase("normal o<[{TR}]>\<ESC>")

        while c < columns
            let c = c + 1
            execute s:HTMLconvertCase("normal o<[{TD}]>\<CR></[{TD}]>\<ESC>")
        endwhile

        execute s:HTMLconvertCase("normal o</[{TR}]>\<ESC>")

    endwhile

    execute s:HTMLconvertCase("normal o</[{TABLE}]>\<ESC>")

    if byteoffset == -1
      go 1
    else
      execute ":go " . byteoffset
    endif

    normal jjj^

endfunction

" s:HTMLmappingsControl()  {{{2
"
" Disable/enable all the mappings defined by HTMLmap()/HTMLmapo().
"
" Arguments:
"  1 - String:  Whether to disable or enable the mappings:
"                d/disable: Clear the mappings
"                e/enable:  Redefine the mappings
"                r/reload:  Completely reload the script
"                h/html:    Reload the mapppings in HTML mode
"                x/xhtml:   Reload the mapppings in XHTML mode
" Return Value:
"  None
silent! function! s:HTMLmappingsControl(dowhat)
  if ! exists('b:did_html_mappings_init')
    echohl ErrorMsg
    echomsg "The HTML mappings were not sourced for this buffer."
    echohl None
    return
  endif

  if b:did_html_mappings_init < 0
    unlet b:did_html_mappings_init
  endif

  if a:dowhat =~? '^d\(isable\)\=\|off$'
    if exists('b:did_html_mappings')
      silent execute b:HTMLclearMappings
      unlet b:did_html_mappings
      if exists("g:did_html_menus")
        call s:HTMLmenuControl('disable')
      endif
    else
      echohl ErrorMsg
      echomsg "The HTML mappings are already disabled."
      echohl None
    endif
  elseif a:dowhat =~? '^e\(nable\)\=\|on$'
    if exists('b:did_html_mappings')
      echohl ErrorMsg
      echomsg "The HTML mappings are already enabled."
      echohl None
    else
      execute "source " . s:thisfile
      if exists('b:HTMLextraMappings')
        let s:doing_internal_html_mappings = 1
        silent execute b:HTMLextraMappings
        unlet s:doing_internal_html_mappings
      endif
    endif
  elseif a:dowhat =~? '^r\(eload\|einit\)\=$'
    HTMLmappings off
    let b:did_html_mappings_init=-1
    silent! unlet g:did_html_menus g:did_html_toolbar
    silent! unmenu HTML
    silent! unmenu! HTML
    HTMLmappings on
  elseif a:dowhat =~? '^h\(tml\)\=$'
    if exists('b:html_tag_case_save')
      let b:html_tag_case = b:html_tag_case_save
    endif
    let b:do_xhtml_mappings=0
    HTMLmappings off
    let b:did_html_mappings_init=-1
    HTMLmappings on
  elseif a:dowhat =~? '^x\(html\)\=$'
    let b:do_xhtml_mappings=1
    HTMLmappings off
    let b:did_html_mappings_init=-1
    HTMLmappings on
  else
    echohl ErrorMsg
    echomsg "Invalid argument: " . a:dowhat
    echohl None
  endif
endfunction

command! -nargs=1 HTMLmappings call <SID>HTMLmappingsControl(<f-args>)


" s:HTMLmenuControl()  {{{2
"
" Disable/enable the HTML menu and toolbar.
"
" Arguments:
"  1 - String:  Optional, Whether to disable or enable the mappings:
"                empty: Detect which to do
"                "disable": Disable the menu and toolbar
"                "enable": Enable the menu and toolbar
" Return Value:
"  None
function! s:HTMLmenuControl(...)
  if a:0 > 0
    if a:1 !~? '^\(dis\|en\)able$'
      echoerr "Invalid argument: " . a:1
      return
    else
      let bool = a:1
    endif
  else
    let bool = ''
  endif

  if bool == 'disable' || ! exists("b:did_html_mappings")
    amenu disable HTML
    amenu disable HTML.*
    if exists('g:did_html_toolbar')
      amenu disable ToolBar.*
      amenu enable ToolBar.Open
      amenu enable ToolBar.Save
      amenu enable ToolBar.SaveAll
      amenu enable ToolBar.Cut
      amenu enable ToolBar.Copy
      amenu enable ToolBar.Paste
      amenu enable ToolBar.Find
      amenu enable ToolBar.Replace
    endif
    if exists('b:did_html_mappings_init') && ! exists('b:did_html_mappings')
      amenu enable HTML
      amenu enable HTML.Enable\ Mappings
    endif
  elseif bool == 'enable' || exists("b:did_html_mappings_init")
    amenu enable HTML
    if exists("b:did_html_mappings")
      amenu enable HTML.*
      amenu disable HTML.Enable\ Mappings
      if exists('g:did_html_toolbar')
        amenu enable ToolBar.*
      endif
    else
      amenu enable HTML.Enable\ Mappings
    endif
  endif
endfunction

" -- Template Creation Stuff: {{{2

let s:internal_html_template=
  \" <[{HEAD}]>\n\n" .
  \"  <[{TITLE></TITLE}]>\n\n" .
  \"  <[{META NAME}]=\"Generator\" [{CONTENT}]=\"Vim %vimversion% (Vi IMproved editor; http://www.vim.org/)\" />\n" .
  \"  <[{META NAME}]=\"Author\" [{CONTENT}]=\"%authorname%\" />\n" .
  \"  <[{META NAME}]=\"Copyright\" [{CONTENT}]=\"Copyright (C) %date% %authorname%\" />\n" .
  \"  <[{LINK REV}]=\"made\" [{HREF}]=\"mailto:%authoremail%\" />\n\n" .
  \" </[{HEAD}]>\n" .
  \" <[{BODY BGCOLOR}]=\"%bgcolor%\"" .
    \" [{TEXT}]=\"%textcolor%\"" .
    \" [{LINK}]=\"%linkcolor%\"" .
    \" [{ALINK}]=\"%alinkcolor%\"" .
    \" [{VLINK}]=\"%vlinkcolor%\">\n\n" .
  \"  <[{H1 ALIGN=\"CENTER\"></H1}]>\n\n" .
  \"  <[{P}]>\n" .
  \"  </[{P}]>\n\n" .
  \"  <[{HR WIDTH}]=\"75%\" />\n\n" .
  \"  <[{P}]>\n" .
  \"  Last Modified: <[{I}]>%date%</[{I}]>\n" .
  \"  </[{P}]>\n\n" .
  \"  <[{ADDRESS}]>\n" .
  \"   <[{A HREF}]=\"mailto:%authoremail%\">%authorname% &lt;%authoremail%&gt;</[{A}]>\n" .
  \"  </[{ADDRESS}]>\n" .
  \" </[{BODY}]>\n" .
  \"</[{HTML}]>"

if s:BoolVar('b:do_xhtml_mappings')
  let b:internal_html_template = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n" .
        \ " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" .
        \ "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" .
        \ s:internal_html_template
else
  let b:internal_html_template = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n" .
        \ " \"http://www.w3.org/TR/html4/loose.dtd\">\n" .
        \ "<[{HTML}]>\n" .
        \ s:internal_html_template
  let b:internal_html_template = substitute(b:internal_html_template, ' />', '>', 'g')
endif

let b:internal_html_template = s:HTMLconvertCase(b:internal_html_template)

" HTMLtemplate()  {{{3
"
" Determine whether to insert the HTML template:
"
" Arguments:
"  None
" Return Value:
"  0 - The cursor is not on an insert point.
"  1 - The cursor is on an insert point.
function! HTMLtemplate()
  let ret = 0
  let save_ruler = &ruler
  let save_showcmd = &showcmd
  set noruler noshowcmd
  if (line('$') == 1 && getline(1) == "")
    let ret = s:HTMLtemplate2()
  else
    let YesNoOverwrite = confirm("Non-empty file.\nInsert template anyway?", "&Yes\n&No\n&Overwrite", 2, "W")
    if (YesNoOverwrite == 1)
      let ret = s:HTMLtemplate2()
    elseif (YesNoOverwrite == 3)
      execute "1,$delete"
      let ret = s:HTMLtemplate2()
    endif
  endif
  let &ruler = save_ruler
  let &showcmd = save_showcmd
  return ret
endfunction  " }}}3

" s:HTMLtemplate2()  {{{3
"
" Actually insert the HTML template:
"
" Arguments:
"  None
" Return Value:
"  0 - The cursor is not on an insert point.
"  1 - The cursor is on an insert point.
function! s:HTMLtemplate2()

  if g:html_authoremail != ''
    let g:html_authoremail_encoded = HTMLencodeString(g:html_authoremail)
  else
    let g:html_authoremail_encoded = ''
  endif

  let template = ''

  if (exists('b:html_template') && b:html_template != '')
    let template = b:html_template
  elseif (exists('g:html_template') && g:html_template != '')
    let template = g:html_template
  endif

  if template != ''
    if filereadable(expand(template))
      silent execute "0read " . template
    else
      echohl ErrorMsg
      echomsg "Unable to insert template file: " . template
      echomsg "Either it doesn't exist or it isn't readable."
      echohl None
      return 0
    endif
  else
    0put =b:internal_html_template
  endif

  if getline('$') =~ '^\s*$'
    $delete
  endif

  " Replace the various tokens with appropriate values:
  silent! %s/\C%authorname%/\=g:html_authorname/g
  silent! %s/\C%authoremail%/\=g:html_authoremail_encoded/g
  silent! %s/\C%bgcolor%/\=g:html_bgcolor/g
  silent! %s/\C%textcolor%/\=g:html_textcolor/g
  silent! %s/\C%linkcolor%/\=g:html_linkcolor/g
  silent! %s/\C%alinkcolor%/\=g:html_alinkcolor/g
  silent! %s/\C%vlinkcolor%/\=g:html_vlinkcolor/g
  silent! %s/\C%date%/\=strftime('%B %d, %Y')/g
  "silent! %s/\C%date\s*\([^%]\{-}\)\s*%/\=strftime(substitute(submatch(1),'\\\@<!!','%','g'))/g
  silent! %s/\C%date\s*\(\%(\\%\|[^%]\)\{-}\)\s*%/\=strftime(substitute(substitute(submatch(1),'\\%','%%','g'),'\\\@<!!','%','g'))/g
  silent! %s/\C%time%/\=strftime('%r %Z')/g
  silent! %s/\C%time12%/\=strftime('%r %Z')/g
  silent! %s/\C%time24%/\=strftime('%T')/g
  silent! %s/\C%charset%/\=<SID>HTMLdetectCharset()/g
  silent! %s/\C%vimversion%/\=strpart(v:version, 0, 1) . '.' . (strpart(v:version, 1, 2) + 0)/g

  go 1

  call HTMLnextInsertPoint('n')
  if getline('.')[col('.') - 2] . getline('.')[col('.') - 1] == '><'
        \ || (getline('.') =~ '^\s*$' && line('.') != 1)
    return 1
  else
    return 0
  endif

endfunction  " }}}3

" ----------------------------------------------------------------------------

endif " ! exists("b:did_html_mappings_init")


" ---- Misc. Mappings: -------------------------------------------------- {{{1

if ! exists("b:did_html_mappings")
let b:did_html_mappings = 1

let b:HTMLclearMappings = 'normal '

" Make it convenient to use ; as "normal":
"if g:html_map_leader == ';'
"  call HTMLmap("inoremap", ";;", ";")
"  call HTMLmap("vnoremap", ";;", ";", -1)
"  call HTMLmap("nnoremap", ";;", ";")
"endif
" ...Actually need to do that with whatever the map leader is set to:
call HTMLmap("inoremap", '<lead>' . g:html_map_leader, g:html_map_leader)
call HTMLmap("vnoremap", '<lead>' . g:html_map_leader, g:html_map_leader, -1)
call HTMLmap("nnoremap", '<lead>' . g:html_map_leader, g:html_map_leader)
" ...Make it easy to insert a & in insert mode:
call HTMLmap("inoremap", "<lead>&", "&")

if ! s:BoolVar('g:no_html_tab_mapping')
  " Allow hard tabs to be inserted:
  call HTMLmap("inoremap", "<lead><tab>", "<tab>")
  call HTMLmap("nnoremap", "<lead><tab>", "<tab>")

  " Tab takes us to a (hopefully) reasonable next insert point:
  call HTMLmap("inoremap", "<tab>", "<C-O>:call HTMLnextInsertPoint('i')<CR>")
  call HTMLmap("nnoremap", "<tab>", ":call HTMLnextInsertPoint('n')<CR>")
  call HTMLmap("vnoremap", "<tab>", "<C-C>:call HTMLnextInsertPoint('n')<CR>", -1)
else
  call HTMLmap("inoremap", "<lead><tab>", "<C-O>:call HTMLnextInsertPoint('i')<CR>")
  call HTMLmap("nnoremap", "<lead><tab>", ":call HTMLnextInsertPoint('n')<CR>")
  call HTMLmap("vnoremap", "<lead><tab>", "<C-C>:call HTMLnextInsertPoint('n')<CR>", -1)
endif

" Update an image tag's WIDTH & HEIGHT attributes (experimental!):
runtime! MangleImageTag.vim
if exists("*MangleImageTag")
  call HTMLmap("nnoremap", "<lead>mi", ":call MangleImageTag()<CR>")
  call HTMLmap("inoremap", "<lead>mi", "<C-O>:call MangleImageTag()<CR>")
endif

call HTMLmap("nnoremap", "<lead>html", ":if (HTMLtemplate()) \\| startinsert \\| endif<CR>")

" ----------------------------------------------------------------------------


" ---- General Markup Tag Mappings: ------------------------------------- {{{1

"       SGML Doctype Command
"call HTMLmap("nnoremap", "<lead>4", "1GO<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"><ESC>``")

"       SGML Doctype Command
if ! s:BoolVar('b:do_xhtml_mappings')
  " Transitional HTML (Looser):
  call HTMLmap("nnoremap", "<lead>4", ":call append(0, '<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"') \\\| call append(1, ' \"http://www.w3.org/TR/html4/loose.dtd\">')<CR>")
  " Strict HTML:
  call HTMLmap("nnoremap", "<lead>s4", ":call append(0, '<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"') \\\| call append(1, ' \"http://www.w3.org/TR/html4/strict.dtd\">')<CR>")
else
  " Transitional XHTML (Looser):
  call HTMLmap("nnoremap", "<lead>4", ":call append(0, '<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"') \\\| call append(1, ' \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">')<CR>")
  " Strict XHTML:
  call HTMLmap("nnoremap", "<lead>s4", ":call append(0, '<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"') \\\| call append(1, ' \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">')<CR>")
endif
call HTMLmap("imap", "<lead>4", "<C-O>" . g:html_map_leader . "4")
call HTMLmap("imap", "<lead>s4", "<C-O>" . g:html_map_leader . "s4")

"       Content-Type META tag
call HTMLmap("inoremap", "<lead>ct", "<[{META HTTP-EQUIV}]=\"Content-Type\" [{CONTENT}]=\"text/html; charset=<C-R>=<SID>HTMLdetectCharset()<CR>\" />")

"       Comment Tag
call HTMLmap("inoremap", "<lead>cm", "<C-R>=<SID>tag('comment','i')<CR>")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>cm", "<C-C>:execute \"normal \" . <SID>tag('comment','v')<CR>", 2)
" Motion mapping:
call HTMLmapo('<lead>cm', 0)

"       A HREF  Anchor Hyperlink        HTML 2.0
call HTMLmap("inoremap", "<lead>ah", "<[{A HREF=\"\"></A}]><C-O>F\"")
call HTMLmap("inoremap", "<lead>aH", "<[{A HREF=\"<C-R>*\"></A}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>ah", "<ESC>`>a</[{A}]><C-O>`<<[{A HREF}]=\"\"><C-O>F\"", 0)
call HTMLmap("vnoremap", "<lead>aH", "<ESC>`>a\"></[{A}]><C-O>`<<[{A HREF}]=\"<C-O>f<", 0)
" Motion mappings:
call HTMLmapo('<lead>ah', 1)
call HTMLmapo('<lead>aH', 1)

"       A HREF  Anchor Hyperlink, with TARGET=""
call HTMLmap("inoremap", "<lead>at", "<[{A HREF=\"\" TARGET=\"\"></A}]><C-O>3F\"")
call HTMLmap("inoremap", "<lead>aT", "<[{A HREF=\"<C-R>*\" TARGET=\"\"></A}]><C-O>F\"")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>at", "<ESC>`>a</[{A}]><C-O>`<<[{A HREF=\"\" TARGET}]=\"\"><C-O>3F\"", 0)
call HTMLmap("vnoremap", "<lead>aT", "<ESC>`>a\" [{TARGET=\"\"></A}]><C-O>`<<[{A HREF}]=\"<C-O>3f\"", 0)
" Motion mappings:
call HTMLmapo('<lead>at', 1)
call HTMLmapo('<lead>aT', 1)

"       A NAME  Named Anchor            HTML 2.0
call HTMLmap("inoremap", "<lead>an", "<[{A NAME=\"\"></A}]><C-O>F\"")
call HTMLmap("inoremap", "<lead>aN", "<[{A NAME=\"<C-R>*\"></A}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>an", "<ESC>`>a</[{A}]><C-O>`<<[{A NAME}]=\"\"><C-O>F\"", 0)
call HTMLmap("vnoremap", "<lead>aN", "<ESC>`>a\"></[{A}]><C-O>`<<[{A NAME}]=\"<C-O>f<", 0)
" Motion mappings:
call HTMLmapo('<lead>an', 1)
call HTMLmapo('<lead>aN', 1)

"       ABBR  Abbreviation              HTML 4.0
call HTMLmap("inoremap", "<lead>ab", "<[{ABBR TITLE=\"\"></ABBR}]><C-O>F\"")
call HTMLmap("inoremap", "<lead>aB", "<[{ABBR TITLE=\"<C-R>*\"></ABBR}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>ab", "<ESC>`>a</[{ABBR}]><C-O>`<<[{ABBR TITLE}]=\"\"><C-O>F\"", 0)
call HTMLmap("vnoremap", "<lead>aB", "<ESC>`>a\"></[{ABBR}]><C-O>`<<[{ABBR TITLE}]=\"<C-O>f<", 0)
" Motion mappings:
call HTMLmapo('<lead>ab', 1)
call HTMLmapo('<lead>aB', 1)

"       ACRONYM                         HTML 4.0
call HTMLmap("inoremap", "<lead>ac", "<[{ACRONYM TITLE=\"\"></ACRONYM}]><C-O>F\"")
call HTMLmap("inoremap", "<lead>aC", "<[{ACRONYM TITLE=\"<C-R>*\"></ACRONYM}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>ac", "<ESC>`>a</[{ACRONYM}]><C-O>`<<[{ACRONYM TITLE}]=\"\"><C-O>F\"", 0)
call HTMLmap("vnoremap", "<lead>aC", "<ESC>`>a\"></[{ACRONYM}]><C-O>`<<[{ACRONYM TITLE}]=\"<C-O>f<", 0)
" Motion mappings:
call HTMLmapo('<lead>ac', 1)
call HTMLmapo('<lead>aC', 1)

"       ADDRESS                         HTML 2.0
call HTMLmap("inoremap", "<lead>ad", "<[{ADDRESS></ADDRESS}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ad", "<ESC>`>a</[{ADDRESS}]><C-O>`<<[{ADDRESS}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>ad', 0)

"       B       Boldfaced Text          HTML 2.0
call HTMLmap("inoremap", "<lead>bo", "<C-R>=<SID>tag('b','i')<CR>")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>bo", "<C-C>:execute \"normal \" . <SID>tag('b','v')<CR>", 2)
" Motion mapping:
call HTMLmapo('<lead>bo', 0)

"       BASE                            HTML 2.0        HEADER
call HTMLmap("inoremap", "<lead>bh", "<[{BASE HREF}]=\"\" /><C-O>F\"")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>bh", "<ESC>`>a\" /><C-O>`<<[{BASE HREF}]=\"<ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>bh', 0)

"       BIG                             HTML 3.0
call HTMLmap("inoremap", "<lead>bi", "<[{BIG></BIG}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>bi", "<ESC>`>a</[{BIG}]><C-O>`<<[{BIG}]><ESC>")
" Motion mapping:
call HTMLmapo('<lead>bi', 0)

"       BLOCKQUOTE                      HTML 2.0
call HTMLmap("inoremap", "<lead>bl", "<[{BLOCKQUOTE}]><CR></[{BLOCKQUOTE}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>bl", "<ESC>`>a<CR></[{BLOCKQUOTE}]><C-O>`<<[{BLOCKQUOTE}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>bl', 0)

"       BODY                            HTML 2.0
call HTMLmap("inoremap", "<lead>bd", "<[{BODY}]><CR></[{BODY}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>bd", "<ESC>`>a<CR></[{BODY}]><C-O>`<<[{BODY}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>bd', 0)

"       BR      Line break              HTML 2.0
call HTMLmap("inoremap", "<lead>br", "<[{BR}] />")

"       CENTER                          NETSCAPE
call HTMLmap("inoremap", "<lead>ce", "<[{CENTER></CENTER}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ce", "<ESC>`>a</[{CENTER}]><C-O>`<<[{CENTER}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>ce', 0)

"       CITE                            HTML 2.0
call HTMLmap("inoremap", "<lead>ci", "<[{CITE></CITE}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ci", "<ESC>`>a</[{CITE}]><C-O>`<<[{CITE}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>ci', 0)

"       CODE                            HTML 2.0
call HTMLmap("inoremap", "<lead>co", "<[{CODE></CODE}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>co", "<ESC>`>a</[{CODE}]><C-O>`<<[{CODE}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>co', 0)

"       DEFINITION LIST COMPONENTS      HTML 2.0
"               DL      Definition List
"               DT      Definition Term
"               DD      Definition Body
call HTMLmap("inoremap", "<lead>dl", "<[{DL}]><CR></[{DL}]><ESC>O")
call HTMLmap("inoremap", "<lead>dt", "<[{DT}]></[{DT}]><C-O>F<")
call HTMLmap("inoremap", "<lead>dd", "<[{DD}]></[{DD}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>dl", "<ESC>`>a<CR></[{DL}]><C-O>`<<[{DL}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>dt", "<ESC>`>a</[{DT}]><C-O>`<<[{DT}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>dd", "<ESC>`>a</[{DD}]><C-O>`<<[{DD}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>dl', 0)
call HTMLmapo('<lead>dt', 0)
call HTMLmapo('<lead>dd', 0)

"       DEL     Deleted Text            HTML 3.0
call HTMLmap("inoremap", "<lead>de", "<lt>[{DEL></DEL}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>de", "<ESC>`>a</[{DEL}]><C-O>`<<lt>[{DEL}]><ESC>")
" Motion mapping:
call HTMLmapo('<lead>de', 0)

"       DFN     Defining Instance       HTML 3.0
call HTMLmap("inoremap", "<lead>df", "<[{DFN></DFN}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>df", "<ESC>`>a</[{DFN}]><C-O>`<<[{DFN}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>df', 0)

"       DIV     Document Division       HTML 3.0
call HTMLmap("inoremap", "<lead>dv", "<[{DIV}]><CR></[{DIV}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>dv", "<ESC>`>a<CR></[{DIV}]><C-O>`<<[{DIV}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>dv', 0)

"       SPAN    Delimit Arbitrary Text  HTML 4.0
call HTMLmap("inoremap", "<lead>sn", "<[{SPAN></SPAN}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>sn", "<ESC>`>a</[{SPAN}]><C-O>`<<[{SPAN}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>sn', 0)

"       EM      Emphasize               HTML 2.0
call HTMLmap("inoremap", "<lead>em", "<C-R>=<SID>tag('em','i')<CR>")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>em", "<C-C>:execute \"normal \" . <SID>tag('em','v')<CR>", 2)
" Motion mapping:
call HTMLmapo('<lead>em', 0)

"       FONT                            NETSCAPE
call HTMLmap("inoremap", "<lead>fo", "<[{FONT SIZE=\"\"></FONT}]><C-O>F\"")
call HTMLmap("inoremap", "<lead>fc", "<[{FONT COLOR=\"\"></FONT}]><C-O>F\"")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>fo", "<ESC>`>a</[{FONT}]><C-O>`<<[{FONT SIZE}]=\"\"><C-O>F\"", 0)
call HTMLmap("vnoremap", "<lead>fc", "<ESC>`>a</[{FONT}]><C-O>`<<[{FONT COLOR}]=\"\"><C-O>F\"", 0)
" Motion mappings:
call HTMLmapo('<lead>fo', 1)
call HTMLmapo('<lead>fc', 1)

"       HEADERS, LEVELS 1-6             HTML 2.0
call HTMLmap("inoremap", "<lead>h1", "<[{H1}]></[{H1}]><C-O>F<")
call HTMLmap("inoremap", "<lead>h2", "<[{H2}]></[{H2}]><C-O>F<")
call HTMLmap("inoremap", "<lead>h3", "<[{H3}]></[{H3}]><C-O>F<")
call HTMLmap("inoremap", "<lead>h4", "<[{H4}]></[{H4}]><C-O>F<")
call HTMLmap("inoremap", "<lead>h5", "<[{H5}]></[{H5}]><C-O>F<")
call HTMLmap("inoremap", "<lead>h6", "<[{H6}]></[{H6}]><C-O>F<")
call HTMLmap("inoremap", "<lead>H1", "<[{H1 ALIGN=\"CENTER}]\"></[{H1}]><C-O>F<")
call HTMLmap("inoremap", "<lead>H2", "<[{H2 ALIGN=\"CENTER}]\"></[{H2}]><C-O>F<")
call HTMLmap("inoremap", "<lead>H3", "<[{H3 ALIGN=\"CENTER}]\"></[{H3}]><C-O>F<")
call HTMLmap("inoremap", "<lead>H4", "<[{H4 ALIGN=\"CENTER}]\"></[{H4}]><C-O>F<")
call HTMLmap("inoremap", "<lead>H5", "<[{H5 ALIGN=\"CENTER}]\"></[{H5}]><C-O>F<")
call HTMLmap("inoremap", "<lead>H6", "<[{H6 ALIGN=\"CENTER}]\"></[{H6}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>h1", "<ESC>`>a</[{H1}]><C-O>`<<[{H1}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>h2", "<ESC>`>a</[{H2}]><C-O>`<<[{H2}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>h3", "<ESC>`>a</[{H3}]><C-O>`<<[{H3}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>h4", "<ESC>`>a</[{H4}]><C-O>`<<[{H4}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>h5", "<ESC>`>a</[{H5}]><C-O>`<<[{H5}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>h6", "<ESC>`>a</[{H6}]><C-O>`<<[{H6}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>H1", "<ESC>`>a</[{H1}]><C-O>`<<[{H1 ALIGN=\"CENTER}]\"><ESC>", 2)
call HTMLmap("vnoremap", "<lead>H2", "<ESC>`>a</[{H2}]><C-O>`<<[{H2 ALIGN=\"CENTER}]\"><ESC>", 2)
call HTMLmap("vnoremap", "<lead>H3", "<ESC>`>a</[{H3}]><C-O>`<<[{H3 ALIGN=\"CENTER}]\"><ESC>", 2)
call HTMLmap("vnoremap", "<lead>H4", "<ESC>`>a</[{H4}]><C-O>`<<[{H4 ALIGN=\"CENTER}]\"><ESC>", 2)
call HTMLmap("vnoremap", "<lead>H5", "<ESC>`>a</[{H5}]><C-O>`<<[{H5 ALIGN=\"CENTER}]\"><ESC>", 2)
call HTMLmap("vnoremap", "<lead>H6", "<ESC>`>a</[{H6}]><C-O>`<<[{H6 ALIGN=\"CENTER}]\"><ESC>", 2)
" Motion mappings:
call HTMLmapo("<lead>h1", 0)
call HTMLmapo("<lead>h2", 0)
call HTMLmapo("<lead>h3", 0)
call HTMLmapo("<lead>h4", 0)
call HTMLmapo("<lead>h5", 0)
call HTMLmapo("<lead>h6", 0)
call HTMLmapo("<lead>H1", 0)
call HTMLmapo("<lead>H2", 0)
call HTMLmapo("<lead>H3", 0)
call HTMLmapo("<lead>H4", 0)
call HTMLmapo("<lead>H5", 0)
call HTMLmapo("<lead>H6", 0)

"       HEAD                            HTML 2.0
call HTMLmap("inoremap", "<lead>he", "<[{HEAD}]><CR></[{HEAD}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>he", "<ESC>`>a<CR></[{HEAD}]><C-O>`<<[{HEAD}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>he', 0)

"       HR      Horizontal Rule         HTML 2.0 W/NETSCAPISM
call HTMLmap("inoremap", "<lead>hr", "<[{HR}] />")
"       HR      Horizontal Rule         HTML 2.0 W/NETSCAPISM
call HTMLmap("inoremap", "<lead>Hr", "<[{HR WIDTH}]=\"75%\" />")

"       HTML
if ! s:BoolVar('b:do_xhtml_mappings')
  call HTMLmap("inoremap", "<lead>ht", "<[{HTML}]><CR></[{HTML}]><ESC>O")
  " Visual mapping:
  call HTMLmap("vnoremap", "<lead>ht", "<ESC>`>a<CR></[{HTML}]><C-O>`<<[{HTML}]><CR><ESC>", 1)
else
  call HTMLmap("inoremap", "<lead>ht", "<html xmlns=\"http://www.w3.org/1999/xhtml\"><CR></html><ESC>O")
  " Visual mapping:
  call HTMLmap("vnoremap", "<lead>ht", "<ESC>`>a<CR></html><C-O>`<<html xmlns=\"http://www.w3.org/1999/xhtml\"><CR><ESC>", 1)
endif
" Motion mapping:
call HTMLmapo('<lead>ht', 0)

"       I       Italicized Text         HTML 2.0
call HTMLmap("inoremap", "<lead>it", "<C-R>=<SID>tag('i','i')<CR>")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>it", "<C-C>:execute \"normal \" . <SID>tag('i','v')<CR>", 2)
" Motion mapping:
call HTMLmapo('<lead>it', 0)

"       IMG     Image                   HTML 2.0
call HTMLmap("inoremap", "<lead>im", "<[{IMG SRC=\"\" ALT}]=\"\" /><C-O>3F\"")
call HTMLmap("inoremap", "<lead>iM", "<[{IMG SRC=\"<C-R>*\" ALT}]=\"\" /><C-O>F\"")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>im", "<ESC>`>a\" /><C-O>`<<[{IMG SRC=\"\" ALT}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>iM", "<ESC>`>a\" [{ALT}]=\"\" /><C-O>`<<[{IMG SRC}]=\"<C-O>3f\"", 0)
" Motion mapping:
call HTMLmapo('<lead>im', 1)
call HTMLmapo('<lead>iM', 1)

"       INS     Inserted Text           HTML 3.0
call HTMLmap("inoremap", "<lead>in", "<lt>[{INS></INS}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>in", "<ESC>`>a</[{INS}]><C-O>`<<lt>[{INS}]><ESC>")
" Motion mapping:
call HTMLmapo('<lead>in', 0)

"       ISINDEX Identifies Index        HTML 2.0
call HTMLmap("inoremap", "<lead>ii", "<[{ISINDEX}] />")

"       KBD     Keyboard Text           HTML 2.0
call HTMLmap("inoremap", "<lead>kb", "<[{KBD></KBD}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>kb", "<ESC>`>a</[{KBD}]><C-O>`<<[{KBD}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>kb', 0)

"       LI      List Item               HTML 2.0
call HTMLmap("inoremap", "<lead>li", "<[{LI}]></[{LI}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>li", "<ESC>`>a</[{LI}]><C-O>`<<[{LI}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>li', 0)

"       LINK                            HTML 2.0        HEADER
call HTMLmap("inoremap", "<lead>lk", "<[{LINK HREF}]=\"\" /><C-O>F\"")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>lk", "<ESC>`>a\" /><C-O>`<<[{LINK HREF}]=\"<ESC>")
" Motion mapping:
call HTMLmapo('<lead>lk', 0)

"       META    Meta Information        HTML 2.0        HEADER
call HTMLmap("inoremap", "<lead>me", "<[{META NAME=\"\" CONTENT}]=\"\" /><C-O>3F\"")
call HTMLmap("inoremap", "<lead>mE", "<[{META NAME=\"\" CONTENT}]=\"<C-R>*\" /><C-O>3F\"")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>me", "<ESC>`>a\" [{CONTENT}]=\"\" /><C-O>`<<[{META NAME}]=\"<C-O>3f\"", 0)
call HTMLmap("vnoremap", "<lead>mE", "<ESC>`>a\" /><C-O>`<<[{META NAME=\"\" CONTENT}]=\"<C-O>2F\"", 0)
" Motion mappings:
call HTMLmapo('<lead>me', 1)
call HTMLmapo('<lead>mE', 1)

"       OL      Ordered List            HTML 3.0
call HTMLmap("inoremap", "<lead>ol", "<[{OL}]><CR></[{OL}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ol", "<ESC>`>a<CR></[{OL}]><C-O>`<<[{OL}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>ol', 0)

"       P       Paragraph               HTML 3.0
call HTMLmap("inoremap", "<lead>pp", "<[{P}]><CR></[{P}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>pp", "<ESC>`>a<CR></[{P}]><C-O>`<<[{P}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>pp', 0)
" A special mapping... If you're between <P> and </P> this will insert the
" close tag and then the open tag in insert mode:
call HTMLmap("inoremap", "<lead>/p", "</[{P}]><CR><CR><[{P}]><CR>")

"       PRE     Preformatted Text       HTML 2.0
call HTMLmap("inoremap", "<lead>pr", "<[{PRE}]><CR></[{PRE}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>pr", "<ESC>`>a<CR></[{PRE}]><C-O>`<<[{PRE}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>pr', 0)

"       Q       Quote                   HTML 3.0
call HTMLmap("inoremap", "<lead>qu", "<[{Q></Q}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>qu", "<ESC>`>a</[{Q}]><C-O>`<<[{Q}]><ESC>")
" Motion mapping:
call HTMLmapo('<lead>qu', 0)

"       S       Strikethrough           HTML 3.0
call HTMLmap("inoremap", "<lead>sk", "<[{STRIKE></STRIKE}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>sk", "<ESC>`>a</[{STRIKE}]><C-O>`<<[{STRIKE}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>sk', 0)

"       SAMP    Sample Text             HTML 2.0
call HTMLmap("inoremap", "<lead>sa", "<[{SAMP></SAMP}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>sa", "<ESC>`>a</[{SAMP}]><C-O>`<<[{SAMP}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>sa', 0)

"       SMALL   Small Text              HTML 3.0
call HTMLmap("inoremap", "<lead>sm", "<[{SMALL></SMALL}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>sm", "<ESC>`>a</[{SMALL}]><C-O>`<<[{SMALL}]><ESC>")
" Motion mapping:
call HTMLmapo('<lead>sm', 0)

"       STRONG                          HTML 2.0
call HTMLmap("inoremap", "<lead>st", "<C-R>=<SID>tag('strong','i')<CR>")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>st", "<C-C>:execute \"normal \" . <SID>tag('strong','v')<CR>", 2)
" Motion mapping:
call HTMLmapo('<lead>st', 0)

"       STYLE                           HTML 4.0        HEADER
call HTMLmap("inoremap", "<lead>cs", "<[{STYLE TYPE}]=\"text/css\"><CR><!--<CR>--><CR></[{STYLE}]><ESC>kO")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>cs", "<ESC>`>a<CR> --><CR></[{STYLE}]><C-O>`<<[{STYLE TYPE}]=\"text/css\"><CR><!--<CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>cs', 0)

"       Linked CSS stylesheet
call HTMLmap("inoremap", "<lead>ls", "<[{LINK REL}]=\"stylesheet\" [{TYPE}]=\"text/css\" [{HREF}]=\"\"><C-O>F\"")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ls", "<ESC>`>a\"><C-O>`<<[{LINK REL}]=\"stylesheet\" [{TYPE}]=\"text/css\" [{HREF}]=\"<ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>ls', 0)

"       SUB     Subscript               HTML 3.0
call HTMLmap("inoremap", "<lead>sb", "<[{SUB></SUB}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>sb", "<ESC>`>a</[{SUB}]><C-O>`<<[{SUB}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>sb', 0)

"       SUP     Superscript             HTML 3.0
call HTMLmap("inoremap", "<lead>sp", "<[{SUP></SUP}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>sp", "<ESC>`>a</[{SUP}]><C-O>`<<[{SUP}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>sp', 0)

"       TITLE                           HTML 2.0        HEADER
call HTMLmap("inoremap", "<lead>ti", "<[{TITLE></TITLE}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ti", "<ESC>`>a</[{TITLE}]><C-O>`<<[{TITLE}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>ti', 0)

"       TT      Teletype Text (monospaced)      HTML 2.0
call HTMLmap("inoremap", "<lead>tt", "<[{TT></TT}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>tt", "<ESC>`>a</[{TT}]><C-O>`<<[{TT}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>tt', 0)

"       U       Underlined Text         HTML 2.0
call HTMLmap("inoremap", "<lead>un", "<C-R>=<SID>tag('u','i')<CR>")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>un", "<C-C>:execute \"normal \" . <SID>tag('u','v')<CR>", 2)
" Motion mapping:
call HTMLmapo('<lead>un', 0)

"       UL      Unordered List          HTML 2.0
call HTMLmap("inoremap", "<lead>ul", "<[{UL}]><CR></[{UL}]><ESC>O")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>ul", "<ESC>`>a<CR></[{UL}]><C-O>`<<[{UL}]><CR><ESC>", 1)
" Motion mapping:
call HTMLmapo('<lead>ul', 0)

"       VAR     Variable                HTML 3.0
call HTMLmap("inoremap", "<lead>va", "<[{VAR></VAR}]><C-O>F<")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>va", "<ESC>`>a</[{VAR}]><C-O>`<<[{VAR}]><ESC>", 2)
" Motion mapping:
call HTMLmapo('<lead>va', 0)

"       Embedded JavaScript
call HTMLmap("inoremap", "<lead>js", "<C-O>:call <SID>TC(0)<CR><[{SCRIPT TYPE}]=\"text/javascript\"><CR><!--<CR>// --><CR></[{SCRIPT}]><ESC>:call <SID>TC(1)<CR>kko")

"       Sourced JavaScript
call HTMLmap("inoremap", "<lead>sj", "<[{SCRIPT SRC}]=\"\" [{TYPE}]=\"text/javascript\"></[{SCRIPT}]><C-O>5F\"")

"       EMBED
call HTMLmap("inoremap", "<lead>eb", "<[{EMBED SRC=\"\" WIDTH=\"\" HEIGHT}]=\"\" /><CR><[{NOEMBED></NOEMBED}]><ESC>k$5F\"i")

"       NOSCRIPT
call HTMLmap("inoremap", "<lead>ns", "<[{NOSCRIPT}]><CR></[{NOSCRIP}]T><C-O>O")
call HTMLmap("vnoremap", "<lead>ns", "<ESC>`>a<CR></[{NOSCRIPT}]><C-O>`<<[{NOSCRIPT}]><CR><ESC>", 1)
call HTMLmapo('<lead>ns', 0)

"       OBJECT
call HTMLmap("inoremap", "<lead>ob", "<[{OBJECT DATA=\"\" WIDTH=\"\" HEIGHT}]=\"\"><CR></[{OBJECT}]><ESC>k$5F\"i")
call HTMLmap("vnoremap", "<lead>ob", "<ESC>`>a<CR></[{OBJECT}]><C-O>`<<[{OBJECT DATA=\"\" WIDTH=\"\" HEIGHT}]=\"\"><CR><ESC>k$5F\"", 1)
call HTMLmapo('<lead>ob', 0)

" Table stuff:
call HTMLmap("inoremap", "<lead>ca", "<[{CAPTION></CAPTION}]><C-O>F<")
call HTMLmap("inoremap", "<lead>ta", "<[{TABLE}]><CR></[{TABLE}]><ESC>O")
call HTMLmap("inoremap", "<lead>tH", "<[{THEAD}]><CR></[{THEAD}]><ESC>O")
call HTMLmap("inoremap", "<lead>tb", "<[{TBODY}]><CR></[{TBODY}]><ESC>O")
call HTMLmap("inoremap", "<lead>tf", "<[{TFOOT}]><CR></[{TFOOT}]><ESC>O")
call HTMLmap("inoremap", "<lead>tr", "<[{TR}]><CR></[{TR}]><ESC>O")
call HTMLmap("inoremap", "<lead>td", "<[{TD}]><CR></[{TD}]><ESC>O")
call HTMLmap("inoremap", "<lead>th", "<[{TH></TH}]><C-O>F<")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>ca", "<ESC>`>a<CR></[{CAPTION}]><C-O>`<<[{CAPTION}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>ta", "<ESC>`>a<CR></[{TABLE}]><C-O>`<<[{TABLE}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>tH", "<ESC>`>a<CR></[{THEAD}]><C-O>`<<[{THEAD}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>tb", "<ESC>`>a<CR></[{TBODY}]><C-O>`<<[{TBODY}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>tf", "<ESC>`>a<CR></[{TFOOT}]><C-O>`<<[{TFOOT}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>tr", "<ESC>`>a<CR></[{TR}]><C-O>`<<[{TR}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>td", "<ESC>`>a<CR></[{TD}]><C-O>`<<[{TD}]><CR><ESC>", 1)
call HTMLmap("vnoremap", "<lead>th", "<ESC>`>a</[{TH}]><C-O>`<<[{TH}]><ESC>", 2)
" Motion mappings:
call HTMLmapo("<lead>ca", 0)
call HTMLmapo("<lead>ta", 0)
call HTMLmapo("<lead>tH", 0)
call HTMLmapo("<lead>tb", 0)
call HTMLmapo("<lead>tf", 0)
call HTMLmapo("<lead>tr", 0)
call HTMLmapo("<lead>td", 0)
call HTMLmapo("<lead>th", 0)

" Interactively generate a table of Rows x Columns:
call HTMLmap("nnoremap", "<lead>tA", ":call HTMLgenerateTable()<CR>")

" Frames stuff:
call HTMLmap("inoremap", "<lead>fs", "<[{FRAMESET ROWS=\"\" COLS}]=\"\"><CR></[{FRAMESET}]><ESC>k$3F\"i")
call HTMLmap("inoremap", "<lead>fr", "<[{FRAME SRC}]=\"\" /><C-O>F\"")
call HTMLmap("inoremap", "<lead>nf", "<[{NOFRAMES}]><CR></[{NOFRAMES}]><ESC>O")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>fs", "<ESC>`>a<CR></[{FRAMESET}]><C-O>`<<[{FRAMESET ROWS=\"\" COLS}]=\"\"><CR><ESC>k$3F\"", 1)
call HTMLmap("vnoremap", "<lead>fr", "<ESC>`>a\" /><C-O>`<<[{FRAME SRC}]=\"<ESC>")
call HTMLmap("vnoremap", "<lead>nf", "<ESC>`>a<CR></[{NOFRAMES}]><C-O>`<<[{NOFRAMES}]><CR><ESC>", 1)
" Motion mappings:
call HTMLmapo("<lead>fs", 0)
call HTMLmapo("<lead>fr", 0)
call HTMLmapo("<lead>nf", 0)

"       IFRAME  Inline Frame            HTML 4.0
call HTMLmap("inoremap", "<lead>if", "<[{IFRAME SRC}]=\"\"><CR></[{IFRAME}]><ESC>k$F\"i")
" Visual mapping:
call HTMLmap("vnoremap", "<lead>if", "<ESC>`>a<CR></[{IFRAME}]><C-O>`<<[{IFRAME SRC}]=\"\"><CR><ESC>k$F\"", 1)
" Motion mapping:
call HTMLmapo('<lead>if', 0)

" Forms stuff:
call HTMLmap("inoremap", "<lead>fm", "<[{FORM ACTION}]=\"\"><CR></[{FORM}]><ESC>k$F\"i")
call HTMLmap("inoremap", "<lead>bu", "<[{INPUT TYPE=\"BUTTON\" NAME=\"\" VALUE}]=\"\" /><C-O>3F\"")
call HTMLmap("inoremap", "<lead>ch", "<[{INPUT TYPE=\"CHECKBOX\" NAME=\"\" VALUE}]=\"\" /><C-O>3F\"")
call HTMLmap("inoremap", "<lead>ra", "<[{INPUT TYPE=\"RADIO\" NAME=\"\" VALUE}]=\"\" /><C-O>3F\"")
call HTMLmap("inoremap", "<lead>hi", "<[{INPUT TYPE=\"HIDDEN\" NAME=\"\" VALUE}]=\"\" /><C-O>3F\"")
call HTMLmap("inoremap", "<lead>pa", "<[{INPUT TYPE=\"PASSWORD\" NAME=\"\" VALUE=\"\" SIZE}]=\"20\" /><C-O>5F\"")
call HTMLmap("inoremap", "<lead>te", "<[{INPUT TYPE=\"TEXT\" NAME=\"\" VALUE=\"\" SIZE}]=\"20\" /><C-O>5F\"")
call HTMLmap("inoremap", "<lead>fi", "<[{INPUT TYPE=\"FILE\" NAME=\"\" VALUE=\"\" SIZE}]=\"20\" /><C-O>5F\"")
call HTMLmap("inoremap", "<lead>se", "<[{SELECT NAME}]=\"\"><CR></[{SELECT}]><ESC>O")
call HTMLmap("inoremap", "<lead>ms", "<[{SELECT NAME=\"\" MULTIPLE}]><CR></[{SELECT}]><ESC>O")
call HTMLmap("inoremap", "<lead>op", "<[{OPTION></OPTION}]><C-O>F<")
call HTMLmap("inoremap", "<lead>og", "<[{OPTGROUP LABEL}]=\"\"><CR></[{OPTGROUP}]><ESC>k$F\"i")
call HTMLmap("inoremap", "<lead>tx", "<[{TEXTAREA NAME=\"\" ROWS=\"10\" COLS}]=\"50\"><CR></[{TEXTAREA}]><ESC>k$5F\"i")
call HTMLmap("inoremap", "<lead>su", "<[{INPUT TYPE=\"SUBMIT\" VALUE}]=\"Submit\" />")
call HTMLmap("inoremap", "<lead>re", "<[{INPUT TYPE=\"RESET\" VALUE}]=\"Reset\" />")
call HTMLmap("inoremap", "<lead>la", "<[{LABEL FOR=\"\"></LABEL}]><C-O>F\"")
" Visual mappings:
call HTMLmap("vnoremap", "<lead>fm", "<ESC>`>a<CR></[{FORM}]><C-O>`<<[{FORM ACTION}]=\"\"><CR><ESC>k$F\"", 1)
call HTMLmap("vnoremap", "<lead>bu", "<ESC>`>a\" /><C-O>`<<[{INPUT TYPE=\"BUTTON\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>ch", "<ESC>`>a\" /><C-O>`<<[{INPUT TYPE=\"CHECKBOX\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>ra", "<ESC>`>a\" /><C-O>`<<[{INPUT TYPE=\"RADIO\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>hi", "<ESC>`>a\" /><C-O>`<<[{INPUT TYPE=\"HIDDEN\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>pa", "<ESC>`>a\" [{SIZE}]=\"20\" /><C-O>`<<[{INPUT TYPE=\"PASSWORD\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>te", "<ESC>`>a\" [{SIZE}]=\"20\" /><C-O>`<<[{INPUT TYPE=\"TEXT\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>fi", "<ESC>`>a\" [{SIZE}]=\"20\" /><C-O>`<<[{INPUT TYPE=\"FILE\" NAME=\"\" VALUE}]=\"<C-O>2F\"", 0)
call HTMLmap("vnoremap", "<lead>se", "<ESC>`>a<CR></[{SELECT}]><C-O>`<<[{SELECT NAME}]=\"\"><CR><ESC>k$F\"", 1)
call HTMLmap("vnoremap", "<lead>ms", "<ESC>`>a<CR></[{SELECT}]><C-O>`<<[{SELECT NAME=\"\" MULTIPLE}]><CR><ESC>k$F\"", 1)
call HTMLmap("vnoremap", "<lead>op", "<ESC>`>a</[{OPTION}]><C-O>`<<[{OPTION}]><ESC>", 2)
call HTMLmap("vnoremap", "<lead>og", "<ESC>`>a<CR></[{OPTGROUP}]><C-O>`<<[{OPTGROUP LABEL}]=\"\"><CR><ESC>k$F\"", 1)
call HTMLmap("vnoremap", "<lead>tx", "<ESC>`>a<CR></[{TEXTAREA}]><C-O>`<<[{TEXTAREA NAME=\"\" ROWS=\"10\" COLS}]=\"50\"><CR><ESC>k$5F\"", 1)
call HTMLmap("vnoremap", "<lead>la", "<ESC>`>a</[{LABEL}]><C-O>`<<[{LABEL FOR}]=\"\"><C-O>F\"", 0)
call HTMLmap("vnoremap", "<lead>lA", "<ESC>`>a\"></[{LABEL}]><C-O>`<<[{LABEL FOR}]=\"<C-O>f<", 0)
" Motion mappings:
call HTMLmapo("<lead>fm", 0)
call HTMLmapo("<lead>bu", 1)
call HTMLmapo("<lead>ch", 1)
call HTMLmapo("<lead>ra", 1)
call HTMLmapo("<lead>hi", 1)
call HTMLmapo("<lead>pa", 1)
call HTMLmapo("<lead>te", 1)
call HTMLmapo("<lead>fi", 1)
call HTMLmapo("<lead>se", 0)
call HTMLmapo("<lead>ms", 0)
call HTMLmapo("<lead>op", 0)
call HTMLmapo("<lead>og", 0)
call HTMLmapo("<lead>tx", 0)
call HTMLmapo("<lead>la", 1)
call HTMLmapo("<lead>lA", 1)

" ----------------------------------------------------------------------------


" ---- Special Character (Character Entities) Mappings: ----------------- {{{1

" Convert the character under the cursor or the highlighted string to straight
" HTML entities:
call HTMLmap("vnoremap", "<lead>&", "s<C-R>=HTMLencodeString(@\")<CR><Esc>")
"call HTMLmap("nnoremap", "<lead>&", "s<C-R>=HTMLencodeString(@\")<CR><Esc>")
call HTMLmapo("<lead>&", 0)

" Convert the character under the cursor or the highlighted string to a %XX
" string:
call HTMLmap("vnoremap", "<lead>%", "s<C-R>=HTMLencodeString(@\", '%')<CR><Esc>")
"call HTMLmap("nnoremap", "<lead>%", "s<C-R>=HTMLencodeString(@\", '%')<CR><Esc>")
call HTMLmapo("<lead>%", 0)

" Decode a &#...; or %XX encoded string:
call HTMLmap("vnoremap", "<lead>^", "s<C-R>=HTMLencodeString(@\", 'd')<CR><Esc>")
call HTMLmapo("<lead>^", 0)

call HTMLmap("inoremap", "&&", "&amp;")
call HTMLmap("inoremap", "&cO", "&copy;")
call HTMLmap("inoremap", "&rO", "&reg;")
call HTMLmap("inoremap", "&tm", "&trade;")
call HTMLmap("inoremap", "&'", "&quot;")
call HTMLmap("inoremap", "&<", "&lt;")
call HTMLmap("inoremap", "&>", "&gt;")
call HTMLmap("inoremap", "&<space>", "&nbsp;")
call HTMLmap("inoremap", "<lead><space>", "&nbsp;")
call HTMLmap("inoremap", "&#", "&pound;")
call HTMLmap("inoremap", "&Y=", "&yen;")
call HTMLmap("inoremap", "&c\\|", "&cent;")
call HTMLmap("inoremap", "&A`", "&Agrave;")
call HTMLmap("inoremap", "&A'", "&Aacute;")
call HTMLmap("inoremap", "&A^", "&Acirc;")
call HTMLmap("inoremap", "&A~", "&Atilde;")
call HTMLmap("inoremap", "&A\"", "&Auml;")
call HTMLmap("inoremap", "&Ao", "&Aring;")
call HTMLmap("inoremap", "&AE", "&AElig;")
call HTMLmap("inoremap", "&C,", "&Ccedil;")
call HTMLmap("inoremap", "&E`", "&Egrave;")
call HTMLmap("inoremap", "&E'", "&Eacute;")
call HTMLmap("inoremap", "&E^", "&Ecirc;")
call HTMLmap("inoremap", "&E\"", "&Euml;")
call HTMLmap("inoremap", "&I`", "&Igrave;")
call HTMLmap("inoremap", "&I'", "&Iacute;")
call HTMLmap("inoremap", "&I^", "&Icirc;")
call HTMLmap("inoremap", "&I\"", "&Iuml;")
call HTMLmap("inoremap", "&N~", "&Ntilde;")
call HTMLmap("inoremap", "&O`", "&Ograve;")
call HTMLmap("inoremap", "&O'", "&Oacute;")
call HTMLmap("inoremap", "&O^", "&Ocirc;")
call HTMLmap("inoremap", "&O~", "&Otilde;")
call HTMLmap("inoremap", "&O\"", "&Ouml;")
call HTMLmap("inoremap", "&O/", "&Oslash;")
call HTMLmap("inoremap", "&U`", "&Ugrave;")
call HTMLmap("inoremap", "&U'", "&Uacute;")
call HTMLmap("inoremap", "&U^", "&Ucirc;")
call HTMLmap("inoremap", "&U\"", "&Uuml;")
call HTMLmap("inoremap", "&Y'", "&Yacute;")
call HTMLmap("inoremap", "&a`", "&agrave;")
call HTMLmap("inoremap", "&a'", "&aacute;")
call HTMLmap("inoremap", "&a^", "&acirc;")
call HTMLmap("inoremap", "&a~", "&atilde;")
call HTMLmap("inoremap", "&a\"", "&auml;")
call HTMLmap("inoremap", "&ao", "&aring;")
call HTMLmap("inoremap", "&ae", "&aelig;")
call HTMLmap("inoremap", "&c,", "&ccedil;")
call HTMLmap("inoremap", "&e`", "&egrave;")
call HTMLmap("inoremap", "&e'", "&eacute;")
call HTMLmap("inoremap", "&e^", "&ecirc;")
call HTMLmap("inoremap", "&e\"", "&euml;")
call HTMLmap("inoremap", "&i`", "&igrave;")
call HTMLmap("inoremap", "&i'", "&iacute;")
call HTMLmap("inoremap", "&i^", "&icirc;")
call HTMLmap("inoremap", "&i\"", "&iuml;")
call HTMLmap("inoremap", "&n~", "&ntilde;")
call HTMLmap("inoremap", "&o`", "&ograve;")
call HTMLmap("inoremap", "&o'", "&oacute;")
call HTMLmap("inoremap", "&o^", "&ocirc;")
call HTMLmap("inoremap", "&o~", "&otilde;")
call HTMLmap("inoremap", "&o\"", "&ouml;")
call HTMLmap("inoremap", "&x", "&times;")
call HTMLmap("inoremap", "&u`", "&ugrave;")
call HTMLmap("inoremap", "&u'", "&uacute;")
call HTMLmap("inoremap", "&u^", "&ucirc;")
call HTMLmap("inoremap", "&u\"", "&uuml;")
call HTMLmap("inoremap", "&y'", "&yacute;")
call HTMLmap("inoremap", "&y\"", "&yuml;")
call HTMLmap("inoremap", "&2<", "&laquo;")
call HTMLmap("inoremap", "&2>", "&raquo;")
call HTMLmap("inoremap", "&\"", "&uml;")
call HTMLmap("inoremap", "&/", "&divide;")
call HTMLmap("inoremap", "&o/", "&oslash;")
call HTMLmap("inoremap", "&!", "&iexcl;")
call HTMLmap("inoremap", "&?", "&iquest;")
call HTMLmap("inoremap", "&dg", "&deg;")
call HTMLmap("inoremap", "&mi", "&micro;")
call HTMLmap("inoremap", "&pa", "&para;")
call HTMLmap("inoremap", "&.", "&middot;")
call HTMLmap("inoremap", "&14", "&frac14;")
call HTMLmap("inoremap", "&12", "&frac12;")
call HTMLmap("inoremap", "&34", "&frac34;")
call HTMLmap("inoremap", "&n-", "&ndash;")  " Math symbol
call HTMLmap("inoremap", "&2-", "&ndash;")  " ...
call HTMLmap("inoremap", "&m-", "&mdash;")  " Sentence break
call HTMLmap("inoremap", "&3-", "&mdash;")  " ...
call HTMLmap("inoremap", "&--", "&mdash;")  " ...
call HTMLmap("inoremap", "&3.", "&hellip;")
" Greek letters:
"   ... Capital:
call HTMLmap("inoremap", "&Al", "&Alpha;")
call HTMLmap("inoremap", "&Be", "&Beta;")
call HTMLmap("inoremap", "&Ga", "&Gamma;")
call HTMLmap("inoremap", "&De", "&Delta;")
call HTMLmap("inoremap", "&Ep", "&Epsilon;")
call HTMLmap("inoremap", "&Ze", "&Zeta;")
call HTMLmap("inoremap", "&Et", "&Eta;")
call HTMLmap("inoremap", "&Th", "&Theta;")
call HTMLmap("inoremap", "&Io", "&Iota;")
call HTMLmap("inoremap", "&Ka", "&Kappa;")
call HTMLmap("inoremap", "&Lm", "&Lambda;")
call HTMLmap("inoremap", "&Mu", "&Mu;")
call HTMLmap("inoremap", "&Nu", "&Nu;")
call HTMLmap("inoremap", "&Xi", "&Xi;")
call HTMLmap("inoremap", "&Oc", "&Omicron;")
call HTMLmap("inoremap", "&Pi", "&Pi;")
call HTMLmap("inoremap", "&Rh", "&Rho;")
call HTMLmap("inoremap", "&Si", "&Sigma;")
call HTMLmap("inoremap", "&Ta", "&Tau;")
call HTMLmap("inoremap", "&Up", "&Upsilon;")
call HTMLmap("inoremap", "&Ph", "&Phi;")
call HTMLmap("inoremap", "&Ch", "&Chi;")
call HTMLmap("inoremap", "&Ps", "&Psi;")
"   ... Lowercase/small:
call HTMLmap("inoremap", "&al;", "&alpha;")
call HTMLmap("inoremap", "&be;", "&beta;")
call HTMLmap("inoremap", "&ga;", "&gamma;")
call HTMLmap("inoremap", "&de;", "&delta;")
call HTMLmap("inoremap", "&ep;", "&epsilon;")
call HTMLmap("inoremap", "&ze;", "&zeta;")
call HTMLmap("inoremap", "&et;", "&eta;")
call HTMLmap("inoremap", "&th;", "&theta;")
call HTMLmap("inoremap", "&io;", "&iota;")
call HTMLmap("inoremap", "&ka;", "&kappa;")
call HTMLmap("inoremap", "&lm;", "&lambda;")
call HTMLmap("inoremap", "&mu;", "&mu;")
call HTMLmap("inoremap", "&nu;", "&nu;")
call HTMLmap("inoremap", "&xi;", "&xi;")
call HTMLmap("inoremap", "&oc;", "&omicron;")
call HTMLmap("inoremap", "&pi;", "&pi;")
call HTMLmap("inoremap", "&rh;", "&rho;")
call HTMLmap("inoremap", "&si;", "&sigma;")
call HTMLmap("inoremap", "&sf;", "&sigmaf;")
call HTMLmap("inoremap", "&ta;", "&tau;")
call HTMLmap("inoremap", "&up;", "&upsilon;")
call HTMLmap("inoremap", "&ph;", "&phi;")
call HTMLmap("inoremap", "&ch;", "&chi;")
call HTMLmap("inoremap", "&ps;", "&psi;")
call HTMLmap("inoremap", "&og;", "&omega;")
call HTMLmap("inoremap", "&ts;", "&thetasym;")
call HTMLmap("inoremap", "&uh;", "&upsih;")
call HTMLmap("inoremap", "&pv;", "&piv;")
" single-line arrows:
call HTMLmap("inoremap", "&la", "&larr;")
call HTMLmap("inoremap", "&ua", "&uarr;")
call HTMLmap("inoremap", "&ra", "&rarr;")
call HTMLmap("inoremap", "&da", "&darr;")
call HTMLmap("inoremap", "&ha", "&harr;")
"call HTMLmap("inoremap", "&ca", "&crarr;")
" double-line arrows:
call HTMLmap("inoremap", "&lA", "&lArr;")
call HTMLmap("inoremap", "&uA", "&uArr;")
call HTMLmap("inoremap", "&rA", "&rArr;")
call HTMLmap("inoremap", "&dA", "&dArr;")
call HTMLmap("inoremap", "&hA", "&hArr;")

" ----------------------------------------------------------------------------


" ---- Browser Remote Controls: ----------------------------------------- {{{1
if has("unix")
  runtime! browser_launcher.vim

  if exists("*LaunchBrowser")
    " Firefox: View current file, starting Firefox if it's not running:
    call HTMLmap("nnoremap", "<lead>ff", ":call LaunchBrowser('f',0)<CR>")
    " Firefox: Open a new window, and view the current file:
    call HTMLmap("nnoremap", "<lead>nff", ":call LaunchBrowser('f',1)<CR>")
    " Firefox: Open a new tab, and view the current file:
    call HTMLmap("nnoremap", "<lead>tff", ":call LaunchBrowser('f',2)<CR>")

    " Mozilla: View current file, starting Mozilla if it's not running:
    call HTMLmap("nnoremap", "<lead>mo", ":call LaunchBrowser('m',0)<CR>")
    " Mozilla: Open a new window, and view the current file:
    call HTMLmap("nnoremap", "<lead>nmo", ":call LaunchBrowser('m',1)<CR>")
    " Mozilla: Open a new tab, and view the current file:
    call HTMLmap("nnoremap", "<lead>tmo", ":call LaunchBrowser('m',2)<CR>")

    " Netscape: View current file, starting Netscape if it's not running:
    call HTMLmap("nnoremap", "<lead>ne", ":call LaunchBrowser('n',0)<CR>")
    " Netscape: Open a new window, and view the current file:
    call HTMLmap("nnoremap", "<lead>nne", ":call LaunchBrowser('n',1)<CR>")

    " Opera: View current file, starting Opera if it's not running:
    call HTMLmap("nnoremap", "<lead>oa", ":call LaunchBrowser('o',0)<CR>")
    " Opera: View current file in a new window, starting Opera if it's not running:
    call HTMLmap("nnoremap", "<lead>noa", ":call LaunchBrowser('o',1)<CR>")
    " Opera: Open a new tab, and view the current file:
    call HTMLmap("nnoremap", "<lead>toa", ":call LaunchBrowser('o',2)<CR>")

    " Lynx:  (This happens anyway if there's no DISPLAY environmental variable.)
    call HTMLmap("nnoremap","<lead>ly",":call LaunchBrowser('l',0)<CR>")
    " Lynx in an xterm:  (This happens regardless in the Vim GUI.)
    call HTMLmap("nnoremap", "<lead>nly", ":call LaunchBrowser('l',1)<CR>")

    " w3m:
    call HTMLmap("nnoremap","<lead>w3",":call LaunchBrowser('w',0)<CR>")
    " w3m in an xterm:  (This happens regardless in the Vim GUI.)
    call HTMLmap("nnoremap", "<lead>nw3", ":call LaunchBrowser('w',1)<CR>")
  endif
elseif has("win32")
  " Run the default Windows browser:
   call HTMLmap("nnoremap", "<lead>db", ":exe '!start RunDll32.exe shell32.dll,ShellExec_RunDLL ' . expand('%:p')<CR>")

  " This assumes that IE is installed and the file explorer will become IE
  " when given an URL to open:
  call HTMLmap("nnoremap", "<lead>ie", ":exe '!start explorer ' . expand('%:p')<CR>")
endif

" ----------------------------------------------------------------------------

endif " ! exists("b:did_html_mappings")


" ---- ToolBar Buttons: ------------------------------------------------- {{{1
if ! has("gui_running") && ! s:BoolVar('g:force_html_menu')
  augroup HTMLplugin
  au!
  execute 'autocmd GUIEnter * source ' . expand('<sfile>:p <bar> autocmd! HTMLplugin GUIEnter *')
  augroup END
elseif exists("g:did_html_menus")
  call s:HTMLmenuControl()
elseif ! s:BoolVar('g:no_html_menu')

  command! -nargs=+ HTMLmenu call s:HTMLleadmenu(<f-args>)
  function! s:HTMLleadmenu(type, level, name, item, ...)
    if a:0 == 1
      let pre = a:1
    else
      let pre = ''
    endif

    if a:level == '-'
      let level = ''
    else
      let level = a:level
    endif

    let name = escape(a:name, ' ')

    execute a:type . ' ' . level . ' ' . name . '<tab>' . g:html_map_leader . a:item
      \ . ' ' . pre . g:html_map_leader . a:item
  endfunction

if ! s:BoolVar('g:no_html_toolbar')
      \ && (has("toolbar") || has("win32") || has("gui_gtk")
      \ || (v:version >= 600 && (has("gui_athena") || has("gui_motif") || has("gui_photon"))))

  if (has("win32") && globpath(&rtp, 'bitmaps/Browser.bmp') == '')
      \ || globpath(&rtp, 'bitmaps/Browser.xpm') == ''
    let s:tmp = "Warning:\nYou need to install the Toolbar Bitmaps for the "
    let s:tmp = s:tmp . fnamemodify(s:thisfile, ':t') . " plugin. "
    let s:tmp = s:tmp . "See: http://www.infynity.spodzone.com/vim/HTML/#files\n"
    let s:tmp = s:tmp . 'Or see ":help g:no_html_toolbar".'
    if has('win32') || has('unix')
      let s:tmp = confirm(s:tmp, "&Dismiss\nView &Help\nGet &Bitmaps", 1, 'Warning')
    else
      let s:tmp = confirm(s:tmp, "&Dismiss\nView &Help", 1, 'Warning')
    endif

    if s:tmp == 2
      help g:no_html_toolbar
      " Go to the previous window or everything gets messy:
      wincmd p
    elseif s:tmp == 3
      if has('win32')
        execute '!start RunDll32.exe shell32.dll,ShellExec_RunDLL http://www.infynity.spodzone.com/vim/HTML/#files'
      else
        call LaunchBrowser('default', 2, 'http://www.infynity.spodzone.com/vim/HTML/#files')
      endif
    endif

    unlet s:tmp
  endif

  set guioptions+=T

  " A kluge to overcome a problem with the GTK2 interface:
  command! -nargs=+ HTMLtmenu call s:HTMLtmenu(<f-args>)
  function! s:HTMLtmenu(icon, level, menu, tip)
    if has('gui_gtk2') && v:version <= 602 && ! has('patch240')
      execute 'tmenu icon=' . a:icon . ' ' . a:level . ' ' . a:menu . ' ' . a:tip
    else
      execute 'tmenu ' . a:level . ' ' . a:menu . ' ' . a:tip
    endif
  endfunction

  "tunmenu ToolBar
  silent! unmenu ToolBar
  silent! unmenu! ToolBar

  tmenu 1.10          ToolBar.Open      Open file
  amenu 1.10          ToolBar.Open      :browse e<CR>
  tmenu 1.20          ToolBar.Save      Save current file
  amenu 1.20          ToolBar.Save      :w<CR>
  tmenu 1.30          ToolBar.SaveAll   Save all files
  amenu 1.30          ToolBar.SaveAll   :wa<CR>

   menu 1.50          ToolBar.-sep1-    <nul>

  HTMLtmenu Template  1.60  ToolBar.Template   Create\ Template
  HTMLmenu amenu      1.60  ToolBar.Template   html

   menu               1.65  ToolBar.-sep2-     <nul>

  HTMLtmenu Paragraph 1.70  ToolBar.Paragraph  Create\ Paragraph
  HTMLmenu imenu      1.70  ToolBar.Paragraph  pp
  HTMLmenu vmenu      1.70  ToolBar.Paragraph  pp
  HTMLmenu nmenu      1.70  ToolBar.Paragraph  pp i
  HTMLtmenu Break     1.80  ToolBar.Break      Line\ Break
  HTMLmenu imenu      1.80  ToolBar.Break      br
  HTMLmenu vmenu      1.80  ToolBar.Break      br
  HTMLmenu nmenu      1.80  ToolBar.Break      br i

   menu               1.85  ToolBar.-sep3-     <nul>

  HTMLtmenu Link      1.90  ToolBar.Link       Create\ Hyperlink
  HTMLmenu imenu      1.90  ToolBar.Link       ah
  HTMLmenu vmenu      1.90  ToolBar.Link       ah
  HTMLmenu nmenu      1.90  ToolBar.Link       ah i
  HTMLtmenu Target    1.100 ToolBar.Target     Create\ Target\ (Named\ Anchor)
  HTMLmenu imenu      1.100 ToolBar.Target     an
  HTMLmenu vmenu      1.100 ToolBar.Target     an
  HTMLmenu nmenu      1.100 ToolBar.Target     an i
  HTMLtmenu Image     1.110 ToolBar.Image      Insert\ Image
  HTMLmenu imenu      1.110 ToolBar.Image      im
  HTMLmenu vmenu      1.110 ToolBar.Image      im
  HTMLmenu nmenu      1.110 ToolBar.Image      im i

   menu               1.115 ToolBar.-sep4-     <nul>

  HTMLtmenu Hline     1.120 ToolBar.Hline      Create\ Horizontal\ Rule
  HTMLmenu imenu      1.120 ToolBar.Hline      hr
  HTMLmenu nmenu      1.120 ToolBar.Hline      hr i

   menu               1.125 ToolBar.-sep5-     <nul>

  HTMLtmenu Table     1.130 ToolBar.Table      Create\ Table
  HTMLmenu imenu      1.130 ToolBar.Table     tA <ESC>
  HTMLmenu nmenu      1.130 ToolBar.Table     tA

   menu               1.135 ToolBar.-sep6-     <nul>

  HTMLtmenu Blist     1.140 ToolBar.Blist      Create\ Bullet\ List
  exe 'imenu          1.140 ToolBar.Blist'     g:html_map_leader . 'ul' . g:html_map_leader . 'li'
  exe 'vmenu          1.140 ToolBar.Blist'     g:html_map_leader . 'uli' . g:html_map_leader . 'li<ESC>'
  exe 'nmenu          1.140 ToolBar.Blist'     'i' . g:html_map_leader . 'ul' . g:html_map_leader . 'li'
  HTMLtmenu Nlist     1.150 ToolBar.Nlist      Create\ Numbered\ List
  exe 'imenu          1.150 ToolBar.Nlist'     g:html_map_leader . 'ol' . g:html_map_leader . 'li'
  exe 'vmenu          1.150 ToolBar.Nlist'     g:html_map_leader . 'oli' . g:html_map_leader . 'li<ESC>'
  exe 'nmenu          1.150 ToolBar.Nlist'     'i' . g:html_map_leader . 'ol' . g:html_map_leader . 'li'
  HTMLtmenu Litem     1.160 ToolBar.Litem      Add\ List\ Item
  HTMLmenu imenu      1.160 ToolBar.Litem      li
  HTMLmenu nmenu      1.160 ToolBar.Litem      li i

   menu               1.165 ToolBar.-sep7-     <nul>

  HTMLtmenu Bold      1.170 ToolBar.Bold       Bold
  HTMLmenu imenu      1.170 ToolBar.Bold       bo
  HTMLmenu vmenu      1.170 ToolBar.Bold       bo
  HTMLmenu nmenu      1.170 ToolBar.Bold       bo i
  HTMLtmenu Italic    1.180 ToolBar.Italic     Italic
  HTMLmenu imenu      1.180 ToolBar.Italic     it
  HTMLmenu vmenu      1.180 ToolBar.Italic     it
  HTMLmenu nmenu      1.180 ToolBar.Italic     it i
  HTMLtmenu Underline 1.190 ToolBar.Underline  Underline
  HTMLmenu imenu      1.190 ToolBar.Underline  un
  HTMLmenu vmenu      1.190 ToolBar.Underline  un
  HTMLmenu nmenu      1.190 ToolBar.Underline  un i

   menu               1.195 ToolBar.-sep8-    <nul>

  tmenu               1.200 ToolBar.Cut       Cut to clipboard
  vmenu               1.200 ToolBar.Cut       "*x
  tmenu               1.210 ToolBar.Copy      Copy to clipboard
  vmenu               1.210 ToolBar.Copy      "*y
  tmenu               1.220 ToolBar.Paste     Paste from Clipboard
  nmenu               1.220 ToolBar.Paste     i<C-R>*<Esc>
  vmenu               1.220 ToolBar.Paste     "-xi<C-R>*<Esc>
  menu!               1.220 ToolBar.Paste     <C-R>*

   menu               1.225 ToolBar.-sep9-    <nul>

  tmenu               1.230 ToolBar.Find      Find...
  tmenu               1.240 ToolBar.Replace   Find & Replace

  if has("win32") || has("win16") || has("gui_gtk") || has("gui_motif")
    amenu 1.250 ToolBar.Find    :promptfind<CR>
    vunmenu     ToolBar.Find
    vmenu       ToolBar.Find    y:promptfind <C-R>"<CR>
    amenu 1.260 ToolBar.Replace :promptrepl<CR>
    vunmenu     ToolBar.Replace
    vmenu       ToolBar.Replace y:promptrepl <C-R>"<CR>
  else
    amenu 1.250 ToolBar.Find    /
    amenu 1.260 ToolBar.Replace :%s/
    vunmenu     ToolBar.Replace
    vmenu       ToolBar.Replace :s/
  endif


  if exists("*LaunchBrowser")
    amenu 1.500 ToolBar.-sep50- <nul>

    let s:browsers = LaunchBrowser()

    if s:browsers =~ 'f'
      HTMLtmenu Firefox  1.510 ToolBar.Firefox   Launch\ Firefox\ on\ Current\ File
      HTMLmenu amenu     1.510 ToolBar.Firefox   ff
    elseif s:browsers =~ 'm'
      HTMLtmenu Mozilla  1.510 ToolBar.Mozilla   Launch\ Mozilla\ on\ Current\ File
      HTMLmenu amenu     1.510 ToolBar.Mozilla   mo
    elseif s:browsers =~ 'n'
      HTMLtmenu Netscape 1.510 ToolBar.Netscape  Launch\ Netscape\ on\ Current\ File
      HTMLmenu amenu     1.510 ToolBar.Netscape  ne
    endif

    if s:browsers =~ 'o'
      HTMLtmenu Opera    1.520 ToolBar.Opera     Launch\ Opera\ on\ Current\ File
      HTMLmenu amenu     1.520 ToolBar.Opera     oa
    endif

    if s:browsers =~ 'w'
      HTMLtmenu w3m      1.530 ToolBar.w3m       Launch\ w3m\ on\ Current\ File
      HTMLmenu amenu     1.530 ToolBar.w3m       w3
    elseif s:browsers =~ 'l'
      HTMLtmenu Lynx     1.530 ToolBar.Lynx      Launch\ Lynx\ on\ Current\ File
      HTMLmenu amenu     1.530 ToolBar.Lynx      ly
    endif

  elseif maparg(g:html_map_leader . 'db', 'n') != ""
    amenu 1.500 ToolBar.-sep50- <nul>

    tmenu 1.510 ToolBar.Browser Launch Default Browser on Current File
    HTMLmenu amenu 1.510 ToolBar.Browser db
  endif

  amenu 1.998 ToolBar.-sep99- <nul>
  tmenu 1.999 ToolBar.Help    HTML Help
  amenu 1.999 ToolBar.Help    :help HTML<CR>

  delcommand HTMLtmenu
  delfunction s:HTMLtmenu

  let did_html_toolbar = 1
endif  " (! exists('g:no_html_toolbar')) && (has("toolbar") || has("win32") [...]
" ----------------------------------------------------------------------------


" ---- Menu Items: ------------------------------------------------------ {{{1

" Add to the PopUp menu:   {{{2
nnoremenu 1.91 PopUp.Select\ Ta&g vat
onoremenu 1.91 PopUp.Select\ Ta&g at
vnoremenu 1.91 PopUp.Select\ Ta&g <C-C>vat
inoremenu 1.91 PopUp.Select\ Ta&g <C-O>vat
cnoremenu 1.91 PopUp.Select\ Ta&g <C-C>vat

nnoremenu 1.92 PopUp.Select\ &Inner\ Ta&g vit
onoremenu 1.92 PopUp.Select\ &Inner\ Ta&g it
vnoremenu 1.92 PopUp.Select\ &Inner\ Ta&g <C-C>vit
inoremenu 1.92 PopUp.Select\ &Inner\ Ta&g <C-O>vit
cnoremenu 1.92 PopUp.Select\ &Inner\ Ta&g <C-C>vit
" }}}2

augroup HTML_menu_autos
au!
"autocmd BufLeave * call s:HTMLmenuControl()
autocmd BufEnter,WinEnter * call s:HTMLmenuControl()
augroup END

amenu HTM&L.Disable\ Mappings<tab>:HTMLmappings\ disable :HTMLmappings disable<CR>
amenu HTM&L.Enable\ Mappings<tab>:HTMLmappings\ enable :HTMLmappings enable<CR>
amenu disable HTML.Enable\ Mappings

 menu HTML.-sep1- <nul>
amenu HTML.HTML\ Help<TAB>:help\ HTML :help HTML<CR>
 menu HTML.-sep2- <nul>

if exists("*LaunchBrowser")
  let s:browsers = LaunchBrowser()

  if s:browsers =~ 'f'
    HTMLmenu amenu - HTML.Preview.Firefox                 ff
    HTMLmenu amenu - HTML.Preview.Firefox\ (New\ Window)  nff
    HTMLmenu amenu - HTML.Preview.Firefox\ (New\ Tab)     tff
    amenu HTML.Preview.-sep1-                             <nop>
  endif
  if s:browsers =~ 'm'
    HTMLmenu amenu - HTML.Preview.Mozilla                 mo
    HTMLmenu amenu - HTML.Preview.Mozilla\ (New\ Window)  nmo
    HTMLmenu amenu - HTML.Preview.Mozilla\ (New\ Tab)     tmo
    amenu HTML.Preview.-sep2-                             <nop>
  endif
  if s:browsers =~ 'n'
    HTMLmenu amenu - HTML.Preview.Netscape                ne
    HTMLmenu amenu - HTML.Preview.Netscape\ (New\ Window) nne
    amenu HTML.Preview.-sep3-                             <nop>
  endif
  if s:browsers =~ 'o'
    HTMLmenu amenu - HTML.Preview.Opera                   oa
    HTMLmenu amenu - HTML.Preview.Opera\ (New\ Window)    noa
    HTMLmenu amenu - HTML.Preview.Opera\ (New\ Tab)       toa
    amenu HTML.Preview.-sep4-                             <nop>
  endif
  if s:browsers =~ 'l'
    HTMLmenu amenu - HTML.Preview.Lynx                    ly
  endif
  if s:browsers =~ 'w'
    HTMLmenu amenu - HTML.Preview.w3m                     w3
  endif
elseif maparg(g:html_map_leader . 'db', 'n') != ""
  HTMLmenu amenu - HTML.Preview.Default\ Browser    db
  HTMLmenu amenu - HTML.Preview.Internet\ Explorer  ie
endif

 menu HTML.-sep3- <nul>

HTMLmenu amenu - HTM&L.Template html

 menu HTML.-sep4- <nul>

" Character Entities menu:   {{{2

let b:save_encoding=&encoding
let &encoding='latin1'

nmenu HTML.Character\ Entities.Convert\ to\ Entity<tab>;\&         ;&
vmenu HTML.Character\ Entities.Convert\ to\ Entity<tab>;\&         ;&
vmenu HTML.Character\ Entities.Convert\ from\ Entities<tab>;^      ;^
 menu HTML.Character\ Entities.-sep0- <nul>
imenu HTML.Character\ Entities.Ampersand<tab>\&\&                  &&
imenu HTML.Character\ Entities.Greaterthan\ (>)<tab>\&>            &>
imenu HTML.Character\ Entities.Lessthan\ (<)<tab>\&<               &<
imenu HTML.Character\ Entities.Space\ (nonbreaking\)<tab>\&<space> &<space>
imenu HTML.Character\ Entities.Quotation\ mark\ (")<tab>\&'        &'
 menu HTML.Character\ Entities.-sep1- <nul>
imenu HTML.Character\ Entities.Cent\ (¢)<tab>\&c\|                 &c\|
imenu HTML.Character\ Entities.Pound\ (£)<tab>\&#                  &#
imenu HTML.Character\ Entities.Yen\ (¥)<tab>\&Y=                   &Y=
imenu HTML.Character\ Entities.Left\ Angle\ Quote\ («)<tab>\&2<    &2<
imenu HTML.Character\ Entities.Right\ Angle\ Quote\ (»)<tab>\&2>   &2>
imenu HTML.Character\ Entities.Copyright\ (©)<tab>\&cO             &cO
imenu HTML.Character\ Entities.Registered\ (®)<tab>\&rO            &rO
imenu HTML.Character\ Entities.Trademark\ (TM)<tab>\&tm            &tm
imenu HTML.Character\ Entities.Multiply\ (×)<tab>\&x               &x
imenu HTML.Character\ Entities.Divide\ (÷)<tab>\&/                 &/
imenu HTML.Character\ Entities.Inverted\ Exlamation\ (¡)<tab>\&!   &!
imenu HTML.Character\ Entities.Inverted\ Question\ (¿)<tab>\&?     &?
imenu HTML.Character\ Entities.Degree\ (°)<tab>\&dg                &dg
imenu HTML.Character\ Entities.Micro\ (µ)<tab>\&mi                 &mi
imenu HTML.Character\ Entities.Paragraph\ (¶)<tab>\&pa             &pa
imenu HTML.Character\ Entities.Middle\ Dot\ (·)<tab>\&\.           &.
imenu HTML.Character\ Entities.One\ Quarter\ (¼)<tab>\&14          &14
imenu HTML.Character\ Entities.One\ Half\ (½)<tab>\&12             &12
imenu HTML.Character\ Entities.Three\ Quarters\ (¾)<tab>\&34       &34
imenu HTML.Character\ Entities.En\ dash\ (-)<tab>\&n-/\&2-         &n-
imenu HTML.Character\ Entities.Em\ dash\ (--)<tab>\&m-/\&--/\&3-   &m-
imenu HTML.Character\ Entities.Ellipsis\ (\.\.\.)<tab>\&3\.        &3.
imenu HTML.Character\ Entities.-sep2- <nul>
imenu HTML.Character\ Entities.Graves.A-grave\ (À)<tab>\&A` &A`
imenu HTML.Character\ Entities.Graves.a-grave\ (à)<tab>\&a` &a`
imenu HTML.Character\ Entities.Graves.E-grave\ (È)<tab>\&E` &E`
imenu HTML.Character\ Entities.Graves.e-grave\ (è)<tab>\&e` &e`
imenu HTML.Character\ Entities.Graves.I-grave\ (Ì)<tab>\&I` &I`
imenu HTML.Character\ Entities.Graves.i-grave\ (ì)<tab>\&i` &i`
imenu HTML.Character\ Entities.Graves.O-grave\ (Ò)<tab>\&O` &O`
imenu HTML.Character\ Entities.Graves.o-grave\ (ò)<tab>\&o` &o`
imenu HTML.Character\ Entities.Graves.U-grave\ (Ù)<tab>\&U` &U`
imenu HTML.Character\ Entities.Graves.u-grave\ (ù)<tab>\&u` &u`
imenu HTML.Character\ Entities.Acutes.A-acute\ (Á)<tab>\&A' &A'
imenu HTML.Character\ Entities.Acutes.a-acute\ (á)<tab>\&a' &a'
imenu HTML.Character\ Entities.Acutes.E-acute\ (É)<tab>\&E' &E'
imenu HTML.Character\ Entities.Acutes.e-acute\ (é)<tab>\&e' &e'
imenu HTML.Character\ Entities.Acutes.I-acute\ (Í)<tab>\&I' &I'
imenu HTML.Character\ Entities.Acutes.i-acute\ (í)<tab>\&i' &i'
imenu HTML.Character\ Entities.Acutes.O-acute\ (Ó)<tab>\&O' &O'
imenu HTML.Character\ Entities.Acutes.o-acute\ (ó)<tab>\&o' &o'
imenu HTML.Character\ Entities.Acutes.U-acute\ (Ú)<tab>\&U' &U'
imenu HTML.Character\ Entities.Acutes.u-acute\ (ú)<tab>\&u' &u'
imenu HTML.Character\ Entities.Acutes.Y-acute\ (Ý)<tab>\&Y' &Y'
imenu HTML.Character\ Entities.Acutes.y-acute\ (ý)<tab>\&y' &y'
imenu HTML.Character\ Entities.Tildes.A-tilde\ (Ã)<tab>\&A~ &A~
imenu HTML.Character\ Entities.Tildes.a-tilde\ (ã)<tab>\&a~ &a~
imenu HTML.Character\ Entities.Tildes.N-tilde\ (Ñ)<tab>\&N~ &N~
imenu HTML.Character\ Entities.Tildes.n-tilde\ (ñ)<tab>\&n~ &n~
imenu HTML.Character\ Entities.Tildes.O-tilde\ (Õ)<tab>\&O~ &O~
imenu HTML.Character\ Entities.Tildes.o-tilde\ (õ)<tab>\&o~ &o~
imenu HTML.Character\ Entities.Circumflexes.A-circumflex\ (Â)<tab>\&A^ &A^
imenu HTML.Character\ Entities.Circumflexes.a-circumflex\ (â)<tab>\&a^ &a^
imenu HTML.Character\ Entities.Circumflexes.E-circumflex\ (Ê)<tab>\&E^ &E^
imenu HTML.Character\ Entities.Circumflexes.e-circumflex\ (ê)<tab>\&e^ &e^
imenu HTML.Character\ Entities.Circumflexes.I-circumflex\ (Î)<tab>\&I^ &I^
imenu HTML.Character\ Entities.Circumflexes.i-circumflex\ (î)<tab>\&i^ &i^
imenu HTML.Character\ Entities.Circumflexes.O-circumflex\ (Ô)<tab>\&O^ &O^
imenu HTML.Character\ Entities.Circumflexes.o-circumflex\ (ô)<tab>\&o^ &o^
imenu HTML.Character\ Entities.Circumflexes.U-circumflex\ (Û)<tab>\&U^ &U^
imenu HTML.Character\ Entities.Circumflexes.u-circumflex\ (û)<tab>\&u^ &u^
imenu HTML.Character\ Entities.Umlauts.A-umlaut\ (Ä)<tab>\&A" &A"
imenu HTML.Character\ Entities.Umlauts.a-umlaut\ (ä)<tab>\&a" &a"
imenu HTML.Character\ Entities.Umlauts.E-umlaut\ (Ë)<tab>\&E" &E"
imenu HTML.Character\ Entities.Umlauts.e-umlaut\ (ë)<tab>\&e" &e"
imenu HTML.Character\ Entities.Umlauts.I-umlaut\ (Ï)<tab>\&I" &I"
imenu HTML.Character\ Entities.Umlauts.i-umlaut\ (ï)<tab>\&i" &i"
imenu HTML.Character\ Entities.Umlauts.O-umlaut\ (Ö)<tab>\&O" &O"
imenu HTML.Character\ Entities.Umlauts.o-umlaut\ (ö)<tab>\&o" &o"
imenu HTML.Character\ Entities.Umlauts.U-umlaut\ (Ü)<tab>\&U" &U"
imenu HTML.Character\ Entities.Umlauts.u-umlaut\ (ü)<tab>\&u" &u"
imenu HTML.Character\ Entities.Umlauts.y-umlaut\ (ÿ)<tab>\&y" &y"
imenu HTML.Character\ Entities.Umlauts.Umlaut\ (¨)<tab>\&"    &"
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Alpha<tab>\&Al    &Al
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Beta<tab>\&Be     &Be
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Gamma<tab>\&Ga    &Ga
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Delta<tab>\&De    &De
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Epsilon<tab>\&Ep  &Ep
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Zeta<tab>\&Ze     &Ze
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Eta<tab>\&Et      &Et
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Theta<tab>\&Th    &Th
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Iota<tab>\&Io     &Io
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Kappa<tab>\&Ka    &Ka
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Lambda<tab>\&Lm   &Lm
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Mu<tab>\&Mu       &Mu
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Nu<tab>\&Nu       &Nu
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Xi<tab>\&Xi       &Xi
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Omicron<tab>\&Oc  &Oc
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Pi<tab>\&Pi       &Pi
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Rho<tab>\&Rh      &Rh
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Sigma<tab>\&Si    &Si
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Tau<tab>\&Ta      &Ta
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Upsilon<tab>\&Up  &Up
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Phi<tab>\&Ph      &Ph
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Chi<tab>\&Ch      &Ch
imenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Psi<tab>\&Ps      &Ps
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.alpha<tab>\&al    &al
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.beta<tab>\&be     &be
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.gamma<tab>\&ga    &ga
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.delta<tab>\&de    &de
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.epsilon<tab>\&ep  &ep
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.zeta<tab>\&ze     &ze
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.eta<tab>\&et      &et
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.theta<tab>\&th    &th
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.iota<tab>\&io     &io
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.kappa<tab>\&ka    &ka
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.lambda<tab>\&lm   &lm
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.mu<tab>\&mu       &mu
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.nu<tab>\&nu       &nu
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.xi<tab>\&xi       &xi
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.omicron<tab>\&oc  &oc
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.pi<tab>\&pi       &pi
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.rho<tab>\&rh      &rh
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.sigma<tab>\&si    &si
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.sigmaf<tab>\&sf   &sf
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.tau<tab>\&ta      &ta
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.upsilon<tab>\&up  &up
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.phi<tab>\&ph      &ph
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.chi<tab>\&ch      &ch
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.psi<tab>\&ps      &ps
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.omega<tab>\&og    &og
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.thetasym<tab>\&ts &ts
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.upsih<tab>\&uh    &uh
imenu HTML.Character\ Entities.Greek\ Letters.Lowercase.piv<tab>\&pv      &pv
imenu HTML.Character\ Entities.Arrows.Left\ single\ arrow<tab>\&la       &la
imenu HTML.Character\ Entities.Arrows.Right\ single\ arrow<tab>\&ra      &ra
imenu HTML.Character\ Entities.Arrows.Up\ single\ arrow<tab>\&ua         &ua
imenu HTML.Character\ Entities.Arrows.Down\ single\ arrow<tab>\&da       &da
imenu HTML.Character\ Entities.Arrows.Left-right\ single\ arrow<tab>\&ha &ha
imenu HTML.Character\ Entities.Arrows.-sep1-                             <nul>
imenu HTML.Character\ Entities.Arrows.Left\ double\ arrow<tab>\&lA       &lA
imenu HTML.Character\ Entities.Arrows.Right\ double\ arrow<tab>\&rA      &rA
imenu HTML.Character\ Entities.Arrows.Up\ double\ arrow<tab>\&uA         &uA
imenu HTML.Character\ Entities.Arrows.Down\ double\ arrow<tab>\&dA       &dA
imenu HTML.Character\ Entities.Arrows.Left-right\ double\ arrow<tab>\&hA &hA
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..A-ring\ (Å)<tab>\&Ao      &Ao
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..a-ring\ (å)<tab>\&ao      &ao
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..AE-ligature\ (Æ)<tab>\&AE &AE
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..ae-ligature\ (æ)<tab>\&ae &ae
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..C-cedilla\ (Ç)<tab>\&C,   &C,
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..c-cedilla\ (ç)<tab>\&c,   &c,
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..O-slash\ (Ø)<tab>\&O/     &O/
imenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..o-slash\ (ø)<tab>\&o/     &o/
" Normal mode versions of the above.  If you change the above, it's usually
" easier to just delete it yank the above, paste it, and run a pair of
" substitute commands.
nmenu HTML.Character\ Entities.Ampersand<tab>\&\&                  i&&<ESC>
nmenu HTML.Character\ Entities.Greaterthan\ (>)<tab>\&>            i&><ESC>
nmenu HTML.Character\ Entities.Lessthan\ (<)<tab>\&<               i&<<ESC>
nmenu HTML.Character\ Entities.Space\ (nonbreaking\)<tab>\&<space> i&<space><ESC>
nmenu HTML.Character\ Entities.Quotation\ mark\ (")<tab>\&'        i&'<ESC>
nmenu HTML.Character\ Entities.Cent\ (¢)<tab>\&c\|                 i&c\|<ESC>
nmenu HTML.Character\ Entities.Pound\ (£)<tab>\&#                  i&#<ESC>
nmenu HTML.Character\ Entities.Yen\ (¥)<tab>\&Y=                   i&Y=<ESC>
nmenu HTML.Character\ Entities.Left\ Angle\ Quote\ («)<tab>\&2<    i&2<<ESC>
nmenu HTML.Character\ Entities.Right\ Angle\ Quote\ (»)<tab>\&2>   i&2><ESC>
nmenu HTML.Character\ Entities.Copyright\ (©)<tab>\&cO             i&cO<ESC>
nmenu HTML.Character\ Entities.Registered\ (®)<tab>\&rO            i&rO<ESC>
nmenu HTML.Character\ Entities.Trademark\ (TM)<tab>\&tm            i&tm<ESC>
nmenu HTML.Character\ Entities.Multiply\ (×)<tab>\&x               i&x<ESC>
nmenu HTML.Character\ Entities.Divide\ (÷)<tab>\&/                 i&/<ESC>
nmenu HTML.Character\ Entities.Inverted\ Exlamation\ (¡)<tab>\&!   i&!<ESC>
nmenu HTML.Character\ Entities.Inverted\ Question\ (¿)<tab>\&?     i&?<ESC>
nmenu HTML.Character\ Entities.Degree\ (°)<tab>\&dg                i&dg<ESC>
nmenu HTML.Character\ Entities.Micro\ (µ)<tab>\&mi                 i&mi<ESC>
nmenu HTML.Character\ Entities.Paragraph\ (¶)<tab>\&pa             i&pa<ESC>
nmenu HTML.Character\ Entities.Middle\ Dot\ (·)<tab>\&\.           i&.<ESC>
nmenu HTML.Character\ Entities.One\ Quarter\ (¼)<tab>\&14          i&14<ESC>
nmenu HTML.Character\ Entities.One\ Half\ (½)<tab>\&12             i&12<ESC>
nmenu HTML.Character\ Entities.Three\ Quarters\ (¾)<tab>\&34       i&34<ESC>
nmenu HTML.Character\ Entities.En\ dash\ (-)<tab>\&n-              i&n-
nmenu HTML.Character\ Entities.Em\ dash\ (--)<tab>\&m-/\&--        i&m-
nmenu HTML.Character\ Entities.Ellipsis\ (\.\.\.)<tab>\&3\.        i&3.
nmenu HTML.Character\ Entities.Graves.A-grave\ (À)<tab>\&A` i&A`<ESC>
nmenu HTML.Character\ Entities.Graves.a-grave\ (à)<tab>\&a` i&a`<ESC>
nmenu HTML.Character\ Entities.Graves.E-grave\ (È)<tab>\&E` i&E`<ESC>
nmenu HTML.Character\ Entities.Graves.e-grave\ (è)<tab>\&e` i&e`<ESC>
nmenu HTML.Character\ Entities.Graves.I-grave\ (Ì)<tab>\&I` i&I`<ESC>
nmenu HTML.Character\ Entities.Graves.i-grave\ (ì)<tab>\&i` i&i`<ESC>
nmenu HTML.Character\ Entities.Graves.O-grave\ (Ò)<tab>\&O` i&O`<ESC>
nmenu HTML.Character\ Entities.Graves.o-grave\ (ò)<tab>\&o` i&o`<ESC>
nmenu HTML.Character\ Entities.Graves.U-grave\ (Ù)<tab>\&U` i&U`<ESC>
nmenu HTML.Character\ Entities.Graves.u-grave\ (ù)<tab>\&u` i&u`<ESC>
nmenu HTML.Character\ Entities.Acutes.A-acute\ (Á)<tab>\&A' i&A'<ESC>
nmenu HTML.Character\ Entities.Acutes.a-acute\ (á)<tab>\&a' i&a'<ESC>
nmenu HTML.Character\ Entities.Acutes.E-acute\ (É)<tab>\&E' i&E'<ESC>
nmenu HTML.Character\ Entities.Acutes.e-acute\ (é)<tab>\&e' i&e'<ESC>
nmenu HTML.Character\ Entities.Acutes.I-acute\ (Í)<tab>\&I' i&I'<ESC>
nmenu HTML.Character\ Entities.Acutes.i-acute\ (í)<tab>\&i' i&i'<ESC>
nmenu HTML.Character\ Entities.Acutes.O-acute\ (Ó)<tab>\&O' i&O'<ESC>
nmenu HTML.Character\ Entities.Acutes.o-acute\ (ó)<tab>\&o' i&o'<ESC>
nmenu HTML.Character\ Entities.Acutes.U-acute\ (Ú)<tab>\&U' i&U'<ESC>
nmenu HTML.Character\ Entities.Acutes.u-acute\ (ú)<tab>\&u' i&u'<ESC>
nmenu HTML.Character\ Entities.Acutes.Y-acute\ (Ý)<tab>\&Y' i&Y'<ESC>
nmenu HTML.Character\ Entities.Acutes.y-acute\ (ý)<tab>\&y' i&y'<ESC>
nmenu HTML.Character\ Entities.Tildes.A-tilde\ (Ã)<tab>\&A~ i&A~<ESC>
nmenu HTML.Character\ Entities.Tildes.a-tilde\ (ã)<tab>\&a~ i&a~<ESC>
nmenu HTML.Character\ Entities.Tildes.N-tilde\ (Ñ)<tab>\&N~ i&N~<ESC>
nmenu HTML.Character\ Entities.Tildes.n-tilde\ (ñ)<tab>\&n~ i&n~<ESC>
nmenu HTML.Character\ Entities.Tildes.O-tilde\ (Õ)<tab>\&O~ i&O~<ESC>
nmenu HTML.Character\ Entities.Tildes.o-tilde\ (õ)<tab>\&o~ i&o~<ESC>
nmenu HTML.Character\ Entities.Circumflexes.A-circumflex\ (Â)<tab>\&A^ i&A^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.a-circumflex\ (â)<tab>\&a^ i&a^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.E-circumflex\ (Ê)<tab>\&E^ i&E^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.e-circumflex\ (ê)<tab>\&e^ i&e^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.I-circumflex\ (Î)<tab>\&I^ i&I^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.i-circumflex\ (î)<tab>\&i^ i&i^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.O-circumflex\ (Ô)<tab>\&O^ i&O^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.o-circumflex\ (ô)<tab>\&o^ i&o^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.U-circumflex\ (Û)<tab>\&U^ i&U^<ESC>
nmenu HTML.Character\ Entities.Circumflexes.u-circumflex\ (û)<tab>\&u^ i&u^<ESC>
nmenu HTML.Character\ Entities.Umlauts.A-umlaut\ (Ä)<tab>\&A" i&A"<ESC>
nmenu HTML.Character\ Entities.Umlauts.a-umlaut\ (ä)<tab>\&a" i&a"<ESC>
nmenu HTML.Character\ Entities.Umlauts.E-umlaut\ (Ë)<tab>\&E" i&E"<ESC>
nmenu HTML.Character\ Entities.Umlauts.e-umlaut\ (ë)<tab>\&e" i&e"<ESC>
nmenu HTML.Character\ Entities.Umlauts.I-umlaut\ (Ï)<tab>\&I" i&I"<ESC>
nmenu HTML.Character\ Entities.Umlauts.i-umlaut\ (ï)<tab>\&i" i&i"<ESC>
nmenu HTML.Character\ Entities.Umlauts.O-umlaut\ (Ö)<tab>\&O" i&O"<ESC>
nmenu HTML.Character\ Entities.Umlauts.o-umlaut\ (ö)<tab>\&o" i&o"<ESC>
nmenu HTML.Character\ Entities.Umlauts.U-umlaut\ (Ü)<tab>\&U" i&U"<ESC>
nmenu HTML.Character\ Entities.Umlauts.u-umlaut\ (ü)<tab>\&u" i&u"<ESC>
nmenu HTML.Character\ Entities.Umlauts.y-umlaut\ (ÿ)<tab>\&y" i&y"<ESC>
nmenu HTML.Character\ Entities.Umlauts.Umlaut\ (¨)<tab>\&"    i&"<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Alpha<tab>\&Al    i&Al<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Beta<tab>\&Be     i&Be<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Gamma<tab>\&Ga    i&Ga<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Delta<tab>\&De    i&De<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Epsilon<tab>\&Ep  i&Ep<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Zeta<tab>\&Ze     i&Ze<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Eta<tab>\&Et      i&Et<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Theta<tab>\&Th    i&Th<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Iota<tab>\&Io     i&Io<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Kappa<tab>\&Ka    i&Ka<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Lambda<tab>\&Lm   i&Lm<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Mu<tab>\&Mu       i&Mu<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Nu<tab>\&Nu       i&Nu<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Xi<tab>\&Xi       i&Xi<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Omicron<tab>\&Oc  i&Oc<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Pi<tab>\&Pi       i&Pi<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Rho<tab>\&Rh      i&Rh<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Sigma<tab>\&Si    i&Si<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Tau<tab>\&Ta      i&Ta<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Upsilon<tab>\&Up  i&Up<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Phi<tab>\&Ph      i&Ph<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Chi<tab>\&Ch      i&Ch<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Uppercase.Psi<tab>\&Ps      i&Ps<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.alpha<tab>\&al    i&al<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.beta<tab>\&be     i&be<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.gamma<tab>\&ga    i&ga<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.delta<tab>\&de    i&de<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.epsilon<tab>\&ep  i&ep<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.zeta<tab>\&ze     i&ze<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.eta<tab>\&et      i&et<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.theta<tab>\&th    i&th<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.iota<tab>\&io     i&io<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.kappa<tab>\&ka    i&ka<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.lambda<tab>\&lm   i&lm<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.mu<tab>\&mu       i&mu<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.nu<tab>\&nu       i&nu<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.xi<tab>\&xi       i&xi<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.omicron<tab>\&oc  i&oc<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.pi<tab>\&pi       i&pi<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.rho<tab>\&rh      i&rh<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.sigma<tab>\&si    i&si<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.sigmaf<tab>\&sf   i&sf<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.tau<tab>\&ta      i&ta<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.upsilon<tab>\&up  i&up<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.phi<tab>\&ph      i&ph<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.chi<tab>\&ch      i&ch<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.psi<tab>\&ps      i&ps<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.omega<tab>\&og    i&og<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.thetasym<tab>\&ts i&ts<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.upsih<tab>\&uh    i&uh<ESC>
nmenu HTML.Character\ Entities.Greek\ Letters.Lowercase.piv<tab>\&pv      i&pv<ESC>
nmenu HTML.Character\ Entities.Arrows.Left\ single\ arrow<tab>\&la       i&la<ESC>
nmenu HTML.Character\ Entities.Arrows.Right\ single\ arrow<tab>\&ra      i&ra<ESC>
nmenu HTML.Character\ Entities.Arrows.Up\ single\ arrow<tab>\&ua         i&ua<ESC>
nmenu HTML.Character\ Entities.Arrows.Down\ single\ arrow<tab>\&da       i&da<ESC>
nmenu HTML.Character\ Entities.Arrows.Left-right\ single\ arrow<tab>\&ha i&ha<ESC>
nmenu HTML.Character\ Entities.Arrows.Left\ double\ arrow<tab>\&lA       i&lA<ESC>
nmenu HTML.Character\ Entities.Arrows.Right\ double\ arrow<tab>\&rA      i&rA<ESC>
nmenu HTML.Character\ Entities.Arrows.Up\ double\ arrow<tab>\&uA         i&uA<ESC>
nmenu HTML.Character\ Entities.Arrows.Down\ double\ arrow<tab>\&dA       i&dA<ESC>
nmenu HTML.Character\ Entities.Arrows.Left-right\ double\ arrow<tab>\&hA i&hA<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..A-ring\ (Å)<tab>\&Ao      i&Ao<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..a-ring\ (å)<tab>\&ao      i&ao<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..AE-ligature\ (Æ)<tab>\&AE i&AE<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..ae-ligature\ (æ)<tab>\&ae i&ae<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..C-cedilla\ (Ç)<tab>\&C,   i&C,<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..c-cedilla\ (ç)<tab>\&c,   i&c,<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..O-slash\ (Ø)<tab>\&O/     i&O/<ESC>
nmenu HTML.Character\ Entities.\ \ \ \ \ \ \ etc\.\.\..o-slash\ (ø)<tab>\&o/     i&o/<ESC>

let &encoding=b:save_encoding
unlet b:save_encoding

" Colors menu:   {{{2

nmenu HTML.Colors.&A.AliceBlue<TAB>(#F0F8FF)            i#F0F8FF<ESC>
nmenu HTML.Colors.&A.AntiqueWhite<TAB>(#FAEBD7)         i#FAEBD7<ESC>
nmenu HTML.Colors.&A.Aqua<TAB>(#00FFFF)                 i#00FFFF<ESC>
nmenu HTML.Colors.&A.Aquamarine<TAB>(#7FFFD4)           i#7FFFD4<ESC>
nmenu HTML.Colors.&A.Azure<TAB>(#F0FFFF)                i#F0FFFF<ESC>

nmenu HTML.Colors.&B.Beige<TAB>(#F5F5DC)                i#F5F5DC<ESC>
nmenu HTML.Colors.&B.Bisque<TAB>(#FFE4C4)               i#FFE4C4<ESC>
nmenu HTML.Colors.&B.Black<TAB>(#000000)                i#000000<ESC>
nmenu HTML.Colors.&B.BlanchedAlmond<TAB>(#FFEBCD)       i#FFEBCD<ESC>
nmenu HTML.Colors.&B.Blue<TAB>(#0000FF)                 i#0000FF<ESC>
nmenu HTML.Colors.&B.BlueViolet<TAB>(#8A2BE2)           i#8A2BE2<ESC>
nmenu HTML.Colors.&B.Brown<TAB>(#A52A2A)                i#A52A2A<ESC>
nmenu HTML.Colors.&B.Burlywood<TAB>(#DEB887)            i#DEB887<ESC>

nmenu HTML.Colors.&C.CadetBlue<TAB>(#5F9EA0)            i#5F9EA0<ESC>
nmenu HTML.Colors.&C.Chartreuse<TAB>(#7FFF00)           i#7FFF00<ESC>
nmenu HTML.Colors.&C.Chocolate<TAB>(#D2691E)            i#D2691E<ESC>
nmenu HTML.Colors.&C.Coral<TAB>(#FF7F50)                i#FF7F50<ESC>
nmenu HTML.Colors.&C.CornflowerBlue<TAB>(#6495ED)       i#6495ED<ESC>
nmenu HTML.Colors.&C.Cornsilk<TAB>(#FFF8DC)             i#FFF8DC<ESC>
nmenu HTML.Colors.&C.Crimson<TAB>(#DC143C)              i#DC143C<ESC>
nmenu HTML.Colors.&C.Cyan<TAB>(#00FFFF)                 i#00FFFF<ESC>

nmenu HTML.Colors.&D.DarkBlue<TAB>(#00008B)             i#00008B<ESC>
nmenu HTML.Colors.&D.DarkCyan<TAB>(#008B8B)             i#008B8B<ESC>
nmenu HTML.Colors.&D.DarkGoldenrod<TAB>(#B8860B)        i#B8860B<ESC>
nmenu HTML.Colors.&D.DarkGray<TAB>(#A9A9A9)             i#A9A9A9<ESC>
nmenu HTML.Colors.&D.DarkGreen<TAB>(#006400)            i#006400<ESC>
nmenu HTML.Colors.&D.DarkKhaki<TAB>(#BDB76B)            i#BDB76B<ESC>
nmenu HTML.Colors.&D.DarkMagenta<TAB>(#8B008B)          i#8B008B<ESC>
nmenu HTML.Colors.&D.DarkOliveGreen<TAB>(#556B2F)       i#556B2F<ESC>
nmenu HTML.Colors.&D.DarkOrange<TAB>(#FF8C00)           i#FF8C00<ESC>
nmenu HTML.Colors.&D.DarkOrchid<TAB>(#9932CC)           i#9932CC<ESC>
nmenu HTML.Colors.&D.DarkRed<TAB>(#8B0000)              i#8B0000<ESC>
nmenu HTML.Colors.&D.DarkSalmon<TAB>(#E9967A)           i#E9967A<ESC>
nmenu HTML.Colors.&D.DarkSeagreen<TAB>(#8FBC8F)         i#8FBC8F<ESC>
nmenu HTML.Colors.&D.DarkSlateBlue<TAB>(#483D8B)        i#483D8B<ESC>
nmenu HTML.Colors.&D.DarkSlateGray<TAB>(#2F4F4F)        i#2F4F4F<ESC>
nmenu HTML.Colors.&D.DarkTurquoise<TAB>(#00CED1)        i#00CED1<ESC>
nmenu HTML.Colors.&D.DarkViolet<TAB>(#9400D3)           i#9400D3<ESC>
nmenu HTML.Colors.&D.DeepPink<TAB>(#FF1493)             i#FF1493<ESC>
nmenu HTML.Colors.&D.DeepSkyblue<TAB>(#00BFFF)          i#00BFFF<ESC>
nmenu HTML.Colors.&D.DimGray<TAB>(#696969)              i#696969<ESC>
nmenu HTML.Colors.&D.Dodgerblue<TAB>(#1E90FF)           i#1E90FF<ESC>

nmenu HTML.Colors.&F.Firebrick<TAB>(#B22222)            i#B22222<ESC>
nmenu HTML.Colors.&F.FloralWhite<TAB>(#FFFAF0)          i#FFFAF0<ESC>
nmenu HTML.Colors.&F.ForestGreen<TAB>(#228B22)          i#228B22<ESC>
nmenu HTML.Colors.&F.Fuchsia<TAB>(#FF00FF)              i#FF00FF<ESC>

nmenu HTML.Colors.&G.Gainsboro<TAB>(#DCDCDC)            i#DCDCDC<ESC>
nmenu HTML.Colors.&G.GhostWhite<TAB>(#F8F8FF)           i#F8F8FF<ESC>
nmenu HTML.Colors.&G.Gold<TAB>(#FFD700)                 i#FFD700<ESC>
nmenu HTML.Colors.&G.Goldenrod<TAB>(#DAA520)            i#DAA520<ESC>
nmenu HTML.Colors.&G.Gray<TAB>(#808080)                 i#808080<ESC>
nmenu HTML.Colors.&G.Green<TAB>(#008000)                i#008000<ESC>
nmenu HTML.Colors.&G.GreenYellow<TAB>(#ADFF2F)          i#ADFF2F<ESC>

nmenu HTML.Colors.&H-K.Honeydew<TAB>(#F0FFF0)           i#F0FFF0<ESC>
nmenu HTML.Colors.&H-K.HotPink<TAB>(#FF69B4)            i#FF69B4<ESC>
nmenu HTML.Colors.&H-K.IndianRed<TAB>(#CD5C5C)          i#CD5C5C<ESC>
nmenu HTML.Colors.&H-K.Indigo<TAB>(#4B0082)             i#4B0082<ESC>
nmenu HTML.Colors.&H-K.Ivory<TAB>(#FFFFF0)              i#FFFFF0<ESC>
nmenu HTML.Colors.&H-K.Khaki<TAB>(#F0E68C)              i#F0E68C<ESC>

nmenu HTML.Colors.&L.Lavender<TAB>(#E6E6FA)             i#E6E6FA<ESC>
nmenu HTML.Colors.&L.LavenderBlush<TAB>(#FFF0F5)        i#FFF0F5<ESC>
nmenu HTML.Colors.&L.LawnGreen<TAB>(#7CFC00)            i#7CFC00<ESC>
nmenu HTML.Colors.&L.LemonChiffon<TAB>(#FFFACD)         i#FFFACD<ESC>
nmenu HTML.Colors.&L.LightBlue<TAB>(#ADD8E6)            i#ADD8E6<ESC>
nmenu HTML.Colors.&L.LightCoral<TAB>(#F08080)           i#F08080<ESC>
nmenu HTML.Colors.&L.LightCyan<TAB>(#E0FFFF)            i#E0FFFF<ESC>
nmenu HTML.Colors.&L.LightGoldenrodYellow<TAB>(#FAFAD2) i#FAFAD2<ESC>
nmenu HTML.Colors.&L.LightGreen<TAB>(#90EE90)           i#90EE90<ESC>
nmenu HTML.Colors.&L.LightGrey<TAB>(#D3D3D3)            i#D3D3D3<ESC>
nmenu HTML.Colors.&L.LightPink<TAB>(#FFB6C1)            i#FFB6C1<ESC>
nmenu HTML.Colors.&L.LightSalmon<TAB>(#FFA07A)          i#FFA07A<ESC>
nmenu HTML.Colors.&L.LightSeaGreen<TAB>(#20B2AA)        i#20B2AA<ESC>
nmenu HTML.Colors.&L.LightSkyBlue<TAB>(#87CEFA)         i#87CEFA<ESC>
nmenu HTML.Colors.&L.LightSlaTegray<TAB>(#778899)       i#778899<ESC>
nmenu HTML.Colors.&L.LightSteelBlue<TAB>(#B0C4DE)       i#B0C4DE<ESC>
nmenu HTML.Colors.&L.LightYellow<TAB>(#FFFFE0)          i#FFFFE0<ESC>
nmenu HTML.Colors.&L.Lime<TAB>(#00FF00)                 i#00FF00<ESC>
nmenu HTML.Colors.&L.LimeGreen<TAB>(#32CD32)            i#32CD32<ESC>
nmenu HTML.Colors.&L.Linen<TAB>(#FAF0E6)                i#FAF0E6<ESC>

nmenu HTML.Colors.&M.Magenta<TAB>(#FF00FF)              i#FF00FF<ESC>
nmenu HTML.Colors.&M.Maroon<TAB>(#800000)               i#800000<ESC>
nmenu HTML.Colors.&M.MediumAquamarine<TAB>(#66CDAA)     i#66CDAA<ESC>
nmenu HTML.Colors.&M.MediumBlue<TAB>(#0000CD)           i#0000CD<ESC>
nmenu HTML.Colors.&M.MediumOrchid<TAB>(#BA55D3)         i#BA55D3<ESC>
nmenu HTML.Colors.&M.MediumPurple<TAB>(#9370DB)         i#9370DB<ESC>
nmenu HTML.Colors.&M.MediumSeaGreen<TAB>(#3CB371)       i#3CB371<ESC>
nmenu HTML.Colors.&M.MediumSlateBlue<TAB>(#7B68EE)      i#7B68EE<ESC>
nmenu HTML.Colors.&M.MediumSpringGreen<TAB>(#00FA9A)    i#00FA9A<ESC>
nmenu HTML.Colors.&M.MediumTurquoise<TAB>(#48D1CC)      i#48D1CC<ESC>
nmenu HTML.Colors.&M.MediumVioletRed<TAB>(#C71585)      i#C71585<ESC>
nmenu HTML.Colors.&M.MidnightBlue<TAB>(#191970)         i#191970<ESC>
nmenu HTML.Colors.&M.Mintcream<TAB>(#F5FFFA)            i#F5FFFA<ESC>
nmenu HTML.Colors.&M.Mistyrose<TAB>(#FFE4E1)            i#FFE4E1<ESC>
nmenu HTML.Colors.&M.Moccasin<TAB>(#FFE4B5)             i#FFE4B5<ESC>

nmenu HTML.Colors.&N.NavajoWhite<TAB>(#FFDEAD)          i#FFDEAD<ESC>
nmenu HTML.Colors.&N.Navy<TAB>(#000080)                 i#000080<ESC>

nmenu HTML.Colors.&O.OldLace<TAB>(#FDF5E6)              i#FDF5E6<ESC>
nmenu HTML.Colors.&O.Olive<TAB>(#808000)                i#808000<ESC>
nmenu HTML.Colors.&O.OliveDrab<TAB>(#6B8E23)            i#6B8E23<ESC>
nmenu HTML.Colors.&O.Orange<TAB>(#FFA500)               i#FFA500<ESC>
nmenu HTML.Colors.&O.OrangeRed<TAB>(#FF4500)            i#FF4500<ESC>
nmenu HTML.Colors.&O.Orchid<TAB>(#DA70D6)               i#DA70D6<ESC>

nmenu HTML.Colors.&P.PaleGoldenrod<TAB>(#EEE8AA)        i#EEE8AA<ESC>
nmenu HTML.Colors.&P.PaleGreen<TAB>(#98FB98)            i#98FB98<ESC>
nmenu HTML.Colors.&P.PaleTurquoise<TAB>(#AFEEEE)        i#AFEEEE<ESC>
nmenu HTML.Colors.&P.PaleVioletred<TAB>(#DB7093)        i#DB7093<ESC>
nmenu HTML.Colors.&P.Papayawhip<TAB>(#FFEFD5)           i#FFEFD5<ESC>
nmenu HTML.Colors.&P.Peachpuff<TAB>(#FFDAB9)            i#FFDAB9<ESC>
nmenu HTML.Colors.&P.Peru<TAB>(#CD853F)                 i#CD853F<ESC>
nmenu HTML.Colors.&P.Pink<TAB>(#FFC0CB)                 i#FFC0CB<ESC>
nmenu HTML.Colors.&P.Plum<TAB>(#DDA0DD)                 i#DDA0DD<ESC>
nmenu HTML.Colors.&P.PowderBlue<TAB>(#B0E0E6)           i#B0E0E6<ESC>
nmenu HTML.Colors.&P.Purple<TAB>(#800080)               i#800080<ESC>

nmenu HTML.Colors.&R.Red<TAB>(#FF0000)                  i#FF0000<ESC>
nmenu HTML.Colors.&R.RosyBrown<TAB>(#BC8F8F)            i#BC8F8F<ESC>
nmenu HTML.Colors.&R.RoyalBlue<TAB>(#4169E1)            i#4169E1<ESC>

nmenu HTML.Colors.&S.SaddleBrown<TAB>(#8B4513)          i#8B4513<ESC>
nmenu HTML.Colors.&S.Salmon<TAB>(#FA8072)               i#FA8072<ESC>
nmenu HTML.Colors.&S.SandyBrown<TAB>(#F4A460)           i#F4A460<ESC>
nmenu HTML.Colors.&S.SeaGreen<TAB>(#2E8B57)             i#2E8B57<ESC>
nmenu HTML.Colors.&S.Seashell<TAB>(#FFF5EE)             i#FFF5EE<ESC>
nmenu HTML.Colors.&S.Sienna<TAB>(#A0522D)               i#A0522D<ESC>
nmenu HTML.Colors.&S.Silver<TAB>(#C0C0C0)               i#C0C0C0<ESC>
nmenu HTML.Colors.&S.SkyBlue<TAB>(#87CEEB)              i#87CEEB<ESC>
nmenu HTML.Colors.&S.SlateBlue<TAB>(#6A5ACD)            i#6A5ACD<ESC>
nmenu HTML.Colors.&S.SlateGray<TAB>(#708090)            i#708090<ESC>
nmenu HTML.Colors.&S.Snow<TAB>(#FFFAFA)                 i#FFFAFA<ESC>
nmenu HTML.Colors.&S.SpringGreen<TAB>(#00FF7F)          i#00FF7F<ESC>
nmenu HTML.Colors.&S.SteelBlue<TAB>(#4682B4)            i#4682B4<ESC>

nmenu HTML.Colors.&T-Z.Tan<TAB>(#D2B48C)                i#D2B48C<ESC>
nmenu HTML.Colors.&T-Z.Teal<TAB>(#008080)               i#008080<ESC>
nmenu HTML.Colors.&T-Z.Thistle<TAB>(#D8BFD8)            i#D8BFD8<ESC>
nmenu HTML.Colors.&T-Z.Tomato<TAB>(#FF6347)             i#FF6347<ESC>
nmenu HTML.Colors.&T-Z.Turquoise<TAB>(#40E0D0)          i#40E0D0<ESC>
nmenu HTML.Colors.&T-Z.Violet<TAB>(#EE82EE)             i#EE82EE<ESC>


imenu HTML.Colors.&A.AliceBlue<TAB>(#F0F8FF)            #F0F8FF
imenu HTML.Colors.&A.AntiqueWhite<TAB>(#FAEBD7)         #FAEBD7
imenu HTML.Colors.&A.Aqua<TAB>(#00FFFF)                 #00FFFF
imenu HTML.Colors.&A.Aquamarine<TAB>(#7FFFD4)           #7FFFD4
imenu HTML.Colors.&A.Azure<TAB>(#F0FFFF)                #F0FFFF

imenu HTML.Colors.&B.Beige<TAB>(#F5F5DC)                #F5F5DC
imenu HTML.Colors.&B.Bisque<TAB>(#FFE4C4)               #FFE4C4
imenu HTML.Colors.&B.Black<TAB>(#000000)                #000000
imenu HTML.Colors.&B.BlanchedAlmond<TAB>(#FFEBCD)       #FFEBCD
imenu HTML.Colors.&B.Blue<TAB>(#0000FF)                 #0000FF
imenu HTML.Colors.&B.BlueViolet<TAB>(#8A2BE2)           #8A2BE2
imenu HTML.Colors.&B.Brown<TAB>(#A52A2A)                #A52A2A
imenu HTML.Colors.&B.Burlywood<TAB>(#DEB887)            #DEB887

imenu HTML.Colors.&C.CadetBlue<TAB>(#5F9EA0)            #5F9EA0
imenu HTML.Colors.&C.Chartreuse<TAB>(#7FFF00)           #7FFF00
imenu HTML.Colors.&C.Chocolate<TAB>(#D2691E)            #D2691E
imenu HTML.Colors.&C.Coral<TAB>(#FF7F50)                #FF7F50
imenu HTML.Colors.&C.CornflowerBlue<TAB>(#6495ED)       #6495ED
imenu HTML.Colors.&C.Cornsilk<TAB>(#FFF8DC)             #FFF8DC
imenu HTML.Colors.&C.Crimson<TAB>(#DC143C)              #DC143C
imenu HTML.Colors.&C.Cyan<TAB>(#00FFFF)                 #00FFFF

imenu HTML.Colors.&D.DarkBlue<TAB>(#00008B)             #00008B
imenu HTML.Colors.&D.DarkCyan<TAB>(#008B8B)             #008B8B
imenu HTML.Colors.&D.DarkGoldenrod<TAB>(#B8860B)        #B8860B
imenu HTML.Colors.&D.DarkGray<TAB>(#A9A9A9)             #A9A9A9
imenu HTML.Colors.&D.DarkGreen<TAB>(#006400)            #006400
imenu HTML.Colors.&D.DarkKhaki<TAB>(#BDB76B)            #BDB76B
imenu HTML.Colors.&D.DarkMagenta<TAB>(#8B008B)          #8B008B
imenu HTML.Colors.&D.DarkOliveGreen<TAB>(#556B2F)       #556B2F
imenu HTML.Colors.&D.DarkOrange<TAB>(#FF8C00)           #FF8C00
imenu HTML.Colors.&D.DarkOrchid<TAB>(#9932CC)           #9932CC
imenu HTML.Colors.&D.DarkRed<TAB>(#8B0000)              #8B0000
imenu HTML.Colors.&D.DarkSalmon<TAB>(#E9967A)           #E9967A
imenu HTML.Colors.&D.DarkSeagreen<TAB>(#8FBC8F)         #8FBC8F
imenu HTML.Colors.&D.DarkSlateBlue<TAB>(#483D8B)        #483D8B
imenu HTML.Colors.&D.DarkSlateGray<TAB>(#2F4F4F)        #2F4F4F
imenu HTML.Colors.&D.DarkTurquoise<TAB>(#00CED1)        #00CED1
imenu HTML.Colors.&D.DarkViolet<TAB>(#9400D3)           #9400D3
imenu HTML.Colors.&D.DeepPink<TAB>(#FF1493)             #FF1493
imenu HTML.Colors.&D.DeepSkyblue<TAB>(#00BFFF)          #00BFFF
imenu HTML.Colors.&D.DimGray<TAB>(#696969)              #696969
imenu HTML.Colors.&D.Dodgerblue<TAB>(#1E90FF)           #1E90FF

imenu HTML.Colors.&F.Firebrick<TAB>(#B22222)            #B22222
imenu HTML.Colors.&F.FloralWhite<TAB>(#FFFAF0)          #FFFAF0
imenu HTML.Colors.&F.ForestGreen<TAB>(#228B22)          #228B22
imenu HTML.Colors.&F.Fuchsia<TAB>(#FF00FF)              #FF00FF

imenu HTML.Colors.&G.Gainsboro<TAB>(#DCDCDC)            #DCDCDC
imenu HTML.Colors.&G.GhostWhite<TAB>(#F8F8FF)           #F8F8FF
imenu HTML.Colors.&G.Gold<TAB>(#FFD700)                 #FFD700
imenu HTML.Colors.&G.Goldenrod<TAB>(#DAA520)            #DAA520
imenu HTML.Colors.&G.Gray<TAB>(#808080)                 #808080
imenu HTML.Colors.&G.Green<TAB>(#008000)                #008000
imenu HTML.Colors.&G.GreenYellow<TAB>(#ADFF2F)          #ADFF2F

imenu HTML.Colors.&H-K.Honeydew<TAB>(#F0FFF0)           #F0FFF0
imenu HTML.Colors.&H-K.HotPink<TAB>(#FF69B4)            #FF69B4
imenu HTML.Colors.&H-K.IndianRed<TAB>(#CD5C5C)          #CD5C5C
imenu HTML.Colors.&H-K.Indigo<TAB>(#4B0082)             #4B0082
imenu HTML.Colors.&H-K.Ivory<TAB>(#FFFFF0)              #FFFFF0
imenu HTML.Colors.&H-K.Khaki<TAB>(#F0E68C)              #F0E68C

imenu HTML.Colors.&L.Lavender<TAB>(#E6E6FA)             #E6E6FA
imenu HTML.Colors.&L.LavenderBlush<TAB>(#FFF0F5)        #FFF0F5
imenu HTML.Colors.&L.LawnGreen<TAB>(#7CFC00)            #7CFC00
imenu HTML.Colors.&L.LemonChiffon<TAB>(#FFFACD)         #FFFACD
imenu HTML.Colors.&L.LightBlue<TAB>(#ADD8E6)            #ADD8E6
imenu HTML.Colors.&L.LightCoral<TAB>(#F08080)           #F08080
imenu HTML.Colors.&L.LightCyan<TAB>(#E0FFFF)            #E0FFFF
imenu HTML.Colors.&L.LightGoldenrodYellow<TAB>(#FAFAD2) #FAFAD2
imenu HTML.Colors.&L.LightGreen<TAB>(#90EE90)           #90EE90
imenu HTML.Colors.&L.LightGrey<TAB>(#D3D3D3)            #D3D3D3
imenu HTML.Colors.&L.LightPink<TAB>(#FFB6C1)            #FFB6C1
imenu HTML.Colors.&L.LightSalmon<TAB>(#FFA07A)          #FFA07A
imenu HTML.Colors.&L.LightSeaGreen<TAB>(#20B2AA)        #20B2AA
imenu HTML.Colors.&L.LightSkyBlue<TAB>(#87CEFA)         #87CEFA
imenu HTML.Colors.&L.LightSlaTegray<TAB>(#778899)       #778899
imenu HTML.Colors.&L.LightSteelBlue<TAB>(#B0C4DE)       #B0C4DE
imenu HTML.Colors.&L.LightYellow<TAB>(#FFFFE0)          #FFFFE0
imenu HTML.Colors.&L.Lime<TAB>(#00FF00)                 #00FF00
imenu HTML.Colors.&L.LimeGreen<TAB>(#32CD32)            #32CD32
imenu HTML.Colors.&L.Linen<TAB>(#FAF0E6)                #FAF0E6

imenu HTML.Colors.&M.Magenta<TAB>(#FF00FF)              #FF00FF
imenu HTML.Colors.&M.Maroon<TAB>(#800000)               #800000
imenu HTML.Colors.&M.MediumAquamarine<TAB>(#66CDAA)     #66CDAA
imenu HTML.Colors.&M.MediumBlue<TAB>(#0000CD)           #0000CD
imenu HTML.Colors.&M.MediumOrchid<TAB>(#BA55D3)         #BA55D3
imenu HTML.Colors.&M.MediumPurple<TAB>(#9370DB)         #9370DB
imenu HTML.Colors.&M.MediumSeaGreen<TAB>(#3CB371)       #3CB371
imenu HTML.Colors.&M.MediumSlateBlue<TAB>(#7B68EE)      #7B68EE
imenu HTML.Colors.&M.MediumSpringGreen<TAB>(#00FA9A)    #00FA9A
imenu HTML.Colors.&M.MediumTurquoise<TAB>(#48D1CC)      #48D1CC
imenu HTML.Colors.&M.MediumVioletRed<TAB>(#C71585)      #C71585
imenu HTML.Colors.&M.MidnightBlue<TAB>(#191970)         #191970
imenu HTML.Colors.&M.Mintcream<TAB>(#F5FFFA)            #F5FFFA
imenu HTML.Colors.&M.Mistyrose<TAB>(#FFE4E1)            #FFE4E1
imenu HTML.Colors.&M.Moccasin<TAB>(#FFE4B5)             #FFE4B5

imenu HTML.Colors.&N.NavajoWhite<TAB>(#FFDEAD)          #FFDEAD
imenu HTML.Colors.&N.Navy<TAB>(#000080)                 #000080

imenu HTML.Colors.&O.OldLace<TAB>(#FDF5E6)              #FDF5E6
imenu HTML.Colors.&O.Olive<TAB>(#808000)                #808000
imenu HTML.Colors.&O.OliveDrab<TAB>(#6B8E23)            #6B8E23
imenu HTML.Colors.&O.Orange<TAB>(#FFA500)               #FFA500
imenu HTML.Colors.&O.OrangeRed<TAB>(#FF4500)            #FF4500
imenu HTML.Colors.&O.Orchid<TAB>(#DA70D6)               #DA70D6

imenu HTML.Colors.&P.PaleGoldenrod<TAB>(#EEE8AA)        #EEE8AA
imenu HTML.Colors.&P.PaleGreen<TAB>(#98FB98)            #98FB98
imenu HTML.Colors.&P.PaleTurquoise<TAB>(#AFEEEE)        #AFEEEE
imenu HTML.Colors.&P.PaleVioletred<TAB>(#DB7093)        #DB7093
imenu HTML.Colors.&P.Papayawhip<TAB>(#FFEFD5)           #FFEFD5
imenu HTML.Colors.&P.Peachpuff<TAB>(#FFDAB9)            #FFDAB9
imenu HTML.Colors.&P.Peru<TAB>(#CD853F)                 #CD853F
imenu HTML.Colors.&P.Pink<TAB>(#FFC0CB)                 #FFC0CB
imenu HTML.Colors.&P.Plum<TAB>(#DDA0DD)                 #DDA0DD
imenu HTML.Colors.&P.PowderBlue<TAB>(#B0E0E6)           #B0E0E6
imenu HTML.Colors.&P.Purple<TAB>(#800080)               #800080

imenu HTML.Colors.&R.Red<TAB>(#FF0000)                  #FF0000
imenu HTML.Colors.&R.RosyBrown<TAB>(#BC8F8F)            #BC8F8F
imenu HTML.Colors.&R.RoyalBlue<TAB>(#4169E1)            #4169E1

imenu HTML.Colors.&S.SaddleBrown<TAB>(#8B4513)          #8B4513
imenu HTML.Colors.&S.Salmon<TAB>(#FA8072)               #FA8072
imenu HTML.Colors.&S.SandyBrown<TAB>(#F4A460)           #F4A460
imenu HTML.Colors.&S.SeaGreen<TAB>(#2E8B57)             #2E8B57
imenu HTML.Colors.&S.Seashell<TAB>(#FFF5EE)             #FFF5EE
imenu HTML.Colors.&S.Sienna<TAB>(#A0522D)               #A0522D
imenu HTML.Colors.&S.Silver<TAB>(#C0C0C0)               #C0C0C0
imenu HTML.Colors.&S.SkyBlue<TAB>(#87CEEB)              #87CEEB
imenu HTML.Colors.&S.SlateBlue<TAB>(#6A5ACD)            #6A5ACD
imenu HTML.Colors.&S.SlateGray<TAB>(#708090)            #708090
imenu HTML.Colors.&S.Snow<TAB>(#FFFAFA)                 #FFFAFA
imenu HTML.Colors.&S.SpringGreen<TAB>(#00FF7F)          #00FF7F
imenu HTML.Colors.&S.SteelBlue<TAB>(#4682B4)            #4682B4

imenu HTML.Colors.&T-Z.Tan<TAB>(#D2B48C)                #D2B48C
imenu HTML.Colors.&T-Z.Teal<TAB>(#008080)               #008080
imenu HTML.Colors.&T-Z.Thistle<TAB>(#D8BFD8)            #D8BFD8
imenu HTML.Colors.&T-Z.Tomato<TAB>(#FF6347)             #FF6347
imenu HTML.Colors.&T-Z.Turquoise<TAB>(#40E0D0)          #40E0D0
imenu HTML.Colors.&T-Z.Violet<TAB>(#EE82EE)             #EE82EE

" Font Styles menu:   {{{2

HTMLmenu imenu - HTML.Font\ Styles.Bold bo 
HTMLmenu vmenu - HTML.Font\ Styles.Bold bo 
HTMLmenu nmenu - HTML.Font\ Styles.Bold bo i
HTMLmenu imenu - HTML.Font\ Styles.Italics it 
HTMLmenu vmenu - HTML.Font\ Styles.Italics it 
HTMLmenu nmenu - HTML.Font\ Styles.Italics it i
HTMLmenu imenu - HTML.Font\ Styles.Underline un 
HTMLmenu vmenu - HTML.Font\ Styles.Underline un 
HTMLmenu nmenu - HTML.Font\ Styles.Underline un i
HTMLmenu imenu - HTML.Font\ Styles.Big bi 
HTMLmenu vmenu - HTML.Font\ Styles.Big bi 
HTMLmenu nmenu - HTML.Font\ Styles.Big bi i
HTMLmenu imenu - HTML.Font\ Styles.Small sm 
HTMLmenu vmenu - HTML.Font\ Styles.Small sm 
HTMLmenu nmenu - HTML.Font\ Styles.Small sm i
 menu HTML.Font\ Styles.-sep1- <nul>
HTMLmenu imenu - HTML.Font\ Styles.Font\ Size fo 
HTMLmenu vmenu - HTML.Font\ Styles.Font\ Size fo 
HTMLmenu nmenu - HTML.Font\ Styles.Font\ Size fo i
HTMLmenu imenu - HTML.Font\ Styles.Font\ Color fc 
HTMLmenu vmenu - HTML.Font\ Styles.Font\ Color fc 
HTMLmenu nmenu - HTML.Font\ Styles.Font\ Color fc i
 menu HTML.Font\ Styles.-sep2- <nul>
HTMLmenu imenu - HTML.Font\ Styles.CITE ci 
HTMLmenu vmenu - HTML.Font\ Styles.CITE ci 
HTMLmenu nmenu - HTML.Font\ Styles.CITE ci i
HTMLmenu imenu - HTML.Font\ Styles.CODE co 
HTMLmenu vmenu - HTML.Font\ Styles.CODE co 
HTMLmenu nmenu - HTML.Font\ Styles.CODE co i
HTMLmenu imenu - HTML.Font\ Styles.Inserted\ Text in 
HTMLmenu vmenu - HTML.Font\ Styles.Inserted\ Text in 
HTMLmenu nmenu - HTML.Font\ Styles.Inserted\ Text in i
HTMLmenu imenu - HTML.Font\ Styles.Deleted\ Text de 
HTMLmenu vmenu - HTML.Font\ Styles.Deleted\ Text de 
HTMLmenu nmenu - HTML.Font\ Styles.Deleted\ Text de i
HTMLmenu imenu - HTML.Font\ Styles.Emphasize em 
HTMLmenu vmenu - HTML.Font\ Styles.Emphasize em 
HTMLmenu nmenu - HTML.Font\ Styles.Emphasize em i
HTMLmenu imenu - HTML.Font\ Styles.Keyboard\ Text kb 
HTMLmenu vmenu - HTML.Font\ Styles.Keyboard\ Text kb 
HTMLmenu nmenu - HTML.Font\ Styles.Keyboard\ Text kb i
HTMLmenu imenu - HTML.Font\ Styles.Sample\ Text sa 
HTMLmenu vmenu - HTML.Font\ Styles.Sample\ Text sa 
HTMLmenu nmenu - HTML.Font\ Styles.Sample\ Text sa i
HTMLmenu imenu - HTML.Font\ Styles.Strikethrough sk 
HTMLmenu vmenu - HTML.Font\ Styles.Strikethrough sk 
HTMLmenu nmenu - HTML.Font\ Styles.Strikethrough sk i
HTMLmenu imenu - HTML.Font\ Styles.STRONG st 
HTMLmenu vmenu - HTML.Font\ Styles.STRONG st 
HTMLmenu nmenu - HTML.Font\ Styles.STRONG st i
HTMLmenu imenu - HTML.Font\ Styles.Subscript sb 
HTMLmenu vmenu - HTML.Font\ Styles.Subscript sb 
HTMLmenu nmenu - HTML.Font\ Styles.Subscript sb i
HTMLmenu imenu - HTML.Font\ Styles.Superscript sp 
HTMLmenu vmenu - HTML.Font\ Styles.Superscript sp 
HTMLmenu nmenu - HTML.Font\ Styles.Superscript sp i
HTMLmenu imenu - HTML.Font\ Styles.Teletype\ Text tt 
HTMLmenu vmenu - HTML.Font\ Styles.Teletype\ Text tt 
HTMLmenu nmenu - HTML.Font\ Styles.Teletype\ Text tt i
HTMLmenu imenu - HTML.Font\ Styles.Variable va 
HTMLmenu vmenu - HTML.Font\ Styles.Variable va 
HTMLmenu nmenu - HTML.Font\ Styles.Variable va i


" Frames menu:   {{{2

HTMLmenu imenu - HTML.Frames.FRAMESET fs 
HTMLmenu vmenu - HTML.Frames.FRAMESET fs 
HTMLmenu nmenu - HTML.Frames.FRAMESET fs i
HTMLmenu imenu - HTML.Frames.FRAME fr 
HTMLmenu vmenu - HTML.Frames.FRAME fr 
HTMLmenu nmenu - HTML.Frames.FRAME fr i
HTMLmenu imenu - HTML.Frames.NOFRAMES nf 
HTMLmenu vmenu - HTML.Frames.NOFRAMES nf 
HTMLmenu nmenu - HTML.Frames.NOFRAMES nf i
HTMLmenu imenu - HTML.Frames.IFRAME if 
HTMLmenu vmenu - HTML.Frames.IFRAME if 
HTMLmenu nmenu - HTML.Frames.IFRAME if i


" Headers menu:   {{{2

HTMLmenu imenu - HTML.Headers.Header\ Level\ 1 h1 
HTMLmenu imenu - HTML.Headers.Header\ Level\ 2 h2 
HTMLmenu imenu - HTML.Headers.Header\ Level\ 3 h3 
HTMLmenu imenu - HTML.Headers.Header\ Level\ 4 h4 
HTMLmenu imenu - HTML.Headers.Header\ Level\ 5 h5 
HTMLmenu imenu - HTML.Headers.Header\ Level\ 6 h6 
HTMLmenu vmenu - HTML.Headers.Header\ Level\ 1 h1 
HTMLmenu vmenu - HTML.Headers.Header\ Level\ 2 h2 
HTMLmenu vmenu - HTML.Headers.Header\ Level\ 3 h3 
HTMLmenu vmenu - HTML.Headers.Header\ Level\ 4 h4 
HTMLmenu vmenu - HTML.Headers.Header\ Level\ 5 h5 
HTMLmenu vmenu - HTML.Headers.Header\ Level\ 6 h6 
HTMLmenu nmenu - HTML.Headers.Header\ Level\ 1 h1 i
HTMLmenu nmenu - HTML.Headers.Header\ Level\ 2 h2 i
HTMLmenu nmenu - HTML.Headers.Header\ Level\ 3 h3 i
HTMLmenu nmenu - HTML.Headers.Header\ Level\ 4 h4 i
HTMLmenu nmenu - HTML.Headers.Header\ Level\ 5 h5 i
HTMLmenu nmenu - HTML.Headers.Header\ Level\ 6 h6 i


" Lists menu:   {{{2

HTMLmenu imenu - HTML.Lists.Ordered\ List ol 
HTMLmenu vmenu - HTML.Lists.Ordered\ List ol 
HTMLmenu nmenu - HTML.Lists.Ordered\ List ol i
HTMLmenu imenu - HTML.Lists.Unordered\ List ul 
HTMLmenu vmenu - HTML.Lists.Unordered\ List ul 
HTMLmenu nmenu - HTML.Lists.Unordered\ List ul i
HTMLmenu imenu - HTML.Lists.List\ Item li 
HTMLmenu vmenu - HTML.Lists.List\ Item li 
HTMLmenu nmenu - HTML.Lists.List\ Item li i
 menu HTML.Lists.-sep1- <nul>
HTMLmenu imenu - HTML.Lists.Definition\ List dl 
HTMLmenu vmenu - HTML.Lists.Definition\ List dl 
HTMLmenu nmenu - HTML.Lists.Definition\ List dl i
HTMLmenu imenu - HTML.Lists.Definition\ Term dt 
HTMLmenu vmenu - HTML.Lists.Definition\ Term dt
HTMLmenu nmenu - HTML.Lists.Definition\ Term dt i
HTMLmenu imenu - HTML.Lists.Definition\ Body dd 
HTMLmenu vmenu - HTML.Lists.Definition\ Body dd
HTMLmenu nmenu - HTML.Lists.Definition\ Body dd i


" Tables menu:   {{{2

HTMLmenu nmenu - HTML.Tables.Interactive\ Table tA 
HTMLmenu imenu - HTML.Tables.TABLE ta 
HTMLmenu vmenu - HTML.Tables.TABLE ta 
HTMLmenu nmenu - HTML.Tables.TABLE ta i
HTMLmenu imenu - HTML.Tables.Row tr 
HTMLmenu vmenu - HTML.Tables.Row tr 
HTMLmenu nmenu - HTML.Tables.Row tr i
HTMLmenu imenu - HTML.Tables.Data td 
HTMLmenu vmenu - HTML.Tables.Data td 
HTMLmenu nmenu - HTML.Tables.Data td i
HTMLmenu imenu - HTML.Tables.CAPTION ca 
HTMLmenu vmenu - HTML.Tables.CAPTION ca 
HTMLmenu nmenu - HTML.Tables.CAPTION ca i
HTMLmenu imenu - HTML.Tables.Header th 
HTMLmenu vmenu - HTML.Tables.Header th 
HTMLmenu nmenu - HTML.Tables.Header th i


" Forms menu:   {{{2

HTMLmenu imenu - HTML.Forms.FORM fm 
HTMLmenu vmenu - HTML.Forms.FORM fm 
HTMLmenu nmenu - HTML.Forms.FORM fm i
HTMLmenu imenu - HTML.Forms.BUTTON bu 
HTMLmenu vmenu - HTML.Forms.BUTTON bu 
HTMLmenu nmenu - HTML.Forms.BUTTON bu i
HTMLmenu imenu - HTML.Forms.CHECKBOX ch 
HTMLmenu vmenu - HTML.Forms.CHECKBOX ch 
HTMLmenu nmenu - HTML.Forms.CHECKBOX ch i
HTMLmenu imenu - HTML.Forms.RADIO ra 
HTMLmenu vmenu - HTML.Forms.RADIO ra 
HTMLmenu nmenu - HTML.Forms.RADIO ra i
HTMLmenu imenu - HTML.Forms.HIDDEN hi 
HTMLmenu vmenu - HTML.Forms.HIDDEN hi 
HTMLmenu nmenu - HTML.Forms.HIDDEN hi i
HTMLmenu imenu - HTML.Forms.PASSWORD pa 
HTMLmenu vmenu - HTML.Forms.PASSWORD pa 
HTMLmenu nmenu - HTML.Forms.PASSWORD pa i
HTMLmenu imenu - HTML.Forms.TEXT te 
HTMLmenu vmenu - HTML.Forms.TEXT te 
HTMLmenu nmenu - HTML.Forms.TEXT te i
HTMLmenu imenu - HTML.Forms.SELECT se 
HTMLmenu vmenu - HTML.Forms.SELECT se 
HTMLmenu nmenu - HTML.Forms.SELECT se i
HTMLmenu imenu - HTML.Forms.SELECT\ MULTIPLE ms 
HTMLmenu vmenu - HTML.Forms.SELECT\ MULTIPLE ms 
HTMLmenu nmenu - HTML.Forms.SELECT\ MULTIPLE ms i
HTMLmenu imenu - HTML.Forms.OPTION op 
HTMLmenu vmenu - HTML.Forms.OPTION op
HTMLmenu nmenu - HTML.Forms.OPTION op i
HTMLmenu imenu - HTML.Forms.OPTGROUP og 
HTMLmenu vmenu - HTML.Forms.OPTGROUP og 
HTMLmenu nmenu - HTML.Forms.OPTGROUP og i
HTMLmenu imenu - HTML.Forms.TEXTAREA tx 
HTMLmenu vmenu - HTML.Forms.TEXTAREA tx 
HTMLmenu nmenu - HTML.Forms.TEXTAREA tx i
HTMLmenu imenu - HTML.Forms.SUBMIT su 
HTMLmenu nmenu - HTML.Forms.SUBMIT su i
HTMLmenu imenu - HTML.Forms.RESET re 
HTMLmenu nmenu - HTML.Forms.RESET re i
HTMLmenu imenu - HTML.Forms.LABEL la 
HTMLmenu vmenu - HTML.Forms.LABEL la 
HTMLmenu nmenu - HTML.Forms.LABEL la i

" }}}2

 menu HTML.-sep5- <nul>

HTMLmenu nmenu - HTML.Doctype\ (transitional) 4 
HTMLmenu nmenu - HTML.Doctype\ (strict) s4 
HTMLmenu imenu - HTML.Content-Type ct 
HTMLmenu nmenu - HTML.Content-Type ct i

 menu HTML.-sep6- <nul>

HTMLmenu imenu - HTML.BODY bd 
HTMLmenu vmenu - HTML.BODY bd 
HTMLmenu nmenu - HTML.BODY bd i
HTMLmenu imenu - HTML.CENTER ce 
HTMLmenu vmenu - HTML.CENTER ce 
HTMLmenu nmenu - HTML.CENTER ce i
HTMLmenu imenu - HTML.Comment cm 
HTMLmenu vmenu - HTML.Comment cm 
HTMLmenu nmenu - HTML.Comment cm i
HTMLmenu imenu - HTML.HEAD he 
HTMLmenu vmenu - HTML.HEAD he 
HTMLmenu nmenu - HTML.HEAD he i
HTMLmenu imenu - HTML.Horizontal\ Rule hr 
HTMLmenu nmenu - HTML.Horizontal\ Rule hr i
HTMLmenu imenu - HTML.HTML ht 
HTMLmenu vmenu - HTML.HTML ht 
HTMLmenu nmenu - HTML.HTML ht i
HTMLmenu imenu - HTML.Hyperlink ah 
HTMLmenu vmenu - HTML.Hyperlink ah 
HTMLmenu nmenu - HTML.Hyperlink ah i
HTMLmenu imenu - HTML.Inline\ Image im 
HTMLmenu vmenu - HTML.Inline\ Image im 
HTMLmenu nmenu - HTML.Inline\ Image im i
if exists("*MangleImageTag")
  HTMLmenu imenu - HTML.Update\ Image\ Size\ Attributes mi 
  HTMLmenu vmenu - HTML.Update\ Image\ Size\ Attributes mi <ESC>
  HTMLmenu nmenu - HTML.Update\ Image\ Size\ Attributes mi 
endif
HTMLmenu imenu - HTML.Line\ Break br 
HTMLmenu nmenu - HTML.Line\ Break br i
HTMLmenu imenu - HTML.Named\ Anchor an 
HTMLmenu vmenu - HTML.Named\ Anchor an 
HTMLmenu nmenu - HTML.Named\ Anchor an i
HTMLmenu imenu - HTML.Paragraph pp 
HTMLmenu vmenu - HTML.Paragraph pp 
HTMLmenu nmenu - HTML.Paragraph pp i
HTMLmenu imenu - HTML.Preformatted\ Text pr 
HTMLmenu vmenu - HTML.Preformatted\ Text pr 
HTMLmenu nmenu - HTML.Preformatted\ Text pr i
HTMLmenu imenu - HTML.TITLE ti 
HTMLmenu vmenu - HTML.TITLE ti 
HTMLmenu nmenu - HTML.TITLE ti i

HTMLmenu imenu - HTML.More\.\.\..ADDRESS ad 
HTMLmenu vmenu - HTML.More\.\.\..ADDRESS ad 
HTMLmenu nmenu - HTML.More\.\.\..ADDRESS ad i
HTMLmenu imenu - HTML.More\.\.\..BASE\ HREF bh 
HTMLmenu vmenu - HTML.More\.\.\..BASE\ HREF bh 
HTMLmenu nmenu - HTML.More\.\.\..BASE\ HREF bh i
HTMLmenu imenu - HTML.More\.\.\..BLOCKQUTE bl 
HTMLmenu vmenu - HTML.More\.\.\..BLOCKQUTE bl 
HTMLmenu nmenu - HTML.More\.\.\..BLOCKQUTE bl i
HTMLmenu imenu - HTML.More\.\.\..Defining\ Instance df 
HTMLmenu vmenu - HTML.More\.\.\..Defining\ Instance df 
HTMLmenu nmenu - HTML.More\.\.\..Defining\ Instance df i
HTMLmenu imenu - HTML.More\.\.\..Document\ Division dv 
HTMLmenu vmenu - HTML.More\.\.\..Document\ Division dv 
HTMLmenu nmenu - HTML.More\.\.\..Document\ Division dv i
HTMLmenu imenu - HTML.More\.\.\..EMBED eb 
HTMLmenu nmenu - HTML.More\.\.\..EMBED eb i
HTMLmenu imenu - HTML.More\.\.\..ISINDEX ii 
HTMLmenu nmenu - HTML.More\.\.\..ISINDEX ii i
HTMLmenu imenu - HTML.More\.\.\..JavaScript js 
HTMLmenu nmenu - HTML.More\.\.\..JavaScript js i
HTMLmenu imenu - HTML.More\.\.\..Sourced\ JavaScript sj 
HTMLmenu nmenu - HTML.More\.\.\..Sourced\ JavaScript sj i
HTMLmenu imenu - HTML.More\.\.\..LINK\ HREF lk 
HTMLmenu vmenu - HTML.More\.\.\..LINK\ HREF lk 
HTMLmenu nmenu - HTML.More\.\.\..LINK\ HREF lk i
HTMLmenu imenu - HTML.More\.\.\..Linked\ CSS ls 
HTMLmenu vmenu - HTML.More\.\.\..Linked\ CSS ls 
HTMLmenu nmenu - HTML.More\.\.\..Linked\ CSS ls i
HTMLmenu imenu - HTML.More\.\.\..META me 
HTMLmenu vmenu - HTML.More\.\.\..META me 
HTMLmenu nmenu - HTML.More\.\.\..META me i
HTMLmenu imenu - HTML.More\.\.\..NOSCRIPT nj 
HTMLmenu vmenu - HTML.More\.\.\..NOSCRIPT nj 
HTMLmenu nmenu - HTML.More\.\.\..NOSCRIPT nj i
HTMLmenu imenu - HTML.More\.\.\..Generic\ Embedded\ Object ob 
HTMLmenu vmenu - HTML.More\.\.\..Generic\ Embedded\ Object ob 
HTMLmenu nmenu - HTML.More\.\.\..Generic\ Embedded\ Object ob i
HTMLmenu imenu - HTML.More\.\.\..Quoted\ Text qu 
HTMLmenu vmenu - HTML.More\.\.\..Quoted\ Text qu 
HTMLmenu nmenu - HTML.More\.\.\..Quoted\ Text qu i
HTMLmenu imenu - HTML.More\.\.\..SPAN sn 
HTMLmenu vmenu - HTML.More\.\.\..SPAN sn 
HTMLmenu nmenu - HTML.More\.\.\..SPAN sn i
HTMLmenu imenu - HTML.More\.\.\..STYLE cs 
HTMLmenu vmenu - HTML.More\.\.\..STYLE cs 
HTMLmenu nmenu - HTML.More\.\.\..STYLE cs i

delcommand HTMLmenu
delfunction s:HTMLleadmenu

let g:did_html_menus = 1
endif  " ! has("gui_running") && ! exists("g:force_html_menu")
" ---------------------------------------------------------------------------


" ---- Clean Up: -------------------------------------------------------- {{{1

silent! unlet s:browsers

" Restore cpoptions:
let &cpoptions = s:savecpo
unlet s:savecpo

unlet s:doing_internal_html_mappings

" vim:ts=2:sw=2:expandtab:tw=78:fo=croq2:comments=b\:\":
" vim600:fdm=marker:fdc=4:cms=\ "\ %s:
