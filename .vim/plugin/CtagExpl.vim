" CtagExpl.vim
"   Author: Charles E. Campbell, Jr.
"   Date:   Feb 22, 2011
"   Version: 1k	ASTRO-ONLY
"
"   Philippians 1:21: For to me to live is Christ, and to die is gain.
"redraw!|call DechoSep()|call inputsave()|call input("Press <cr> to continue")|call inputrestore()
" =====================================================================
"  Load Once: {{{1
if &cp || exists("g:loaded_CtagExpl")
 finish
endif
let g:loaded_CtagExpl= "v1k"
if v:version < 702
 echohl WarningMsg
 echo "***warning*** this version of CtagExpl needs vim 7.2"
 echohl Normal
 finish
endif
let s:keepcpo= &cpo
set cpo&vim
"DechoTabOn

" ---------------------------------------------------------------------
"  Public Interface: {{{1
com!		-nargs=* -bang	CtagExpl	call s:CtagExpl(<bang>0,<args>)
silent! com	-nargs=* -bang	CT			call s:CtagExpl(<bang>0,<args>)

" ---------------------------------------------------------------------
"  Variables: {{{1
if !exists("g:CtagExplwinwidth")
 let g:CtagExplwinwidth= 25
endif
if !exists("g:CtagExpl_pgm")
 let g:CtagExpl_pgm= "ctags"
endif
if !exists("g:CtagExpl_mktags")
 let g:CtagExpl_mktags= "mktags"
endif
if !exists("g:CtagExpl_cygwin")
 if has("win32") || has("win95") || has("win64") || has("win16")
  if &shell == "bash"
   let g:CtagExpl_cygwin= 1
  else
   let g:CtagExpl_cygwin= 0
  endif
 else
  let g:CtagExpl_cygwin= 0
 endif
endif
if !exists("g:DrChipTopLvlMenu")
 let g:DrChipTopLvlMenu= "DrChip."
endif
if !exists("g:CtagExpl_winname")
 let g:CtagExpl_winname= "[CtagExpl]"
endif

if !exists("g:CtagExpl_kinds")
 " this list was generated with the help of "ctags --list-kinds"
 let s:CtagExpl_kinds= {
   \ 'asm':{'d':'defines', 'l':'labels', 'm':'macros'},
   \ 'awk':{'f':'functions'},
   \ 'c':{'c':'classes', 'd':'macros', 'e':'enumerators', 'f':'functions', 'g':'enums', 'm':'members', 'n':'namespaces', 's':'structures', 't':'typedefs', 'u':'unions', 'v':'variables'},
   \ 'cpp':{'c':'classes', 'd':'macros', 'e':'enumerators', 'f':'functions', 'g':'enums', 'm':'members', 'n':'namespaces', 's':'structures', 't':'typedefs', 'u':'unions', 'v':'variables'},
   \ 'cs':{'c':'classes', 'd':'macros', 'e':'enumerators', 'f':'functions', 'n':'namespaces', 's':'structures', 't':'typedefs', 'u':'unions', 'v':'variables'},
   \ 'for':{'b':'block data', 'c':'common blocks', 'e':'entry points', 'f':'functions', 'l':'labels', 'm':'modules', 'n':'namelists', 'p':'programs', 'v':'pgm variables'},
   \ 'f77':{'b':'block data', 'c':'common blocks', 'e':'entry points', 'f':'functions', 'l':'labels', 'm':'modules', 'n':'namelists', 'p':'programs', 'v':'pgm variables'},
   \ 'java':{'c':'classes', 'f':'fields', 'i':'interfaces', 'm':'methods', 'p':'packages'},
   \ 'jav':{'c':'classes', 'f':'fields', 'i':'interfaces', 'm':'methods', 'p':'packages'},
   \ 'lisp':{'f':'functions'},
   \ 'lsp':{'f':'functions'},
   \ 'el':{'f':'functions'},
   \ 'cl':{'f':'functions'},
   \ 'L':{'f':'functions'},
   \ 'pas':{'f':'functions', 'p':'procedures'},
   \ 'dpr':{'f':'functions', 'p':'procedures'},
   \ 'pl':{'c':'constants', 'l':'labels', 's':'subroutines'},
   \ 'pm':{'c':'constants', 'l':'labels', 's':'subroutines'},
   \ 'plx':{'c':'constants', 'l':'labels', 's':'subroutines'},
   \ 'php':{'c':'classes', 'd':'constants', 'f':'functions', 'v':'variables'},
   \ 'py':{'c':'classes', 'f':'functions', 'm':'members'},
   \ 'pyw':{'c':'classes', 'f':'functions', 'm':'members'},
   \ 'sh':{'f':'functions'},
   \ 'vim':{'a':'autocmds', 'f':'functions', 'v':'variables'},
   \ 'y':{'l':'labels'},
   \ 'yy':{'l':'labels'}
   \  }
else
 let s:CtagExpl_kinds= g:CtagExpl_kinds
endif

" =====================================================================
"  Functions: {{{1

" ---------------------------------------------------------------------
" s:CtagExpl: {{{2
fun! s:CtagExpl(bang,...)
"  call Dfunc("CtagExpl(bang=".a:bang.") a:0=".a:0)

  let ctagexplbuf= bufnr(escape(g:CtagExpl_winname,'[]'))
"  call Decho("ctagexplbuf#".ctagexplbuf)

  if ctagexplbuf == -1
   " CtagExpl buffer doesn't exist
"   call Decho("buffer named ".g:CtagExpl_winname." doesn't exist")
   if a:0 > 0 && a:1 == 0
	" CtagExpl 0  and CtagExpl already doesn't exist
"	call Dret("CtagExpl : already doesn't exist")
	return
   endif
   call s:CtagExplInit(a:bang)

  else
   " CtagExpl buffer exists
"   call Decho("buffer named ".g:CtagExpl_winname." exists")
   let ctagexplwin= bufwinnr(ctagexplbuf)

   if ctagexplwin > 0
	" CtagExpl window exists
"	call Decho("window holding ".g:CtagExpl_winname." exists")
    if a:0 > 0 && a:1 == 1
	 " CtagExpl 1  and CtagExpl already exists
"	 call Dret("CtagExpl : already exists")
	 return
	elseif a:0 > 0 && a:1 == 2
	 " force CtagExpl initialization, even though its buffer & window exist
"	 call Decho("CtagExpl : forced initialization")
     call s:CtagExplInit(a:bang)
"	 call Dret("CtagExpl : forced initialization")
	 return
    endif
	call s:CtagExplOff()

   else
	" CtagExpl window doesn't exist (but the buffer does)
"	call Decho("window holding ".g:CtagExpl_winname." doesn't exist")

    if a:0 > 0 && a:1 == 0
	 call s:CtagExplOff()
"	 call Dret("CtagExpl : turned off (buffer existed, window didn't)"
	 return
	endif

	" bring CtagExpl window to foreground
	if exists("g:CtagExplForeground") && type(g:CtagExplForeground) == 2
"	 call Decho("use g:CtagExplForeground() to bring CtagExpl to foreground")
	 call g:CtagExplForeground()
     if bufwinnr(ctagexplbuf) > 0
"	  call Dret("CtagExpl : brought to foreground")
	  return
	 endif
	endif
"    call Decho('exe topleft '.g:CtagExplwinwidth."vsplit")
    exe 'topleft '.g:CtagExplwinwidth."vsplit"
"	call Decho("exe b ".hdrtagbuf)
	exe "b ".ctagexplbuf
	if line("$") == 1 && getline(1) == ""
	 " looks like the buffer got wiped out somewhere.  Re-create it.
	 exe "bw ".ctagexplbuf
     call s:CtagExplInit(a:bang)
	endif
   endif
  endif

"  call Dret("CtagExpl")
endfun

" ---------------------------------------------------------------------
" s:CtagExplInit: {{{2
fun! s:CtagExplInit(dotags)
"  call Dfunc("CtagExplInit(dotags=".a:dotags.")")

  if a:dotags && filereadable("tags")
"   call Decho("deleting <tags>")
   call delete("tags")
  endif

  " if <tags> doesn't exist:
  "  1) try &tags path
  "  2) run mktags script if it exists.  If not,
  "  3) run CtagExpl_pgm on current file
  let tagfile= "tags"

  if !filereadable(tagfile)
   " tagfile not readable, try &tags
"   call Decho(tagfile." not readable, trying &tags")
   let taglist= split(&tags,',')
   for tagpath in taglist
"    call Decho("trying <".tagpath.">")
    if filereadable(tagpath)
     let tagfile= tagpath
     break
    endif
   endfor
"   call Decho("tagfile<".tagfile.">")
  endif

  if !filereadable(tagfile)
   " tagfile not readable, try making <tags> with mktags script
   if executable(g:CtagExpl_mktags)
   	" run mktags script to generate tags (presumably for multiple files)
"	call Decho(tagfile." not readable, try mktags script<".g:CtagExpl_mktags.">")
   	call system(g:CtagExpl_mktags)
   endif
  endif

  if !filereadable(tagfile)
   " tagfile not readable, try making <tags> using pgm
"   call Decho("tagfile not readable, try making tags with ".g:CtagExpl_pgm)
   if exists("g:CtagExpl_chgwin")
"    call Decho("srcfile=bufname(bufwinnr(chgwin#".g:CtagExpl_chgwin.")=".bufwinnr(g:CtagExpl_chgwin).")<".simplify(bufname(bufwinnr(g:CtagExpl_chgwin))).">")
    let srcfile= simplify(bufname(bufwinnr(g:CtagExpl_chgwin)))
   else
"    call Decho("srcfile=expand('%')<".expand("%").">")
    let srcfile= simplify(expand("%"))
   endif
"   call Decho("system(".g:CtagExpl_pgm." ".shellescape(srcfile).')')
   call system(g:CtagExpl_pgm." ".shellescape(srcfile))
  endif

  if !filereadable(tagfile)
   " <tags> still not readable, giving up!
"   call Decho("<tags> still not readable, giving up!")
  	redraw!
   echohl Error | echo "unable to find or create a <tags> file!" | echohl None
"   call Dret("CtagExplInit : unable to initialize")
   return
  endif

"  call Decho(tagfile." file present and readable")

  if !exists("g:CtagExpl_chgwin")
   " create CtagExpl window/buffer
"   call Decho("create ".g:CtagExpl_winname." window, g:CtagExpl_chgwin doesn't exist")
"   call Decho('exe topleft '.g:CtagExplwinwidth."vsplit")
   exe 'topleft '.g:CtagExplwinwidth."vsplit"
   wincmd l
   let g:CtagExpl_chgwin         = winnr()
   let s:CtagExpl_chgwin_created = 1
   wincmd h
   enew
   setlocal bt=nofile noswf
"   call Decho("create ".g:CtagExpl_winname." w#".winnr()." buf#".bufnr("%")." src-w#".g:CtagExpl_chgwin." src-buf#".bufwinnr(g:CtagExpl_chgwin))
  elseif line("$") != 1 || getline(1) != ""
"   call Decho("create ".g:CtagExpl_winname." window, chgwin=".g:CtagExpl_chgwin)
   enew
   setlocal bt=nofile noswf
"   call Decho("using ".g:CtagExpl_winname." w#".winnr()." buf#".bufnr("%")." src-w#".g:CtagExpl_chgwin." src-buf#".bufwinnr(g:CtagExpl_chgwin))
  endif
  let s:CtagExplwin   = winnr()
  let s:CtagExplbufnr = bufnr("%")
"  call Decho("CtagExplwin#".s:CtagExplwin." CtagExplbufnr#".s:CtagExplbufnr)

  " read and sort <tags>
"  call Decho(" read and sort <tags>")
  exe "silent r ".tagfile
"  call Decho("read ".line("$")." lines of ".tagfile)
  exe 'silent file '.g:CtagExpl_winname
  setlocal bt=nofile
  1d

  " remove comments
"  call Decho("remove comments")
  silent! g/^!_/d

  " transform into Hdrtag format
"  call Decho("transform into Hdrtag format")
  %s/^\([^\t]\+\)\t\([^\t]\+\)\t.*;"\t\(.\).*$/\=s:GetLabel(submatch(2),submatch(3))."\t".submatch(1)."\t".submatch(3)/e

  " get and sort lines
"  call Decho("get and sort lines")
  let lines = getbufline("%",1,"$")
  let lines = sort(lines)
  %d
  call setline(1,lines)
  1
  let akeep     = @a
  let s:lblkind = {}
  while search('^\S','cW')
   let lbl= expand("<cWORD>")
"   call Decho("getline<".getline(".").">")
   let s:lblkind[lbl]= substitute(getline("."),'^.*\t\(.\)$','\1','')
"   call Decho('s:lblkind['.lbl.']<'.s:lblkind[lbl].'>')
   let @a= ' '.lbl.': {{'.'{1'
   put! a
   exe 'silent! .,/^\%(\<'.lbl.'\>\)\@!\|\%'.line("$").'l./s/^\<'.lbl.'\>//'
   if line(".") == line("$")
	break
   endif
  endwhile

  " cleanup
"  call Decho("cleanup")
  let @a= akeep
  silent %s/^ /\r/e
  silent %s/^\t/  /
  silent g/^  \S/s/\t.$//e
  let curline= 1
  1d

  " get rid of adjacent duplicates
"  call Decho("get rid of adjacent duplicates, if any")
"  call Decho('exe silent g/^\%<'.line("$").'l\(.*\)\n\1$/d')
  exe 'silent g/^\%<'.line("$").'l\(.*\)\n\1$/d'
  1
  setlocal nomod noma ro fdm=marker
  set ft=CtagExpl
  setlocal wfw

  " CtagExpl List Window Interface
"  call Decho("set up maps")
  nnoremap <silent> <buffer> <s-left>	:call <sid>CtagExplPrev()<cr>
  nnoremap <silent> <buffer> <s-right>	:call <sid>CtagExplNext()<cr>
  nnoremap <silent> <buffer> <cr>		:call <sid>CtagExplSelect(0)<cr>
  nnoremap <silent> <buffer> v			:call <sid>CtagExplSelect(1)<cr>
  nnoremap <silent> <buffer> o			:call <sid>CtagExplSelect(2)<cr>
  nnoremap <silent> <buffer> t			:call <sid>CtagExplSelect(3)<cr>
  nnoremap <silent> <buffer> h			:call <sid>CtagExplPrev()<bar>call <sid>CtagExplSelect(2)<cr>
  nnoremap <silent> <buffer> l			:call <sid>CtagExplNext()<bar>call <sid>CtagExplSelect(2)<cr>
  nnoremap <silent> <buffer> <s-up>		:call search('^\S','bW')<cr>
  nnoremap <silent> <buffer> <s-down>	:call search('^\S','W')<cr>
  augroup CtagExpl
   au!
   exe "au CursorMoved	".escape(g:CtagExpl_winname,'[]')." call s:CtagExplInfo(1)"
  augroup END
  call s:CtagExplMenu()

"  call Dret("CtagExplInit")
endfun

" ---------------------------------------------------------------------
" s:CtagExplOff: {{{2
fun! s:CtagExplOff()
"  call Dfunc("CtagExplOff()")

  let CtagExplbuf= bufnr(escape(g:CtagExpl_winname,'[]'))
  if CtagExplbuf == -1
"   call Dret("CtagExplOff")
   return
  endif

  let CtagExplwin= bufwinnr(CtagExplbuf)
  if CtagExplwin > 0
   exe CtagExplwin."wincmd w"
   if exists("s:CtagExpl_win")
    unlet s:CtagExpl_win
   endif
   silent! q!
  else
"   call Decho("exe bd ".CtagExplbuf)
   exe "bd ".CtagExplbuf
  endif

  augroup CtagExpl
   au!
  augroup END
  aug! CtagExpl

  if exists("s:CtagExpl_chgwin_created")
   unlet g:CtagExpl_chgwin
   unlet s:CtagExpl_chgwin_created
  endif

  call s:CtagExplMenu()
"  call Dret("CtagExplOff")
endfun

" ---------------------------------------------------------------------
" s:CtagExplSelect: {{{2
fun! s:CtagExplSelect(openmode)
"  call Dfunc("CtagExplSelect(openmode=".a:openmode.") s:kindcnt=".(exists("s:kindcnt")? s:kindcnt : "doesn't exist"))

  if getline(".") =~ '^  '
   norm! 0w
 
   let select = expand("<cWORD>")
"   call Decho("selecting word<".select.">  openmode=".a:openmode." s:infocnt=".(exists("s:infocnt")? s:infocnt : "doesn't exist"))

   if a:openmode == 0 || a:openmode == 2
	" re-use change window, put cursor there (<cr> o h l maps)
	exe g:CtagExpl_chgwin."wincmd w"
   elseif a:openmode == 1
	" vertically split change window, put cursor there (v map)
	exe s:CtagExpl_chgwin."wincmd w"
   	wincmd v
   elseif a:openmode == 3
	" open in a new tab (t map)
	tab split
	let s:CtagExpl_chgwin= winnr()
   endif

   if exists("s:infocnt")
"    call Decho("exe e taglist(^".select."$)[".(s:infocnt-1)."][filename]<".taglist('^'.select.'$')[s:infocnt-1]["filename"].">")
    exe "silent e ".taglist('^'.select.'$')[s:infocnt-1]["filename"]
"    call Decho("exe e taglist(^".select."$)[".(s:infocnt-1)."][cmde]<".taglist('^'.select.'$')[s:infocnt-1]["cmd"].">")
    1
	let srchstring = taglist('^'.select.'$')[s:infocnt-1]["cmd"]
	let srchstring = escape(srchstring,'*[]')
"	call Decho("exe silent ".srchstring)
	exe "silent ".srchstring
"	call Decho('exe match TabLineSel /\<'.select.'\>/')
	exe 'match TabLineSel /\<'.select.'\>/'
   else
   	redraw!
	echo "no tag item selected yet"
"	call Decho("no tag item selected yet")
   endif
   if foldclosed('.') > 0
    norm! zMzx
   endif
   norm! z.
   
   " optional FuncRef call
   if exists("g:CtagExpl_funcref") && type(g:CtagExpl_funcref) == 2
"	call Decho("making call to g:CtagExpl_funcref()")
	call g:CtagExpl_funcref()
   endif
   if a:openmode == 2
	" return cursor to CtagExpl window (o h l maps)
"	call Decho("exe ".s:CtagExplwin."wincmd w")
	exe s:CtagExplwin."wincmd w"
   endif

"   call Dret("CtagExplSelect : select<".select.">")
   return
  endif

"  call Dret("CtagExplSelect : not a selectable line")
endfun

" ---------------------------------------------------------------------
" s:CtagExplInfo: invoked when the cursor line has changed {{{2
fun! s:CtagExplInfo(kindcnt)
"  call Dfunc("CtagExplInfo(kindcnt=".a:kindcnt.")")
  let s:kindcnt= a:kindcnt
  " insure that CtagExpl list window is of standard width
  "exe "vertical res ".g:CtagExplwinwidth
  if winline() == 1
   redraw!
   exe "silent norm! \<c-y>"
  elseif winline() == winheight(0)
   redraw!
   exe "silent norm! \<c-e>"
  endif
  if bufnr("%") == s:CtagExplbufnr
   let curline= getline(".")
   if curline =~ '^  \S'
    let select    = expand("<cWORD>")
    let lblline   = search('^\S.*: {\{3}1$','nbWc')
    let lbl       = substitute(getline(lblline),': {\{3}1','','')
    let kind      = s:lblkind[lbl]
"    call Decho('s:lblkind['.lbl.']<'.kind.'>')
    let tl        = taglist('^'.select.'$')
    let s:infocnt = 0
	let ikindcnt  = 0
    for itl in tl
	 let s:infocnt= s:infocnt + 1
     if itl["kind"] == kind
	  let ikindcnt    = ikindcnt + 1
	  let lastmsg     = "(".ikindcnt.") ".itl["filename"].": ".substitute(itl["cmd"],'^\/\^\(.*\)\$\/$','\1','')
	  let lastinfocnt = s:infocnt
	  let lastkindcnt = ikindcnt
"	  call Decho("for loop: lastmsg<".lastmsg."> kind<".itl["kind"]."> lastinfocnt=".lastinfocnt." lastkindcnt=".lastkindcnt)
	  if ikindcnt == a:kindcnt
"	   call Decho("[ikindcnt=".ikindcnt."] == [a:kindcnt=".a:kindcnt."] s:infocnt=".s:infocnt)
	   break
	  endif
     endif
    endfor
   endif
   if exists("lastmsg")
   	echo lastmsg
	let s:infocnt= lastinfocnt
	let s:kindcnt= lastkindcnt
"	call Decho("final: lastmsg<".lastmsg."> lastinfocnt=".lastinfocnt." lastkindcnt=".lastkindcnt)
   endif
  else
   let s:kindcnt= 1
   let s:infocnt= 1
   echo " "
  endif
"  call Dret("CtagExplInfo : s:infocnt=".(exists("s:infocnt")? s:infocnt : -1)." s:kindcnt=".(exists("s:kindcnt")? s:kindcnt : -1))
endfun

" ---------------------------------------------------------------------
" s:CtagExplPrev: {{{2
fun! s:CtagExplPrev()
"  call Dfunc("s:CtagExplPrev()")
  if !exists("s:kindcnt") || s:kindcnt <= 1
   let s:kindcnt= 1
  else
   let s:kindcnt= s:kindcnt - 1
  endif
"  call Decho("s:kindcnt=".s:kindcnt)
  call s:CtagExplInfo(s:kindcnt)
"  call Dret("s:CtagExplPrev")
endfun

" ---------------------------------------------------------------------
" s:CtagExplNext: {{{2
fun! s:CtagExplNext()
"  call Dfunc("s:CtagExplNext()")
  if !exists("s:kindcnt") 
   let s:kindcnt= 2
  else
   let s:kindcnt= s:kindcnt + 1
  endif
"  call Decho("s:kindcnt=".s:kindcnt)
  call s:CtagExplInfo(s:kindcnt)
"  call Dret("s:CtagExplNext")
endfun

" ---------------------------------------------------------------------
" s:GetLabel: {{{2
fun! s:GetLabel(fname,id)
"  call Dfunc("s:GetLabel(fname<".a:fname."> id<".a:id.">)")
  let sfx= substitute(a:fname,'^.*\.\([^.]\+\)$','\1','')
  let lbl= a:id
  if sfx != a:fname
   if exists("s:CtagExpl_kinds[sfx]")
	if exists("s:CtagExpl_kinds[sfx][a:id]")
	 let lbl= s:CtagExpl_kinds[sfx][a:id]
	else
"	 call Decho("s:CtagExpl_kinds[".sfx."][".a:id."] doesn't exist")
	endif
   else
"	call Decho("s:CtagExpl_kinds[".sfx."] doesn't exist")
   endif
  endif
"  call Dret("s:GetLabel ".lbl." : sfx<".sfx.">")
  return lbl
endfun

" ---------------------------------------------------------------------
" s:CtagExplMenu: {{{2
fun! s:CtagExplMenu()
"  call Dfunc("s:CtagExplMenu()")
  if has("gui") && has("menu") && has("gui_running") && &go =~# 'm'
   if expand("%") == g:CtagExpl_winname
	exe 'silent! unmenu '.g:DrChipTopLvlMenu."CtagExpl"
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Prev<tab>\<s-left>	<s-left>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Next<tab>\<s-right>	<s-right>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Select<tab>\<cr>	:call <sid>CtagExplSelect(0)<cr>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Select\ into\ Vsplit<tab>v	v'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Select\ into\ Hsplit<tab>v	o'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Select\ into\ Tab<tab>v	t'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Up<tab>\<s-up>	<s-up>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Down<tab>\<s-down>	<s-down>'
   else
	exe 'silent! unmenu '.g:DrChipTopLvlMenu."CtagExpl"
	exe 'silent! menu '.g:DrChipTopLvlMenu.'CtagExpl.Init	:CtagExpl<cr>'
   endif
  endif
"  call Dret("s:CtagExplMenu")
endfun

" initialize the menu
call s:CtagExplMenu()

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
