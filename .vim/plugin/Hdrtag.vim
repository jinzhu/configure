" Hdrtag.vim
"   Author: Charles E. Campbell, Jr.
"   Date:   Feb 22, 2011
"   Version: 1t	ASTRO-ONLY
"
"   Philippians 1:21: For to me to live is Christ, and to die is gain.
"redraw!|call DechoSep()|call inputsave()|call input("Press <cr> to continue")|call inputrestore()
" =====================================================================
"  Load Once: {{{1
if &cp || exists("g:loaded_Hdrtag")
 finish
endif
let g:loaded_Hdrtag= "v1t"
if v:version < 702
 echohl WarningMsg
 echo "***warning*** this version of Hdrtag needs vim 7.2"
 echohl Normal
 finish
endif
let s:keepcpo= &cpo
set cpo&vim
"DechoRemOn

" ---------------------------------------------------------------------
"  Variables: {{{1
if !exists("g:hdrtagwinwidth")
 let g:hdrtagwinwidth= 25
endif
if !exists("g:hdrtag_pgm")
 let g:hdrtag_pgm= "hdrtag"
endif
if !exists("g:hdrtag_mktags")
 let g:hdrtag_mktags= "mktags"
endif
if !exists("g:hdrtag_cygwin")
 if has("win32") || has("win95") || has("win64") || has("win16")
  if &shell == "bash"
   let g:hdrtag_cygwin= 1
  else
   let g:hdrtag_cygwin= 0
  endif
 else
  let g:hdrtag_cygwin= 0
 endif
endif
if !exists("g:DrChipTopLvlMenu")
 let g:DrChipTopLvlMenu= "DrChip."
endif
if !exists("g:hdrtag_winname")
 let g:hdrtag_winname= "[Hdrtag]"
endif

" =====================================================================
"  Functions: {{{1

" ---------------------------------------------------------------------
" Hdrtag#Hdrtag: {{{2
fun! Hdrtag#Hdrtag(bang,...)
"  "  call Dfunc("Hdrtag#Hdrtag(a:bang=".a:bang.") a:0=".a:0)

  let hdrtagbuf= bufnr(escape(g:hdrtag_winname,'[]'))
"  call Decho("hdrtagbuf#".hdrtagbuf)

  if hdrtagbuf == -1
   " Hdrtag buffer doesn't exist
"   call Decho("buffer named ".g:hdrtag_winname." doesn't exist")
   if a:0 > 0 && a:1 == 0
	" Hdrtag 0  and Hdrtag already doesn't exist
"	call Dret("Hdrtag#Hdrtag : already doesn't exist")
	return
   endif
   call s:HdrtagInit(a:bang)

  else
   " Hdrtag buffer exists
"   call Decho("buffer named ".g:hdrtag_winname." exists")
   let hdrtagwin= bufwinnr(hdrtagbuf)

   if hdrtagwin > 0
	" Hdrtag window exists
"	call Decho("window holding ".g:hdrtag_winname." exists")
    if a:0 > 0 && a:1 == 1
	 " Hdrtag 1  and Hdrtag already exists
"	 call Dret("Hdrtag#Hdrtag : already exists and is showing")
	 return
	elseif a:0 > 0 && a:1 == 2
	 " force hdrtag initialization, even though its buffer & window exist
     call s:HdrtagInit(a:bang)
"	 call Dret("Hdrtag#Hdrtag : forced initialization")
	 return
    endif
    call s:HdrtagOff()

   else
	" Hdrtag window doesn't exist (but the buffer does)
"	call Decho("window holding ".g:hdrtag_winname." doesn't exist")

    if a:0 > 0 && a:1 == 0
	 call s:HdrtagOff()
"	 call Dret("Hdrtag#Hdrtag : turned off (buffer existed, window didn't)"
	 return
	endif

	" bring Hdrtag window to foreground
	if exists("g:HdrtagForeground") && type(g:HdrtagForeground) == 2
"	 call Decho("use g:HdrtagForeground() to bring Hdrtag to foreground")
	 call g:HdrtagForeground()
     if bufwinnr(hdrtagbuf) > 0
"	  call Dret("Hdrtag#Hdrtag : brought to foreground")
	  return
	 endif
	endif
"    call Decho('exe topleft '.g:hdrtagwinwidth."vsplit")
    exe 'topleft '.g:hdrtagwinwidth."vsplit"
"	call Decho("exe b ".hdrtagbuf)
	exe "b ".hdrtagbuf
	if line("$") == 1 && getline(1) == ""
	 " looks like the buffer got wiped out somewhere.  Re-create it.
	 exe "bw ".hdrtagbuf
     call s:HdrtagInit(a:bang)
	endif
   endif
  endif

"  call Dret("Hdrtag#Hdrtag")
endfun

" ---------------------------------------------------------------------
" s:HdrtagInit: {{{2
fun! s:HdrtagInit(dotags)
"  call Dfunc("HdrtagInit(dotags=".a:dotags.")")

  if a:dotags && filereadable("tags")
"   call Decho("deleting <tags>")
   call delete("tags")
  endif

  " if <tags> doesn't exist:
  "  1) try &tags path
  "  2) run mktags script if it exists.  If not,
  "  3) run hdrtag_pgm on current file
  let tagfile= "tags"

  if !filereadable(tagfile)
   " tagfile not readable,
   " if it exists, try g:hdrtag_taglist,
   " otherwise try &tags
"   call Decho(tagfile." not readable, trying &tags")
   if exists("g:hdrtag_taglist") && type(g:hdrtag_taglist) == 0
	let taglist= &tags
   elseif exists("g:hdrtag_taglist") && type(g:hdrtag_taglist) == 1
	let taglist= split(g:hdrtag_taglist,',')
   else
    let taglist= split(&tags,',')
   endif
   for tagpath in taglist
	if exists("g:hdrtag_exclude")
	 if tagpath =~ g:hdrtag_exclude
	  continue
	 endif
	endif
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
   if executable(g:hdrtag_mktags)
   	" run mktags script to generate tags (presumably for multiple files)
"	call Decho(tagfile." not readable, try mktags script<".g:hdrtag_mktags.">")
   	call system(g:hdrtag_mktags)
   endif
  endif

  if !filereadable(tagfile)
   " tagfile not readable, try making <tags> using pgm
"   call Decho("tagfile not readable, try making tags with ".g:hdrtag_pgm)
   if exists("g:hdrtag_chgwin")
"    call Decho("srcfile=bufname(bufwinnr(chgwin#".g:hdrtag_chgwin.")=".bufwinnr(g:hdrtag_chgwin).")<".simplify(bufname(bufwinnr(g:hdrtag_chgwin))).">")
    let srcfile= simplify(bufname(bufwinnr(g:hdrtag_chgwin)))
   else
"    call Decho("srcfile=expand('%')<".expand("%").">")
    let srcfile= simplify(expand("%"))
   endif
"   call Decho("system(".g:hdrtag_pgm." ".shellescape(srcfile).')')
   call system(g:hdrtag_pgm." ".shellescape(srcfile))
  endif

  if !filereadable(tagfile)
   " <tags> still not readable, giving up!
"   call Decho("<tags> still not readable, giving up!")
  	redraw!
   echohl Error | echo "unable to find or create a <tags> file!" | echohl None
"   call Dret("HdrtagInit : unable to initialize")
   return
  endif

"  call Decho(tagfile." file present and readable")

  " create hdrtag window/buffer
  if !exists("g:hdrtag_chgwin")
"   call Decho("create ".g:hdrtag_winname." window, g:hdrtag_chgwin doesn't exist")
"   call Decho('exe topleft '.g:hdrtagwinwidth."vsplit")
   exe 'topleft '.g:hdrtagwinwidth."vsplit"
   wincmd l
   let g:hdrtag_chgwin         = winnr()
   let s:hdrtag_chgwin_created = 1
   wincmd h
   enew
   setlocal bt=nofile noswf
"   call Decho("create ".g:hdrtag_winname." w#".winnr()." buf#".bufnr("%")." src-w#".g:hdrtag_chgwin." src-buf#".bufwinnr(g:hdrtag_chgwin))

  elseif line("$") != 1 || getline(1) != ""
"   call Decho("create ".g:hdrtag_winname." window, chgwin=".g:hdrtag_chgwin)
   enew
   setlocal bt=nofile noswf
"   call Decho("using ".g:hdrtag_winname." w#".winnr()." buf#".bufnr("%")." src-w#".g:hdrtag_chgwin." src-buf#".bufwinnr(g:hdrtag_chgwin))
  endif
  let s:hdrtagwin   = winnr()
  let s:hdrtagbufnr = bufnr("%")
"  call Decho("hdrtagwin#".s:hdrtagwin." hdrtagbufnr#".s:hdrtagbufnr)

  " read and sort <tags>
"  call Decho(" read and sort <tags>")
  if exists("g:hdrtag_taglist") && (type(g:hdrtag_taglist) == 1 || type(g:hdrtag_taglist) == 0)
"   call Decho("taglist<".string(taglist).">")
   for tagpath in taglist
	if exists("g:hdrtag_exclude")
	 if tagpath =~ g:hdrtag_exclude
	  continue
	 endif
	endif
    if filereadable(tagpath)
"     call Decho("reading <".tagpath.">")
     exe "silent r ".tagpath
    endif
   endfor
  else
   exe "silent r ".tagfile
  endif
"  call Decho("read ".line("$")." lines of ".tagfile)
  exe 'silent file '.g:hdrtag_winname
  setlocal bt=nofile bh=hide
  1d

  " remove comments
"  call Decho("remove comments")
  silent! g/^!_/d

  " transform into Hdrtag format
  silent %s/^\([^\t]\+\)\t.*;"\t\(\S\{,2}\)\t\(\S\+\)$/\3	\1	\2/e
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
"   call Decho("lbl<".lbl."> getline<".getline(".").">")
   let s:lblkind[lbl]= substitute(getline("."),'^.*\t\(\S\{,2}\)$','\1','')
"   call Decho('s:lblkind['.lbl.']<'.s:lblkind[lbl].'>')
   let @a= ' '.lbl.': {{'.'{1'
"   call Decho("@a<".@a.">")
   put! a
"   call Decho('exe silent! .,/^\%(\<'.lbl.'\>\)\@!\|\%'.line("$").'l./s/^\<'.lbl.'\>//')
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
  silent g/^  \S/s/\t\S\{,2}$//
  let curline= 1
  1d

  " get rid of adjacent duplicates
"  call Decho("get rid of adjacent duplicates, if any")
  exe 'silent g/^\%<'.line("$").'l\(.*\)\n\1$/d'
  1
  setlocal nomod noma ro fdm=marker
  set ft=Hdrtag
  setlocal wfw

  " Hdrtag List Window Interface
"  call Decho("set up maps")
  nnoremap <silent> <buffer> <s-left>	:call <sid>HdrtagPrev()<cr>
  nnoremap <silent> <buffer> <s-right>	:call <sid>HdrtagNext()<cr>
  nnoremap <silent> <buffer> <cr>		:call <sid>HdrtagSelect(0)<cr>
  nnoremap <silent> <buffer> v			:call <sid>HdrtagSelect(1)<cr>
  nnoremap <silent> <buffer> o			:call <sid>HdrtagSelect(2)<cr>
  nnoremap <silent> <buffer> t			:call <sid>HdrtagSelect(3)<cr>
  nnoremap <silent> <buffer> h			:call <sid>HdrtagPrev()<bar>call <sid>HdrtagSelect(2)<cr>
  nnoremap <silent> <buffer> l			:call <sid>HdrtagNext()<bar>call <sid>HdrtagSelect(2)<cr>
  nnoremap <silent> <buffer> <s-up>		:call search('^\S','bW')<cr>
  nnoremap <silent> <buffer> <s-down>	:call search('^\S','W')<cr>
  augroup Hdrtag
   au!
   exe "au CursorMoved	".escape(g:hdrtag_winname,'[]')." call s:HdrtagInfo(1)"
  augroup END
  call s:HdrtagMenu()

"  call Dret("HdrtagInit")
endfun

" ---------------------------------------------------------------------
" s:HdrtagOff: {{{2
fun! s:HdrtagOff()
"  call Dfunc("HdrtagOff()")

  let hdrtagbuf= bufnr(escape(g:hdrtag_winname,'[]'))
  if hdrtagbuf == -1
"   call Dret("HdrtagOff")
   return
  endif
"  call Decho("hdrtagbuf#".hdrtagbuf)

  let hdrtagwin= bufwinnr(hdrtagbuf)
  if hdrtagwin > 0
"   call Decho("exe ".hdrtagwin."wincmd w")
   exe hdrtagwin."wincmd w"
   if exists("s:hdrtag_win")
    unlet s:hdrtag_win
   endif
"   call Decho("silent q! (bh=".&bh." bt=".&bt.")")
   silent q!
  endif

  augroup Hdrtag
   au!
  augroup END
  aug! Hdrtag

  if exists("s:hdrtag_chgwin_created")
"   call Decho("unlet'ing g:hdrtag_chgwin and s:hdrtag_chgwin_created")
   unlet g:hdrtag_chgwin
   unlet s:hdrtag_chgwin_created
  endif

  call s:HdrtagMenu()
"  call Dret("HdrtagOff")
endfun

" ---------------------------------------------------------------------
" s:HdrtagSelect: {{{2
fun! s:HdrtagSelect(openmode)
"  call Dfunc("HdrtagSelect(openmode=".a:openmode.") s:kindcnt=".(exists("s:kindcnt")? s:kindcnt : "doesn't exist"))

  if getline(".") =~ '^  '
   norm! 0w
 
   let select = expand("<cWORD>")
"   call Decho("selecting word<".select.">  openmode=".a:openmode." s:infocnt=".(exists("s:infocnt")? s:infocnt : "doesn't exist"))
   if a:openmode == 0 || a:openmode == 2
	" re-use change window, put cursor there (<cr> o h l maps)
"	call Decho("re-use change window, putting cursor there")
	exe g:hdrtag_chgwin."wincmd w"
   elseif a:openmode == 1
	" vertically split change window, put cursor there (v map)
"	call Decho("vertically splitting change window, putting cursor there")
	exe s:hdrtag_chgwin."wincmd w"
   	wincmd v
   elseif a:openmode == 3
	" open in a new tab (t map)
"	call Decho("opening a new tab")
	tab split
	let s:hdrtag_chgwin= winnr()
   endif

   if exists("s:infocnt") && s:infocnt > 0
"    call Decho("s:infocnt=".s:infocnt." select<".select.">")
"    call Decho("exe silent e ".taglist('^'.select.'$')[s:infocnt-1]["filename"])
    exe "silent e ".taglist('^'.select.'$')[s:infocnt-1]["filename"]
    1
	let srchstring = taglist('^'.select.'$')[s:infocnt-1]["cmd"]
	let srchstring = escape(srchstring,'*[]')
"	call Decho("srchstring<".srchstring.">")
	exe "sil! ".srchstring
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
   if exists("g:Hdrtag_funcref") && type(g:Hdrtag_funcref) == 2
"	 call Decho("making call to g:Hdrtag_funcref()")
	call g:Hdrtag_funcref()
   endif
   if a:openmode == 2
	" return cursor to hdrtag window (o h l maps)
"	call Decho("exe ".s:hdrtagwin."wincmd w")
	exe s:hdrtagwin."wincmd w"
   endif

"   call Dret("HdrtagSelect : select<".select.">")
   return
  endif

"  call Dret("HdrtagSelect : not a selectable line")
endfun

" ---------------------------------------------------------------------
" s:HdrtagInfo: invoked when the cursor line has changed {{{2
fun! s:HdrtagInfo(kindcnt)
"  call Dfunc("HdrtagInfo(kindcnt=".a:kindcnt.")")
  let s:kindcnt= a:kindcnt
  " insure that hdrtag list window is of standard width
  "exe "vertical res ".g:hdrtagwinwidth
  if winline() == 1
   redraw!
   exe "silent norm! \<c-y>"
  elseif winline() == winheight(0)
   redraw!
   exe "silent norm! \<c-e>"
  endif
"  call Decho("bufnr(%)=".bufnr("%")." hdrtagbufnr=".s:hdrtagbufnr." kindcnt=".s:kindcnt)
  if bufnr("%") == s:hdrtagbufnr
   let curline= getline(".")
"   call Decho("curline<".curline.">")
   if curline =~ '^  \S'
    let select    = expand("<cWORD>")
    let lblline   = search('^\S.*: {\{3}1$','nbWc')
    let lbl       = substitute(getline(lblline),': {\{3}1','','')
    let kind      = s:lblkind[lbl]
"	call Decho("select  <".select.">")
"	call Decho("lblline <".lblline.">")
"	call Decho("kind    <".kind.">")
"    call Decho('s:lblkind['.lbl.']<'.kind.'>')
    let tl        = taglist('^'.select.'$')
    let s:infocnt = 0
	let ikindcnt  = 0
    for itl in tl
	 let s:infocnt= s:infocnt + 1
"	 call Decho('for loop: itl["kind"]<'.itl["kind"]."> kind<".kind."> ikindcnt=".ikindcnt." infocnt=".s:infocnt)
     if itl["kind"] == kind
	  let ikindcnt    = ikindcnt + 1
	  let lastmsg     = "(".ikindcnt."/".len(tl).") ".itl["filename"].": ".substitute(itl["cmd"],'^\/\^\(.*\)\$\/$','\1','')
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
"	call Decho("final: lastmsg<".lastmsg."/".len(tl)."> lastinfocnt=".lastinfocnt." lastkindcnt=".lastkindcnt)
   endif
  else
   let s:kindcnt= 1
   let s:infocnt= 1
   echo " "
  endif
"  call Dret("HdrtagInfo : s:infocnt=".(exists("s:infocnt")? s:infocnt : -1)." s:kindcnt=".(exists("s:kindcnt")? s:kindcnt : -1))
endfun

" ---------------------------------------------------------------------
" s:HdrtagPrev: {{{2
fun! s:HdrtagPrev()
"  call Dfunc("s:HdrtagPrev()")
  if !exists("s:kindcnt") || s:kindcnt <= 1
   let s:kindcnt= 1
  else
   let s:kindcnt= s:kindcnt - 1
  endif
"  call Decho("s:kindcnt=".s:kindcnt)
  call s:HdrtagInfo(s:kindcnt)
"  call Dret("s:HdrtagPrev")
endfun

" ---------------------------------------------------------------------
" s:HdrtagNext: {{{2
fun! s:HdrtagNext()
"  call Dfunc("s:HdrtagNext()")
  if !exists("s:kindcnt") 
   let s:kindcnt= 2
  else
   let s:kindcnt= s:kindcnt + 1
  endif
"  call Decho("s:kindcnt=".s:kindcnt)
  call s:HdrtagInfo(s:kindcnt)
"  call Dret("s:HdrtagNext")
endfun

" ---------------------------------------------------------------------
" s:HdrtagMenu: {{{2
fun! s:HdrtagMenu()
"  call Dfunc("s:HdrtagMenu()")
  if has("gui") && has("menu") && has("gui_running") && &go =~# 'm'
   if expand("%") == g:hdrtag_winname
	exe 'silent! unmenu '.g:DrChipTopLvlMenu."Hdrtag"
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Prev<tab>\<s-left>	<s-left>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Next<tab>\<s-right>	<s-right>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Select<tab>\<cr>	:call <sid>HdrtagSelect(0)<cr>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Select\ into\ Vsplit<tab>v	v'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Select\ into\ Hsplit<tab>v	o'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Select\ into\ Tab<tab>v	t'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Up<tab>\<s-up>	<s-up>'
    exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Down<tab>\<s-down>	<s-down>'
   else
	exe 'silent! unmenu '.g:DrChipTopLvlMenu."Hdrtag"
	exe 'silent! menu '.g:DrChipTopLvlMenu.'Hdrtag.Init	:Hdrtag<cr>'
   endif
  endif
"  call Dret("s:HdrtagMenu")
endfun

" initialize the menu
call s:HdrtagMenu()

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
