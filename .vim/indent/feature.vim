" Vim indent file
" Language:		Text, Cucumber Feature
" Maintainer:	Ben Mabey <ben@benmabey.com>
" Original Author: Mike Vincent <mike@vincent.ws> (http://github.com/agile/vim-story/)

" 0. Initialization {{{1
" =================

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal autoindent
setlocal indentexpr=GetFeatureIndent()
setlocal indentkeys=*<Return>,o,O,!^F ",=Feature,=Scenario,=Given,=When,=Then,=And

" Only define the function once.
if exists("*GetFeatureIndent")
  finish
endif

function! GetFeatureIndent()
  let vcol = col('.')
  call cursor(v:lnum,1)
  call cursor(v:lnum,vcol)
  let ind = indent(v:lnum)

  let lnum = prevnonblank(v:lnum-1)
  let line = getline(lnum)
  let cline = getline(v:lnum)

  " Feature statements should always start at margin 0
  if cline =~# '^\s*Feature' 
    let ind = 0
  endif

  " Scenario statements should always start 2 indentations in
  if cline =~# '^\s*Scenario'
    let ind = 0 + (&sw * 2)
  endif

  " Feature content should start 1 indentation in
  if line =~# '^\s*Feature' && cline !~# '^\s*Scenario'
    let ind = 0 + &sw
  endif
  
  " Given, When, Then, And company should start 3 indentations in 
  if cline =~# '^\s*Given'
    let ind = 0 + (&sw * 3)
  endif

  return ind
endfunction

" vim:set sw=2 sts=2 ts=2 et ff=unix:
