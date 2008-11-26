" Vim filetype plugin
" Language:	Cucumber Feature
" Maintainer:	Ben Mabey <ben@benmabey.com>
" Original Author: Mike Vincent <mike@vincent.ws> (http://github.com/agile/vim-story/)
" Last Change:	2008 Nov 03

" Only do this when not done yet for this buffer
if (exists("b:did_ftplugin"))
  finish
endif
let b:did_ftplugin = 1

let b:feature_dir = expand("%:p:h")
let &l:path = fnamemodify(b:feature_dir, ':h').",".&l:path
let b:undo_ftplugin = "setl path<"

setlocal textwidth=72
setlocal expandtab
setlocal shiftwidth=2
setlocal tabstop=2
" FIXME: spell does not support japanese
" setlocal spell

" TODO: Write a helper function that will grab a selected block of
" Given, When, Then, And statements and open the alternate file and
" create step stubs
"
" TODO: make it so when a feature/step is saved it's run and the output is 
" tossed into a preview window..
