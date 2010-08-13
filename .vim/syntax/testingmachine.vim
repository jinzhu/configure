syntax keyword TMTodo OBSOLETE FIXME TODO OPTIMIZE
syntax match TMComment "^[^|].*$"
syntax match TMTitle "^==\s\+.*$"
syntax match TMDelimiter "|"

highlight TMTodo guifg=yellow ctermfg=yellow
highlight TMTitle guifg=#AD7FA8 ctermfg=magenta term=bold
highlight TMDelimiter guifg=yellow ctermfg=yellow
hi link   TMComment Comment
