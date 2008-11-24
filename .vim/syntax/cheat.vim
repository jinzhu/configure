syntax keyword CheatTodo OBSOLETE FIXME TODO OPTIMIZE
syntax match CheatComment "^\s*#.*$" contains=CheatTodo

syntax match Level1 "^=\s\+.*$"
syntax match Level2 "^\s*==\s\+.*$"
syntax match Level3 "^\s*===\s\+.*$"
syntax match Level4 "^\s*====\s\+.*$"
syntax match Level5 "^\s*=====\s\+.*$"
syntax match Level6 "^\s*======\s\+.*$"
syntax match Level7 "^\s*=======\s\+.*$"

syntax match Empha "@.*@"

highlight Level1 guifg=#ff6600 ctermfg=blue term=bold
highlight Level2 guifg=red ctermfg=red term=bold
highlight Level3 guifg=magenta ctermfg=magenta term=bold
highlight Level4 guifg=magenta ctermfg=magenta term=bold
highlight Level5 guifg=magenta ctermfg=magenta term=bold
highlight Level6 guifg=magenta ctermfg=magenta term=bold
highlight Level7 guifg=magenta ctermfg=magenta term=bold

highlight Empha guifg=#ffff00 ctermfg=yellow term=bold

highlight CheatComment guifg=LightGray ctermfg=LightGray
highlight CheatTodo guifg=yellow ctermfg=yellow
