syntax keyword CheatTodo OBSOLETE FIXME TODO OPTIMIZE
syntax match CheatComment "^\s*#.*$" contains=CheatTodo

syntax match Level1 "^=\s\+.*$"
syntax match Level2 "^\s*==\s\+.*$"
syntax match Level3 "^\s*===\s\+.*$"
syntax match Level4 "^\s*====\s\+.*$"
syntax match Level5 "^\s*=====\s\+.*$"
syntax match Level6 "^\s*======\s\+.*$"
syntax match Level7 "^\s*=======\s\+.*$"

syntax match Command "^\s*\$\s\+.*$"
syntax match Empha "@.*@"

highlight Level1 guifg=#E20003 ctermfg=darkred term=bold
highlight Level2 guifg=#43A7FF ctermfg=red     term=bold
highlight Level3 guifg=#00FFE2 ctermfg=blue    term=bold
highlight Level4 guifg=magenta ctermfg=cyan    term=bold
highlight Level5 guifg=magenta ctermfg=cyan    term=bold
highlight Level6 guifg=magenta ctermfg=cyan    term=bold
highlight Level7 guifg=magenta ctermfg=cyan    term=bold

highlight Command guifg=#AD7FA8 ctermfg=magenta term=bold
highlight Empha guifg=#F8FC00 ctermfg=yellow term=bold

highlight CheatComment guifg=LightGray ctermfg=LightGray
highlight CheatTodo guifg=yellow ctermfg=yellow
