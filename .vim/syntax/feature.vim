if version < 600
	syntax clear
elseif exists("b:current_syntax")
  finish
endif

" make things case sensitive
syn case match

"setlocal iskeyword+=:
" english
syn keyword xStatement Feature Scenario Given When Then And More Examples contained
" japanese
syn keyword xStatement フィーチャ シナリオ 前提 もし ならば かつ 他のサンプル contained

" english
syn match featureContext  /^\s*Feature.*/   contains=xStatement
syn match scenarioContext /^\s*Scenario.*$/ contains=xStatement
syn match givenContext    /^\s*Given.*$/    contains=ALL
syn match whenContext     /^\s*When.*$/     contains=ALL
syn match thenContext     /^\s*Then.*$/     contains=ALL
syn match andContext      /^\s*And.*$/      contains=ALL
syn match moreContext      /^\s*More Examples.*$/ contains=ALL

" japanese
syn match featureContextJ  /^\s*フィーチャ.*/ contains=xStatement
syn match scenarioContextJ /^\s*シナリオ.*$/  contains=xStatement
syn match givenContextJ    /^\s*前提.*$/      contains=ALL
syn match whenContextJ     /^\s*もし.*$/      contains=ALL
syn match thenContextJ     /^\s*ならば.*$/    contains=ALL
syn match andContextJ      /^\s*かつ.*$/      contains=ALL
syn match moreContextJ      /^\s*他のサンプル.*$/ contains=ALL

" FIXME matches apostrophes, too :(
syn region stringToken start=/'/ skip=/\\'/ end=/'/ contained
syn region dblStringToken start=/"/ skip=/\\"/ end=/"/ contained
" FIXME this matches 
"syn match stringToken /'.+'/ contained

hi link xStatement Statement

" english
hi link featureContext  Title
hi link scenarioContext Title
hi link givenContext    Function
hi link whenContext     Function
hi link thenContext     Function
hi link andContext      Function
hi link moreContext     Function
hi link stringToken     Question
hi link dblStringToken  Question

" japanese
hi link featureContextJ  Title
hi link scenarioContextJ Title
hi link givenContextJ    Function
hi link whenContextJ     Function
hi link thenContextJ     Function
hi link andContextJ      Function
hi link moreContextJ     Function
hi link stringTokenJ     Question
hi link dblStringTokenJ  Question

let b:current_syntax = "feature"
