"set dictionary+=/usr/share/dict/words
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"LaTex
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set winaltkeys=no "shielded ALT
set grepprg=grep\ -nH\ $*
let g:tex_flavor = "latex"
set iskeyword+=:
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,perl,tex set shiftwidth=2

autocmd FileType c,cpp,java,javascript,python,xml,xhtml,html set shiftwidth=4

autocmd BufNewFile,BufRead *.yml setf eruby
autocmd BufNewFile,BufRead *_spec.rb source ~/.vim/ftplugin/rails/rspec.vim
autocmd BufNewFile,BufRead *_test.rb source ~/.vim/ftplugin/rails/shoulda.vim
"use \rci in normal mode to indent ruby code,should install kode ,sudo gem install kode
nmap <leader>rci :%!ruby-code-indenter<cr>

autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
" Load matchit (% to bounce from do to end, etc.)
runtime! macros/matchit.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vimrc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Fast reloading of the .vimrc
map <leader>s :source ~/.vimrc<cr>
"Fast editing of .vimrc
map <leader>e :tabedit ~/.vimrc<cr>
"When .vimrc is edited, reload it
autocmd! bufwritepost .vimrc source ~/.vimrc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"STARDICT: Use stardict translate Ctrl+\ , should install sdcv
nmap <C-\> :!sdcv -u 朗道英汉字典5.0 -u 牛津简明英汉袖珍辞 -u 五笔86 -n <C-R>=expand("<cword>")<CR><CR>
" Ctags
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let Tlist_Sort_Type = "name"
" 在右侧显示窗口
let Tlist_Use_Right_Window = 1
" 压缩方式
let Tlist_Compart_Format = 1
" 如果只有一个buffer，kill窗口也kill掉buffer
let Tlist_Exist_OnlyWindow = 1
" 不要关闭其他文件的tags
let Tlist_File_Fold_Auto_Close = 0
" 不要显示折叠树
let Tlist_Enable_Fold_Column = 0
let Tlist_Close_On_Select=1
let Tlist_Show_Menu = 1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HTML
let g:do_xhtml_mappings = 'yes'
let g:force_html_menu = 'yes'
"设置为HTML小写
let g:html_tag_case = 'lowercase'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 颜色
if has("gui_running")
  set guioptions-=T
  colorscheme vibrantink
else
  set background=dark
endif
"GUI设置color：guifg, guibg, gui
"支持彩色显示的Terminal：ctermfg, ctermbg
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"map <C-t> :tabnew<CR>
"map <C-w> :tabclose<CR>
" <N n>
:nn <M-1> 1gt
:nn <M-2> 2gt
:nn <M-3> 3gt
:nn <M-4> 4gt
:nn <M-5> 5gt
:nn <M-6> 6gt
:nn <M-7> 7gt
:nn <M-8> 8gt
:nn <M-9> 9gt
:nn <M-0> :tablast<CR>

:ino <M-1> <C-o>1gt
:ino <M-2> <C-o>2gt
:ino <M-3> <C-o>3gt
:ino <M-4> <C-o>4gt
:ino <M-5> <C-o>5gt
:ino <M-6> <C-o>6gt
:ino <M-7> <C-o>7gt
:ino <M-8> <C-o>8gt
:ino <M-9> <C-o>9gt
:ino <M-0> <C-o>:tablast<CR>

:nn <F2> :tabnew<CR>
:nn <F3> :%s/\s*$//g<cr>:nohlsearch<cr>''
:nn <F4> :set nu! <CR>
autocmd BufRead,BufNewFile *.rb map <F5> :% w !ruby<CR>
map <F6> <Esc>:set suffixesadd=.html.erb<CR>gf
map <F7> <Esc>:set suffixesadd=.rb<CR>gf
:nn <F8> :TlistToggle<CR>
:nn <F9> :shell <CR>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:nn <F10> :call ToggleSketch()<CR>  "没事，鼠标画线玩的

" <leader n>
map <leader>1 :set syntax=ruby<cr>
map <leader>2 :set syntax=xhtml<cr>
map <leader>3 :set ft=javascript<cr>
map <leader>4 :set ft=vim<cr>
map <leader>5 :set ft=sh<cr>
" <M-hjkl>
:ino <M-j> <DOWN>
:ino <M-k> <UP>
:ino <M-h> <LEFT>
:ino <M-l> <RIGHT>

:nn <C-tab> <C-PageDown>
:nn <C-S-tab> <C-PageUp>

:nn <C-h> <C-w>h
:nn <C-j> <C-w>j
:nn <C-k> <C-w>k
:nn <C-l> <C-w>l


map <C-q> "+gP
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"minibufexpl.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" encoding
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set fileencoding=utf-8
set fileencodings=utf-8,gb18030,ucs-bom,gbk,gb2312,cp936
set encoding=utf8 "设置创建、读取、编辑时都采用utf-8编码
"set langmenu=none
"language messages en_US.UTF8
"let $LANG='zh'
""set fileencodings=utf-8,GBK
"set fileencoding=utf8
"set encoding=utf8
"set tenc=utf8
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set history=400
set helplang=cn
set ignorecase
set hlsearch

" 使用鼠标
set mouse=a
" 输入的命令显示出来，看的清楚些。
set showcmd

set nocompatible " We're running Vim, not Vi!
syntax on " Enable syntax highlighting
filetype plugin indent on " Enable filetype-specific indenting and plugins
set autoindent
set smartindent

set backup " make backup file
set backupdir=~/.tmp" where to put backup file
set directory=~/.tmp " directory is the directory for temp file
set autoread "Set to auto read when a file is changed from the outside
set noshowmatch "show matching bracets
set formatoptions=tcrqn "自动格式化
set cmdheight=2 "命令行（在状态行下）的高度，默认为1，这里是2
set magic "Set magic on
set smarttab
set expandtab

imap ;w <ESC>:w<CR>
:nn <Space>w <ESC>:w<CR>
