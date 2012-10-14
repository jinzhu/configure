set nocompatible

if filereadable(expand("~/.vim/vundlerc"))
  source ~/.vim/vundlerc
endif

" ================ General Config ====================

set history=1000                "Store lots of :cmdline history
set lazyredraw                  " Don't redraw screen during macros
set tf                          " Improves redrawing for newer computers
set sc                          " Show incomplete command at bottom right
set timeoutlen=500              " Lower timeout for mappings
set cot=menu                    " Don't show extra info on completions
set spelllang=en_us

" Search home directory path on cd. But can't complete.
set cdpath+=~
set cdpath+=~/GIT

set pastetoggle=<F10>          " toggle between paste and normal: for 'safer' pasting from keyboard
set shiftround                 " round indent to multiple of 'shiftwidth'
set tags=./tags;$HOME          " walk directory tree upto $HOME looking for tags

let g:is_posix = 1             " vim's default is archaic bourne shell, bring it up to the 90s

set modeline
set modelines=5                " default numbers of lines to read for modeline instructions

" set autochdir
let bufpane_showhelp = 0
set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set gcr=a:blinkon0              "Disable cursor blink
set noerrorbells                " No noise
set novisualbell                " No blinking
set visualbell t_vb=            " disable any beeps or flashes on error
set shortmess=atI

set laststatus=2   " Always show the statusline

"some stuff to get the mouse going in term
set selectmode+=mouse
set mouse=a
set mousehide                 " Hide mouse after chars typed
set ttymouse=xterm2

set showmatch           " Show matching brackets.
" Highlight when CursorMoved.
set cpoptions-=m
set matchtime=3
set matchpairs+=<:>     " Highlight <>.
set formatoptions=tcrqn " 自动格式化
set magic               " Set magic on

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" This makes vim act like all other editors, buffers can
" exist in the background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden
set title

"turn on syntax highlighting
syntax on

" ================ Search ==============

set hlsearch                   " highlight search
set ignorecase                 " be case insensitive when searching
set smartcase                  " be case sensitive when input has a capital letter
set incsearch                  " show matches while typing
set gdefault                    " search/replace "globally" (on a line) by default

set viminfo='100,f1            "Save up to 100 marks, enable capital marks

" ================ Turn Off Swap Files ==============

set backup              " make backup file
set backupdir==~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp  " where to put backup file
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp   " directory is the directory for temp file
set noswapfile
set nowb

set autowrite                   " Writes on make/shell commands
set autoread                    "Reload files changed outside vim


" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.

set undofile
set undodir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

" ================ Indentation ======================

set autoindent
set cindent
set indentkeys-=0#            " do not break indent on #
set cinkeys-=0#
set cinoptions=:s,ps,ts,cs
set cinwords=if,else,while,do
set cinwords+=for,switch,case

set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab


filetype plugin on
filetype indent on

set wrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ================ Folds ============================

set foldenable                " Turn on folding
set foldmethod=marker         " Fold on the marker
set foldlevel=100             " Don't autofold anything (but I can still fold manually)

set foldopen=block,hor,tag    " what movements open folds
set foldopen+=percent,mark
set foldopen+=quickfix

set virtualedit=block

set splitbelow
set splitright

" ================ Completion =======================

set wildmode=list:longest
set completeopt+=preview

set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*vendor/rails/**
set wildignore+=*vendor/cache/**
set wildignore+=*.gem
set wildignore+=*log/**
set wildignore+=*tmp/**
set wildignore+=*.png,*.jpg,*.gif
set wildignore+=*.a,*.lib,*.so,CVS,vendor/qor,public/system
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*  " Linux/MacOSX

" set list                      " display unprintable characters f12 - switches
" set listchars=tab:\ ·,eol:¬
" set listchars+=trail:·
" set listchars+=extends:»,precedes:«
" map <silent> <F12> :set invlist<CR>

" ================ Scrolling ========================

set scrolloff=0         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

for f in split(glob('~/.vim/plugin/settings/*.vim'), '\n')
  exe 'source' f
endfor


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Color
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
  set guioptions-=T
  colorscheme vibrantink
else
  set background=dark
  colorscheme vibrantink
endif

set fileencoding=utf-8
set fileencodings=utf-8,gb18030,ucs-bom,gbk,gb2312,cp936
set encoding=utf8
set guifont=Monaco\ 14
" GUI - color：guifg, guibg, gui
" Terminal：ctermfg, ctermbg

"tell the term has 128 colors
set t_Co=128

runtime macros/matchit.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HTML
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:do_xhtml_mappings = 'yes'
let g:force_html_menu   = 'yes'
let g:html_tag_case     = 'lowercase'


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" My Leader HotKeys
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <F2> :tabedit <CR>
map <F3> :shell <CR>
map <F4> :set nu! <CR>
autocmd BufRead,BufNewFile *.rb map <F5>      :% w !ruby<CR>
imap <F6> <Esc>:ColorPicker<Cr>a
vmap <F6> <Del><Esc>h:ColorPicker<Cr>a
map <F7> :IndentGuidesToggle <CR>
nmap <F8> :TagbarToggle<CR>
map <F9> :GundoToggle<CR>
nmap <F11> <Plug>ToggleAutoCloseMappings

let mapleader = ";"
let g:ctrlp_cmd = 'CtrlPMixed'
map <Leader>t :CtrlP<CR>
map <Leader>b :CtrlPBuffer<CR>
map <Leader>r :CtrlPMRUFiles<CR>
map \c :%s/\s\+$//<CR>

cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%
map <Leader>ec :tabedit ~/.vimrc<CR>

" copy filename with line
noremap <silent> <leader>fl :let @+=expand("%:p:l").":".line(".")<CR>
" copy filename
noremap <silent> <F12> :let @+=expand('%:p')<CR>
noremap <silent> <leader>fn :let @+=expand('%:p')<CR>
" copy file directory
noremap <silent> <leader>fd :let @+=expand('%:p:h')<CR>

map <Leader>a :Ack

map <Leader>s :SessionList<CR>
autocmd! bufwritepost .vimrc source ~/.vimrc
autocmd! BufWritePost *.go execute ':Fmt'

map <Leader>p :YRShow<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gd :Git! diff<CR>
map <Leader>gc :Gcommit<CR>
map <Leader>gl :Extradite<CR>

map <leader>cd :lcd %:p:h<CR>:pwd<CR>

""" My `;` HotKeys
" Windows (;c, ;o)
nnoremap <Leader> <C-w>
nnoremap <Leader>o <C-w>o
nnoremap <Leader>c <C-w>c
imap <Leader>w <ESC>:w<CR>
map   <Leader>w <ESC>:w<CR>


"""" Copy & Paste
nmap gp "0p
nmap gP "+p
vmap gP "+p
vmap gy "+y


""" Navigation <M-hjkl>
ino <M-j> <DOWN>
ino <M-k> <UP>
ino <M-h> <LEFT>
ino <M-l> <RIGHT>

nnoremap k gk
nnoremap j gj
nnoremap gk k
nnoremap gj j

vnoremap k gk
vnoremap j gj
vnoremap gk k
vnoremap gj j

" make <c-l> clear the highlight as well as redraw
nnoremap <C-L> :nohls<CR><C-L>
inoremap <C-L> <C-O>:nohls<CR>

noreabbrev te tabedit
cnoremap <C-A>    <Home>
cnoremap <C-E>    <End>

nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

""" Tab
map <silent> <M-PageUp> :call MoveCurrentTab(-1)<Esc>
map <silent> <M-PageDown> :call MoveCurrentTab(1)<Esc>

nn ;1 1gt
nn ;2 2gt
nn ;3 3gt
nn ;4 4gt
nn ;5 5gt
nn ;6 6gt
nn ;7 7gt
nn ;8 8gt
nn ;9 9gt
nn ;0 :tablast<CR>

ino ;1 <C-o>1gt
ino ;2 <C-o>2gt
ino ;3 <C-o>3gt
ino ;4 <C-o>4gt
ino ;5 <C-o>5gt
ino ;6 <C-o>6gt
ino ;7 <C-o>7gt
ino ;8 <C-o>8gt
ino ;9 <C-o>9gt
ino ;0 <C-o>:tablast<CR>

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
