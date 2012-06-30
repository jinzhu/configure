set nocompatible

if filereadable(expand("~/.vim/vundlerc"))
  source ~/.vim/vundlerc
endif

" ================ General Config ====================

set lazyredraw             " Don't redraw screen during macros
set tf                     " Improves redrawing for newer computers
set sc                     " Show incomplete command at bottom right
set tm=500                 " Lower timeout for mappings
set cot=menu               " Don't show extra info on completions

" set autochdir
let bufpane_showhelp = 0
set number                      "Line numbers are good
set backspace=indent,eol,start  "Allow backspace in insert mode
set history=1000                "Store lots of :cmdline history
set showcmd                     "Show incomplete cmds down the bottom
set showmode                    "Show current mode down the bottom
set gcr=a:blinkon0              "Disable cursor blink
set noerrorbells
set novisualbell
set visualbell t_vb=
set shortmess=atI

set laststatus=2   " Always show the statusline

set autoread                    "Reload files changed outside vim
set gdefault                    " search/replace "globally" (on a line) by default

"some stuff to get the mouse going in term
set selectmode+=mouse
set mouse=a
set ttymouse=xterm2

set noshowmatch         " show matching bracets
set formatoptions=tcrqn " 自动格式化
set magic               " Set magic on
set smartcase

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" This makes vim act like all other editors, buffers can
" exist in the background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right
set hidden
set title

"turn on syntax highlighting
syntax on

" ================ Search Settings  =================

" set incsearch        "Find the next match as we type the search
set hlsearch         "Hilight searches by default
set viminfo='100,f1  "Save up to 100 marks, enable capital marks

" ================ Turn Off Swap Files ==============

set backup              " make backup file
set backupdir==~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp  " where to put backup file
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp   " directory is the directory for temp file
set noswapfile
set nowb

" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.

set undodir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set undofile

" ================ Indentation ======================

set autoindent
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

set foldmethod=indent   "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" ================ Completion =======================

set wildmode=list:longest
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
" nn <F8> :TlistToggle<CR>
map <F9> :GundoToggle<CR>
nmap <F11> <Plug>ToggleAutoCloseMappings
noremap <silent> <F12> :let @+=expand("%:p:l").":".line(".")<CR>

let mapleader = ";"
map <Leader>t :CtrlP<CR>
map <Leader>b :CtrlPBuffer<CR>
map <Leader>r :CtrlPMRUFiles<CR>
" map <Leader>f :CtrlPCurFile<CR>
" map <Leader>t :CommandT<CR>
" map <Leader>b :CommandTBuffer<CR>
map \c :%s/\s\+$//<CR>

cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%
map <Leader>ec :tabedit ~/.vimrc<CR>

map <Leader>a :Ack 
map <Leader>p :YRShow<CR>

map <Leader>s :SessionList<CR>
autocmd! bufwritepost .vimrc source ~/.vimrc
autocmd! BufWritePost *.go execute ':Fmt'

map <Leader>gs :Gstatus<CR>
map <Leader>gd :Git! diff<CR>
map <Leader>gc :Gcommit<CR>
map <Leader>gl :Extradite<CR>

map <leader>cd :cd %:p:h<CR>


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
