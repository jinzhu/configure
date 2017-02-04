set nocompatible

let mapleader = ";"
let maplocalleader = "\\"

if filereadable(expand("~/.vim/vundlerc"))
  source ~/.vim/vundlerc
endif

" Color Theme
let g:airline_theme='papercolor'
set background=dark
colorscheme PaperColor
set foldcolumn=1

" ================ General Config ====================
" disable error sounds and screen flashing
set vb t_vb=
set history=1000                " Store lots of :cmdline history
set lazyredraw                  " Don't redraw screen during macros
set tf                          " Improves redrawing for newer computers
set sc                          " Show incomplete command at bottom right
set timeoutlen=500              " Lower timeout for mappings
set cot=menu                    " Don't show extra info on completions
set mousemodel=popup
set spelllang=en_us
set complete+=kspell
" set spell
" z= correct, 1z= take the first suggestion
" zg add good word, zw add bad word
" ]s next err, ]S next bad error, [s previous error, [S previous bad error
" nmap <F12> :set spell! spelllang=en_us<CR>

" set dictionary+=/usr/share/dict/words
" set isk+=-

" Search home directory path on cd. But can't complete.
set cdpath+=~
set cdpath+=~/Projects

set shiftround                 " round indent to multiple of 'shiftwidth'
set tags=tags,./tags,tmp/tags,doc/tags
map <LocalLeader>gt :!ctags --extra=+f -R<CR><CR>

let g:is_posix = 1             " vim's default is archaic bourne shell, bring it up to the 90s

set modeline
set modelines=5                " default numbers of lines to read for modeline instructions

" set autochdir
let bufpane_showhelp = 0
" set number                      " Line numbers are good
set backspace=indent,eol,start  " Allow backspace in insert mode
set showcmd                     " Show incomplete cmds down the bottom
set showmode                    " Show current mode down the bottom
set gcr=a:blinkon0              " Disable cursor blink
set noerrorbells                " No noise
set novisualbell                " No blinking
set visualbell t_vb=            " disable any beeps or flashes on error
set shortmess=atI

set laststatus=2   " Always show the status line

"some stuff to get the mouse going in term
set selectmode+=mouse
set mouse=a
set mousehide                 " Hide mouse after chars typed

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

set viminfo='100,f1            " Save up to 100 marks, enable capital marks

" ================ Turn Off Swap Files ==============

set backup              " make backup file
set backupdir=~/.cache/vim,/tmp  " where to put backup file
set directory=~/.cache/vim,/tmp   " directory is the directory for temp file
set noswapfile
set nowb

autocmd filetype crontab setlocal nobackup nowritebackup

set autowrite                   " Writes on make/shell commands
set autoread                    " Reload files changed outside vim

" no ex mode
nnoremap Q <nop>

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

set wrap       " Don't wrap lines
set linebreak    " Wrap lines at convenient points

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

set wildmenu                " enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ " stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*vendor/rails/**
set wildignore+=*vendor/cache/**
set wildignore+=*.gem
set wildignore+=*log/**
set wildignore+=*tmp/**
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set wildignore+=*.png,*.jpg,*.gif
set wildignore+=*.a,*.lib,*.so,CVS,vendor/qor,public/system
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*  " Linux/MacOSX

" set list                      " display unprintable characters f12 - switches
" set listchars=tab:\ ·,eol:¬
" set listchars+=trail:·
" set listchars+=extends:»,precedes:«
" map <silent> <F12> :set invlist<CR>

" ================ Scrolling ========================

set scrolloff=0         " Start scrolling when we're 8 lines away from margins
set sidescrolloff=18
set sidescroll=1

for f in split(glob('~/.vim/plugin/settings/*.vim'), '\n')
  exe 'source' f
endfor


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Color
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set fileencoding=utf-8
set fileencodings=utf-8,gb18030,ucs-bom,gbk,gb2312,cp936
set encoding=utf8
set guifont=Monaco\ 16
highlight clear SignColumn
" GUI - color：guifg, guibg, gui
" Terminal：ctermfg, ctermbg

runtime macros/matchit.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" My Hot Keys
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" map <F2> :tabedit<CR>
" map <F3> :lcd %:p:h<CR>:shell<CR>
" map <F4> :set nu!<CR>
" autocmd BufRead,BufNewFile *.rb map <F5>      :% w !ruby<CR>
" imap <F6> <Esc>:ColorPicker<Cr>a
" vmap <F6> <Del><Esc>h:ColorPicker<Cr>a
" map <F7> :IndentLinesToggle <CR>
" set pastetoggle=<F10>          " toggle between paste and normal: for 'safer' pasting from keyboard
" nmap <F11> <Plug>ToggleAutoCloseMappings

cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <Leader>ew :e %%
map <Leader>es :sp %%
map <Leader>ev :vsp %%
map <Leader>et :tabe %%
map <Leader>ec :tabedit ~/.vimrc<CR>

" copy filename with line
noremap <silent> <leader>yl :let @+=expand("%:p:l").":".line(".")<CR>:echo @+<CR>
" copy full filename
noremap <silent> <Leader>yf :let @+=expand('%:p')<CR>:echo @+<CR>
" copy short filename
noremap <silent> <Leader>yF :let @+=expand('%')<CR>:echo @+<CR>
" copy file directory
noremap <silent> <Leader>yd :let @+=expand('%:p:h')<CR>:echo @+<CR>
" copy yanked text to clipboard
noremap <silent> <Leader>yy :let @+=@"<CR>:echo @+<CR>

map <LocalLeader>cd :lcd %:p:h<CR>:pwd<CR>

""" Save file
nnoremap <Leader>o <C-w>o
nnoremap <Leader>c <C-w>c
imap <Leader>w <ESC>:update<CR>
map  <Leader>w <ESC>:update<CR>


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
nnoremap <Leader>l :nohls<CR><C-L>
inoremap <Leader>l <C-O>:nohls<CR>

cnoremap <C-A>    <Home>
cnoremap <C-E>    <End>

nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

""" Tab
nn <M-PageUp> :call MoveCurrentTab(-1)<CR>
nn <M-PageDown> :call MoveCurrentTab(1)<CR>

nn <Leader>1 1gt
nn <Leader>2 2gt
nn <Leader>3 3gt
nn <Leader>4 4gt
nn <Leader>5 5gt
nn <Leader>6 6gt
nn <Leader>7 7gt
nn <Leader>8 8gt
nn <Leader>9 9gt
nn <Leader>0 :tablast<CR>

ino <Leader>1 <C-o>1gt
ino <Leader>2 <C-o>2gt
ino <Leader>3 <C-o>3gt
ino <Leader>4 <C-o>4gt
ino <Leader>5 <C-o>5gt
ino <Leader>6 <C-o>6gt
ino <Leader>7 <C-o>7gt
ino <Leader>8 <C-o>8gt
ino <Leader>9 <C-o>9gt
ino <Leader>0 <C-o>:tablast<CR>


" Git commits.
autocmd FileType gitcommit setlocal spell
" Subversion commits.
autocmd FileType svn       setlocal spell
" Mercurial commits.
autocmd FileType asciidoc  setlocal spell


function! FollowSymlink()
  let b:orig_file = fnameescape(expand('%:p'))
  if getftype(b:orig_file) == 'link'
    execute 'lcd' fnamemodify(resolve(b:orig_file), ':p:h')
    execute 'file' fnameescape(resolve(b:orig_file))
    :edit!
  endif
  redraw!
endfunction
command! FollowSymlink :call FollowSymlink()
nnoremap <Leader>F :FollowSymlink<CR>


function! OpenURL(url)
  let b:escape_url = escape(a:url, '"')
  if executable("chromium")
    exe "silent !chromium \"".b:escape_url."\""
  elseif executable("gnome-open")
    exe "silent !gnome-open \"".b:escape_url."\""
  endif
  redraw!
endfunction
command! -nargs=1 OpenURL :call OpenURL(<q-args>)
" open URL under cursor in browser
nnoremap <Leader>G :OpenURL http://www.google.com/search?q=<cword><CR>
vnoremap <Leader>G "zy:OpenURL http://www.google.com/search?q=<C-R>z<CR>
nnoremap <Leader>D :OpenURL http://dict.youdao.com/search?q=<cword><CR>
vnoremap <Leader>D "zy:OpenURL http://dict.youdao.com/search?q=<C-R>z<CR>

augroup filetypedetect
  au BufNewFile,BufRead  *.yml set filetype=yml.eruby
  au BufNewFile,BufRead  *.erb set filetype=eruby.html
augroup END

hi Visual ctermbg=240
hi CursorLine ctermbg=240 cterm=none

" :verbose map -> list all your maps
" :verbose abbr -> list all your maps
" :earlier 2m -> go to 2 minutes ago
" :.! date -> insert shell command result
"
" q: -> open your command history (<C-f> after :)
" q/ -> open your search history
" @: -> run last command -> @@ rerun
"
" gv -> reselect last visual selection
" g; / g, -> to move (forward, backward) through the changelist.
" gi  -> Go to position where Insert mode was stopped last
" '. -> will go to the last edited line, And `. will go to the last edited
"
" :g/match/y A -> will yank (copy) all lines containing "match" into the "a/@a register. (The capitalization as A makes vim append yankings instead of replacing the previous register contents.)
" :g!/match/normal dd -> delete all lines that don't containing "match" (same as :v/match/normal dd)
