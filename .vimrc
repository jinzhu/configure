" Vundle
set rtp+=~/.vim/bundle/vundle/
set rtp+=$GOROOT/misc/vim
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'L9'

" Ruby & Rails
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-rake'
" ~/.vim/macros/rails.vim

Bundle 'tpope/vim-ragtag'
" <C-X>= <C-X>+ <C-X>- <C-X>_ <C-X>' <C-X>" <C-X><Space> <C-X><CR> <C-X>/ <C-X>! <C-X>@ <C-X># <C-X>$
Bundle 'tpope/vim-eunuch'
" :Unlink :Remove :Rename :SudoWrite :W
Bundle 'tpope/vim-unimpaired'
" ]o, [o, ]n, [n
" ]e, [e               Exchange the current line with lines above/below it
" ]<Space>, [<Spance>  Add [count] blank lines above/below the cursor.
" `[x` XML encode, `]x` XML decode, `[u` URL encode, `]u` URL decode, `[y` C String encode, `]y` C String decode

Bundle 'tpope/vim-surround'
" cs'<q>, cst", dst, ys2w), yss), v<move>S)

Bundle 'tpope/vim-abolish'
" :Abolish {despa,sepe}rat{e,es,ed,ing,ely,ion,ions,or}  {despe,sepa}rat{}
" :%Subvert/facilit{y,ies}/building{,s}/g

" GIT
Bundle 'tpope/vim-fugitive'
" ~/.vim/bundle/vim-fugitive/doc/fugitive.txt

Bundle 'Command-T'
" :CommandT, :CommandTBuffer

Bundle 'Gist.vim'

Bundle 'w3cvalidator'

" Snipmate
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "honza/snipmate-snippets"
Bundle "garbas/vim-snipmate"

Bundle 'pix/vim-align'
Bundle 'tComment'
Bundle 'mileszs/ack.vim'
" :Ack
" o (open) go (preview open) t (tab) T (new tab silently) v (vertical) gv q (close)

Bundle 'AutoComplPop'
Bundle 'jsbeautify'
Bundle 'MultipleSearch'
Bundle 'reorder-tabs'
" <M-PgUp> / <M-PgDn>
Bundle 'sessionman.vim'
" :SessionClose :SessionList :SessionOpen :SessionOpenLast :SessionSave :SessionSaveAs :SessionShowLast
Bundle 'scrooloose/nerdtree'
Bundle 'Glob-Edit'
" edit plugin/*vim
Bundle 'tsaleh/vim-matchit'
Bundle 'pangloss/vim-simplefold'
Bundle 'taglist.vim'
Bundle 'sketch.vim'
Bundle 'hallettj/jslint.vim'
Bundle 'md5.vim'
Bundle 'mru.vim'
Bundle 'qiushibaike'

" Maintains a history of yanks
Bundle 'YankRing.vim'

" LaTex
Bundle 'imaps.vim'
Bundle 'gerw/vim-latex-suite'

" save/restore window position
Bundle 'cecutil'

" write HTML code faster
Bundle 'rstacruz/sparkup'

Bundle 'Raimondi/delimitMate'

" ColorScheme
Bundle 'tpope/vim-vividchalk'

" Syntax Support
Bundle 'juvenn/mustache.vim'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-liquid'
Bundle 'vim-coffee-script'
Bundle 'kogent/vim-nagios'
Bundle 'pangloss/vim-javascript'
Bundle 'cespare/mxml.vim'
Bundle 'nono/jquery.vim'
Bundle 'actionscript.vim'
Bundle 'sunaku/vim-ruby-minitest'
" erlang
Bundle 'oscarh/vimerl'

Bundle 'LargeFile'

Bundle 'VIM-Color-Picker'
" :ColorPicker

Bundle 'Lokaltog/vim-easymotion'
" Vim motions on speed
Bundle 'spiiph/vim-space'

Bundle 'nathanaelkane/vim-indent-guides'
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1

Bundle 'Lokaltog/vim-powerlin0ie'
Bundle 'Shougo/neocomplcache'
let g:neocomplcache_enable_at_startup = 1

" AutoCmd
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set completefunc=syntaxcomplete#Complete
autocmd FileType c          set omnifunc=ccomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html       set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css        set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml        set omnifunc=xmlcomplete#CompleteTags
" autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete

augroup filetypedetect
  au! BufNewFile,BufRead  *.js      setf javascript.jquery
  au! BufNewFile,BufRead  *.ch      setf cheat
  au! BufNewFile,BufRead  *.yac     setf cheat
  au! BufNewFile,BufRead  *.liquid  setf liquid
  au! BufNewFile,BufRead  *.haml    setf haml
  au! BufNewFile,BufRead  *.yml     setf eruby
  au! BufNewFile,BufRead  *.mxml    setf mxml
  au! BufNewFile,BufRead  *.as      setf actionscript
  au! BufNewFile,BufRead  *.feature setf cucumber
  au! BufNewFile,BufRead  *.table   setf ruby.testingmachine
  au! BufNewFile,BufRead  .autotest setf ruby
  au! BufNewFile,BufRead  *.mobile.erb setf eruby.html
  au! BufRead,BufNewFile  *.go      setf go
augroup END

if has("autocmd")
  " Enable filetype detection
  filetype plugin indent on

  " Restore cursor position
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
endif

" autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
" autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
" autocmd FileType ruby,eruby let g:rubycomplete_rails = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set lz                     " Don't redraw screen during macros
set tf                     " Improves redrawing for newer computers
set sc                     " Show incomplete command at bottom right
set tm=500                 " Lower timeout for mappings
set cot=menu               " Don't show extra info on completions
"set autochdir
"if &diff | syn off | endif " Turn syntax highlighting off for diff
let bufpane_showhelp = 0

compiler rubyunit

" Load matchit (% to bounce from do to end, etc.)
runtime! macros/matchit.vim
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

" GUI - color：guifg, guibg, gui
" Terminal：ctermfg, ctermbg

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Encoding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set fileencoding=utf-8
set fileencodings=utf-8,gb18030,ucs-bom,gbk,gb2312,cp936
set encoding=utf8
set guifont=simhei\ 20

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set helplang=cn

"some stuff to get the mouse going in term
set mouse=a
set ttymouse=xterm2

"turn on syntax highlighting
syntax on

"load ftplugins and indent files
filetype plugin on
filetype indent on

set backup              " make backup file
set backupdir=/tmp      " where to put backup file
set directory=/tmp      " directory is the directory for temp file
set autoread            " auto read when a file is changed from the outside
set noshowmatch         " show matching bracets
set formatoptions=tcrqn " 自动格式化
set magic               " Set magic on
set smartcase
set nocompatible

"indent settings
set ai ts=2 sw=2  "autoindent shiftwidth softtabstop
set smarttab
set expandtab
set smartindent

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

"tell the term has 128 colors
set t_Co=128

set incsearch   "find the next match as we type the search
set hlsearch    "hilight searches by default

" FuzzyFinderTextMate
let g:fuzzy_ignore = "*.log"
let g:fuzzy_matching_limit = 70
let g:fuzzy_ceiling = 100000

" set wrap      "dont wrap lines
" set linebreak   "wrap lines at convenient points
set noerrorbells
set novisualbell
set vb t_vb=

"folding settings
set foldmethod=indent   "fold based on indent
set foldnestmax=3       "deepest fold is 3 levels
set nofoldenable        "dont fold by default

" store lots of :cmdline history
set history=1000

" display tabs and trailing spaces
" set list
" set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CMD status
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set wildmode=list:longest   "make cmdline tab completion similar to bash
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing

set showcmd     "show incomplete cmds down the bottom
set showmode    "show current mode down the bottom

"Add the variable with the name a:varName to the statusline. Highlight it as
"'error' unless its value is in a:goodValues (a comma separated string)
function! AddStatuslineFlag(varName, goodValues)
  set statusline+=%#error#
  exec "set statusline+=%{RenderStlFlag(".a:varName.",'".a:goodValues."',1)}"
  set statusline+=%*
  exec "set statusline+=%{RenderStlFlag(".a:varName.",'".a:goodValues."',0)}"
endfunction

"returns a:value or ''
"
"a:goodValues is a comma separated string of values that shouldn't be
"highlighted with the error group
"
"a:error indicates whether the string that is returned will be highlighted as
"'error'
"
function! RenderStlFlag(value, goodValues, error)
  let goodValues = split(a:goodValues, ',', 1)
  let good = index(goodValues, a:value) != -1
  if (a:error && !good) || (!a:error && good)
    return '[' . a:value . ']'
  else
    return ''
  endif
endfunction

"statusline setup
set statusline=%t       "tail of the filename
call AddStatuslineFlag('&ff', 'unix,')    "fileformat
call AddStatuslineFlag('&fenc', 'utf-8,') "file encoding
set statusline+=%h      "help file flag
set statusline+=%y      "filetype
set statusline+=%r      "read only flag
set statusline+=%m      "modified flag

function! SlSpace()
  if exists("*GetSpaceMovement")
    return "[" . GetSpaceMovement() . "]"
  else
    return ""
  endif
endfunc
set statusline+=%{SlSpace()}

"display a warning if &et is wrong, or we have mixed-indenting
set statusline+=%#error#
set statusline+=%{StatuslineTabWarning()}
set statusline+=%*

set statusline+=%{StatuslineTrailingSpaceWarning()}

"display a warning if &paste is set
set statusline+=%#error#
set statusline+=%{&paste?'[paste]':''}
set statusline+=%*

set statusline+=%=      "left/right separator
set statusline+=%{StatuslineCurrentHighlight()}\ \ "current highlight
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file
set laststatus=2

"recalculate the trailing whitespace warning when idle, and after saving
autocmd cursorhold,bufwritepost * unlet! b:statusline_trailing_space_warning

"return '[\s]' if trailing white space is detected
"return '' otherwise
function! StatuslineTrailingSpaceWarning()
    if !exists("b:statusline_trailing_space_warning")
        if search('\s\+$', 'nw') != 0
            let b:statusline_trailing_space_warning = '[\s]'
        else
            let b:statusline_trailing_space_warning = ''
        endif
    endif
    return b:statusline_trailing_space_warning
endfunction

"return the syntax highlight group under the cursor ''
function! StatuslineCurrentHighlight()
    let name = synIDattr(synID(line('.'),col('.'),1),'name')
    if name == ''
        return ''
    else
        return '[' . name . ']'
    endif
endfunction

"recalculate the tab warning flag when idle and after writing
autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning

"return '[&et]' if &et is set wrong
"return '[mixed-indenting]' if spaces and tabs are used to indent
"return an empty string if everything is fine
function! StatuslineTabWarning()
    if !exists("b:statusline_tab_warning")
        let tabs = search('^\t', 'nw') != 0
        let spaces = search('^ ', 'nw') != 0

        if tabs && spaces
            let b:statusline_tab_warning =  '[mixed-indenting]'
        elseif (spaces && !&et) || (tabs && &et)
            let b:statusline_tab_warning = '[&et]'
        else
            let b:statusline_tab_warning = ''
        endif
    endif
    return b:statusline_tab_warning
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIMIM
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vimim_enable_wildcard_search=1
let g:vimim_menu_color=1
let g:vimim_enable_menu_extra_text=1
let g:vimim_enable_menu_ctrl_jk=1
let g:vimim_disable_chinese_punctuation=1
let g:vimim_enable_english_to_chinese=1
let g:vimim_disable_search=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LaTex
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set winaltkeys=no         "shielded ALT
set grepprg=grep\ -nH\ $*
let g:tex_flavor = "latex"
set iskeyword+=:

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ctags
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let Tlist_Sort_Type            = "name"
let Tlist_Use_Right_Window     = 1
let Tlist_Compart_Format       = 1
let Tlist_Exist_OnlyWindow     = 1
let Tlist_File_Fold_Auto_Close = 0
let Tlist_Enable_Fold_Column   = 0
let Tlist_Close_On_Select      = 1
let Tlist_Show_Menu            = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HTML
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:do_xhtml_mappings = 'yes'
let g:force_html_menu   = 'yes'
let g:html_tag_case     = 'lowercase'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CommandT
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:CommandTScanDotDirectories = 1
set wildignore+=*.o,*.obj,*.a,*.lib,*.so,.git,.svn,.hg,CVS,*png,*gif,*jpg,vendor/qor,public/system

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ZenCoding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:user_zen_settings = {
      \  'indentation' : '  ',
      \  'perl' : {
      \    'aliases' : {
      \      'req' : 'require '
      \    },
      \    'snippets' : {
      \      'use' : "use strict\nuse warnings\n\n",
      \      'warn' : "warn \"|\";",
      \    }
      \  }
      \}
let g:user_zen_expandabbr_key = '<c-e>'
let g:use_zen_complete_tag = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" My Leader HotKeys
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nn <F2> :tabedit <CR>
nn <F3> :shell <CR>
nn <F4> :set nu! <CR>
autocmd BufRead,BufNewFile *.rb map <F5>      :% w !ruby<CR>
imap <F6> <Esc>:ColorPicker<Cr>a
vmap <F6> <Del><Esc>h:ColorPicker<Cr>a
nn <F7> :IndentGuidesToggle <CR>
nn <F8> :TlistToggle<CR>
nnoremap <silent> <F11> :YRShow<CR>
nmap <F12> <Plug>ToggleAutoCloseMappings

let mapleader = ";"
map <Leader>t :CommandT<CR>
map <Leader>b :CommandTBuffer<CR>
map \c :%s/\s\+$//<CR>
map <Leader>a :Ack 
map <Leader>p :YRShow<CR>

map <Leader>s :SessionList<CR>
map <Leader>e :tabedit ~/.vimrc<CR>
autocmd! bufwritepost .vimrc source ~/.vimrc

map <Leader>r :MRU<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gd :Git! diff<CR>
map <Leader>gc :Gcommit<CR>

map <leader>cd :cd %:p:h<CR>


""" My `;` HotKeys
" Windows (;c, ;o)
nnoremap <Leader> <C-w>
imap <Leader>w <ESC>:w<CR>
nn   <Leader>w <ESC>:w<CR>


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
