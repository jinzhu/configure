""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" AutoCmd
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType c          set omnifunc=ccomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html       set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css        set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml        set omnifunc=xmlcomplete#CompleteTags
" autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete

augroup filetypedetect
  au  BufNewFile,BufRead  *.js     setf javascript.jquery
  au! BufNewFile,BufRead  *.ch     setf cheat
  au! BufNewFile,BufRead  *.yac    setf cheat
  au  BufNewFile,BufRead  *.liquid setf liquid
  au! BufNewFile,BufRead  *.haml   setf haml
  au  BufNewFile,BufRead  *.yml    setf eruby
  au  BufNewFile,BufRead  *.mxml   setf mxml
  au  BufNewFile,BufRead  *.as     setf actionscript
  au! BufNewFile,BufRead  *.feature set filetype=cucumber
  au! BufNewFile,BufRead  *.table set filetype=ruby.testingmachine
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

autocmd BufNewFile,BufRead *_spec.rb source ~/.vim/ftplugin/rails/rspec.vim
autocmd BufNewFile,BufRead *_test.rb source ~/.vim/ftplugin/rails/shoulda.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set lz                     " Don't redraw screen during macros
set tf                     " Improves redrawing for newer computers
set sc                     " Show incomplete command at bottom right
set tm=500                 " Lower timeout for mappings
set cot=menu               " Don't show extra info on completions
if &diff | syn off | endif " Turn syntax highlighting off for diff
let bufpane_showhelp = 0

compiler rubyunit

augroup VCSCommand
  au User VCSBufferCreated silent! nmap <unique> <buffer> q :bwipeout<cr>
  au User VCSBufferCreated setf vcscommit
augroup END

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


"set nowrap      "dont wrap lines
"set linebreak   "wrap lines at convenient points
set noerrorbells
set novisualbell

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
" Git.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:git_command_edit = 'vnew'

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
" FuzzyFinder.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:fuf_modesDisable = []
let g:fuf_abbrevMap = {
      \   '^vr:' : map(filter(split(&runtimepath, ','), 'v:val !~ "after$"'), 'v:val . ''/**/'''),
      \   '^m0:' : [ '/mnt/d/0/', '/mnt/j/0/' ],
      \ }
let g:fuf_mrufile_maxItem = 300
let g:fuf_mrucmd_maxItem = 400
" nnoremap <silent> <C-n>      :FufBuffer<CR>
" nnoremap <silent> <C-p>      :FufFileWithCurrentBufferDir<CR>
" nnoremap <silent> <C-f><C-p> :FufFileWithFullCwd<CR>
" nnoremap <silent> <C-f>p     :FufFile<CR>
" nnoremap <silent> <C-f><C-d> :FufDirWithCurrentBufferDir<CR>
" nnoremap <silent> <C-f>d     :FufDirWithFullCwd<CR>
" nnoremap <silent> <C-f>D     :FufDir<CR>
" nnoremap <silent> <C-j>      :FufMruFile<CR>
" nnoremap <silent> <C-k>      :FufMruCmd<CR>
" nnoremap <silent> <C-b>      :FufBookmark<CR>
" nnoremap <silent> <C-f><C-t> :FufTag<CR>
" nnoremap <silent> <C-f>t     :FufTag!<CR>
" noremap  <silent> g]         :FufTagWithCursorWord!<CR>
" nnoremap <silent> <C-f><C-f> :FufTaggedFile<CR>
" nnoremap <silent> <C-f><C-j> :FufJumpList<CR>
" nnoremap <silent> <C-f><C-g> :FufChangeList<CR>
" nnoremap <silent> <C-f><C-q> :FufQuickfix<CR>
" nnoremap <silent> <C-f><C-l> :FufLine<CR>
" nnoremap <silent> <C-f><C-h> :FufHelp<CR>
" nnoremap <silent> <C-f><C-b> :FufAddBookmark<CR>
" vnoremap <silent> <C-f><C-b> :FufAddBookmarkAsSelectedText<CR>
" nnoremap <silent> <C-f><C-e> :FufEditInfo<CR>
" nnoremap <silent> <C-f><C-r> :FufRenewCache<CR>

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
" MiniBufexpl.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MAP
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Paste Yanked Text
nmap gp "0p
nmap gP "+p
vmap gP "+p
vmap gy "+y

" .vimrc
map <leader>s :source ~/.vimrc<CR>
map <leader>e :tabedit ~/.vimrc<CR>
autocmd! bufwritepost .vimrc source ~/.vimrc
map <leader>c :%s/\s\+$//<CR>

nmap <Leader>fd :cf /tmp/autotest.txt<cr> :compiler rubyunit<cr>

" STARDICT  <install sdcv>
nmap <C-\> :!sdcv -u 朗道英汉字典5.0 -u 牛津简明英汉袖珍辞 -u 五笔86 -n <C-R>=expand("<cword>")<CR><CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

nn <F2> :tabnew<CR>
nn <F3> :%s/\s*$//g<cr>:nohlsearch<cr>''
nn <F4> :set nu! <CR>
autocmd BufRead,BufNewFile *.rb map <F5>      :% w !ruby<CR>
nn <F6> <Esc>:set suffixesadd=.html.erb<CR>gf
nn <F7> <Esc>:set suffixesadd=.rb<CR>gf
nn <F8> :TlistToggle<CR>
nn <F9> :shell <CR>
nn <F10> :AutoComplPopEnable<CR>
nn <F12> :silent !lss &<CR><C-l>
imap <F12> <ESC>:silent !lss &<CR><C-l>
":nn <F10> :call ToggleSketch()<CR>  "没事，鼠标画线玩的

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd BufRead,BufNewFile *.rb map <leader>r :!shoes %:p<CR><C-l>
" <leader n>
map <leader>1 :set ft=ruby<cr>
map <leader>2 :set ft=xhtml<cr>
map <leader>3 :set ft=javascript<cr>
map <leader>4 :set ft=css<cr>
map <leader>5 :set ft=vim<cr>
map <leader>6 :set ft=sh<cr>

" <M-hjkl>
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

" Windows
nnoremap ; <C-w>
" Save
imap ;w <ESC>:w<CR>
nn   ;w <ESC>:w<CR>

noreabbrev te tabedit
map <leader>cd :cd %:p:h<CR>
cnoremap <C-A>    <Home>
cnoremap <C-E>    <End>

let g:browser = 'firefox -new-tab '
" Open the Ruby ApiDock page for the word under cursor, in a new Firefox tab
function! OpenDoc(lang,keyword)
  let url = 'http://apidock.com/'.a:lang.'/'.a:keyword
  exec '!'.g:browser.' '.url.' &'
endfunction
noremap <leader>rb :call OpenDoc('ruby',expand('<cword>'))<CR>
" noremap <leader>rr :call OpenDoc('rails',expand('<cword>'))<CR>


nn <Space>e  :edit 
cmap <C-t> <Esc>:tabedit 
cmap <C-s> <Esc>:sview 
cmap <C-v> <Esc>:vnew 


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
