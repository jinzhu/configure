" File:		MultipleSearch.vim (global plugin)
" Last Changed: 2003 Oct 14
" Maintainer:	Dan Sharp <dwsharp at hotmail dot com>
" Version:	1.2.1
" License:      Vim License

if exists('loaded_multiplesearch')
    finish
endif
let loaded_multiplesearch = 1

"-----------------------------------------------------------------------------
" MultipleSearch allows you to have the results of multiple searches displayed
" on the screen at the same time.  Each search highlights its results in a
" different color, and all searches are displayed at once.  After the maximum
" number of colors is used, the script starts over with the first color.
" 
" The command syntax is: 
" :Search <pattern1> 
" which will highlight all occurrences of <pattern1> in the current buffer.  A
" subsequent :Search <pattern2> will highlight all occurrences of <pattern2>
" in the current buffer, retaining the highlighting of <pattern1> as well.
" <pattern1> and <pattern2> are any search pattern like you would use in a
" normal /<pattern> search.
"
" The :Search command honors Vim's 'ignorecase' and 'smartcase' settings for
" its own search.  You can use the \c and \C flags in the search pattern to
" force case matching no matter the setting of 'ignorecase' and 'smartcase'.
"
" The :SearchBuffers command works just like :Search, but the search occurs in
" all currently listed buffers (i.e., appear in the output of :ls).  The match
" in all buffers will have the same color.  This is different than 
" :bufdo Search <pattern> because in that case, each buffer will highlight the
" match in a different color.  Thanks to Jeff Mei for the suggestion and
" testing!
" 
" To clear the highlighting, issue the command
" :SearchReset (for the current buffer) or :SearchBuffersReset (for all
" buffers).
" 
" You can specify the maximum number of different colors to use by setting the
" g:MultipleSearchMaxColors variable in your .vimrc.  The default setting is
" four, but the script should handle as much as your terminal / GUI can
" display.  The g:MultipleSearchColorSequence variable lets you list the
" colors you want displayed, and in what order.  To make the text more
" readable, you can set the g:MultipleSearchTextColorSequence variable to a
" list of colors for the text, each position corresponding to the color in the
" same position in g:MultipleSearchColorSequence.

" If you change one of the preference variables, you can issue the command
" :SearchReinit
" to update the script with your new selections.

" Thanks to Peter Valach for suggestions and testing!
" ----------------------------------------------------------------------------

" This script uses continuation lines, so make sure it runs using
" Vim-default 'cpoptions'.
let s:save_cpo = &cpo
set cpo&vim

" FUNCTIONS
" --------------------------------------------------

" -----
" Strntok: Utility function to implement C-like strntok() by Michael Geddes
" and Benji Fisher at http://groups.yahoo.com/group/vimdev/message/26788
" -----
function! s:Strntok( s, tok, n)
    return matchstr( a:s.a:tok[0], '\v(\zs([^'.a:tok.']*)\ze['.a:tok.']){'.a:n.'}')
endfun

" -----
" MultipleSearchInit: Initialize the higlight groups.  This function will add
" highlighting groups if g:MultipSearchMaxColors has increased since the
" plugin was first loaded.
" -----
function! s:MultipleSearchInit()

    " Specify a maximum number of colors to use. 
    if exists('g:MultipleSearchMaxColors')
        let s:MaxColors = g:MultipleSearchMaxColors
    else
        let s:MaxColors = 4
    endif

    " Define the sequence of colors to use for searches.
    if exists('g:MultipleSearchColorSequence')
        let s:ColorSequence = g:MultipleSearchColorSequence
    else
        let s:ColorSequence = "red,yellow,blue,green,magenta,cyan,gray,brown"
    endif

    " Define the text color for searches, so that it can still be read against the
    " colored background.
    if exists('g:MultipleSearchTextColorSequence')
        let s:TextColorSequence = g:MultipleSearchTextColorSequence
    else
        let s:TextColorSequence = "white,black,white,black,white,black,black,white"
    endif

    " Start off with the first color
    let s:colorToUse = 0

    " Sanity check: make sure MaxColors is not larger than the number of
    " colors in ColorSequence or the corresponding TextColorSequence.
    let s:MaxColors = s:Min(s:MaxColors, s:ItemCount(s:ColorSequence . ','),
                \     s:ItemCount(s:TextColorSequence . ','))

    let loopCount = 0
    while loopCount < s:MaxColors
        " Define the colors to use
	let bgColor = s:Strntok(s:ColorSequence, ',', loopCount + 1)
	let fgColor = s:Strntok(s:TextColorSequence, ',', loopCount + 1)
        execute 'highlight MultipleSearch' . loopCount
           \ . ' ctermbg=' . bgColor . ' guibg=' . bgColor
           \ . ' ctermfg=' . fgColor . ' guifg=' . fgColor
        let loopCount = loopCount + 1
    endwhile
endfunction

" -----
" ItemCount: Returns the number of items in the given string.
" -----
function! s:ItemCount(string)
    let itemCount = 0
    let newstring = a:string
    let pos = stridx(newstring, ',')
    while pos > -1
        let itemCount = itemCount + 1
        let newstring = strpart(newstring, pos + 1)
        let pos = stridx(newstring, ',')
    endwhile
    return itemCount
endfunction

" -----
" Min: Returns the minimum of the given parameters.
" -----
function! s:Min(...)
    let min = a:1
    let index = 2
    while index <= a:0
        execute "if min > a:" . index . " | let min = a:" . index . " | endif"
        let index = index + 1
    endwhile
    return min
endfunction

" -----
" GetNextSequenceNumber: Determine the next Search color to use.
" -----
function! s:GetNextSequenceNumber()
    let sequenceNumber = s:colorToUse % s:MaxColors 

    let s:colorToUse = s:colorToUse + 1
    if s:colorToUse >= s:MaxColors
        let s:colorToUse = 0
    endif

    return sequenceNumber
endfunction

" -----
" DoSearch: The main searching function that highlights all matches in the
" current buffer.
" -----
function! s:DoSearch(useSearch, forwhat)
    " Clear the previous highlighting for this color
    execute 'silent syntax clear ' . a:useSearch

    " Should it be a case-sensitive match or case-insensitive?
    if &ignorecase == 1  
        " If 'smartcase' is on and our search pattern has an upper-case
        " character, do a case sensitive match.
        if &smartcase == 1
            " match() respects 'ignorecase', so turn it off for now
            set noignorecase

            if match(a:forwhat, '\u') > -1
                syntax case match
            else
                syntax case ignore
            endif

            " Be sure to turn 'ignorecase' back on!
            set ignorecase
        else
            syntax case ignore
        endif
    else
        syntax case match
    endif

    " Highlight the new search
    execute 'syntax match ' . a:useSearch . ' "' . a:forwhat . '" containedin=ALL'
endfunction

" -----
" MultipleSearch: Highlight the given pattern in the next available color.
" -----
function! MultipleSearch(allBuffers, forwhat)
    " Determine which search color to use.
    let useSearch = "MultipleSearch" . s:GetNextSequenceNumber()

    if a:allBuffers
	" If a:allBuffers is on, we want to show the match in all currently
	" listed buffers.
	let counter = 1
	let bufCount = bufnr("$")
	let current = bufnr("%")
	let lz_save = &lazyredraw

	" Loop through all the buffers and perform the search in each one.
	while counter <= bufCount
	    if buflisted(counter)
		exec "buffer " . counter
		call s:DoSearch(useSearch, a:forwhat)
	    endif
	    let counter = counter + 1
	endwhile
	exec "buffer " . current
	let &lazyredraw = lz_save
    else
	" Otherwise, just search in the current buffer.
	call s:DoSearch(useSearch, a:forwhat)
    endif

endfunction

" ---
" DoReset: Clear the highlighting
" ---
function! s:DoReset()
    let seq = 0
    while seq < s:MaxColors
	execute 'syntax clear MultipleSearch' . seq
	let seq = seq + 1
    endwhile
endfunction

" -----
" MultipleSearchReset: Clear all the current search selections.
" -----
function! s:MultipleSearchReset(allBuffers)
    let s:colorToUse = 0
    if a:allBuffers
	" If a:allBuffers is on, we want to clear the match in all
	" currently listed buffers.
	let current = bufnr("%")
	bufdo call s:DoReset()
	execute "buffer " . current
    else
	" Otherwise, just clear the current buffer.
	call s:DoReset()
    endif
endfunction

" Initialize the script the first time through.
call <SID>MultipleSearchInit()

let &cpo = s:save_cpo

" COMMANDS
" ------------------------------------------------

" Clear the current search selections and start over with the first color in
" the sequence.
if !(exists(":SearchReset") == 2)
    command -nargs=0 SearchReset :silent call <SID>MultipleSearchReset(0) 
endif

" Clear the current search selections and start over with the first color in
" the sequence.
if !(exists(":SearchBuffersReset") == 2)
    command -nargs=0 SearchBuffersReset :silent call <SID>MultipleSearchReset(1) 
endif

" Reinitialize the script after changing one of the global preferences.
if !(exists(":SearchReinit") == 2)
    command -nargs=0 SearchReinit :silent call <SID>MultipleSearchInit() 
endif

" Only autoload the Search commands, since we shouldn't need to use
" :SearchReset and :SearchReinit until after the first :Search.
if exists('g:autoload') | finish | endif " used by the autoload generator

if !(exists(":SearchBuffers") == 2)
    command -nargs=* SearchBuffers :silent call MultipleSearch(1, <q-args>)
endif

if !(exists(":Search") == 2)
    command -nargs=* Search :silent call MultipleSearch(0, <q-args>)
endif
