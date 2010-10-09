"=============================================================================
" What Is This: Calendar
" File: calendar.vim
" Author: Yasuhiro Matsumoto <mattn.jp@gmail.com>
" Last Change: 06-May-2010.
" Version: 2.2

let g:calendar_version = "2.2"
if &compatible
  finish
endif

if !exists("g:calendar_action")
  let g:calendar_action = "<SID>CalendarDiary"
endif
if !exists("g:calendar_sign")
  let g:calendar_sign = "<SID>CalendarSign"
endif
if !exists("g:calendar_mark")
 \|| (g:calendar_mark != 'left'
 \&& g:calendar_mark != 'left-fit'
 \&& g:calendar_mark != 'right')
  let g:calendar_mark = 'left'
endif
if !exists("g:calendar_navi")
 \|| (g:calendar_navi != 'top'
 \&& g:calendar_navi != 'bottom'
 \&& g:calendar_navi != 'both'
 \&& g:calendar_navi != '')
  let g:calendar_navi = 'top'
endif
if !exists("g:calendar_navi_label")
  let g:calendar_navi_label = "Prev,Today,Next"
endif
if !exists("g:calendar_focus_today")
  let g:calendar_focus_today = 0
endif
if !exists("g:calendar_datetime")
 \|| (g:calendar_datetime != ''
 \&& g:calendar_datetime != 'title'
 \&& g:calendar_datetime != 'statusline')
  let g:calendar_datetime = 'title'
endif

if !exists("g:calendar_current_idx")
    let g:calendar_current_idx = 0
endif
if !exists("g:calendar_list") && !exists("g:calendar_diary")
    let g:calendar_diary = '~/diary'
    let g:calendar_list = [{'name': 'Diary', 'path': g:calendar_diary, 'ext': 'cal'}]
elseif !exists("g:calendar_diary")
    let g:calendar_diary = g:calendar_list[g:calendar_current_idx].path
elseif !exists("g:calendar_list")
    let g:calendar_list = [{'name': 'Diary', 'path': g:calendar_diary, 'ext': 'cal'}]
endif

"*****************************************************************
"* Calendar commands
"*****************************************************************
command! -nargs=* Calendar  call Calendar(0,<f-args>)
command! -nargs=* CalendarH call Calendar(1,<f-args>)
command! -nargs=0 Cal Calendar
exe "command! -nargs=0 CalendarDiarys NERDTree " . g:calendar_diary
exe "command! -nargs=* CalendarSearch vimgrep /<args>/".escape(g:calendar_diary," ")."**/*.".g:calendar_list[g:calendar_current_idx]["ext"]."|syntax on|cw"
"command! -nargs=* CalendarSearch call CalendarSearch(<f-args>)
"function! CalendarSearch(...)
    "exe "vimgrep /" . a:1 . "/" . escape(g:calendar_diary, " ") . "**/*.".g:calendar_list[g:calendar_current_idx]["ext"]
    "syntax off
"endfunction
autocmd filetype calendar nmap <buffer> <C-j> :call CalendarDiaryGoto("next")<cr>
autocmd filetype calendar nmap <buffer> <C-k> :call CalendarDiaryGoto("prev")<cr>

function! s:CalendarDiaryGetDateByFileName()
    let filepath = expand("%")
    let matches = split(filepath, "[/\]")
    let year = str2nr(matches[len(matches)-3])
    let month = str2nr(matches[len(matches)-2])
    let day = matches[len(matches)-1]
    let day = str2nr(strpart(day, 0, strlen(day)-4))
    return [year, month, day]
endfunction
function! s:CalendarIsLeap(year)
	return !empty(a:year) && (a:year%400==0 ||(a:year%4==0&&a:year%100!=0))
endfunction

" FIXME: this function just fix one next-day/prev-day.
function! s:CalendarFixDate(year, month, day)
    let year = a:year
    let month = a:month
    let day = a:day
    let DAYS = [0,31,28,31,30,31,30,31,31,30,31,30,31]
    if s:CalendarIsLeap(year)
        let DAYS[2] = 29
    endif
    if day<1
        let month = month - 1
        let day = DAYS[month]
    elseif day>DAYS[month]
        let month = month+1
        let day = 1
    endif
    if month<1
        let year = year-1
        let month = 12
    elseif month>12
        let year = year+1
        let month = 1
    endif
    return [year, month, day]
endfunction
function! CalendarDiaryGoto(...)
    let diary_path = finddir(g:calendar_diary)
    if exists('+shellslash') && &shellslash
        let diary_path = substitute(diary_path, "\\", "/", "g")
    endif

    if a:1=="next"
        if stridx(expand("%"), diary_path)!=0
            echo ""
            return
        endif
        let date = s:CalendarDiaryGetDateByFileName()
        let date = s:CalendarFixDate(date[0], date[1], date[2]+1)
        let year = date[0]
        let month = date[1]
        let day = date[2]
    elseif a:1=="prev"
        if stridx(expand("%"), diary_path)!=0
            echo ""
            return
        endif
        let date = s:CalendarDiaryGetDateByFileName()
        let date = s:CalendarFixDate(date[0], date[1], date[2]-1)
        let year = date[0]
        let month = date[1]
        let day = date[2]
    else
        let year = a:1
        let month = a:2
        let day = a:3
    endif
    exe "edit ".g:calendar_diary."/".year."/".month."/".day.".".g:calendar_list[g:calendar_current_idx]["ext"]
endfunction

function! NumberOfWeek(year,month,day)
    " lets calc weeknumber the cruel and hard way :D
    " Find JulianDay

    let a = float2nr(floor((14-(a:month))/12))
    let y = a:year+4800-a
    let m = a:month+(12*a)-3

    " (gregorian calendar)
    let jd = a:day + float2nr(floor(((153*m)+2)/5)) + (365*y) + float2nr(floor(y/4)) -
        \ float2nr(floor(y/100)) + float2nr(floor(y/400)) - 32045

    " (julian calendar)
    " var jd = (a:day+1)+round(((153*m)+2)/5)+(365+y) + round(y/4)-32083;

    " now calc weeknumber according to JD
    let d4 = (jd+31741-(jd%7))%146097%36524%1461
    let L = float2nr(floor(d4/1460))
    let d1 = ((d4-L)%365)+L
    return float2nr(floor(d1/7)) + 1
endfunction

function! NumberOfDay(year,month,day)
    let MONTH_DAYS=[0,31,28,31,30,31,30,31,31,30,31,30,31]
    if IsLeap(a:year)
        let MONTH_DAYS[2] = 29
    endif

    let nr=0
    let idx = 1
    while idx<a:month
        let nr = nr + MONTH_DAYS[idx]
        let idx = idx+1
    endw
    return nr+a:day
endfunction

function! DayOfWeek(year,month,day)
    let a = float2nr(floor((14 - a:month)/12))
    let y = a:year - a
    let m = a:month + 12*a - 2
    let d = (a:day + y + float2nr(floor(y/4)) - float2nr(floor(y/100)) + float2nr(floor(y/400)) + float2nr(floor((31*m)/12))) % 7
    return d
endfunction

function! IsLeap(year)
	return a:year%400==0 || (a:year%4==0 && a:year%100!=0)
endfunction

function! DaysOfMonth(year,month)
    let MONTH_DAYS=[0,31,28,31,30,31,30,31,31,30,31,30,31]
    return (a:month==2 && IsLeap(a:year))?29:MONTH_DAYS[a:month]
endfunction

function! MakeMonthlyCalendar(year,month)
    let weeks = [
        \ ['   ', '   ', '   ', '   ', '   ', '   ', '   '],
        \ ['   ', '   ', '   ', '   ', '   ', '   ', '   '],
        \ ['   ', '   ', '   ', '   ', '   ', '   ', '   '],
        \ ['   ', '   ', '   ', '   ', '   ', '   ', '   ']
    \ ]
    let days = DaysOfMonth(a:year,a:month)
    let jan1weekday = DayOfWeek(a:year, a:month, 1)
    let day = 1
    let weekidx = 0
    while day<=days
        let weekday = DayOfWeek(a:year, a:month, day)
        let weekday = weekday==0 ? 7 : weekday

        if exists("g:calendar_sign") && g:calendar_sign != ""
            exe "let vsign = " . g:calendar_sign . "(day, a:month, a:year)"
            if vsign != ""
                let vsign = vsign[0]
                if vsign !~ "[+!#$%&@?]"
                    let vsign = "+"
                endif
            endif
        else
            let vsign = ''
        endif

        if vsign == ''
            let weeks[weekidx][weekday-1] = day<10 ? '  '.day : ' '.day
        else
            let weeks[weekidx][weekday-1] = day<10 ? ' '.vsign.day : vsign.day
        endif
        if weekday>=7
            let weekidx += 1

            if weekidx>=4
                call add(weeks, ['   ', '   ', '   ', '   ', '   ', '   ', '   '])
            endif
        endif
        let day += 1
    endwhile
    return weeks
endfunction

function! DisplayMonthlyCalendar(year,month)
    let weeks = MakeMonthlyCalendar(a:year, a:month)
    let cal=[]
    let leng = len(weeks)
    let idx = 0
    while idx<leng
        call add(cal, join(weeks[idx], '')." ")
        let idx += 1
    endwhile
    return join(cal, "\n")
endfunction

function! calendar#select(index)
  if a:index < 1 || a:index > len(g:calendar_list)
    return
  endif
  if &ft == 'calendar'
    let b:calendar_idx = g:calendar_current_idx
  endif
  let g:calendar_current_idx = a:index
endfunction


if !hasmapto("<Plug>CalendarV")
  nmap <unique> <Leader>cal <Plug>CalendarV
endif
if !hasmapto("<Plug>CalendarH")
  nmap <unique> <Leader>caL <Plug>CalendarH
endif
nnoremap <silent> <Plug>CalendarV :cal Calendar(0)<CR>
nnoremap <silent> <Plug>CalendarH :cal Calendar(1)<CR>

"*****************************************************************
"* GetToken : get token from source with count
" 从指定的各个 label 的定义中拿到指定位置的内容
" 例如：
"  let g:calendar_navi_label = "Prev,Today,Next"
"  s:GetToken(g:calendar_navi_label, ",", 2)
"  则拿到由 "," 分隔的第2个子串，即 "Today"
"*----------------------------------------------------------------
"*   src : source
"*   dlm : delimiter
"*   cnt : skip count
"*****************************************************************
function! s:GetToken(src,dlm,cnt)
  let tokn_hit=0     " flag of found
  let tokn_fnd=''    " found path
  let tokn_spl=''    " token
  let tokn_all=a:src " all source

  " safe for end
  let tokn_all = tokn_all.a:dlm
  while 1
    let tokn_spl = strpart(tokn_all,0,match(tokn_all,a:dlm))
    let tokn_hit = tokn_hit + 1
    if tokn_hit == a:cnt
      return tokn_spl
    endif
    let tokn_all = strpart(tokn_all,strlen(tokn_spl.a:dlm))
    if tokn_all == ''
      break
    endif
  endwhile
  return ''
endfunction

"*****************************************************************
"* CalendarDoAction : call the action handler function
"*----------------------------------------------------------------
"**************************************************************{{{
function! s:CalendarDoAction(...)
    " for switch calendar list.
    let text = getline(".")
    if text =~ "^( )"
        let list_idx = 0
        let curl = line(".") - 1
        while curl>1
            if getline(curl) =~ "^([Oo ])"
                let list_idx += 1
                let curl -= 1
            else
                let g:calendar_current_idx = list_idx
                let g:calendar_diary = g:calendar_list[list_idx].path
                call Calendar(b:CalendarDir, b:CalendarYear, b:CalendarMonth)
                return
            endif
        endwhile
    endif

  " for navi
  if exists('g:calendar_navi')
    let navi = (a:0 > 0)? a:1 : expand("<cWORD>")
    let curl = line(".")
    if navi == '<' . s:GetToken(g:calendar_navi_label, ',', 1)
      if b:CalendarMonth > 1
        call Calendar(b:CalendarDir, b:CalendarYear, b:CalendarMonth-1)
      else
        call Calendar(b:CalendarDir, b:CalendarYear-1, 12)
      endif
    elseif navi == s:GetToken(g:calendar_navi_label, ',', 3) . '>'
      if b:CalendarMonth < 12
        call Calendar(b:CalendarDir, b:CalendarYear, b:CalendarMonth+1)
      else
        call Calendar(b:CalendarDir, b:CalendarYear+1, 1)
      endif
    elseif navi == s:GetToken(g:calendar_navi_label, ',', 2)
      call Calendar(b:CalendarDir)
      if exists('g:calendar_today')
        exe "call " . g:calendar_today . "()"
      endif
    else
      let navi = ''
    endif
    if navi != ''
      if g:calendar_focus_today == 1 && search("\*","w") > 0
        silent execute "normal! gg/\*\<cr>"
        return
      else
        if curl < line('$')/2
          silent execute "normal! gg0/".navi."\<cr>"
        else
          silent execute "normal! G$?".navi."\<cr>"
        endif
        return
      endif
    endif
  endif

  " if no action defined return
  if !exists("g:calendar_action") || g:calendar_action == ""
    return
  endif

  " b:CalendarDir ???
  if b:CalendarDir
    let dir = 'H'
    if !exists('g:calendar_monday') && exists('g:calendar_weeknm')
      let cnr = col('.') - (col('.')%(24+5)) + 1
    else
      let cnr = col('.') - (col('.')%(24)) + 1
    endif
    let week = ((col(".") - cnr - 1 + cnr/49) / 3)
  else
    let dir = 'V'
    let cnr = 1
    let week = ((col(".")+1) / 3) - 1
  endif
  let lnr = 1
  let hdr = 1
  while 1
    if lnr > line('.')
      break
    endif
    let sline = getline(lnr)
    if sline =~ '^\s*$'
      let hdr = lnr + 1
    endif
    let lnr = lnr + 1
  endwhile
  let lnr = line('.')
  if(exists('g:calendar_monday'))
      let week = week + 1
  elseif(week == 0)
      let week = 7
  endif
  if lnr-hdr < 2
    return
  endif
  let sline = substitute(strpart(getline(hdr),cnr,21),'\s*\(.*\)\s*','\1','')
  if (col(".")-cnr) > 21
    return
  endif

  " extract day
  if g:calendar_mark == 'right' && col('.') > 1
    normal! h
    let day = matchstr(expand("<cword>"), '[^0].*')
    normal! l
  else
    let day = matchstr(expand("<cword>"), '[^0].*')
  endif
  if day == 0
    return
  endif
  " extract year and month
  if exists('g:calendar_erafmt') && g:calendar_erafmt !~ "^\s*$"
    let year = matchstr(substitute(sline, '/.*', '', ''), '\d\+')
    let month = matchstr(substitute(sline, '.*/\(\d\d\=\).*', '\1', ""), '[^0].*')
    if g:calendar_erafmt =~ '.*,[+-]*\d\+'
      let veranum=substitute(g:calendar_erafmt,'.*,\([+-]*\d\+\)','\1','')
      if year-veranum > 0
        let year=year-veranum
      endif
    endif
  else
    let year  = matchstr(substitute(sline, '/.*', '', ''), '[^0].*')
    let month = matchstr(substitute(sline, '\d*/\(\d\d\=\).*', '\1', ""), '[^0].*')
  endif
  " call the action function
  exe "call " . g:calendar_action . "(day, month, year, week, dir)"
endfunc "}}}

function! CalendarOpenFile(path)
    " load the file
    if winnr('#') == 0
        if a:dir == "V"
            exe "vsplit " . a:path
        else
            exe "split " . a:path
        endif
    else
        wincmd p
        if !&hidden && &modified
            exe "new " . a:path
        else
            exe "edit " . a:path
        endif
    endif
endfunction

function! s:CalendarOpen()
    let text = getline(".")
    if text =~ "^([Oo ])"
        let list_idx = 0
        let curl = line(".") - 1
        while curl>1
            if getline(curl) =~ "^([Oo ])"
                let list_idx += 1
                let curl -= 1
            else
                let g:calendar_current_idx = list_idx
                let g:calendar_diary = g:calendar_list[list_idx].path
                let ext = g:calendar_list[list_idx].ext
                call Calendar(b:CalendarDir, b:CalendarYear, b:CalendarMonth)
                call CalendarOpenFile(g:calendar_diary."/index.".ext)
                return
            endif
        endwhile
    endif
endfunction

"*****************************************************************
"* Calendar : build calendar
"*----------------------------------------------------------------
"*   a1 : direction
"*   a2 : month(if given a3, it's year)
"*   a3 : if given, it's month
"*****************************************************************
function! Calendar(...)

  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "+++ ready for build
  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  " remember today
  " divide strftime('%d') by 1 so as to get "1,2,3 .. 9" instead of "01, 02, 03 .. 09"
  let vtoday = strftime('%Y').strftime('%m').strftime('%d')

  " get arguments
  if a:0 == 0
    let dir = 0
    let vyear = strftime('%Y')
    let vmnth = matchstr(strftime('%m'), '[^0].*')
  elseif a:0 == 1
    let dir = a:1
    let vyear = strftime('%Y')
    let vmnth = matchstr(strftime('%m'), '[^0].*')
  elseif a:0 == 2
    let dir = a:1
    let vyear = strftime('%Y')
    let vmnth = matchstr(a:2, '^[^0].*')
  else
    let dir = a:1
    let vyear = a:2
    let vmnth = matchstr(a:3, '^[^0].*')
  endif

  " remember constant
  let vmnth_org = vmnth
  let vyear_org = vyear

  " start with last month
  let vmnth = vmnth - 1
  if vmnth < 1
    let vmnth = 12
    let vyear = vyear - 1
  endif

  " reset display variables
  let vdisplay1 = ''
  let vheight = 1
  let vmcnt = 0

  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "+++ build display
  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if exists("g:calendar_begin")
    exe "call " . g:calendar_begin . "()"
  endif
  while vmcnt < 3
    let vcolumn = 22
    let vnweek = -1
    "--------------------------------------------------------------
    "--- calculating
    "--------------------------------------------------------------
    " set boundary of the month
    if vmnth == 1
      let vmdays = 31
      let vparam = 1
      let vsmnth = 'Jan'
    elseif vmnth == 2
      let vmdays = 28
      let vparam = 32
      let vsmnth = 'Feb'
    elseif vmnth == 3
      let vmdays = 31
      let vparam = 60
      let vsmnth = 'Mar'
    elseif vmnth == 4
      let vmdays = 30
      let vparam = 91
      let vsmnth = 'Apr'
    elseif vmnth == 5
      let vmdays = 31
      let vparam = 121
      let vsmnth = 'May'
    elseif vmnth == 6
      let vmdays = 30
      let vparam = 152
      let vsmnth = 'Jun'
    elseif vmnth == 7
      let vmdays = 31
      let vparam = 182
      let vsmnth = 'Jul'
    elseif vmnth == 8
      let vmdays = 31
      let vparam = 213
      let vsmnth = 'Aug'
    elseif vmnth == 9
      let vmdays = 30
      let vparam = 244
      let vsmnth = 'Sep'
    elseif vmnth == 10
      let vmdays = 31
      let vparam = 274
      let vsmnth = 'Oct'
    elseif vmnth == 11
      let vmdays = 30
      let vparam = 305
      let vsmnth = 'Nov'
    elseif vmnth == 12
      let vmdays = 31
      let vparam = 335
      let vsmnth = 'Dec'
    else
      echo 'Invalid Year or Month'
      return
    endif
    if vyear % 400 == 0
      if vmnth == 2
        let vmdays = 29
      elseif vmnth >= 3
        let vparam = vparam + 1
      endif
    elseif vyear % 100 == 0
      if vmnth == 2
        let vmdays = 28
      endif
    elseif vyear % 4 == 0
      if vmnth == 2
        let vmdays = 29
      elseif vmnth >= 3
        let vparam = vparam + 1
      endif
    endif

    " calc vnweek of the day
    if vnweek == -1
      let vnweek = ( vyear * 365 ) + vparam
      let vnweek = vnweek + ( vyear/4 ) - ( vyear/100 ) + ( vyear/400 )
      if vyear % 4 == 0
        if vyear % 100 != 0 || vyear % 400 == 0
          let vnweek = vnweek - 1
        endif
      endif
      let vnweek = vnweek - 1
    endif

    " fix Gregorian
    if vyear <= 1752
      let vnweek = vnweek - 3
    endif

    let vnweek = vnweek % 7

    if exists('g:calendar_monday')
      " if given g:calendar_monday, the week start with monday
      if vnweek == 0
        let vnweek = 7
      endif
      let vnweek = vnweek - 1
    elseif exists('g:calendar_weeknm')
      " if given g:calendar_weeknm, show week number(ref:ISO8601)
      let viweek = vparam / 7
      let vfweek = vparam % 7
      if vnweek == 0
        let vfweek = vfweek - 7
        let viweek = viweek + 1
      else
        let vfweek = vfweek - vnweek
      endif
      if vfweek <= 0 && viweek > 0
        let viweek = viweek - 1
        let vfweek = vfweek + 7
      endif
      if vfweek > -4
        let viweek = viweek + 1
      endif
      if vfweek > 3
        let viweek = viweek + 1
      endif
      if viweek == 0
        let viweek = '??'
      elseif viweek > 52
        if vnweek != 0 && vnweek < 4
          let viweek = 1
        endif
      endif
      let vcolumn = vcolumn + 5
    endif

    "--------------------------------------------------------------
    "--- displaying
    "--------------------------------------------------------------
    " build header
    if exists('g:calendar_erafmt') && g:calendar_erafmt !~ "^\s*$"
      if g:calendar_erafmt =~ '.*,[+-]*\d\+'
        let veranum=substitute(g:calendar_erafmt,'.*,\([+-]*\d\+\)','\1','')
        if vyear+veranum > 0
          let vdisplay2=substitute(g:calendar_erafmt,'\(.*\),.*','\1','')
          let vdisplay2=vdisplay2.(vyear+veranum).'/'.vmnth.'('
        else
          let vdisplay2=vyear.'/'.vmnth.'('
        endif
      else
        let vdisplay2=vyear.'/'.vmnth.'('
      endif
      let vdisplay2=strpart("                           ",
        \ 1,(vcolumn-strlen(vdisplay2))/2-2).vdisplay2
    else
      let vdisplay2=vyear.'/'.vmnth.'('
      let vdisplay2=strpart("                           ",
        \ 1,(vcolumn-strlen(vdisplay2))/2-2).vdisplay2
    endif
    if exists('g:calendar_mruler') && g:calendar_mruler !~ "^\s*$"
      let vdisplay2=vdisplay2.s:GetToken(g:calendar_mruler,',',vmnth).')'."\n"
    else
      let vdisplay2=vdisplay2.vsmnth.')'."\n"
    endif
    let vwruler = "Su Mo Tu We Th Fr Sa"
    if exists('g:calendar_wruler') && g:calendar_wruler !~ "^\s*$"
      let vwruler = g:calendar_wruler
    endif
    if exists('g:calendar_monday')
      let vwruler = strpart(vwruler,stridx(vwruler, ' ') + 1).' '.strpart(vwruler,0,stridx(vwruler, ' '))
    endif
    let vdisplay2 = vdisplay2.' '.vwruler."\n"
    if g:calendar_mark == 'right'
      let vdisplay2 = vdisplay2.' '
    endif

    " build calendar
    let vinpcur = 0
    while (vinpcur < vnweek)
      let vdisplay2=vdisplay2.'   '
      let vinpcur = vinpcur + 1
    endwhile
    let vdaycur = 1
    while (vdaycur <= vmdays)
      if vmnth < 10
         let vtarget =vyear."0".vmnth
      else
         let vtarget =vyear.vmnth
      endif
      if vdaycur < 10
         let vtarget = vtarget."0".vdaycur
      else
         let vtarget = vtarget.vdaycur
      endif
      if exists("g:calendar_sign") && g:calendar_sign != ""
        exe "let vsign = " . g:calendar_sign . "(vdaycur, vmnth, vyear)"
        if vsign != ""
          let vsign = vsign[0]
          if vsign !~ "[+!#$%&@?]"
            let vsign = "+"
          endif
        endif
      else
        let vsign = ''
      endif

      " show mark
      if g:calendar_mark == 'right'
        if vdaycur < 10
          let vdisplay2=vdisplay2.' '
        endif
        let vdisplay2=vdisplay2.vdaycur
      elseif g:calendar_mark == 'left-fit'
        if vdaycur < 10
          let vdisplay2=vdisplay2.' '
        endif
      endif
      if vtarget == vtoday
        let vdisplay2=vdisplay2.'*'
      elseif vsign != ''
        let vdisplay2=vdisplay2.vsign
      else
        let vdisplay2=vdisplay2.' '
      endif
      if g:calendar_mark == 'left'
        if vdaycur < 10
          let vdisplay2=vdisplay2.' '
        endif
        let vdisplay2=vdisplay2.vdaycur
      endif
      if g:calendar_mark == 'left-fit'
        let vdisplay2=vdisplay2.vdaycur
      endif
      let vdaycur = vdaycur + 1

      " fix Gregorian
      if vyear == 1752 && vmnth == 9 && vdaycur == 3
        let vdaycur = 14
      endif

      let vinpcur = vinpcur + 1
      if vinpcur % 7 == 0
        if !exists('g:calendar_monday') && exists('g:calendar_weeknm')
          if g:calendar_mark != 'right'
            let vdisplay2=vdisplay2.' '
          endif
          " if given g:calendar_weeknm, show week number
          if viweek < 10
            if g:calendar_weeknm == 1
              let vdisplay2=vdisplay2.'WK0'.viweek
            elseif g:calendar_weeknm == 2
              let vdisplay2=vdisplay2.'WK '.viweek
            elseif g:calendar_weeknm == 3
              let vdisplay2=vdisplay2.'KW0'.viweek
            elseif g:calendar_weeknm == 4
              let vdisplay2=vdisplay2.'KW '.viweek
            endif
          else
            if g:calendar_weeknm <= 2
              let vdisplay2=vdisplay2.'WK'.viweek
            else
              let vdisplay2=vdisplay2.'KW'.viweek
            endif
          endif
          let viweek = viweek + 1
        endif
        let vdisplay2=vdisplay2."\n"
        if g:calendar_mark == 'right'
          let vdisplay2 = vdisplay2.' '
        endif
      endif
    endwhile

    " if it is needed, fill with space
    if vinpcur % 7
      while (vinpcur % 7 != 0)
        let vdisplay2=vdisplay2.'   '
        let vinpcur = vinpcur + 1
      endwhile
      if !exists('g:calendar_monday') && exists('g:calendar_weeknm')
        if g:calendar_mark != 'right'
          let vdisplay2=vdisplay2.' '
        endif
        if viweek < 10
          if g:calendar_weeknm == 1
            let vdisplay2=vdisplay2.'WK0'.viweek
          elseif g:calendar_weeknm == 2
            let vdisplay2=vdisplay2.'WK '.viweek
          elseif g:calendar_weeknm == 3
            let vdisplay2=vdisplay2.'KW0'.viweek
          elseif g:calendar_weeknm == 4
            let vdisplay2=vdisplay2.'KW '.viweek
          endif
        else
          if g:calendar_weeknm <= 2
            let vdisplay2=vdisplay2.'WK'.viweek
          else
            let vdisplay2=vdisplay2.'KW'.viweek
          endif
        endif
      endif
    endif

    " build display
    let vstrline = ''
    if dir
      " for horizontal
      "--------------------------------------------------------------
      " +---+   +---+   +------+
      " |   |   |   |   |      |
      " | 1 | + | 2 | = |  1'  |
      " |   |   |   |   |      |
      " +---+   +---+   +------+
      "--------------------------------------------------------------
      let vtokline = 1
      while 1
        let vtoken1 = s:GetToken(vdisplay1,"\n",vtokline)
        let vtoken2 = s:GetToken(vdisplay2,"\n",vtokline)
        if vtoken1 == '' && vtoken2 == ''
          break
        endif
        while strlen(vtoken1) < (vcolumn+1)*vmcnt
          if strlen(vtoken1) % (vcolumn+1) == 0
            let vtoken1 = vtoken1.'|'
          else
            let vtoken1 = vtoken1.' '
          endif
        endwhile
        let vstrline = vstrline.vtoken1.'|'.vtoken2.' '."\n"
        let vtokline = vtokline + 1
      endwhile
      let vdisplay1 = vstrline
      let vheight = vtokline-1
    else
      " for virtical
      "--------------------------------------------------------------
      " +---+   +---+   +---+
      " | 1 | + | 2 | = |   |
      " +---+   +---+   | 1'|
      "                 |   |
      "                 +---+
      "--------------------------------------------------------------
      let vtokline = 1
      while 1
        let vtoken1 = s:GetToken(vdisplay1,"\n",vtokline)
        if vtoken1 == ''
          break
        endif
        let vstrline = vstrline.vtoken1."\n"
        let vtokline = vtokline + 1
        let vheight = vheight + 1
      endwhile
      if vstrline != ''
        let vstrline = vstrline.' '."\n"
        let vheight = vheight + 1
      endif
      let vtokline = 1
      while 1
        let vtoken2 = s:GetToken(vdisplay2,"\n",vtokline)
        if vtoken2 == ''
          break
        endif
        while strlen(vtoken2) < vcolumn
          let vtoken2 = vtoken2.' '
        endwhile
        let vstrline = vstrline.vtoken2."\n"
        let vtokline = vtokline + 1
        let vheight = vtokline + 1
      endwhile
      let vdisplay1 = vstrline
    endif
    let vmnth = vmnth + 1
    let vmcnt = vmcnt + 1
    if vmnth > 12
      let vmnth = 1
      let vyear = vyear + 1
    endif
  endwhile
  if exists("g:calendar_end")
    exe "call " . g:calendar_end . "()"
  endif
  if a:0 == 0
    return vdisplay1
  endif

  "let vdisplay1 = vdisplay1 . "\n      ".vyear."/".vmnth."\n"
  "let vdisplay1 = vdisplay1 . " Mo Tu We Th Fr Sa Su \n"
  "let vdisplay1 = vdisplay1 . DisplayMonthlyCalendar(vyear, vmnth)

  let vdisplay1 = vdisplay1 . "\nCalendars:\n----------------------"
  let diary_index = 0
  for diary in g:calendar_list
      if diary_index == g:calendar_current_idx
          let diary_list = "\n(O) " . diary["name"]
          let diary_list = diary_list . repeat(" ", 23-len(diary_list))
      else
          let diary_list = "\n( ) " . diary["name"]
          let diary_list = diary_list . repeat(" ", 23-len(diary_list))
      endif
      let vdisplay1 = vdisplay1 . diary_list
      let diary_index = diary_index + 1
  endfor

  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "+++ build window
  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  " make window
  let vwinnum=bufnr('__Calendar')
  if getbufvar(vwinnum, 'Calendar')=='Calendar'
    let vwinnum=bufwinnr(vwinnum)
  else
    let vwinnum=-1
  endif

  if vwinnum >= 0
    " if already exist
    if vwinnum != bufwinnr('%')
      exe vwinnum . 'wincmd w'
    endif
    setlocal modifiable
    silent %d _
  else
    " make title
    if g:calendar_datetime == "title" && (!exists('s:bufautocommandsset'))
      auto BufEnter *Calendar let b:sav_titlestring = &titlestring | let &titlestring = '%{strftime("%c")}'
      auto BufLeave *Calendar let &titlestring = b:sav_titlestring
      let s:bufautocommandsset=1
    endif

    if exists('g:calendar_navi') && dir
      if g:calendar_navi == 'both'
        let vheight = vheight + 4
      else
        let vheight = vheight + 2
      endif
    endif

    " or not
    if dir
      execute 'bo '.vheight.'split __Calendar'
      setlocal winfixheight
    else
      execute 'to '.vcolumn.'vsplit __Calendar'
      setlocal winfixwidth
    endif
    call s:CalendarBuildKeymap(dir, vyear, vmnth)
    setlocal noswapfile
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal nonumber
    " Without this, the 'sidescrolloff' setting may cause the left side of the
    " calendar to disappear if the last inserted element is near the right
    " window border.
    setlocal nowrap
    setlocal norightleft
    setlocal foldcolumn=0
    setlocal modifiable
    setlocal nolist
    let b:Calendar='Calendar'
    setlocal filetype=calendar
    " is this a vertical (0) or a horizontal (1) split?
  endif
  if g:calendar_datetime == "statusline"
    setlocal statusline=%{strftime('%c')}
  endif
  let b:CalendarDir=dir
  let b:CalendarYear = vyear_org
  let b:CalendarMonth = vmnth_org

  " navi
  if exists('g:calendar_navi')
    let navi_label = '<'
        \.s:GetToken(g:calendar_navi_label, ',', 1).' '
        \.s:GetToken(g:calendar_navi_label, ',', 2).' '
        \.s:GetToken(g:calendar_navi_label, ',', 3).'>'
    if dir
      let navcol = vcolumn + (vcolumn-strlen(navi_label)+2)/2
    else
      let navcol = (vcolumn-strlen(navi_label)+2)/2
    endif
    if navcol < 3
      let navcol = 3
    endif

    if g:calendar_navi == 'top'
      execute "normal gg".navcol."i "
      silent exec "normal! a".navi_label."\<cr>\<cr>"
      silent put! =vdisplay1
    endif
    if g:calendar_navi == 'bottom'
      silent put! =vdisplay1
      silent exec "normal! Gi\<cr>"
      execute "normal ".navcol."i "
      silent exec "normal! a".navi_label
    endif
    if g:calendar_navi == 'both'
      execute "normal gg".navcol."i "
      silent exec "normal! a".navi_label."\<cr>\<cr>"
      silent put! =vdisplay1
      silent exec "normal! Gi\<cr>"
      execute "normal ".navcol."i "
      silent exec "normal! a".navi_label
    endif
  else
    silent put! =vdisplay1
  endif

  setlocal nomodifiable
  " In case we've gotten here from insert mode (via <C-O>:Calendar<CR>)...
  stopinsert

  let vyear = vyear_org
  let vmnth = vmnth_org

  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  "+++ build highlight
  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  " today
  syn clear
  if g:calendar_mark =~ 'left-fit'
    syn match CalToday display "\s*\*\d*"
    syn match CalMemo display "\s*[+!#$%&@?]\d*"
  elseif g:calendar_mark =~ 'right'
    syn match CalToday display "\d*\*\s*"
    syn match CalMemo display "\d*[+!#$%&@?]\s*"
  else
    syn match CalToday display "\*\s*\d*"
    syn match CalMemo display "[+!#$%&@?]\s*\d*"
  endif
  " header
  syn match CalHeader display "[^ ]*\d\+\/\d\+([^)]*)"

  " navi
  if exists('g:calendar_navi')
    exec "silent! syn match CalNavi display \"\\(<"
        \.s:GetToken(g:calendar_navi_label, ',', 1)."\\|"
        \.s:GetToken(g:calendar_navi_label, ',', 3).">\\)\""
    exec "silent! syn match CalNavi display \"\\s"
        \.s:GetToken(g:calendar_navi_label, ',', 2)."\\s\"hs=s+1,he=e-1"
  endif

  " saturday, sunday
  let dayorspace = '\(\*\|\s\)\(\s\|\d\)\(\s\|\d\)'
  if !exists('g:calendar_weeknm') || g:calendar_weeknm <= 2
    let wknmstring = '\(\sWK[0-9\ ]\d\)*'
  else
    let wknmstring = '\(\sKW[0-9\ ]\d\)*'
  endif
  let eolnstring = '\s\(|\|$\)'
  if exists('g:calendar_monday')
    execute "syn match CalSaturday display \'"
      \.dayorspace.dayorspace.wknmstring.eolnstring."\'ms=s+1,me=s+3"
    execute "syn match CalSunday display \'"
      \.dayorspace.wknmstring.eolnstring."\'ms=s+1,me=s+3"
  else
    if dir
      execute "syn match CalSaturday display \'"
        \.dayorspace.wknmstring.eolnstring."\'ms=s+1,me=s+3"
      execute "syn match CalSunday display \'\|"
        \.dayorspace."\'ms=s+2,me=s+4"
    else
      execute "syn match CalSaturday display \'"
        \.dayorspace.wknmstring.eolnstring."\'ms=s+1,me=s+3"
      execute "syn match CalSunday display \'^"
        \.dayorspace."\'ms=s+1,me=s+3"
    endif
  endif

  syn match CalCurrList display "^(O).*$"

  " week number
  if !exists('g:calendar_weeknm') || g:calendar_weeknm <= 2
    syn match CalWeeknm display "WK[0-9\ ]\d"
  else
    syn match CalWeeknm display "KW[0-9\ ]\d"
  endif

  " ruler
  execute 'syn match CalRuler "'.vwruler.'"'

  if search("\*","w") > 0
    silent execute "normal! gg/\*\<cr>"
  endif

  return ''
endfunction

"*****************************************************************
"* CalendarMakeDir : make directory
"*----------------------------------------------------------------
"*   dir : directory
"*****************************************************************
function! s:CalendarMakeDir(dir)
  if(has("unix"))
    call system("mkdir " . a:dir)
    let rc = v:shell_error
  elseif(has("win16") || has("win32") || has("win95") ||
              \has("dos16") || has("dos32") || has("os2"))
    call system("mkdir \"" . a:dir . "\"")
    let rc = v:shell_error
  else
    let rc = 1
  endif
  if rc != 0
    call confirm("can't create directory : " . a:dir, "&OK")
  endif
  return rc
endfunc

"*****************************************************************
"* CalendarDiary : calendar hook function
"*----------------------------------------------------------------
"*   day   : day you actioned
"*   month : month you actioned
"*   year  : year you actioned
"*****************************************************************
function! s:CalendarDiary(day, month, year, week, dir)
  " build the file name and create directories as needed
  if !isdirectory(expand(g:calendar_diary))
    call confirm("please create diary directory : ".g:calendar_diary, 'OK')
    return
  endif
  let sfile = expand(g:calendar_diary) . "/" . a:year
  if isdirectory(sfile) == 0
    if s:CalendarMakeDir(sfile) != 0
      return
    endif
  endif
  let sfile = sfile . "/" . a:month
  if isdirectory(sfile) == 0
    if s:CalendarMakeDir(sfile) != 0
      return
    endif
  endif
  let sfile = expand(sfile)."/".a:day.".".g:calendar_list[g:calendar_current_idx]["ext"]
  let sfile = substitute(sfile, ' ', '\\ ', 'g')
  let vbufnr = bufnr('__Calendar')

  " load the file
  if winnr('#') == 0
    if a:dir == "V"
      exe "vsplit " . sfile
    else
      exe "split " . sfile
    endif
  else
    wincmd p
    if !&hidden && &modified
      exe "new " . sfile
    else
      exe "edit " . sfile
    endif
  endif

  "setlocal ft=calendar
  let dir = getbufvar(vbufnr, "CalendarDir")
  let vyear = getbufvar(vbufnr, "CalendarYear")
  let vmnth = getbufvar(vbufnr, "CalendarMonth")
  exe "auto BufDelete ".escape(sfile, ' \\')." call Calendar(" . dir . "," . vyear . "," . vmnth . ")"
endfunc

"*****************************************************************
"* CalendarSign : calendar sign function
"*----------------------------------------------------------------
"*   day   : day of sign
"*   month : month of sign
"*   year  : year of sign
"*****************************************************************
function! s:CalendarSign(day, month, year)
  let sfile = g:calendar_diary."/".a:year."/".a:month."/".a:day.".".g:calendar_list[g:calendar_current_idx]["ext"]
  return filereadable(expand(sfile))
endfunction

"*****************************************************************
"* CalendarVar : get variable
"*----------------------------------------------------------------
"*****************************************************************
function! s:CalendarVar(var)
  if !exists(a:var)
    return ''
  endif
  exec 'return ' . a:var
endfunction

"*****************************************************************
"* CalendarBuildKeymap : build keymap
"*----------------------------------------------------------------
"*****************************************************************
function! s:CalendarBuildKeymap(dir, vyear, vmnth)
  " make keymap
  execute 'nnoremap <silent> <buffer> q :close<bar>wincmd p<cr>'

  execute 'nnoremap <silent> <buffer> <Plug>CalendarDoAction  :call <SID>CalendarDoAction()<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarDoAction  :call <SID>CalendarDoAction()<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarOpen      :call <SID>CalendarOpen()<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarGotoToday :call Calendar(b:CalendarDir)<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarShowHelp  :call <SID>CalendarHelp()<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarReDisplay :call Calendar(' . a:dir . ',' . a:vyear . ',' . (a:vmnth-2) . ')<cr>'
  let pnav = s:GetToken(g:calendar_navi_label, ',', 1)
  let nnav = s:GetToken(g:calendar_navi_label, ',', 3)
  execute 'nnoremap <silent> <buffer> <Plug>CalendarGotoPrevMonth :call <SID>CalendarDoAction("<' . pnav . '")<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarGotoNextMonth :call <SID>CalendarDoAction("' . nnav . '>")<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarGotoPrevYear  :call Calendar('.a:dir.','.(a:vyear-1).','.a:vmnth.')<cr>'
  execute 'nnoremap <silent> <buffer> <Plug>CalendarGotoNextYear  :call Calendar('.a:dir.','.(a:vyear+1).','.a:vmnth.')<cr>'

  nmap <buffer> <CR>          <Plug>CalendarDoAction
  nmap <buffer> <2-LeftMouse> <Plug>CalendarDoAction
  nmap <buffer> t             <Plug>CalendarGotoToday
  nmap <buffer> ?             <Plug>CalendarShowHelp
  nmap <buffer> r             <Plug>CalendarReDisplay
  nmap <buffer> o             <Plug>CalendarOpen

  nmap <buffer> <Left>  <Plug>CalendarGotoPrevMonth
  nmap <buffer> <Right> <Plug>CalendarGotoNextMonth
  nmap <buffer> <Up>    <Plug>CalendarGotoPrevYear
  nmap <buffer> <Down>  <Plug>CalendarGotoNextYear
endfunction

"*****************************************************************
"* CalendarHelp : show help for Calendar
"*----------------------------------------------------------------
"*****************************************************************
function! s:CalendarHelp()
  echohl None
  echo 'Calendar version ' . g:calendar_version
  echohl SpecialKey
  echo '<Left>    : goto prev month'
  echo '<Right>   : goto next month'
  echo '<Up>      : goto prev year'
  echo '<Down>    : goto next year'
  echo 't         : goto today'
  echo 'q         : close window'
  echo 'r         : re-display window'
  echo '?         : show this help'
  if g:calendar_action == "<SID>CalendarDiary"
    echo '<cr>      : show diary'
  endif
  echo ''
  echohl Question
  echo 'calendar_erafmt=' . s:CalendarVar('g:calendar_erafmt')
  echo 'calendar_mruler=' . s:CalendarVar('g:calendar_mruler')
  echo 'calendar_wruler=' . s:CalendarVar('g:calendar_wruler')
  echo 'calendar_weeknm=' . s:CalendarVar('g:calendar_weeknm')
  echo 'calendar_navi_label=' . s:CalendarVar('g:calendar_navi_label')
  echo 'calendar_diary=' . s:CalendarVar('g:calendar_diary')
  echo 'calendar_mark=' . s:CalendarVar('g:calendar_mark')
  echo 'calendar_navi=' . s:CalendarVar('g:calendar_navi')
  echohl MoreMsg
  echo "[Hit any key]"
  echohl None
  call getchar()
  redraw!
endfunction

hi def link CalNavi     Search
hi def link CalSaturday Statement
hi def link CalSunday   Type
hi def link CalRuler    StatusLine
hi def link CalWeeknm   Comment
hi def link CalToday    Directory
hi def link CalHeader   Special
hi def link CalMemo     Identifier
hi def link CalCurrList Error
