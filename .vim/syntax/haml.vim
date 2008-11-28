" Vim syntax file
" Language: Haml (XHTML Abstraction Markup Language)
" Maintainer: Dmitry A. Ilyashevich <dmitry.ilyashevich@gmail.com>
" License: This file can be redistribued and/or modified under the same terms
"   as Vim itself.
"
" Version: 0.3
" Last Change: 2008-03-16
" Notes: Last synced with Haml 1.8
" TODO: Support for indented multiline sections
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
"
" Changes:
"   - David Bushong added support for Haml 1.8's == syntax for ruby strings;
"   - Lasse Jansen make syntax highlighting of multiline ruby commands work
"     ("|" at the end of the line).
"

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'haml'
endif

if version < 600
  so <sfile>:p:h/html.vim
  syn include @rubyTop <sfile>:p:h/ruby.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
  syn include @rubyTop syntax/ruby.vim
endif

syn region  hamlLineTag          start="^\s*%[a-zA-Z0-9\-_\:]\+" end="$" oneline keepend contains=hamlHtmlTag,hamlCssClassIncluded,hamlCssIdIncluded,hamlHtmlTagEnd,hamlRubyCodeIncluded,hamlRubyHash,hamlSpecialChar
syn region  hamlLineTag          start="^\s*%[a-zA-Z0-9\-_\:]\+" end="$" oneline keepend contains=hamlHtmlTag,hamlCssClassIncluded,hamlCssIdIncluded,hamlHtmlTagEnd,hamlRubyCodeIncluded,hamlRubyHash,hamlRubyStringIncluded
syn match   hamlHtmlTagEnd       "/$" contained
syn match   hamlHtmlTag          "^\s*%[a-zA-Z0-9\-_\:]\+" contained contains=htmlTagName
syn match   hamlCssClassIncluded "\.[a-zA-Z0-9\-_\:]\+" contained
syn match   hamlCssIdIncluded    "\#[a-zA-Z0-9\-_\:]\+" contained

syn region  hamlLineClass        start="^\s*\.[a-zA-Z0-9\-_\:]*" end="$" oneline keepend contains=hamlCssClass,hamlCssClassIncluded,hamlCssIdIncluded,hamlRubyCodeIncluded,hamlRubyStringIncluded
syn region  hamlLineId           start="^\s*\#[a-zA-Z0-9\-_\:]*" end="$" oneline keepend contains=hamlCssId,hamlCssClassIncluded,hamlCssIdIncluded,hamlRubyCodeIncluded,hamlRubyStringIncluded

syn match   hamlCssId            "^\s*#[a-zA-Z0-9\-_\:]*" contained
syn match   hamlCssClass         "^\s*\.[a-zA-Z0-9\-_\:]*" contained

syn region  hamlRubyCodeIncluded   matchgroup=Delimiter start="[=~-] " end="$" contained contains=@rubyTop,hamlRubyHash keepend
syn region  hamlRubyHash           matchgroup=Delimiter start="{" end="}" contained contains=@rubyTop keepend
syn region  hamlRubyCode           matchgroup=Delimiter start="^\s*[=~-]" end="$" contains=@rubyTop,hamlRubyHash keepend
syn region  hamlRubyStringIncluded matchgroup=Delimiter start="== " end="$" contained contains=@rubyStringSpecial keepend
syn region  hamlRubyString         matchgroup=Delimiter start="^\s*==" end="$" contains=@rubyStringSpecial


syn match   hamlPreDef           "^\s*:[a-zA-Z0-9\-_\:]\+"
syn region  hamlPreProc          start="^\s*\\" end="$"
syn match   hamlPreProc          " |$"

syn match   hamlComment          "^!!!.*$"
syn match   hamlComment          "^\s*/.*$" contains=hamlTodo,@Spell
syn keyword hamlTodo             TODO FIXME XXX contained

" special characters
syn match   hamlSpecialChar      contained "&#\=[0-9A-Za-z]\{1,8};"


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_haml_syntax_inits")
  if version < 508
    let did_haml_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink hamlLineClass          hamlLineTag
  HiLink hamlLineId             hamlLineTag
  HiLink hamlCssClassIncluded   hamlCssClass
  HiLink hamlCssIdIncluded      Type
  HiLink hamlCssId              Type
  HiLink hamlHtmlTagEnd         hamlHtmlTag
  HiLink hamlPreDef             hamlHtmlTag
  HiLink hamlRubyHash           hamlLineTag
  HiLink hamlRubyCode           hamlLineTag
  HiLink hamlRubyCodeIncluded   hamlLineTag
  HiLink hamlRubyString         hamlLineTag
  HiLink hamlRubyStringIncluded hamlLineTag

  HiLink hamlLineTag            Text
  HiLink hamlHtmlTag            Statement
  HiLink hamlCssClass           Type
  HiLink hamlPreProc            PreProc
  HiLink hamlComment		Comment
  HiLink hamlTodo               Todo

  HiLink hamlSpecialChar        Special

  delcommand HiLink
endif
let b:current_syntax = "haml"

if main_syntax == 'haml'
  unlet main_syntax
endif

" vim: nowrap sw=2 sts=2 ts=8 ff=unix:
