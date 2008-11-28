" Vim syntax file
" Language:    jQuery
" Maintainer:  Bruno Michel <brmichel@free.fr>
" Last Change: Oct 6, 2008
" Version:     0.1
" URL:         http://jquery.com/

if exists("b:current_syntax") && b:current_syntax =~ "jquery"
  finish
endif


syn keyword jQuery          jQuery


syn region  jFunc           start="." end="(" transparent contains=ALL

syn keyword jCore           contained each eq get index length size extend noConflict
syn keyword jAttributes     contained attr removeAttr addClass removeClass toggleClass html text val
syn keyword jTraversing     contained filter hasClass is map not slice
syn keyword jTraversing     contained add children contents find next nextAll parent parents prev prevAll siblings
syn keyword jTraversing     contained andSelf end
syn keyword jManipulation   contained append appendTo preprend prependTo
syn keyword jManipulation   contained after before insertAfter insertB
syn keyword jManipulation   contained wrap wrapAll wrapInner
syn keyword jManipulation   contained replaceAll replaceWith
syn keyword jManipulation   contained empty remove
syn keyword jManipulation   contained clone
syn keyword jCSS            contained css offset height width
syn keyword jEvents         contained ready
syn keyword jEvents         contained bind one trigger triggerHandler
syn keyword jEvents         contained unbind
syn keyword jEvents         contained hover toggle
syn keyword jEvents         contained blur change click dblclick error focus keydown keypress keyup load
syn keyword jEvents         contained mousedown mousemove mouseout mouseover mouseup resize scroll select submit unload
syn keyword jEffects        contained hide show
syn keyword jEffects        contained slideDown slideToggle slideUp
syn keyword jEffects        contained fadeIn fadeOut fadeTo
syn keyword jEffects        contained animate dequeue queue stop
syn keyword jAjax           contained ajax get getJSON getScript post load
syn keyword jAjax           contained ajaxComplete ajaxError ajaxSend ajaxStart ajaxStop ajaxSuccess
syn keyword jAjax           contained ajaxSetup serialize serializeArray
syn keyword jUtilities      contained boxModel browser
syn keyword jUtilities      contained extend grep inArray makeArray map unique
syn keyword jUtilities      contained isFunction trim
syn keyword jInternals      contained data removeData param


syn region  javaScriptStringD          start=+"+  skip=+\\\\\|\\"+  end=+"\|$+  contains=javaScriptSpecial,@htmlPreproc,@jSelectors
syn region  javaScriptStringS          start=+'+  skip=+\\\\\|\\'+  end=+'\|$+  contains=javaScriptSpecial,@htmlPreproc,@jSelectors

syn cluster jSelectors      contains=jId,jClass,jOperators,jBasicFilters,jContentFilters,jVisibility,jChildFilters,jForms,jFormFilters
syn match   jId             contained /#\w\+/
syn match   jClass          contained /\.\w\+/
syn match   jOperators      contained /*\|>\|>|\~/
syn match   jBasicFilters   contained /:(animated\|eq\|even\|first\|gt\|header\|last\|lt\|not\|odd)/
syn match   jContentFilters contained /:(contains\|empty\|has\|parent)/
syn match   jVisibility     contained /:(hidden\|visible)/
syn match   jChildFilters   contained /:(first\|last\|nth\|only)-child/
syn match   jForms          contained /:(button\|checkbox\|file\|hidden\|image\|input\|password\|radio\|reset\|submit\|text)/
syn match   jFormFilters    contained /:(checked\|disabled\|enabled\|selected)/


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lisp_syntax_inits")
  if version < 508
    let did_lisp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink jQuery          Constant

  HiLink jCore           Identifier
  HiLink jAttributes     Identifier
  HiLink jTraversing     Identifier
  HiLink jManipulation   Identifier
  HiLink jCSS            Identifier
  HiLink jEvents         Identifier
  HiLink jEffects        Identifier
  HiLink jAjax           Identifier
  HiLink jUtilities      Identifier
  HiLink jInternals      Identifier

  HiLink jId             Identifier
  HiLink jClass          Constant
  HiLink jOperators      Special
  HiLink jBasicFilters   Statement
  HiLink jContentFilters Statement
  HiLink jVisibility     Statement
  HiLink jChildFilters   Statement
  HiLink jForms          Statement
  HiLink jFormFilters    Statement

  delcommand HiLink
endif


let b:current_syntax = 'javascript.jquery'
