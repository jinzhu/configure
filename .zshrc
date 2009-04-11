EDITOR="vim"
VISUAL=$EDITOR
ZDOTDIR=~/.zsh
[[ -f $ZDOTDIR/.zshenv ]] && . $ZDOTDIR/.zshenv
[[ -f $HOME/.bash_aliases ]] && . $HOME/.bash_aliases
[[ -f $HOME/.zsh_cache ]] || touch $HOME/.zsh_cache


for file in $HOME/.zsh/* ; do
  source $file
done

HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt HIST_IGNORE_DUPS     
setopt SHARE_HISTORY # share history between sessions ???
setopt EXTENDED_HISTORY # add timestamps to history
setopt APPEND_HISTORY # adds history
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

setopt AUTO_CD
setopt AUTO_PUSHD
## Expand directory expressions as if they all started with ~
setopt CDABLE_VARS
setopt PUSHD_IGNORE_DUPS

## Append a slash to autocompleted parameters that correspond to directories.
setopt AUTO_PARAM_SLASH
## Remove trailing slashes if they are not relevant to the command executed.
## (set by default)
unsetopt AUTO_REMOVE_SLASH
## Complete non-ambiguous prefix/suffix first, then display the ambiguities
## after another function call.
setopt LIST_AMBIGUOUS
setopt AUTO_LIST
setopt AUTO_MENU
#开启此选项，补全时会直接选中菜单项
setopt MENU_COMPLETE

## Background jobs notify its status immediately
setopt NOTIFY

setopt PROMPT_SUBST
setopt CORRECT
setopt CORRECT_ALL       # Try to autocorrect commands & file names
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

setopt MULTIOS                      # this enables various goodness

setopt INTERACTIVE_COMMENTS     


# Completion ssh en fonction des known_hosts (recupere la liste des hosts dans known_hosts
# local _myhosts
# if [ -d ~/.ssh ]; then
#   if [ -f ~/.ssh/known_hosts ];then
#     _myhosts=(${=${${(f)"$(<$HOME/.ssh/known_hosts)"}%%[# ]*}//,/ })
#   fi
# fi
# zstyle ':completion:*' hosts $_myhosts

## Allow single quotes inside single quotes: ''
setopt RC_QUOTES
     
CDPATH=".:~:~/GIT:~/Lab:/pillar:/pillar/HOME/:~/Documents/"
#禁用 core dumps
limit coredumpsize 0

#Emacs风格 键绑定
bindkey -e

#以下字符视为单词的一部分
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
#}}}


# this enables autocompletion for pretty much everything
autoload -U compinit
compinit
## Automagically escape URLs correctly :)
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
# ESC-h, ESC-H, ALT-h ou ALT-H lance le man sur la commande en cours.
autoload run-help

#自动补全缓存
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh_cache
zstyle ':completion:*:cd:*' ignore-parents parent pwd

#自动补全选项
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:*' completer _complete _prefix _correct _prefix _match _approximate


#路径补全
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

zstyle ':completion:*' menu select
zstyle ':completion:*:*:default' force-list always

#彩色补全菜单
eval $(dircolors -b)
export ZLSCOLORS="${LS_COLORS}"
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# pretty kill completion. colored, cpu load & process tree
zstyle ':completion:*:kill:*' command 'ps xf -u $USER -o pid,%cpu,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

setopt    bg_nice                      # [set -6] Renice background jobs
#修正大小写
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
#错误校正     
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

#补全类型提示分组
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'
zstyle ':completion:*:messages' format $'\e[01;35m -- %d --\e[0m'
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
#}}}

##空行(光标在行首)补全 "cd " {{{
user-complete(){
    case $BUFFER in
        "" )                       # 空行填入 "cd "
        BUFFER="cd "
        zle end-of-line
        zle expand-or-complete
        ;;
        * )
        zle expand-or-complete
        ;;
    esac
}
zle -N user-complete
bindkey "\t" user-complete
#}}}

##在命令前插入 sudo {{{
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
    zle end-of-line                 #光标移动到行末
}
zle -N sudo-command-line
bindkey "\e\e" sudo-command-line    #定义快捷键为： [Esc] [Esc]
#}}}

# 标题样式
case $TERM in (*xterm*|rxvt|(dt|k|E)term)
   precmd () { print -Pn "\e]0;%n@%~\a" }
   preexec () { print -Pn "\e]0;%n@%~\ $1\a" }
   ;;
esac

# set a fancy prompt (non-color, unless we know we "want" color)
function parse_scm {
  if [ -d '.git' ];then
    GIT_BRANCH=`git branch 2> /dev/null | grep '^\*' | sed 's/^\*\ //'`
    if [ "$GIT_BRANCH" ] ; then
      GIT_STATUS=`git status 2> /dev/null`
      CLR='2'
      if [ "`echo $GIT_STATUS | grep 'Changed but not updated'`" ] ; then
        CLR='1'
      fi

      pt=`echo $GIT_STATUS | grep 'Your branch is ahead' | awk {'print $13'}`
      if [ "${pt}" ] ; then
        PTIMES=" ${pt} ↑ "
      else
        PTIMES=""
      fi

      RSLT="     %{\033[01;3${CLR};40m%}[${GIT_BRANCH}${PTIMES}]%{$reset_color%}"
    fi
    echo -e "${RSLT}" && return
  fi

  if [ -d '.svn' ];then
    local DIRTY REV=$(svn info 2>/dev/null | grep Revision | sed -e 's/Revision: //')
    [ "$REV" ] || return
    
    CLR='2'
    if [ "$(svn st)" ] ; then
      CLR='1'
    fi
    
    echo -e "    %{\033[01;3${CLR};40m%}[r$REV$DIRTY]%{$reset_color%}" && return
  fi
}

#命令提示符 {{{
RPROMPT=$(echo '%{\033[31m%}%D %T%{\033[m%}')
PROMPT="%{$bold_color$fg[grey]$bg[grey]%} [%n@%m] %{$reset_color%}    %{$bold_color$fg[cyan]%}[%t]%{$reset_color%}     %{$bold_color$fg[blue]%}[%1~]%{$reset_color%}$(parse_scm)
%{$bold_color$fg[green]%}%#%{$reset_color%} "
#}}}

# 载入数学模块
zmodload zsh/mathfunc
function calc { echo $(($@)) }

if [ -f ~/.dir_colors ]; then
  eval $(dircolors -b ~/.dir_colors)
fi

bindkey "^[[1;5D" beginning-of-line # CTRL + <
bindkey "^[OH"    beginning-of-line # HOME
bindkey "^[[1;5C" end-of-line       # CTRL + >
bindkey "^[OF"    end-of-line       # END
bindkey "^[[1;3D" backward-word     # ALT  + <
bindkey "^[[1;3C" forward-word      # ALT  + >
bindkey "^[[3~"   delete-char       # DELETE
bindkey "^[[A"    history-search-backward # up   arrow
bindkey "^[[B"    history-search-forward  # down arrow
bindkey "^[[2~"   overwrite-mode          # Insert


## Vi/Emacs-style keybindings
# bindkey -v
# setopt VI
# bindkey -e
# setopt EMACS
