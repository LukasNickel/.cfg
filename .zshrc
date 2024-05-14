
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

function open () {
	# raise an error if the argument does not exist and is not a link
	if [[ "$1" != http*//* && "$1" != www.* && ! -a "$1" ]]; then
		echo "No such file or directory $1" 1>&2
		return 1
	fi

	nohup xdg-open $1 < /dev/null 2>/dev/null >/dev/null &!
}

# ZSH options
setopt EXTENDED_HISTORY
setopt share_history
export HISTSIZE=10000
export SAVEHIST=10000
HISTFILE=~/.zsh_history
HIST_STAMPS="yyyy-mm-dd"
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[3~' delete-char

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# python
alias pytest='pytest -v --lf'

# navigation
alias ..='cd ..' 
alias ...='cd ../..'

# file manipulation
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias du="du -h"

# Changing "ls" to "eza"
alias ls='eza -al --color=always --group-directories-first' # my preferred listing
alias la='eza -a --color=always --group-directories-first'  # all files and dirs
alias ll='eza -l --color=always --group-directories-first'  # long format
alias lt='eza -aT --color=always --group-directories-first' # tree listing
alias l.='eza -a | egrep "^\."'

# git
alias gitgraph="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n'' %C(white)%s%C(reset) %C(dim white)- %an%C(reset)' --all"

alias cat='bat'

# kitty hyperlinked grep
alias rg='kitty +kitten hyperlinked_grep'
# kitty show images in terminal
alias icat="kitty +kitten icat"

# magic to control config files in git 
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$("$HOME/.local/anaconda3/bin/conda" 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/.local/anaconda3/etc/profile.d/conda.sh" ]; then
        . "$HOME/.local/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/.local/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "$HOME/.local/anaconda3/etc/profile.d/mamba.sh" ]; then
    . "$HOME/.local/anaconda3/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Allow Ctrl-z to toggle between suspend and resume
function Resume { 
  fg 
  zle push-input 
  BUFFER="" 
  zle accept-line
} 
zle -N Resume
bindkey "^Z" Resume

export FZF_TMUX=0
export FZF_TMUX_HEIGHT=10
export FZF_DEFAULT_COMMAND='fd --hidden'
export FZF_DEFAULT_OPTS='--no-bold --reverse'
export FZF_CTRL_T_COMMAND=fd
export FZF_ALT_C_COMMAND='fd --type d'
export FZF_ALT_C_OPTS='--preview "eza --color=always {}"'
# -------------------------------------------------------
