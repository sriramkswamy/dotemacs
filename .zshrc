
# The following lines were added by compinstall
zstyle :compinstall filename '/home/sriramkswamy/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install

# terminal
export TERM=screen-256color

# PATH
export PATH=/home/sriramkswamy/.fzf/bin:$PATH
export PATH=/home/sriramkswamy/.cargo/bin:$PATH
export PATH=/home/sriramkswamy/local/bin:$PATH

# python virtualenv
source ~/venv/global/bin/activate

# alias
alias e='emacs'
alias v='nvim'
alias m='tmux'
alias matlab='~/local/MATLAB_R2018a/matlab -nodesktop -nosplash'
alias matlabd='~/local/MATLAB_R2018a/matlab'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
