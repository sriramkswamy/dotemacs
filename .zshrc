
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
alias e='emacs -nw'
alias v='nvim'
alias m='tmux'
alias s='tmux attach-session'

# application aliases
alias d='dropbox status'
alias julia='/hom/sriramkswamy/julia-1.0.0/bin/julia'

# git aliases
alias g='git status'
alias q='git commit -m'
alias c='git commit'
alias a='git add'
alias p='git push'
alias f='git pull'
alias o='git clone'
alias l="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset) %C(bold cyan)(committed: %cD)%C(reset) %C(auto)%d%C(reset)%n''          %C(white)%s%C(reset)%n''          %C(dim white)- %an <%ae> %C(reset) %C(dim white)(committer: %cn <%ce>)%C(reset)'"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# start dropbox
dropbox start
