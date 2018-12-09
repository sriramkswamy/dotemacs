# ls aliases
alias ls='ls -GFh'
alias sl='ls -GFh'
alias la='ls -AGFh'
alias lA='ls -alGFh'
alias ll="ls -lh"
alias lr="ls -Rlh"

# editor alias
alias v='nvim'
alias e='emacs -nw'
alias f='ranger'

# tmux aliases
PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

# application aliases
alias r='R --no-save --quiet'
alias julia='/home/sriramkrish92/julia-1.0.0/bin/julia'
alias mat='/home/sriramkrish92/MATLAB/bin/matlab -nosplash -nodesktop'
alias matlab='/home/sriramkrish92/MATLAB/bin/matlab'

# git aliases
alias g='tig'
alias gs='git status'
alias ga='git add'
alias gca='git commit --amend'
alias gcm='git commit -m'
alias gc='git commit'
alias gp='git push'
alias gf='git pull'
alias gcl='git clone'
alias gco='git checkout'
alias gl="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset) %C(bold cyan)(committed: %cD)%C(reset) %C(auto)%d%C(reset)%n''          %C(white)%s%C(reset)%n''          %C(dim white)- %an <%ae> %C(reset) %C(dim white)(committer: %cn <%ce>)%C(reset)'"
alias gb='git branch'
alias gr='git rebase'
alias gi='git rebase --interactive'
alias gm='git remote'
alias gd='git diff'
alias gn='git init'

# latexmk shortcuts
alias mk='latexmk -pvc -pdf'
alias rmtex="rm -rf *.aux *.fdb_latexmk *.fls *.lof *.out *.log *.lot *.pdf *.toc *.bbl *.blg *.synctex.gz"

# valigrind shortcuts
alias valmem='valgrind --leak-check=full --show-leak-kinds=all --dsymutil=yes --leak-check-heuristics=all --log-file=valmem.log --leak-resolution=low'
alias valcall='valgrind --tool=callgrind --log-file=valcall.log'
alias valmass='valgrind --tool=massif --log-file=valmass.log'
alias valhel='valgrind --tool=helgrind --log-file=valhel.log'

# python virtualenv
alias ve='virtualenv'
alias vm='mkvirtualenv'
alias vw='workon'
alias vr='rmvirtualenv'
alias vd='deactivate'

# other convenient aliases
alias pingu='ping -c 25 www.google.com'
alias please='sudo $(fc -ln -1)'
alias cl="clear"

# youtube downloader
alias ytvid="youtube-dl --format=mp4 -o '~/Downloads/MyVideos/%(title)s.%(ext)s'"
alias ytplay="youtube-dl --format=mp4 -o '~/Downloads/MyVideos/%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s'"
alias ytchannel="youtube-dl --format=mp4 -o '~/Downloads/MyVideos/%(uploader)s/%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s'"

# rclone
alias rcr="rclone listremotes"
alias rcconf="rclone config"
alias rcls="rclone ls"
alias rclsd="rclone lsd"
alias rcmkdir="rclone mkdir"
alias rcrmdir="rclone rmdir"
alias rcdel="rclone delete"
alias rcpurge="rclone purge"
alias rccp="rclone copy"
alias rcsync="rclone sync"
