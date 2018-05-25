# Core packages

Some packages that are pretty much basic and required for every further operation

```
sudo apt-get install openssh-server git wget curl grep gawk sed build-essential tig
```

Basic editor and editor tool support

```
sudo apt-get install neovim vim emacs25 silversearcher-ag ctags cargo tmux editorconfig
```

Subjectively a better shell

```
sudo apt-get install zsh
```

Install emacs configuration

```
git clone --recurse-submodules https://github.com/sriramkswamy/dotemacs.git
```

Install FZF for everything

```
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
```

Install vim/neovim configuration

```
git clone https://github.com/sriramkswamy/dotvim.git ~/.vim
git clone https://github.com/sriramkswamy/dotvim.git ~/.config/nvim
```

Install plugin manager for vim/neovim

```
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

# Dropbox

Install dependencies first

```
sudo apt-get install python-gtk2 libpango1.0-0 python-gpgme
```

Download dropbox install from https://dropbox.com/install-linux and install it

```
sudo dpkg -i dropbox-amd*.deb
```

Start dropbox

```
dropbox start -i
```

# TeX support

Basic TeX packages

```
sudo apt-get install texlive-full latexmk xzdec
```

Initiate the path to install other specific LaTeX packages

```
tlmgr init-usertree
```

# C++

```
sudo apt-get install gcc gdb valgrind llvm lldb clang
```

# Python

```
sudo apt-get install python3 python3-pip
```

Install editor specific stuff

```
python3 -m pip install --user neovim virtualenv
```

# JavaScript

Install nodejs

```
sudo apt-get install nodejs
```

# Visual fluff

Make a better prompt for zsh

```
npm install --global pure-prompt
```
