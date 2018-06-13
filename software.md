# Core packages

Some packages that are pretty much basic and required for every further operation

```
sudo apt-get install openssh-server git wget curl grep gawk sed build-essential tig
```

Basic editor and editor tool support

```
sudo apt-get install neovim vim silversearcher-ag ctags cargo tmux editorconfig
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

# Emacs

Get this whole list of software first

```
sudo apt-get install libc6-dev libjpeg62-turbo-dev libncurses5-dev libpng-dev libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxpm-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev libgnutls28-dev
```

Then configure with a prefix and install it

```
./configure prefix=
make
make install
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

Install `ispell` for Emacs usage

```
sudo apt-get install ispell
```

# C++

```
sudo apt-get install gcc gdb valgrind llvm lldb clang
```

# Python

```
sudo apt-get install python3 python3-pip
```

```
python3 -m pip install --upgrade pip
```

Install editor specific stuff

```
python3 -m pip install --user neovim
```

Install virtualenv for everything

```
python3 -m pip install --user virtualenv
```

Switch to the virtualenv and install required packages

```
python3 -m virtualenv ~/venv/global
source ~/venv/global/bin/activate
pip install -r ~/.emacs.d/requirements-global.txt
```

# JavaScript

Install nodejs

```
sudo apt-get install nodejs
```

# R language

Install the r language

```
sudo apt-get install r-base
```

Install the tidy verse in R

```
install.packages("tidyverse")
```

# Visual fluff

Make a better prompt for zsh

```
npm install --global pure-prompt
```
