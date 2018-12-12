# Linux

First, update the system

```
sudo apt-get update && sudo apt-get upgrade
```

## Core packages

Some packages that are pretty much basic and required for every further operation

```
sudo apt-get install openssh-server git wget curl locate grep gawk sed build-essential tig xclip
```

Basic editor and editor tool support

```
sudo apt-get install vim silversearcher-ag ctags tmux editorconfig
```

Poppler for a lot of things

```
sudo apt-get install poppler-utils libpoppler-private-dev libpoppler-dev libpoppler-glib-dev
```

Plugin support for tmux

```
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

Install FZF for everything

```
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
```

### Vim/Neovim

Neovim pre-requisites

```
sudo apt-get install ninja-build gettext libtool libtool-bin autoconf automake cmake g++ pkg-config unzip
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

Install vim and neovim themselves

```
sudo apt-get install libgtk-3-dev libvte-2.91-dev vim-gtk3 neovim neovim-qt
```

### Emacs

Install emacs configuration

```
git clone --recurse-submodules https://github.com/sriramkswamy/dotemacs.git ~/.emacs.d
```

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

### Visual studio code

Get the dependencies and install code having downloaded the binary from MS's
website

```
sudo apt-get install libnotify4 libnss3 libxkbfile1 libgconf-2-4 libsecret-1-0
```

## Cloud

For cloud services, use `rsync` and `rclone`

## Writing support

Basic TeX packages

```
sudo apt-get install texlive-full latexmk xzdec ispell
```

Initiate the path to install other specific LaTeX packages

```
tlmgr init-usertree
```

Install latex to image converter

```
sudo apt-get install libqt4-sql-sqlite klatexformula
```

Install pdf viewer with synctex support

```
sudo apt-get install zathura zathura-cb zathura-ps zathura-djvu zathura-pdf-poppler zathura-mupdf mupdf mupdf-tools
```

Pandoc to convert between formats

```
sudo apt-get install pandoc
```

## Julia (MATLAB alternative)

Get Julia from their [website](https://julialang.org/downloads/).

```
sudo apt-get install jupyter
```

## Octave (MATLAB alternative)

```
sudo apt-get install octave
```

## C++

```
sudo apt-get install gcc gdb valgrind llvm lldb clang
```

## Python

```
sudo apt-get install python3 python3-pip
```

Install global packages

```
python3 -m pip install --upgrade pip neovim virtualenv virtualenvwrapper
```

Switch to the virtualenv and install required packages

```
python3 -m virtualenv ~/.virtualenvs/global
source ~/.virtualenvs/global/bin/activate
pip install -r ~/.emacs.d/requirements-global.txt
```

## JavaScript

Install nodejs

```
sudo apt-get install nodejs
```

## R language

Install the r language

```
sudo apt-get install r-base
```

Install additional things required for tidyverse

```
sudo apt-get install libcurl4-openssl-dev libxml2-dev libssl-dev
```

Install the tidy verse in R

```
install.packages("tidyverse")
```

## Arduino

For those rare collaborative projects on electronics

```
sudo apt-get install arduino
```

## Other programs

Photo editing and drawing

```
sudo apt-get install inkscape krita darktable mypaint
```

Add Gimp from flatpak though

```
sudo apt-get install flatpak
flatpak install https://flathub.org/repo/appstream/org.gimp.GIMP.flatpakref
```

Audio and Video editing

```
sudo apt-get install audacity kdenlive blender
```

Office suite

```
sudo apt-get install libreoffice
```

Mail reader

```
sudo apt-get install geary
```

Torrents for the very rare occassion

```
sudo apt-get install transmission-qt
```

## Android on Linux (Experimental)

Install snap package support and Android Debug Bridge

```
sudo apt-get install snapd adb
```

Add Anbox kernel modules

```
sudo add-apt-repository ppa:morphis/anbox-support
sudo apt-get update
sudo apt-get install anbox-modules-dkms
```

Install Anbox

```
sudo snap install --devmode --beta anbox
```
# macOS

## Package manager

There is no default package manager on macOS. Homebrew is a good substitute.
Note that this installs homebrew in `/usr/local` directory by default and it
might not be ideal for shared computers.

```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Basic editor and editor tool support

```
brew install silversearcher-ag ctags tmux editorconfig poppler zsh fzf git
```

## Writing support

Basic TeX packages

```
brew install texlive-full latexmk xzdec ispell
```

Initiate the path to install other specific LaTeX packages

```
tlmgr init-usertree
```

Install latex to image converter

```
brew install latexit
```

Pandoc to convert between formats

```
brew install pandoc
```

## Octave (MATLAB alternative)

```
brew install octave
```

## C++

```
brew install gcc gdb valgrind llvm lldb clang
```

## Python

```
brew install python3
```

Install some global packages

```
python3 -m pip install --upgrade pip neovim virtualenv virtualenvwrapper
```

Switch to the virtualenv and install required packages

```
python3 -m virtualenv ~/venv/global
source ~/venv/global/bin/activate
```

Download `requirements-global.txt` from the git repository

```
pip install -r ~/Downloads/requirements-global.txt
```

## JavaScript

Install nodejs

```
brew install nodejs
```

## R language

Install the r language

```
brew install r-base
```

Install the tidy verse in R

```
install.packages("tidyverse")
```
