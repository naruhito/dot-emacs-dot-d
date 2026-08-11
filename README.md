# dot-emacs-dot-d

Compatibility: GNU Emacs 30

## Linux

Install with package managers, or build from [source](https://www.gnu.org/software/emacs/download.html):

```
tar zxvf emacs-30.2.tar.gz
cd emacs-30.2/
./configure --prefix=$HOME/local
make
make install
```

## macOS

Download and copy [the binary](https://emacsformacosx.com) into `/Applications`.

## Windows

Download `installer.exe` from [here](https://ftp.gnu.org/gnu/emacs/windows).

Install linux commands using [git for windows](https://gitforwindows.org/).

Set the following environment variables via windows control panel.

- HOME: `C:\Users\YOUR_USERNAME`
- PATH: `C:\Program Files\Git\usr\bin;C:\Program Files\Git\mingw64\bin;C:\Program Files\Git\cmd`

For aspell, install [MSYS2](https://www.msys2.org/), and run the following.

	pacman -S aspell
	pacman -S aspell-en

## Settings

Git clone this repository as `$HOME/.emacs.d`.

Copy and configure the following files:

- `$HOME/.emacs.d/etc/web-bookmarks.el.copy`
- `$HOME/.emacs.d/etc/set-env-vars.el.copy`

GitHub Copilot setup

	M-x copilot-install-server
	M-x copilot-login

Eglot (LSP server) setup

	mkdir -p ~/.emacs.d/.cache/eglot
	cd ~/.emacs.d/.cache/eglot
	npm install pyright

## Tutorials

- Emacs: `M-x help-with-tutorial`
- SKK: `M-x skk-tutorial`
